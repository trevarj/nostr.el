// nostr-loop.js — autonomous open-ended improvement loop for nostr.el
//
// Each round: fan out assessors across lenses (bugs/tests/perf/cleanup/docs),
// pick the highest-value fresh improvement, implement it, verify with the
// project's ERT suite + byte-compile (+ cargo test if Rust changed), and
// GPG-sign a conventional commit. Rolls back on verify failure. Loops until
// K consecutive dry rounds, maxRounds, or token budget exhaustion.
//
// Invoke:  Workflow({ name: 'nostr-loop' })
// Or:      Workflow({ scriptPath: '.claude/workflows/nostr-loop.js' })
//
// Notes:
// - Agents mutate the real working tree (sequential per round; no parallel
//   mutation), so the loop produces real local commits. It never pushes.
// - Commits are GPG-signed per the user's global rules; the committer runs
//   /home/trev/.codex/bin/codex-gpg-unlock and retries if the key is locked.
// - Only offline tests run; live-relay/network tests are skipped.

export const meta = {
  name: 'nostr-loop',
  description: 'Autonomous open-ended improvement loop for nostr.el — assess, implement, verify, sign-commit, until dry',
  whenToUse: 'Run on demand to let agents autonomously improve nostr.el in a loop: find the highest-value change, implement, verify (ERT + byte-compile), and GPG-sign a commit, repeating until nothing actionable is left.',
  phases: [
    { title: 'Baseline', detail: 'record pre-existing test failures + dirty files before touching anything' },
    { title: 'Assess', detail: 'fan out lens assessors, pick highest-value fresh candidate' },
    { title: 'Implement', detail: 'apply the chosen improvement in the working tree' },
    { title: 'Verify', detail: 'ERT + byte-compile (+ cargo test if Rust changed) in guix shell' },
    { title: 'Commit', detail: 'GPG-signed conventional commit, or rollback on verify failure' },
  ],
}

const MAX_ROUNDS = 50 // high backstop; the token budget is the real stop condition
const DRY_THRESHOLD = 2 // consecutive rounds with no fresh candidate => stop
// Per-run output-token allowance. budget.spent() is cumulative for the whole
// turn/session, so we gate on the DELTA spent since this run started — not an
// absolute (which can already be exceeded before round 1).
const TOKEN_BUDGET = budget.total || 120_000 // ~half a Max 5x 5h window

// Optional per-run controls via Workflow args, e.g. { focus: 'UI/UX', maxRounds: 1 }.
// A focus biases every lens toward one theme; maxRounds caps rounds for a short run.
// args may arrive as an object, a JSON string, or a bare focus string — normalize.
let RUN_ARGS = args
if (typeof RUN_ARGS === 'string') {
  const s = RUN_ARGS.trim()
  try {
    RUN_ARGS = s.startsWith('{') ? JSON.parse(s) : { focus: s }
  } catch {
    RUN_ARGS = { focus: s }
  }
}
const RUN_FOCUS = RUN_ARGS && typeof RUN_ARGS === 'object' ? RUN_ARGS.focus || null : null
const ROUND_CAP = Math.min(MAX_ROUNDS, (RUN_ARGS && RUN_ARGS.maxRounds) || MAX_ROUNDS)
const FOCUS_PREAMBLE = RUN_FOCUS
  ? `RUN FOCUS — ${RUN_FOCUS}: This run targets ${RUN_FOCUS} improvements ONLY. Apply your lens specifically to the end-user experience: rendering, layout, spacing, alignment, faces/colors/theming, interaction flow, keybindings, prompts, minibuffer/echo-area feedback, progress and error messaging, and accessibility. If your lens finds nothing that concretely improves ${RUN_FOCUS}, return found=false — do NOT propose unrelated work.\n\n`
  : ''

const LENSES = [
  {
    key: 'bugs',
    prompt: `Lens: correctness bugs. Read the nostr.el Elisp and backend Rust in this repo. Find the single highest-value concrete bug fix: a real defect, edge case, error-handling gap, or incorrect Nostr protocol handling. Prefer self-contained fixes verifiable without network/live relays. Skip anything requiring a running relay. Return one Candidate (found=false if nothing worth doing).`,
  },
  {
    key: 'tests',
    prompt: `Lens: test quality. Read the existing ERT tests (nostr-test.el, tests/*.el). Find the single highest-value concrete test improvement: cover an untested code path, strengthen a weak assertion, or add a regression test for a known-sensitive area. Must run offline under emacs --batch. Return one Candidate (found=false if nothing worth doing).`,
  },
  {
    key: 'perf',
    prompt: `Lens: performance. Read the Elisp hot paths (nostr-db.el queries, nostr-relay.el IO, nostr-ui.el rendering, nostr-timeline.el). Find the single highest-value concrete perf improvement: a quadratic loop, repeated DB round-trip, redundant re-render, or allocation hotspot. Must be safe and verifiable offline. Return one Candidate (found=false if nothing worth doing).`,
  },
  {
    key: 'cleanup',
    prompt: `Lens: dead code & simplification. Read the Elisp modules. Find the single highest-value concrete cleanup: remove dead code/unused bindings, simplify a convoluted helper, deduplicate logic, or fix a byte-compiler warning. Must not change behavior. Return one Candidate (found=false if nothing worth doing).`,
  },
  {
    key: 'docs',
    prompt: `Lens: in-repo documentation & comments. Read AGENTS.md and the Elisp modules. Find the single highest-value concrete doc improvement: a missing/outdated docstring on a public command or function, or an AGENTS.md inaccuracy about the architecture. Per project rules, do NOT create new README/docs files — only improve existing docstrings or AGENTS.md. Return one Candidate (found=false if nothing worth doing).`,
  },
]

const CANDIDATE_SCHEMA = {
  type: 'object',
  additionalProperties: false,
  properties: {
    found: { type: 'boolean', description: 'false if this lens found nothing worth doing this round' },
    title: { type: 'string', description: 'short imperative summary, e.g. "Fix nil pubkey in thread reactions"' },
    lens: { type: 'string' },
    rationale: { type: 'string', description: 'why this is the highest-value change for this lens, grounded in specific code' },
    files: { type: 'array', items: { type: 'string' }, description: 'repo-relative paths that will change' },
    steps: { type: 'string', description: 'concrete implementation steps the implementer should follow' },
    value: { type: 'integer', minimum: 1, maximum: 10, description: 'estimated value of doing this (10 = highest)' },
    risk: { type: 'string', enum: ['low', 'medium', 'high'] },
  },
  required: ['found', 'title', 'lens', 'rationale', 'files', 'steps', 'value', 'risk'],
}

const IMPLEMENT_SCHEMA = {
  type: 'object',
  additionalProperties: false,
  properties: {
    applied: { type: 'boolean' },
    summary: { type: 'string', description: 'what was changed and why' },
    filesChanged: { type: 'array', items: { type: 'string' }, description: 'every file actually modified or created' },
    rustChanged: { type: 'boolean', description: 'true if any .rs file changed (so verifier runs cargo test)' },
  },
  required: ['applied', 'summary', 'filesChanged'],
}

const VERIFY_SCHEMA = {
  type: 'object',
  additionalProperties: false,
  properties: {
    suiteRan: { type: 'boolean', description: 'true if the ERT suite executed at all (even if some tests failed)' },
    failingTests: {
      type: 'array',
      items: { type: 'string' },
      description: 'exact names of every ERT test that FAILS after the change (the symbols ERT reports as failed/unexpected). Empty if all pass.',
    },
    byteCompileWarnings: {
      type: 'array',
      items: { type: 'string' },
      description:
        'every byte-compile warning/error from the changed .el files, normalized as "<file>: <message>" with the :line:col: location stripped. Empty if compilation was clean.',
    },
    rustOk: { type: 'boolean', description: 'true if cargo test passed OR no Rust changed; false only if cargo test failed' },
    summary: { type: 'string' },
    output: { type: 'string', description: 'trimmed key test/compiler output (last ~40 lines)' },
  },
  required: ['suiteRan', 'failingTests', 'byteCompileWarnings', 'summary'],
}

// Captured once before the loop: tests already red on the untouched tree, and
// files with pre-existing uncommitted edits (which the loop must never clobber).
const BASELINE_SCHEMA = {
  type: 'object',
  additionalProperties: false,
  properties: {
    suiteRan: { type: 'boolean' },
    failingTests: {
      type: 'array',
      items: { type: 'string' },
      description: 'exact names of every ERT test that fails on the UNMODIFIED tree',
    },
    byteCompileWarnings: {
      type: 'array',
      items: { type: 'string' },
      description:
        'every pre-existing byte-compile warning/error across all source files, normalized as "<file>: <message>" with the :line:col: location stripped',
    },
    dirtyFiles: {
      type: 'array',
      items: { type: 'string' },
      description: 'repo-relative paths reported by `git status --porcelain` that already have uncommitted changes',
    },
    note: { type: 'string' },
  },
  required: ['suiteRan', 'failingTests', 'byteCompileWarnings', 'dirtyFiles'],
}

const COMMIT_SCHEMA = {
  type: 'object',
  additionalProperties: false,
  properties: {
    committed: { type: 'boolean' },
    rolledBack: { type: 'boolean', description: 'true if verify failed and changes were reverted instead' },
    hash: { type: 'string' },
    message: { type: 'string' },
    note: { type: 'string' },
  },
  required: ['committed'],
}

const RISK_RANK = { low: 0, medium: 1, high: 2 }

function key(c) {
  return (c.title || '').toLowerCase().trim() + '|' + (c.files || []).slice().sort().join(',')
}

// --- Baseline: record what's already broken / dirty before we touch anything ---
phase('Baseline')
const baselineResult = await agent(
  `Establish a baseline BEFORE any changes. Do NOT modify, stage, or revert any file.\n\n` +
    `1. List files with pre-existing uncommitted changes:  git status --porcelain\n` +
    `2. Run the offline ERT suite from the repo root:\n` +
    `   guix shell -m manifest.scm -- emacs --batch -L . -l nostr-test.el -f ert-run-tests-batch-and-exit\n` +
    `3. Byte-compile ALL source files to capture pre-existing warnings, then remove the .elc:\n` +
    `   guix shell -m manifest.scm -- emacs --batch -L . -f batch-byte-compile *.el tests/*.el ; rm -f *.elc tests/*.elc\n\n` +
    `Report in failingTests the exact symbol name of every ERT test that FAILS on this untouched tree (these are known-preexisting and the loop must tolerate them). ` +
    `Report in byteCompileWarnings every warning/error line from step 3, each normalized as "<file>: <message>" with the ":line:col:" location stripped (so they stay comparable across edits) — these are known-preexisting and the loop must tolerate them. ` +
    `Report in dirtyFiles the repo-relative path of every file git status shows as modified/added/untracked. ` +
    `Set suiteRan=true if ERT executed at all. Do not commit anything.`,
  { label: 'baseline', phase: 'Baseline', schema: BASELINE_SCHEMA }
)
const baselineFails = new Set((baselineResult && baselineResult.failingTests) || [])
const baselineWarnings = new Set((baselineResult && baselineResult.byteCompileWarnings) || [])
// Protect pre-existing uncommitted work: match by basename so abs/rel paths both hit.
const protectedFiles = new Set(
  ((baselineResult && baselineResult.dirtyFiles) || [])
    .map((f) => f.split('/').pop())
    .filter(Boolean)
)
log(
  `baseline: ${baselineFails.size} pre-existing failing test(s)` +
    (baselineFails.size ? ` [${[...baselineFails].join(', ')}]` : '') +
    `; ${baselineWarnings.size} pre-existing byte-compile warning(s)` +
    `; ${protectedFiles.size} dirty file(s) protected from rollback` +
    (protectedFiles.size ? ` [${[...protectedFiles].join(', ')}]` : '')
)

phase('Assess')

const seen = new Set()
const done = []
let dry = 0
let round = 0
const startSpent = budget.spent() // baseline the cumulative counter; gate on delta from here

if (RUN_FOCUS) log(`run focus: ${RUN_FOCUS}; round cap: ${ROUND_CAP}`)

while (round < ROUND_CAP && budget.spent() - startSpent < TOKEN_BUDGET) {
  round++
  log(`--- round ${round}/${ROUND_CAP} — ${Math.round((budget.spent() - startSpent) / 1000)}k/${Math.round(TOKEN_BUDGET / 1000)}k tokens this run — assessing ${LENSES.length} lenses ---`)

  // Fan out one assessor per lens; collect fresh, non-duplicate candidates.
  const assessed = (await parallel(
    LENSES.map((l) => () =>
      agent(FOCUS_PREAMBLE + l.prompt, { label: `assess:${l.key}`, phase: 'Assess', schema: CANDIDATE_SCHEMA })
    )
  )).filter(Boolean)

  const found = assessed.filter((c) => c && c.found)
  const fresh = found.filter((c) => !seen.has(key(c)))

  // Mark everything found this round as seen so repeats across lenses count as dry.
  found.forEach((c) => seen.add(key(c)))

  if (!fresh.length) {
    dry++
    log(`no fresh candidates this round (dry streak ${dry}/${DRY_THRESHOLD})`)
    if (dry >= DRY_THRESHOLD) {
      log(`dry for ${DRY_THRESHOLD} consecutive rounds — stopping.`)
      break
    }
    continue
  }
  dry = 0

  // Never touch files that already had uncommitted changes: a failed round's
  // rollback (git checkout) would clobber the user's work. This guard prevents
  // exactly the data-loss that happened on the first run.
  const touchesProtected = (c) =>
    (c.files || []).some((f) => protectedFiles.has(f.split('/').pop()))
  const safe = fresh.filter((c) => {
    if (touchesProtected(c)) {
      log(`skipping [${c.lens}] "${c.title}" — touches protected dirty file(s): ${c.files.join(', ')}`)
      return false
    }
    return true
  })
  if (!safe.length) {
    log(`all fresh candidates touch protected files this round — skipping`)
    continue
  }

  // Highest value first, then lowest risk as tiebreaker.
  safe.sort((a, b) => b.value - a.value || RISK_RANK[a.risk] - RISK_RANK[b.risk])
  const best = safe[0]
  log(`picked [${best.lens}] "${best.title}" (value ${best.value}, risk ${best.risk}) — ${best.files.join(', ')}`)

  // Implement the chosen improvement.
  phase('Implement')
  const impl = await agent(
    `Implement this improvement in the working tree, following the repo's AGENTS.md rules:\n\n` +
      `Title: ${best.title}\nLens: ${best.lens}\nRationale: ${best.rationale}\n` +
      `Files: ${best.files.join(', ')}\nSteps: ${best.steps}\nRisk: ${best.risk}\n\n` +
      `Rules:\n` +
      `- Make the minimal contained change. Do not touch unrelated code.\n` +
      `- Mimic existing style; do not introduce new patterns or libraries.\n` +
      `- Do NOT run live-relay/network tests. Do NOT leave generated .elc files or Cargo target/ dirs.\n` +
      `- Edit existing files in place; do not create new files unless the steps explicitly require it.\n` +
      `- Report every file you actually modified or created in filesChanged, and set rustChanged if any .rs file changed.\n`,
    { label: `implement:${best.lens}`, phase: 'Implement', schema: IMPLEMENT_SCHEMA }
  )

  if (!impl || !impl.applied) {
    log(`implementer reported applied=false — skipping verify/commit`)
    continue
  }

  // Verify.
  phase('Verify')
  const verify = await agent(
    `Verify the working-tree changes by running the project's offline tests. The repo uses Guix for deps.\n\n` +
      `Run, in order, from the repo root (use a single combined guix shell where possible):\n` +
      `1. ERT suite:  guix shell -m manifest.scm -- emacs --batch -L . -l nostr-test.el -f ert-run-tests-batch-and-exit\n` +
      `2. Byte-compile changed files to catch warnings, then REMOVE any .elc you produced:\n` +
      `   guix shell -m manifest.scm -- emacs --batch -L . -f batch-byte-compile ${impl.filesChanged.join(' ')} ; rm -f ${impl.filesChanged.map((f) => f.replace(/\.el$/, '.elc')).join(' ')}\n` +
      (impl.rustChanged
        ? `3. Backend tests (Rust changed):  guix shell -m manifest.scm -- bash -c 'cd backend && cargo test'\n`
        : ``) +
      `\nDo NOT run any live-relay or network test files. If a guix shell or emacs invocation is missing the binary, set suiteRan=false with the error in output rather than hanging.\n` +
      `Report FACTS, not a verdict (the loop decides pass/fail by comparing against a known baseline):\n` +
      `- failingTests: every failing ERT test by exact name.\n` +
      `- byteCompileWarnings: every warning/error from step 2, each normalized as "<file>: <message>" with the ":line:col:" location stripped.\n` +
      `- rustOk: true if cargo passed or no Rust changed.\n` +
      `- output: the last ~40 lines of combined output.\n`,
    { label: `verify:${best.lens}`, phase: 'Verify', schema: VERIFY_SCHEMA }
  )

  // Pass = the change introduced NO new test failures and NO new byte-compile
  // warnings vs the baseline, and (if Rust changed) cargo passed. Pre-existing
  // red tests / warnings are tolerated so good fixes are no longer discarded.
  const afterFails = new Set((verify && verify.failingTests) || [])
  const newFailures = [...afterFails].filter((t) => !baselineFails.has(t))
  const fixedFailures = [...baselineFails].filter((t) => !afterFails.has(t))
  const newWarnings = ((verify && verify.byteCompileWarnings) || []).filter((w) => !baselineWarnings.has(w))
  const passed = !!(
    verify &&
    verify.suiteRan &&
    verify.rustOk !== false &&
    newFailures.length === 0 &&
    newWarnings.length === 0
  )
  if (verify && newFailures.length) log(`change introduced ${newFailures.length} new failure(s): ${newFailures.join(', ')}`)
  if (newWarnings.length) log(`change introduced ${newWarnings.length} new byte-compile warning(s): ${newWarnings.join(', ')}`)
  if (fixedFailures.length) log(`change also fixed ${fixedFailures.length} pre-existing failure(s): ${fixedFailures.join(', ')}`)

  // Commit on pass, roll back on fail.
  phase('Commit')
  const finalize = await agent(
    (passed
      ? `Verification PASSED. Commit the working-tree changes.\n\n`
      : `Verification FAILED. Roll back the working-tree changes; do NOT commit.\n\n`) +
      `Verify summary: ${verify ? verify.summary : '(no verify result)'}\n` +
      `Files changed: ${(impl.filesChanged || []).join(', ')}\n\n` +
      (passed
        ? `Commit steps:\n` +
          `- Stage exactly these files: git add -- ${(impl.filesChanged || []).join(' ')}\n` +
          `- Generate a Conventional Commit message (type(scope): summary + concise bullet body) from the staged diff. No Co-Authored-By/attribution trailers. No ticket refs.\n` +
          `- Commit with a GPG signature: git commit -S -m "<msg>" (use a multi-line -F file if the body is long).\n` +
          `- If signing fails because the GPG key is locked, run /home/trev/.codex/bin/codex-gpg-unlock, then retry the signed commit. Repeat unlock+retry up to 3 times. Never make an unsigned commit — if it still fails, set committed=false and stop.\n` +
          `- Do NOT push.\n` +
          `- Remove any stray .elc files before committing.\n` +
          `- Return committed=true with the commit hash and message.`
        : `Rollback steps:\n` +
          `- For each file in the changed list: if tracked-modified, run git checkout -- <file>; if untracked/new, run rm -f <file>.\n` +
          `- Remove any .elc files produced for these files.\n` +
          `- Confirm the tree is clean: git status --porcelain -- ${(impl.filesChanged || []).join(' ')}\n` +
          `- Return committed=false, rolledBack=true, with a note. Do not commit anything.\n`),
    { label: passed ? `commit:${best.lens}` : `rollback:${best.lens}`, phase: 'Commit', schema: COMMIT_SCHEMA }
  )

  if (finalize && finalize.committed) {
    done.push({ round, lens: best.lens, title: best.title, hash: finalize.hash, message: finalize.message })
    log(`committed ${finalize.hash || ''} — ${finalize.message ? finalize.message.split('\n')[0] : best.title}`)
  } else if (finalize && finalize.rolledBack) {
    log(`verify failed — rolled back. note: ${finalize.note || ''}`)
  } else {
    log(`finalize step did not commit or roll back cleanly — continuing`)
  }
}

log(`=== nostr-loop complete: ${done.length} improvement(s) committed over ${round} round(s) ===`)
return { rounds: round, committed: done, dryStreak: dry }