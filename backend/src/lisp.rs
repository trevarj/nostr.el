//! Emacs-Lisp `prin1`-compatible encoding for SQLite column values.
//!
//! `emacsql-sqlite` writes each value as `(prin1-to-string value)` and reads it
//! back with `read`. To stay byte-compatible with the Elisp read path, the
//! daemon emits the same printed forms:
//!
//! * strings: double-quoted, with only `"` -> `\"` and `\` -> `\\` escaped.
//!   Newlines, tabs and CR are emitted literally (this matches `prin1`).
//! * lists of tag strings: `(("e" "abc") ("p" "xyz"))`.
//! * empty tag list -> SQL NULL (Emacs reads it back as `nil`).

use serde_json::Value;

/// Encode `s` as an Emacs-Lisp `prin1` string literal.
pub fn prin1_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            // `prin1` emits newlines, tabs and CR literally; everything else
            // (including arbitrary UTF-8) passes through unchanged.
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}

/// Decode an Emacs-Lisp `prin1` string literal back to its contents.
///
/// Inverse of [`prin1_string`]: unwraps the surrounding quotes and unescapes
/// `\"` and `\\`. Returns `None` when `s` is not a quoted string literal (e.g. a
/// SQL value that was stored as a bare symbol or integer). Used to read a stored
/// pubkey back out of the cache for in-daemon comparisons.
pub fn read_string(s: &str) -> Option<String> {
    let inner = s.strip_prefix('"')?.strip_suffix('"')?;
    let mut out = String::with_capacity(inner.len());
    let mut chars = inner.chars();
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            // `prin1` only escapes `"` and `\`; keep any other pair verbatim.
            match chars.next() {
                Some(next) => out.push(next),
                None => out.push('\\'),
            }
        } else {
            out.push(ch);
        }
    }
    Some(out)
}

/// Encode a JSON tags array as an Emacs-Lisp list of string lists.
///
/// Returns `None` for an empty array so the caller binds SQL NULL, matching
/// `emacsql` storing an empty list as NULL (read back as `nil`).
pub fn prin1_tags(tags: &Value) -> Option<String> {
    let arr = tags.as_array()?;
    if arr.is_empty() {
        return None;
    }
    let mut out = String::from("(");
    for (idx, tag) in arr.iter().enumerate() {
        if idx > 0 {
            out.push(' ');
        }
        let inner = tag.as_array();
        out.push('(');
        if let Some(parts) = inner {
            for (i, part) in parts.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                out.push_str(&prin1_string(part.as_str().unwrap_or("")));
            }
        }
        out.push(')');
    }
    out.push(')');
    Some(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prin1_string_round_trips_through_read_string() {
        for s in [
            "",
            "alice",
            "hello\nworld",
            "a\"b\\c",
            "wss://relay.example/x",
        ] {
            assert_eq!(read_string(&prin1_string(s)).as_deref(), Some(s));
        }
    }

    #[test]
    fn read_string_rejects_non_string_literals() {
        // Bare symbols/integers (e.g. an unencoded value) are not string literals.
        assert_eq!(read_string("mention"), None);
        assert_eq!(read_string("42"), None);
    }
}
