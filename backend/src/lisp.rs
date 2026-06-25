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