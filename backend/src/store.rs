//! SQLite cache writer for the relay daemon.
//!
//! The Emacs side owns the schema (`nostr-db-init`) and reads the cache back
//! via `emacsql`. `emacsql-sqlite` encodes every column value as its Emacs-Lisp
//! `prin1` printed form and decodes via `read`, so to stay byte-compatible with
//! the Elisp read path this writer stores:
//!
//! * text columns as the `prin1` string form (double-quoted, only `"` and `\`
//!   escaped; newlines/tabs/CR emitted literally, matching `prin1`), and
//! * integer columns as bare integers.
//!
//! See `lisp::prin1_string` / `lisp::prin1_tags`. A shared schema-shape test
//! guards against drift between this module and `nostr-db-init`.

use rusqlite::{params, Connection, OptionalExtension};
use serde_json::Value;

use crate::lisp;

// Nostr kinds, mirroring `nostr-event.el`.
pub const KIND_METADATA: u16 = 0;
pub const KIND_TEXT_NOTE: u16 = 1;
pub const KIND_CONTACTS: u16 = 3;
pub const KIND_REPOST: u16 = 6;
pub const KIND_REACTION: u16 = 7;
pub const KIND_ZAP_RECEIPT: u16 = 9735;
pub const KIND_MUTE_LIST: u16 = 10000;
pub const KIND_RELAY_LIST: u16 = 10002;

/// Open the cache and enable concurrent-reader-friendly pragmas.
///
/// WAL mode is persistent on the file once set, so Emacs's `emacsql` connection
/// and this connection can interleave reads/writes. `busy_timeout` makes both
/// sides wait briefly instead of erroring on lock contention.
pub fn open(path: &str) -> rusqlite::Result<Connection> {
    let conn = Connection::open(path)?;
    conn.pragma_update(None, "journal_mode", "WAL")?;
    conn.pragma_update(None, "busy_timeout", 5000)?;
    conn.pragma_update(None, "synchronous", "NORMAL")?;
    Ok(conn)
}

/// Create the cache schema if absent. Mirrors `nostr-db-init`.
///
/// Emacs creates the schema first in normal operation; this is the test
/// bootstrap and a safety net so the daemon never writes into a missing table.
pub fn init_schema(conn: &Connection) -> rusqlite::Result<()> {
    conn.execute_batch(
        "create table if not exists meta (key primary key, value);
         insert or ignore into meta (key, value) values ('schema-version', '1');
         create table if not exists events (
             id primary key, pubkey, created_at integer, kind integer,
             tags, content, sig, relay, root_id, reply_id, quote_id);
         create index if not exists idx_events_created_at on events (created_at);
         create index if not exists idx_events_pubkey on events (pubkey);
         create index if not exists idx_events_kind on events (kind);
         create index if not exists idx_events_root_id on events (root_id);
         create table if not exists event_relays (
             event_id, url, seen_at integer, unique (event_id, url));
         create index if not exists idx_event_relays_event on event_relays (event_id);
         create table if not exists profiles (
             pubkey primary key, name, display_name, about, picture,
             nip05, lud16, content, updated_at integer);
         create table if not exists follows (
             pubkey, contact, relay, petname, unique (pubkey, contact));
         create table if not exists mutes (pubkey, muted_pubkey, unique (pubkey, muted_pubkey));
         create index if not exists idx_mutes_pubkey on mutes (pubkey);
         create table if not exists reactions (
             id primary key, event_id, pubkey, content, created_at integer);
         create index if not exists idx_reactions_event_id on reactions (event_id);
         create table if not exists reposts (
             id primary key, event_id, pubkey, created_at integer);
         create table if not exists zaps (
             id primary key, event_id, pubkey, amount_msats integer, created_at integer);
         create index if not exists idx_zaps_event_id on zaps (event_id);
         create table if not exists relay_status (
             url primary key, state, message, updated_at integer);
         create table if not exists publish_receipts (
             event_id, url, state, message, updated_at integer,
             unique (event_id, url));
         create index if not exists idx_publish_receipts_event on publish_receipts (event_id);
         create table if not exists relay_preferences (
             pubkey, url, marker, read integer, write integer,
             unique (pubkey, url, marker));
         create index if not exists idx_relay_preferences_pubkey on relay_preferences (pubkey);
         create table if not exists notifications (
             id primary key, type, event_id, actor_pubkey, target_pubkey,
             created_at integer, seen integer default 0);
         create table if not exists media (
             url primary key, path, content_type, bytes integer, state, message, updated_at integer);
         create table if not exists discover_results (
             provider, scope, timeframe, event_id, rank integer, cursor integer,
             fetched_at integer, unique (provider, scope, timeframe, event_id));
         create index if not exists idx_discover_results_feed
             on discover_results (provider, scope, timeframe, rank);
         create table if not exists discover_stats (
             event_id primary key, likes integer, replies integer, reposts integer,
             zaps integer, satszapped integer, score integer, score24h integer,
             fetched_at integer);",
    )?;
    Ok(())
}

/// Inbound event delivered by `relay_url` (may be None when the source is
/// unknown). Dispatches by kind exactly like `nostr-db-store-event`.
pub fn store_event(conn: &Connection, event: &Value, relay_url: Option<&str>) -> rusqlite::Result<()> {
    store_event_relay(conn, event, relay_url)?;
    let kind = event["kind"].as_u64().map(|k| k as u16).unwrap_or(0);
    match kind {
        KIND_METADATA => store_profile_event(conn, event),
        KIND_TEXT_NOTE => store_text_event(conn, event, relay_url),
        KIND_CONTACTS => store_follows_event(conn, event),
        KIND_REPOST => {
            store_repost_event(conn, event)?;
            store_text_event(conn, event, relay_url)
        }
        KIND_REACTION => {
            store_reaction_event(conn, event)?;
            store_text_event(conn, event, relay_url)
        }
        KIND_ZAP_RECEIPT => {
            store_zap_event(conn, event)?;
            store_text_event(conn, event, relay_url)
        }
        KIND_MUTE_LIST => store_mute_list_event(conn, event),
        KIND_RELAY_LIST => store_relay_list_event(conn, event),
        _ => store_text_event(conn, event, relay_url),
    }
}

/// Record the inbound relay that delivered `event`, mirroring
/// `nostr-db-store-event-relay`.
pub fn store_event_relay(
    conn: &Connection,
    event: &Value,
    relay_url: Option<&str>,
) -> rusqlite::Result<()> {
    let Some(id) = event["id"].as_str() else {
        return Ok(());
    };
    let Some(relay) = relay_url else {
        return Ok(());
    };
    let now = unix_now();
    conn.execute(
        "insert or replace into event_relays (event_id, url, seen_at) values (?, ?, ?)",
        params![id, relay, now],
    )?;
    Ok(())
}

/// Store a kind-1 (or generic) text event with NIP-10 root/reply/quote ids.
pub fn store_text_event(
    conn: &Connection,
    event: &Value,
    relay_url: Option<&str>,
) -> rusqlite::Result<()> {
    let id = as_str_or_empty(event["id"].as_str());
    let pubkey = as_str_or_empty(event["pubkey"].as_str());
    let created_at = event["created_at"].as_u64().unwrap_or(0) as i64;
    let kind = event["kind"].as_u64().unwrap_or(0) as i64;
    let tags = lisp::prin1_tags(&event["tags"]);
    let content = lisp::prin1_string(event["content"].as_str().unwrap_or(""));
    let sig = lisp::prin1_string(event["sig"].as_str().unwrap_or(""));
    let relay = relay_url.map(lisp::prin1_string).unwrap_or_else(|| "nil".into());
    // emacsql stores missing `nil` columns as SQL NULL (read back as nil). Bind
    // Option<String> None for absent root/reply/quote ids.
    let root_id = root_id(event).map(|s| s.to_string());
    let reply_id = reply_id(event).map(|s| s.to_string());
    let quote_id = quote_id(event).map(|s| s.to_string());

    conn.execute(
        "insert or replace into events
         (id, pubkey, created_at, kind, tags, content, sig, relay, root_id, reply_id, quote_id)
         values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params![
            id,
            pubkey,
            created_at,
            kind,
            tags,
            content,
            sig,
            relay,
            root_id,
            reply_id,
            quote_id,
        ],
    )?;
    Ok(())
}

/// Store kind-0 profile metadata, keeping only the newest event per pubkey.
pub fn store_profile_event(conn: &Connection, event: &Value) -> rusqlite::Result<()> {
    let pubkey = match event["pubkey"].as_str() {
        Some(p) => p,
        None => return Ok(()),
    };
    let content = event["content"].as_str().unwrap_or("");
    let created_at = event["created_at"].as_u64().unwrap_or(0) as i64;

    let existing: Option<i64> = conn
        .query_row(
            "select updated_at from profiles where pubkey = ?",
            params![pubkey],
            |r| r.get(0),
        )
        .optional()?;
    if let Some(prev) = existing {
        if created_at < prev {
            return Ok(());
        }
    }

    let meta = serde_json::from_str::<Value>(content).ok();
    let name = profile_field(&meta, &["name", "username"]);
    let display_name = profile_field(&meta, &["display_name", "displayName"]);
    let about = profile_field(&meta, &["about"]);
    let picture = profile_field(&meta, &["picture"]);
    let nip05 = profile_field(&meta, &["nip05"]);
    let lud16 = profile_field(&meta, &["lud16"]);

    conn.execute(
        "insert or replace into profiles
         (pubkey, name, display_name, about, picture, nip05, lud16, content, updated_at)
         values (?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params![
            pubkey,
            name,
            display_name,
            about,
            picture,
            nip05,
            lud16,
            lisp::prin1_string(content),
            created_at,
        ],
    )?;
    Ok(())
}

/// Store kind-3 contact list, replacing the prior set.
pub fn store_follows_event(conn: &Connection, event: &Value) -> rusqlite::Result<()> {
    let pubkey = match event["pubkey"].as_str() {
        Some(p) => p,
        None => return Ok(()),
    };
    let tx = conn.unchecked_transaction()?;
    tx.execute("delete from follows where pubkey = ?", params![pubkey])?;
    for tag in tags_by_name(event, "p") {
        if tag.len() < 2 {
            continue;
        }
        let contact = tag[1].as_str().unwrap_or("");
        let relay = tag.get(2).and_then(|v| v.as_str()).unwrap_or("");
        let petname = tag.get(3).and_then(|v| v.as_str()).unwrap_or("");
        tx.execute(
            "insert or ignore into follows (pubkey, contact, relay, petname) values (?, ?, ?, ?)",
            params![pubkey, contact, relay, petname],
        )?;
    }
    tx.commit()?;
    Ok(())
}

/// Store a kind-7 reaction row targeting the last e-tagged event (NIP-25).
pub fn store_reaction_event(conn: &Connection, event: &Value) -> rusqlite::Result<()> {
    let Some(target) = reaction_event_id(event) else {
        return Ok(());
    };
    let id = as_str_or_empty(event["id"].as_str());
    let pubkey = as_str_or_empty(event["pubkey"].as_str());
    let content = event["content"].as_str().unwrap_or("");
    let created_at = event["created_at"].as_u64().unwrap_or(0) as i64;
    conn.execute(
        "insert or ignore into reactions (id, event_id, pubkey, content, created_at)
         values (?, ?, ?, ?, ?)",
        params![id, target, pubkey, content, created_at],
    )?;
    Ok(())
}

/// Store a kind-6 repost row targeting the first e-tagged event (NIP-18).
pub fn store_repost_event(conn: &Connection, event: &Value) -> rusqlite::Result<()> {
    let Some(target) = repost_event_id(event) else {
        return Ok(());
    };
    let id = as_str_or_empty(event["id"].as_str());
    let pubkey = as_str_or_empty(event["pubkey"].as_str());
    let created_at = event["created_at"].as_u64().unwrap_or(0) as i64;
    conn.execute(
        "insert or ignore into reposts (id, event_id, pubkey, created_at) values (?, ?, ?, ?)",
        params![id, target, pubkey, created_at],
    )?;
    Ok(())
}

/// Store a kind-9735 zap receipt row.
pub fn store_zap_event(conn: &Connection, event: &Value) -> rusqlite::Result<()> {
    let Some(target) = zap_target_event_id(event) else {
        return Ok(());
    };
    let id = as_str_or_empty(event["id"].as_str());
    let pubkey = as_str_or_empty(event["pubkey"].as_str());
    let amount = zap_amount_msats(event).unwrap_or(0);
    let created_at = event["created_at"].as_u64().unwrap_or(0) as i64;
    conn.execute(
        "insert or ignore into zaps (id, event_id, pubkey, amount_msats, created_at)
         values (?, ?, ?, ?, ?)",
        params![id, target, pubkey, amount, created_at],
    )?;
    Ok(())
}

/// Store a kind-10002 NIP-65 relay list, replacing the prior set.
pub fn store_relay_list_event(conn: &Connection, event: &Value) -> rusqlite::Result<()> {
    let pubkey = match event["pubkey"].as_str() {
        Some(p) => p,
        None => return Ok(()),
    };
    let tx = conn.unchecked_transaction()?;
    tx.execute("delete from relay_preferences where pubkey = ?", params![pubkey])?;
    for tag in tags_by_name(event, "r") {
        let Some(url) = tag.get(1).and_then(|v| v.as_str()) else {
            continue;
        };
        if url.is_empty() {
            continue;
        }
        let marker = tag.get(2).and_then(|v| v.as_str()).unwrap_or("");
        let (read, write) = relay_list_policy(marker);
        tx.execute(
            "insert or replace into relay_preferences (pubkey, url, marker, read, write)
             values (?, ?, ?, ?, ?)",
            params![pubkey, url, marker, read, write],
        )?;
    }
    tx.commit()?;
    Ok(())
}

/// Store a kind-10000 NIP-51 mute list, replacing the prior set.
pub fn store_mute_list_event(conn: &Connection, event: &Value) -> rusqlite::Result<()> {
    let pubkey = match event["pubkey"].as_str() {
        Some(p) => p,
        None => return Ok(()),
    };
    let tx = conn.unchecked_transaction()?;
    tx.execute("delete from mutes where pubkey = ?", params![pubkey])?;
    for tag in tags_by_name(event, "p") {
        if let Some(muted) = tag.get(1).and_then(|v| v.as_str()) {
            if !muted.is_empty() {
                tx.execute(
                    "insert or ignore into mutes (pubkey, muted_pubkey) values (?, ?)",
                    params![pubkey, muted],
                )?;
            }
        }
    }
    tx.commit()?;
    Ok(())
}

/// Derive and store notifications caused by `event` for `current_pubkey`.
/// Mirrors `nostr-relay--maybe-store-notification`.
pub fn maybe_store_notification(
    conn: &Connection,
    event: &Value,
    current_pubkey: Option<&str>,
) -> rusqlite::Result<()> {
    let Some(me) = current_pubkey else {
        return Ok(());
    };
    let event_id = match event["id"].as_str() {
        Some(s) => s,
        None => return Ok(()),
    };
    let pubkey = as_str_or_empty(event["pubkey"].as_str());
    let created_at = event["created_at"].as_u64().unwrap_or(0) as i64;
    let kind = event["kind"].as_u64().unwrap_or(0) as u16;

    let notif = |t: &str, target: Option<&str>| {
        let id = format!("{event_id}-{t}");
        conn.execute(
            "insert or ignore into notifications
             (id, type, event_id, actor_pubkey, target_pubkey, created_at, seen) \
             values (?, ?, ?, ?, ?, ?, 0)",
            params![id, t, event_id, pubkey, target.unwrap_or(""), created_at],
        )
    };

    match kind {
        KIND_TEXT_NOTE => {
            if mentioned_pubkeys(event).iter().any(|p| p == me) {
                notif("mention", None)?;
            }
            if let Some(reply) = reply_id(event) {
                if event_pubkey(conn, reply)? == Some(me.to_string()) {
                    notif("reply", None)?;
                }
            }
        }
        KIND_REACTION => {
            if let Some(target) = reaction_event_id(event) {
                if event_pubkey(conn, target)? == Some(me.to_string()) {
                    notif("reaction", None)?;
                }
            }
        }
        KIND_REPOST => {
            if let Some(target) = repost_event_id(event) {
                if event_pubkey(conn, target)? == Some(me.to_string()) {
                    notif("repost", None)?;
                }
            }
        }
        KIND_ZAP_RECEIPT => {
            if let Some(target) = zap_target_event_id(event) {
                if event_pubkey(conn, &target)? == Some(me.to_string()) {
                    notif("zap", None)?;
                }
            }
        }
        KIND_CONTACTS => {
            if mentioned_pubkeys(event).iter().any(|p| p == me) {
                notif("follow", None)?;
            }
        }
        _ => {}
    }
    Ok(())
}

/// Store a relay connection/EOSE/error status row.
pub fn store_relay_status(
    conn: &Connection,
    url: &str,
    state: &str,
    message: Option<&str>,
) -> rusqlite::Result<()> {
    conn.execute(
        "insert or replace into relay_status (url, state, message, updated_at) values (?, ?, ?, ?)",
        params![url, state, message.unwrap_or(""), unix_now()],
    )?;
    Ok(())
}

/// Store a publish receipt for an event id on a relay.
pub fn store_publish_receipt(
    conn: &Connection,
    event_id: &str,
    url: &str,
    state: &str,
    message: Option<&str>,
) -> rusqlite::Result<()> {
    conn.execute(
        "insert or replace into publish_receipts (event_id, url, state, message, updated_at)
         values (?, ?, ?, ?, ?)",
        params![event_id, url, state, message.unwrap_or(""), unix_now()],
    )?;
    Ok(())
}

/// Set a `meta` key/value pair (used for `syncing` state).
pub fn set_meta(conn: &Connection, key: &str, value: &str) -> rusqlite::Result<()> {
    conn.execute(
        "insert or replace into meta (key, value) values (?, ?)",
        params![key, value],
    )?;
    Ok(())
}

/// Return the pubkey that authored `event_id`, or None when not cached.
pub fn event_pubkey(conn: &Connection, event_id: &str) -> rusqlite::Result<Option<String>> {
    let pk: Option<String> = conn
        .query_row(
            "select pubkey from events where id = ?",
            params![event_id],
            |r| r.get(0),
        )
        .optional()?;
    Ok(pk)
}

// ----- tag extraction, mirroring `nostr-event.el` ----------------------------

/// Return the raw tag arrays whose first element equals `name`.
pub fn tags_by_name<'a>(event: &'a Value, name: &str) -> Vec<&'a Vec<Value>> {
    event["tags"]
        .as_array()
        .map(|tags| {
            tags.iter()
                .filter_map(|t| {
                    t.as_array().filter(|a| {
                        a.first().and_then(|v| v.as_str()) == Some(name)
                    })
                })
                .collect()
        })
        .unwrap_or_default()
}

/// First value of the first tag named `name` (the second element).
pub fn first_tag_value<'a>(event: &'a Value, name: &str) -> Option<&'a str> {
    tags_by_name(event, name)
        .first()
        .and_then(|t| t.get(1))
        .and_then(|v| v.as_str())
}

/// NIP-10 root event id: first `:root`-marker e-tag, else the first e-tag.
pub fn root_id(event: &Value) -> Option<&str> {
    e_tags_by_marker(event, "root")
        .first()
        .copied()
        .or_else(|| tags_by_name(event, "e").first().and_then(|t| t.get(1)).and_then(|v| v.as_str()))
}

/// NIP-10 direct reply id: first `:reply`-marker e-tag, else the last of >=2
/// unmarked e-tags (deprecated positional scheme).
pub fn reply_id(event: &Value) -> Option<&str> {
    if let Some(id) = e_tags_by_marker(event, "reply").first().copied() {
        return Some(id);
    }
    let plain: Vec<&Vec<Value>> = tags_by_name(event, "e")
        .into_iter()
        .filter(|t| t.get(3).and_then(|v| v.as_str()).is_none())
        .collect();
    if plain.len() >= 2 {
        plain
            .last()
            .and_then(|t| t.get(1))
            .and_then(|v| v.as_str())
    } else {
        None
    }
}

/// Quoted event id: first `q` tag value.
pub fn quote_id(event: &Value) -> Option<&str> {
    first_tag_value(event, "q")
}

/// NIP-25 reaction target: last e-tag value.
pub fn reaction_event_id(event: &Value) -> Option<&str> {
    tags_by_name(event, "e")
        .last()
        .and_then(|t| t.get(1))
        .and_then(|v| v.as_str())
}

/// NIP-18 repost target: first e-tag value.
pub fn repost_event_id(event: &Value) -> Option<&str> {
    first_tag_value(event, "e")
}

/// NIP-57 zap target: first e-tag value, or the `e` tag inside the JSON
/// `description` tag's zap request. Owned because the description path parses
/// a local JSON value.
pub fn zap_target_event_id(event: &Value) -> Option<String> {
    if let Some(id) = first_tag_value(event, "e") {
        return Some(id.to_string());
    }
    let desc = first_tag_value(event, "description")?;
    let req = serde_json::from_str::<Value>(desc).ok()?;
    req["tags"]
        .as_array()
        .and_then(|tags| {
            tags.iter().find_map(|t| {
                t.as_array().filter(|a| a.first().and_then(|v| v.as_str()) == Some("e"))
            })
        })
        .and_then(|t| t.get(1))
        .and_then(|v| v.as_str())
        .map(String::from)
}

/// NIP-57 zap amount in millisats: `amount` from the description JSON zap
/// request, else the direct `amount` tag.
pub fn zap_amount_msats(event: &Value) -> Option<i64> {
    let direct = first_tag_value(event, "amount").and_then(|s| s.parse::<i64>().ok());
    if let Some(a) = direct {
        return Some(a);
    }
    let desc = first_tag_value(event, "description")?;
    let req = serde_json::from_str::<Value>(desc).ok()?;
    req["tags"]
        .as_array()
        .and_then(|tags| {
            tags.iter().find_map(|t| {
                t.as_array().filter(|a| a.first().and_then(|v| v.as_str()) == Some("amount"))
            })
        })
        .and_then(|t| t.get(1))
        .and_then(|v| v.as_str())
        .and_then(|s| s.parse::<i64>().ok())
}

/// Pubkeys mentioned by `p` tags.
pub fn mentioned_pubkeys(event: &Value) -> Vec<String> {
    tags_by_name(event, "p")
        .iter()
        .filter_map(|t| t.get(1))
        .filter_map(|v| v.as_str())
        .map(String::from)
        .collect()
}

fn e_tags_by_marker<'a>(event: &'a Value, marker: &str) -> Vec<&'a str> {
    tags_by_name(event, "e")
        .iter()
        .filter_map(|t| {
            let m = t.get(3).and_then(|v| v.as_str())?;
            (m.eq_ignore_ascii_case(marker)).then(|| t.get(1).and_then(|v| v.as_str()))?
        })
        .collect()
}

fn relay_list_policy(marker: &str) -> (i64, i64) {
    match marker {
        "read" => (1, 0),
        "write" => (0, 1),
        _ => (1, 1),
    }
}

fn profile_field(meta: &Option<Value>, keys: &[&str]) -> String {
    let Some(m) = meta else {
        return String::new();
    };
    for k in keys {
        if let Some(s) = m[*k].as_str() {
            if !s.is_empty() {
                return s.to_string();
            }
        }
    }
    String::new()
}

fn as_str_or_empty(s: Option<&str>) -> String {
    s.unwrap_or("").to_string()
}

fn unix_now() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn in_memory() -> Connection {
        let conn = Connection::open_in_memory().unwrap();
        init_schema(&conn).unwrap();
        conn
    }

    fn event(kind: u64, pubkey: &str, content: &str, tags: Vec<Vec<&str>>) -> Value {
        serde_json::json!({
            "id": format!("id-{kind}-{pubkey}"),
            "pubkey": pubkey,
            "created_at": 1_700_000_000 + kind as i64,
            "kind": kind,
            "tags": tags,
            "content": content,
            "sig": "sig"
        })
    }

    #[test]
    fn stores_text_event_with_root_and_reply_ids() {
        let conn = in_memory();
        let ev = event(
            1,
            "pk",
            "hello\nworld",
            vec![vec!["e", "root-id", "", "root"], vec!["e", "reply-id", "", "reply"]],
        );
        store_event(&conn, &ev, Some("wss://relay.example")).unwrap();

        let row: (String, String, i64, String, Option<String>, Option<String>) = conn
            .query_row(
                "select content, relay, kind, tags, root_id, reply_id from events where id = ?",
                params![format!("id-1-pk")],
                |r| {
                    Ok((
                        r.get(0)?,
                        r.get(1)?,
                        r.get(2)?,
                        r.get(3)?,
                        r.get(4)?,
                        r.get(5)?,
                    ))
                },
            )
            .unwrap();
        // prin1 form: newline emitted literally, relay and tags quoted.
        assert_eq!(row.0, "\"hello\nworld\"");
        assert_eq!(row.1, "\"wss://relay.example\"");
        assert_eq!(row.2, 1);
        assert_eq!(
            row.3,
            "((\"e\" \"root-id\" \"\" \"root\") (\"e\" \"reply-id\" \"\" \"reply\"))"
        );
        assert_eq!(row.4, Some("root-id".to_string()));
        assert_eq!(row.5, Some("reply-id".to_string()));
    }

    #[test]
    fn stores_profile_event_and_keeps_newest() {
        let conn = in_memory();
        let older = serde_json::json!({
            "id": "p1", "pubkey": "pk", "created_at": 100, "kind": 0, "tags": [],
            "content": r#"{"name":"old","about":"x"}"#, "sig": ""
        });
        let newer = serde_json::json!({
            "id": "p2", "pubkey": "pk", "created_at": 200, "kind": 0, "tags": [],
            "content": r#"{"name":"new","picture":"p"}"#, "sig": ""
        });
        store_event(&conn, &older, None).unwrap();
        store_event(&conn, &newer, None).unwrap();

        let name: String = conn
            .query_row("select name from profiles where pubkey = ?", params!["pk"], |r| r.get(0))
            .unwrap();
        assert_eq!(name, "new");
    }

    #[test]
    fn stores_reaction_targeting_last_e_tag() {
        let conn = in_memory();
        let ev = event(7, "pk", "+", vec![vec!["e", "first"], vec!["e", "reacted"]]);
        store_event(&conn, &ev, None).unwrap();
        let target: String = conn
            .query_row("select event_id from reactions where id = ?", params!["id-7-pk"], |r| {
                r.get(0)
            })
            .unwrap();
        assert_eq!(target, "reacted");
    }

    #[test]
    fn derives_mention_notification_for_current_pubkey() {
        let conn = in_memory();
        // The mentioned event author must be `me` for a reply notification; here
        // exercise the mention path which only needs a `p` tag mentioning `me`.
        let ev = event(1, "alice", "hi", vec![vec!["p", "me"]]);
        maybe_store_notification(&conn, &ev, Some("me")).unwrap();
        let count: i64 = conn
            .query_row("select count(*) from notifications where type = 'mention'", [], |r| {
                r.get(0)
            })
            .unwrap();
        assert_eq!(count, 1);
    }

    #[test]
    fn sets_and_reads_meta() {
        let conn = in_memory();
        set_meta(&conn, "syncing", "1").unwrap();
        let val: String = conn
            .query_row("select value from meta where key = 'syncing'", [], |r| r.get(0))
            .unwrap();
        assert_eq!(val, "1");
    }
}