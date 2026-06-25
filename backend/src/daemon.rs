//! Long-running relay daemon: owns all websocket I/O and event storage.
//!
//! The Emacs side used to open relay websockets itself (`nostr-relay.el`),
//! verify signatures in subprocesses, and write the SQLite cache. This daemon
//! moves that work to the backend: it connects to relays with `nostr-sdk`,
//! subscribes on Emacs's behalf, verifies and stores every inbound event via
//! [`crate::store`], and emits a newline-delimited JSON event stream so Emacs
//! can refresh its views from the database.
//!
//! ## Protocol
//!
//! The daemon is driven over stdin/stdout, both newline-delimited JSON:
//!
//! * The **first** stdin line is an [`InitConfig`] (`db_path`, initial
//!   `relays`, optional `current_pubkey`).
//! * Every subsequent stdin line is a [`Command`] (`subscribe`, `close`,
//!   `add-relay`, `remove-relay`, `set-pubkey`, `shutdown`). EOF also shuts
//!   the daemon down.
//! * stdout carries [`Outbound`] notifications: `ready`, `stored`, `eose`,
//!   `relay-status`, `notice`, `closed`, `error`.
//!
//! Emacs decides *what* to subscribe to and refreshes *from the database*; the
//! daemon never renders anything. A REQ with several filters keeps its single
//! Emacs-facing subscription id: each filter is subscribed under a derived id
//! (`id#N`) but `stored`/`eose` notifications report the original `id`.

use std::collections::HashMap;
use std::io::Write;

use nostr_sdk::prelude::*;
// Imported explicitly so it shadows `nostr_sdk::prelude::Connection` (a Tor/proxy
// config type) — every `Connection` below is the SQLite handle.
use rusqlite::Connection;
use serde::Deserialize;
use serde_json::{json, Value};
use tokio::io::{AsyncBufReadExt, BufReader};

use crate::store;

/// First stdin line: daemon bootstrap configuration.
#[derive(Debug, Deserialize)]
struct InitConfig {
    /// Path to the SQLite cache Emacs reads back via `emacsql`.
    db_path: String,
    /// Relays to connect to at startup. More can be added later.
    #[serde(default)]
    relays: Vec<String>,
    /// Hex pubkey of the local account, used to derive notifications.
    #[serde(default)]
    current_pubkey: Option<String>,
}

/// Newline-delimited control commands, tagged by `op`.
#[derive(Debug, Deserialize)]
#[serde(tag = "op", rename_all = "kebab-case")]
enum Command {
    /// Open subscription `id` with one or more nostr filters.
    ///
    /// With `relays` empty the subscription replaces any prior one under `id`
    /// and targets every connected relay. With `relays` set it is *added* to
    /// those relays under `id` (extending, not replacing), mirroring the old
    /// per-relay `nostr-relay-subscribe`.
    Subscribe {
        id: String,
        filters: Vec<Value>,
        #[serde(default)]
        relays: Vec<String>,
    },
    /// Close subscription `id` on every relay.
    Close { id: String },
    /// Publish a fully-signed event to relays (all, or the given `relays`).
    Publish {
        event: Value,
        #[serde(default)]
        relays: Vec<String>,
    },
    /// Connect to an additional relay.
    AddRelay { url: String },
    /// Disconnect and forget a relay.
    RemoveRelay { url: String },
    /// Change the local pubkey used for notification derivation.
    SetPubkey { pubkey: Option<String> },
    /// Disconnect everything and exit.
    Shutdown,
}

/// Entry point invoked from `main` for the `relay-daemon` subcommand.
///
/// Builds a current-thread Tokio runtime so the SQLite [`Connection`] (which is
/// `!Sync`) can live in the single task that owns both relay notifications and
/// stdin commands, with no locking.
pub fn run() -> std::process::ExitCode {
    let runtime = match tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
    {
        Ok(rt) => rt,
        Err(err) => {
            emit_error("runtime-error", &err.to_string());
            return std::process::ExitCode::FAILURE;
        }
    };
    match runtime.block_on(daemon_main()) {
        Ok(()) => std::process::ExitCode::SUCCESS,
        Err(err) => {
            emit_error("daemon-error", &err.to_string());
            std::process::ExitCode::FAILURE
        }
    }
}

/// Maps each relay-facing subscription id back to the Emacs-facing id, so
/// multi-filter REQs report `stored`/`eose` under the id Emacs chose.
struct SubRegistry {
    /// derived relay id -> Emacs base id.
    derived_to_base: HashMap<String, String>,
    /// Emacs base id -> all derived ids opened for it.
    base_to_derived: HashMap<String, Vec<String>>,
}

impl SubRegistry {
    fn new() -> Self {
        Self {
            derived_to_base: HashMap::new(),
            base_to_derived: HashMap::new(),
        }
    }

    /// Derived ids to open for a base id with `count` filters. A single-filter
    /// REQ keeps the base id verbatim so simple subscriptions stay readable.
    fn derived_ids(base: &str, count: usize) -> Vec<String> {
        if count <= 1 {
            vec![base.to_string()]
        } else {
            (0..count).map(|i| format!("{base}#{i}")).collect()
        }
    }

    fn record(&mut self, base: &str, derived: &str) {
        self.derived_to_base
            .insert(derived.to_string(), base.to_string());
        self.base_to_derived
            .entry(base.to_string())
            .or_default()
            .push(derived.to_string());
    }

    /// Resolve a relay-reported id to the Emacs-facing id (falls back to the
    /// id itself for subscriptions the daemon did not open, e.g. internal ones).
    fn base_of(&self, derived: &str) -> String {
        self.derived_to_base
            .get(derived)
            .cloned()
            .unwrap_or_else(|| derived.to_string())
    }

    /// Forget a base id, returning every derived id that must be unsubscribed.
    fn take(&mut self, base: &str) -> Vec<String> {
        let derived = self.base_to_derived.remove(base).unwrap_or_default();
        for d in &derived {
            self.derived_to_base.remove(d);
        }
        derived
    }
}

async fn daemon_main() -> Result<(), Box<dyn std::error::Error>> {
    let mut reader = BufReader::new(tokio::io::stdin()).lines();

    // The first line bootstraps the daemon.
    let first = match reader.next_line().await? {
        Some(line) => line,
        None => return Ok(()), // empty stdin: nothing to do.
    };
    let config: InitConfig = serde_json::from_str(first.trim())?;

    let conn = store::open(&config.db_path)?;
    // Emacs owns the schema in normal operation; create it if the daemon races
    // ahead of `nostr-db-init` so the first writes never hit a missing table.
    store::init_schema(&conn)?;
    let mut current_pubkey = config.current_pubkey.clone();

    let client = Client::default();
    for url in &config.relays {
        if let Err(err) = client.add_relay(url.as_str()).await {
            emit(&json!({"event": "error", "code": "add-relay", "url": url,
                         "message": err.to_string()}));
        } else {
            store_status(&conn, url, "connecting", None);
        }
    }
    client.connect().await;

    let mut registry = SubRegistry::new();
    let mut notifications = client.notifications();

    emit(&json!({"event": "ready"}));

    loop {
        tokio::select! {
            line = reader.next_line() => {
                match line? {
                    // EOF on stdin: Emacs closed the pipe, shut down cleanly.
                    None => break,
                    Some(line) => {
                        let line = line.trim();
                        if line.is_empty() {
                            continue;
                        }
                        match serde_json::from_str::<Command>(line) {
                            Ok(Command::Shutdown) => break,
                            Ok(cmd) => {
                                handle_command(&client, &conn, &mut registry,
                                               &mut current_pubkey, cmd).await;
                            }
                            Err(err) => {
                                emit_error("bad-command", &err.to_string());
                            }
                        }
                    }
                }
            }
            notif = notifications.recv() => {
                match notif {
                    Ok(RelayPoolNotification::Shutdown) => break,
                    Ok(notification) => {
                        handle_notification(&conn, &registry,
                                            current_pubkey.as_deref(), notification);
                    }
                    // Lagged: the broadcast buffer overflowed during a burst.
                    // Skipping is safe; the events were still stored by the pool
                    // consumer is us, so this only loses our stdout echo, not data.
                    Err(tokio::sync::broadcast::error::RecvError::Lagged(n)) => {
                        emit(&json!({"event": "error", "code": "lagged",
                                     "message": format!("dropped {n} notifications")}));
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Closed) => break,
                }
            }
        }
    }

    client.disconnect().await;
    Ok(())
}

async fn handle_command(
    client: &Client,
    conn: &Connection,
    registry: &mut SubRegistry,
    current_pubkey: &mut Option<String>,
    cmd: Command,
) {
    match cmd {
        Command::Subscribe {
            id,
            filters,
            relays,
        } => {
            // A global (all-relay) subscribe replaces any prior one under this
            // id; a relay-targeted subscribe extends the existing id instead.
            // Subscriptions stay open until an explicit Close: relay-side EOSE
            // auto-close truncates multi-relay results (it fires on the fastest
            // relay's EOSE), so Emacs closes one-shot fetches on a timer instead.
            if relays.is_empty() {
                for derived in registry.take(&id) {
                    client.unsubscribe(&SubscriptionId::new(derived)).await;
                }
            }
            let derived_ids = SubRegistry::derived_ids(&id, filters.len());
            for (filter_value, derived) in filters.iter().zip(derived_ids.iter()) {
                let filter = match serde_json::from_value::<Filter>(filter_value.clone()) {
                    Ok(f) => f,
                    Err(err) => {
                        emit(&json!({"event": "error", "code": "bad-filter",
                                     "sub": id, "message": err.to_string()}));
                        continue;
                    }
                };
                let sub_id = SubscriptionId::new(derived.clone());
                let result = if relays.is_empty() {
                    client.subscribe_with_id(sub_id, filter, None).await.map(|_| ())
                } else {
                    client
                        .subscribe_with_id_to(relays.iter(), sub_id, filter, None)
                        .await
                        .map(|_| ())
                };
                match result {
                    // Record only once per derived id even across targeted calls.
                    Ok(()) => {
                        if !registry.derived_to_base.contains_key(derived) {
                            registry.record(&id, derived);
                        }
                    }
                    Err(err) => {
                        emit(&json!({"event": "error", "code": "subscribe",
                                     "sub": id, "message": err.to_string()}));
                    }
                }
            }
        }
        Command::Close { id } => {
            for derived in registry.take(&id) {
                client.unsubscribe(&SubscriptionId::new(derived)).await;
            }
        }
        Command::Publish { event, relays } => {
            publish_event(client, conn, event, relays).await;
        }
        Command::AddRelay { url } => match client.add_relay(url.as_str()).await {
            Ok(_) => {
                let _ = client.connect_relay(url.as_str()).await;
                store_status(conn, &url, "connecting", None);
            }
            Err(err) => emit(&json!({"event": "error", "code": "add-relay",
                                     "url": url, "message": err.to_string()})),
        },
        Command::RemoveRelay { url } => {
            if let Err(err) = client.remove_relay(url.as_str()).await {
                emit(&json!({"event": "error", "code": "remove-relay",
                             "url": url, "message": err.to_string()}));
            } else {
                store_status(conn, &url, "disconnected", None);
            }
        }
        Command::SetPubkey { pubkey } => {
            *current_pubkey = pubkey;
        }
        Command::Shutdown => {} // handled in the loop.
    }
}

/// Publish a signed event and record per-relay receipts, mirroring
/// `nostr-relay-send-client-message` + `nostr-db-store-publish-receipt`.
async fn publish_event(client: &Client, conn: &Connection, event: Value, relays: Vec<String>) {
    let event: Event = match serde_json::from_value(event) {
        Ok(ev) => ev,
        Err(err) => {
            emit_error("bad-event", &err.to_string());
            return;
        }
    };
    let output = if relays.is_empty() {
        client.send_event(&event).await
    } else {
        client.send_event_to(relays.iter(), &event).await
    };
    match output {
        Ok(output) => {
            let id = output.val.to_hex();
            for url in &output.success {
                store_receipt(conn, &id, url.as_str(), "sent", None);
            }
            for (url, err) in &output.failed {
                store_receipt(conn, &id, url.as_str(), "failed", Some(err));
            }
            emit(&json!({
                "event": "published",
                "id": id,
                "success": output.success.len(),
                "failed": output.failed.len(),
            }));
        }
        Err(err) => {
            emit(&json!({"event": "error", "code": "publish",
                         "id": event.id.to_hex(), "message": err.to_string()}));
        }
    }
}

fn store_receipt(conn: &Connection, event_id: &str, url: &str, state: &str, message: Option<&str>) {
    if let Err(err) = store::store_publish_receipt(conn, event_id, url, state, message) {
        emit_error("publish-receipt", &err.to_string());
    }
}

fn handle_notification(
    conn: &Connection,
    registry: &SubRegistry,
    current_pubkey: Option<&str>,
    notification: RelayPoolNotification,
) {
    match notification {
        RelayPoolNotification::Event {
            relay_url,
            subscription_id,
            event,
        } => {
            let base = registry.base_of(subscription_id.as_str());
            ingest_event(conn, current_pubkey, &base, relay_url.as_str(), &event);
        }
        RelayPoolNotification::Message { relay_url, message } => {
            handle_relay_message(conn, registry, relay_url.as_str(), message);
        }
        RelayPoolNotification::Shutdown => {}
    }
}

/// Verify, store, and announce a single inbound event.
fn ingest_event(
    conn: &Connection,
    current_pubkey: Option<&str>,
    sub: &str,
    relay_url: &str,
    event: &Event,
) {
    // The pool only forwards each event once, but verify explicitly so a relay
    // that serves a forged event can never poison the cache (mirrors
    // `nostr-relay--invalid-event`).
    if event.verify().is_err() {
        store_status(
            conn,
            relay_url,
            "invalid-event",
            Some(&format!("{} signature verification failed", event.id)),
        );
        emit(&json!({"event": "error", "code": "invalid-event",
                     "id": event.id.to_hex(), "relay": relay_url}));
        return;
    }

    let value = match serde_json::to_value(event) {
        Ok(v) => v,
        Err(err) => {
            emit_error("event-encode", &err.to_string());
            return;
        }
    };

    if let Err(err) = store::store_event(conn, &value, Some(relay_url)) {
        emit(&json!({"event": "error", "code": "store",
                     "id": event.id.to_hex(), "message": err.to_string()}));
        return;
    }
    if let Err(err) = store::maybe_store_notification(conn, &value, current_pubkey) {
        emit(&json!({"event": "error", "code": "notification",
                     "id": event.id.to_hex(), "message": err.to_string()}));
    }

    emit(&json!({
        "event": "stored",
        "sub": sub,
        "id": event.id.to_hex(),
        "pubkey": event.pubkey.to_hex(),
        "kind": event.kind.as_u16(),
        "relay": relay_url,
    }));
}

fn handle_relay_message(
    conn: &Connection,
    registry: &SubRegistry,
    relay_url: &str,
    message: RelayMessage,
) {
    match message {
        RelayMessage::EndOfStoredEvents(sub) => {
            let base = registry.base_of(sub.as_str());
            store_status(conn, relay_url, "eose", None);
            emit(&json!({"event": "eose", "sub": base, "relay": relay_url}));
        }
        RelayMessage::Closed {
            subscription_id,
            message,
        } => {
            let base = registry.base_of(subscription_id.as_str());
            emit(&json!({"event": "closed", "sub": base, "relay": relay_url,
                         "message": message}));
        }
        RelayMessage::Notice(message) => {
            store_status(conn, relay_url, "notice", Some(&message));
            emit(&json!({"event": "notice", "relay": relay_url, "message": message}));
        }
        // OK/COUNT/AUTH and event echoes are not needed by the cache.
        _ => {}
    }
}

/// Persist a relay status row, surfacing storage errors to stdout but never
/// aborting the daemon over a transient SQLite hiccup.
fn store_status(conn: &Connection, url: &str, state: &str, message: Option<&str>) {
    if let Err(err) = store::store_relay_status(conn, url, state, message) {
        emit_error("relay-status", &err.to_string());
    }
}

/// Write one newline-delimited JSON notification to stdout.
fn emit(value: &Value) {
    let mut out = std::io::stdout().lock();
    // A broken stdout pipe means Emacs is gone; ignore and let the next stdin
    // read return EOF to drive shutdown.
    let _ = writeln!(out, "{value}");
    let _ = out.flush();
}

fn emit_error(code: &str, message: &str) {
    emit(&json!({"event": "error", "code": code, "message": message}));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_filter_keeps_base_id() {
        assert_eq!(SubRegistry::derived_ids("feed", 1), vec!["feed"]);
        // Zero filters never subscribes, but must not panic deriving ids.
        assert_eq!(SubRegistry::derived_ids("feed", 0), vec!["feed"]);
    }

    #[test]
    fn multi_filter_derives_indexed_ids() {
        assert_eq!(
            SubRegistry::derived_ids("feed", 3),
            vec!["feed#0", "feed#1", "feed#2"]
        );
    }

    #[test]
    fn registry_maps_derived_to_base_and_takes_all() {
        let mut reg = SubRegistry::new();
        for d in SubRegistry::derived_ids("feed", 2) {
            reg.record("feed", &d);
        }
        assert_eq!(reg.base_of("feed#0"), "feed");
        assert_eq!(reg.base_of("feed#1"), "feed");
        // Unknown ids resolve to themselves so internal subscriptions still echo.
        assert_eq!(reg.base_of("other"), "other");

        let mut taken = reg.take("feed");
        taken.sort();
        assert_eq!(taken, vec!["feed#0", "feed#1"]);
        // After taking, the mappings are gone.
        assert_eq!(reg.base_of("feed#0"), "feed#0");
        assert!(reg.take("feed").is_empty());
    }

    #[test]
    fn parses_init_config_with_defaults() {
        let cfg: InitConfig = serde_json::from_str(r#"{"db_path":"/tmp/c.db"}"#).unwrap();
        assert_eq!(cfg.db_path, "/tmp/c.db");
        assert!(cfg.relays.is_empty());
        assert!(cfg.current_pubkey.is_none());
    }

    #[test]
    fn parses_tagged_commands() {
        let subscribe: Command =
            serde_json::from_str(r#"{"op":"subscribe","id":"x","filters":[{"kinds":[1]}]}"#)
                .unwrap();
        assert!(matches!(subscribe, Command::Subscribe { id, filters, .. }
                         if id == "x" && filters.len() == 1));
        assert!(matches!(
            serde_json::from_str::<Command>(r#"{"op":"add-relay","url":"wss://r"}"#).unwrap(),
            Command::AddRelay { url } if url == "wss://r"
        ));
        assert!(matches!(
            serde_json::from_str::<Command>(r#"{"op":"shutdown"}"#).unwrap(),
            Command::Shutdown
        ));
    }
}
