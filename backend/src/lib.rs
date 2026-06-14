use base64::engine::{general_purpose, Engine};
use nostr_sdk::prelude::*;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::fmt;

pub struct CommandResult {
    pub ok: bool,
    pub body: String,
}

#[derive(Debug, Serialize)]
struct Response<T: Serialize> {
    ok: bool,
    #[serde(flatten)]
    data: T,
}

#[derive(Debug, Serialize)]
struct ErrorResponse {
    ok: bool,
    error: ProtocolErrorBody,
}

#[derive(Debug, Serialize)]
struct ProtocolErrorBody {
    code: &'static str,
    message: String,
    details: Value,
}

#[derive(Debug, thiserror::Error)]
#[error("{message}")]
struct ProtocolError {
    code: &'static str,
    message: String,
    details: Value,
}

impl ProtocolError {
    fn new(code: &'static str, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
            details: json!({}),
        }
    }

    fn with_details(code: &'static str, message: impl Into<String>, details: Value) -> Self {
        Self {
            code,
            message: message.into(),
            details,
        }
    }
}

#[derive(Debug, Deserialize)]
struct EmptyRequest {}

#[derive(Debug, Deserialize)]
struct SecretKeyRequest {
    secret_key: String,
}

#[derive(Debug, Deserialize)]
struct SignEventRequest {
    secret_key: String,
    kind: u16,
    #[serde(default)]
    tags: Vec<Vec<String>>,
    #[serde(default)]
    content: String,
    #[serde(default)]
    created_at: Option<u64>,
    #[serde(default)]
    pow: Option<u8>,
    #[serde(default)]
    envelope: bool,
}

#[derive(Debug, Deserialize)]
struct VerifyEventRequest {
    event: Value,
}

#[derive(Debug, Deserialize)]
struct Nip19DecodeRequest {
    value: String,
}

#[derive(Debug, Deserialize)]
struct Nip19EncodeRequest {
    entity: String,
    value: String,
}

#[derive(Debug, Deserialize)]
struct BlossomAuthRequest {
    secret_key: String,
    server: String,
    sha256: String,
    expiration: u64,
}

pub fn handle_command(command: &str, input: &str) -> CommandResult {
    match dispatch(command, input) {
        Ok(value) => ok(value),
        Err(err) => fail(err),
    }
}

pub fn io_error_response(code: &'static str, err: impl fmt::Display) -> String {
    error_json(ProtocolError::new(code, err.to_string()))
}

fn dispatch(command: &str, input: &str) -> Result<Value, ProtocolError> {
    match command {
        "capabilities" => capabilities(input),
        "generate-key" => generate_key(input),
        "pubkey" => pubkey(input),
        "sign-event" => sign_event(input),
        "verify-event" => verify_event(input),
        "nip19-decode" => nip19_decode(input),
        "nip19-encode" => nip19_encode(input),
        "blossom-auth" => blossom_auth(input),
        _ => Err(ProtocolError::with_details(
            "unknown-command",
            "Unknown command",
            json!({ "command": command }),
        )),
    }
}

fn capabilities(input: &str) -> Result<Value, ProtocolError> {
    let _: EmptyRequest = parse_request(input)?;
    Ok(json!({
        "commands": [
            "capabilities",
            "generate-key",
            "pubkey",
            "sign-event",
            "verify-event",
            "nip19-decode",
            "nip19-encode",
            "blossom-auth"
        ],
        "protocol_version": 1,
        "backend": "nostr-el-backend"
    }))
}

fn generate_key(input: &str) -> Result<Value, ProtocolError> {
    let _: EmptyRequest = parse_request(input)?;
    let keys = Keys::generate();
    let public_key = keys.public_key();

    Ok(json!({
        "secret_key": keys.secret_key().to_secret_hex(),
        "nsec": keys.secret_key().to_bech32().map_err(map_nip19_error)?,
        "pubkey": public_key.to_hex(),
        "npub": public_key.to_bech32().map_err(map_nip19_error)?
    }))
}

fn pubkey(input: &str) -> Result<Value, ProtocolError> {
    let request: SecretKeyRequest = parse_request(input)?;
    let keys = parse_keys(&request.secret_key)?;
    let public_key = keys.public_key();

    Ok(json!({
        "pubkey": public_key.to_hex(),
        "npub": public_key.to_bech32().map_err(map_nip19_error)?
    }))
}

fn sign_event(input: &str) -> Result<Value, ProtocolError> {
    let request: SignEventRequest = parse_request(input)?;
    let keys = parse_keys(&request.secret_key)?;
    let tags = parse_tags(request.tags)?;
    let mut builder = EventBuilder::new(Kind::from(request.kind), request.content).tags(tags);

    if let Some(created_at) = request.created_at {
        builder = builder.custom_created_at(Timestamp::from(created_at));
    }

    if let Some(pow) = request.pow {
        builder = builder.pow(pow);
    }

    let event = builder
        .sign_with_keys(&keys)
        .map_err(|err| ProtocolError::new("crypto-error", err.to_string()))?;
    let client_message = if request.envelope {
        Some(ClientMessage::event(event.clone()).as_json())
    } else {
        None
    };

    Ok(json!({
        "event": event,
        "client_message": client_message
    }))
}

fn verify_event(input: &str) -> Result<Value, ProtocolError> {
    let request: VerifyEventRequest = parse_request(input)?;
    let event: Event = serde_json::from_value(request.event)
        .map_err(|err| ProtocolError::new("invalid-event", err.to_string()))?;

    match event.verify() {
        Ok(()) => Ok(json!({ "valid": true })),
        Err(err) => Ok(json!({
            "valid": false,
            "reason": err.to_string()
        })),
    }
}

fn nip19_decode(input: &str) -> Result<Value, ProtocolError> {
    let request: Nip19DecodeRequest = parse_request(input)?;
    let value = request.value.as_str();

    if let Ok(nip19) = Nip19::from_bech32(value) {
        return match nip19 {
            Nip19::Event(event) => Ok(json!({
                "entity": "nevent",
                "event_id": event.event_id.to_hex(),
                "pubkey": event.author.map(|author| author.to_hex()),
                "kind": event.kind.map(|kind| kind.as_u16()),
                "relays": event.relays
                    .iter()
                    .map(|relay| relay.to_string())
                    .collect::<Vec<String>>()
            })),
            Nip19::Pubkey(public_key) => Ok(json!({
                "entity": "npub",
                "pubkey": public_key.to_hex()
            })),
            Nip19::Secret(secret_key) => Ok(json!({
                "entity": "nsec",
                "secret_key": secret_key.to_secret_hex()
            })),
            Nip19::EventId(event_id) => Ok(json!({
                "entity": "note",
                "event_id": event_id.to_hex()
            })),
            Nip19::Profile(profile) => Ok(json!({
                "entity": "nprofile",
                "pubkey": profile.public_key.to_hex(),
                "relays": profile.relays
                    .iter()
                    .map(|relay| relay.to_string())
                    .collect::<Vec<String>>()
            })),
            Nip19::Coordinate(coordinate) => Ok(json!({
                "entity": "naddr",
                "kind": coordinate.coordinate.kind.as_u16(),
                "pubkey": coordinate.coordinate.public_key.to_hex(),
                "identifier": coordinate.coordinate.identifier,
                "relays": coordinate.relays
                    .iter()
                    .map(|relay| relay.to_string())
                    .collect::<Vec<String>>()
            })),
        };
    }

    Err(ProtocolError::new(
        "unsupported-entity",
        "Unsupported or invalid NIP-19 entity",
    ))
}

fn nip19_encode(input: &str) -> Result<Value, ProtocolError> {
    let request: Nip19EncodeRequest = parse_request(input)?;
    match request.entity.as_str() {
        "npub" | "pubkey" => {
            let public_key = PublicKey::parse(&request.value)
                .map_err(|err| ProtocolError::new("invalid-key", err.to_string()))?;
            Ok(json!({ "value": public_key.to_bech32().map_err(map_nip19_error)? }))
        }
        "nsec" | "secret-key" => {
            let secret_key = SecretKey::from_hex(&request.value)
                .map_err(|err| ProtocolError::new("invalid-key", err.to_string()))?;
            Ok(json!({ "value": secret_key.to_bech32().map_err(map_nip19_error)? }))
        }
        "note" | "event-id" => {
            let event_id = EventId::parse(&request.value)
                .map_err(|err| ProtocolError::new("invalid-event", err.to_string()))?;
            Ok(json!({ "value": event_id.to_bech32().map_err(map_nip19_error)? }))
        }
        _ => Err(ProtocolError::with_details(
            "unsupported-entity",
            "Unsupported NIP-19 entity",
            json!({ "entity": request.entity }),
        )),
    }
}

fn blossom_auth(input: &str) -> Result<Value, ProtocolError> {
    let request: BlossomAuthRequest = parse_request(input)?;
    let keys = parse_keys(&request.secret_key)?;
    let tags = parse_tags(vec![
        vec!["t".to_string(), "upload".to_string()],
        vec!["expiration".to_string(), request.expiration.to_string()],
        vec!["server".to_string(), request.server.clone()],
        vec!["x".to_string(), request.sha256.clone()],
    ])?;
    let event = EventBuilder::new(Kind::from(24242), "")
        .tags(tags)
        .sign_with_keys(&keys)
        .map_err(|err| ProtocolError::new("crypto-error", err.to_string()))?;
    let event_json = serde_json::to_string(&event)
        .map_err(|err| ProtocolError::new("serialization-error", err.to_string()))?;
    let authorization = format!("Nostr {}", general_purpose::STANDARD.encode(event_json));

    Ok(json!({
        "authorization": authorization,
        "event": event,
        "server": request.server,
        "sha256": request.sha256,
        "expiration": request.expiration
    }))
}

fn parse_request<T: for<'de> Deserialize<'de>>(input: &str) -> Result<T, ProtocolError> {
    serde_json::from_str(input).map_err(|err| ProtocolError::new("invalid-json", err.to_string()))
}

fn parse_keys(secret_key: &str) -> Result<Keys, ProtocolError> {
    Keys::parse(secret_key).map_err(|err| ProtocolError::new("invalid-key", err.to_string()))
}

fn parse_tags(tags: Vec<Vec<String>>) -> Result<Vec<Tag>, ProtocolError> {
    tags.into_iter()
        .map(|tag| {
            Tag::parse(tag).map_err(|err| ProtocolError::new("invalid-tag", err.to_string()))
        })
        .collect()
}

fn map_nip19_error(err: impl fmt::Display) -> ProtocolError {
    ProtocolError::new("unsupported-entity", err.to_string())
}

fn ok(data: Value) -> CommandResult {
    let body = serde_json::to_string(&Response { ok: true, data })
        .expect("serializing success response should not fail");
    CommandResult { ok: true, body }
}

fn fail(err: ProtocolError) -> CommandResult {
    CommandResult {
        ok: false,
        body: error_json(err),
    }
}

fn error_json(err: ProtocolError) -> String {
    serde_json::to_string(&ErrorResponse {
        ok: false,
        error: ProtocolErrorBody {
            code: err.code,
            message: err.message,
            details: err.details,
        },
    })
    .expect("serializing error response should not fail")
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_SECRET: &str = "0000000000000000000000000000000000000000000000000000000000000001";

    fn response(command: &str, request: Value) -> Value {
        let result = handle_command(command, &request.to_string());
        serde_json::from_str(&result.body).expect("response JSON")
    }

    #[test]
    fn capabilities_lists_supported_commands() {
        let value = response("capabilities", json!({}));

        assert_eq!(value["ok"], true);
        assert_eq!(value["protocol_version"], 1);
        assert!(value["commands"]
            .as_array()
            .unwrap()
            .contains(&json!("sign-event")));
        assert!(value["commands"]
            .as_array()
            .unwrap()
            .contains(&json!("generate-key")));
    }

    #[test]
    fn generate_key_returns_usable_key_material() {
        let value = response("generate-key", json!({}));

        assert_eq!(value["ok"], true);
        assert!(value["secret_key"].as_str().unwrap().len() >= 64);
        assert!(value["nsec"].as_str().unwrap().starts_with("nsec1"));
        assert!(value["npub"].as_str().unwrap().starts_with("npub1"));

        let pubkey = response(
            "pubkey",
            json!({ "secret_key": value["secret_key"].as_str().unwrap() }),
        );
        assert_eq!(pubkey["ok"], true);
        assert_eq!(pubkey["pubkey"], value["pubkey"]);
    }

    #[test]
    fn pubkey_returns_hex_and_bech32_without_secret() {
        let value = response("pubkey", json!({ "secret_key": TEST_SECRET }));

        assert_eq!(value["ok"], true);
        assert_eq!(
            value["pubkey"],
            "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"
        );
        assert!(value["npub"].as_str().unwrap().starts_with("npub1"));
        assert!(!value.to_string().contains(TEST_SECRET));
    }

    #[test]
    fn pubkey_accepts_nsec_secret_key() {
        let nsec = SecretKey::from_hex(TEST_SECRET)
            .unwrap()
            .to_bech32()
            .unwrap();

        let value = response("pubkey", json!({ "secret_key": nsec }));

        assert_eq!(value["ok"], true);
        assert_eq!(
            value["pubkey"],
            "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"
        );
        assert!(value["npub"].as_str().unwrap().starts_with("npub1"));
    }

    #[test]
    fn sign_event_returns_verifiable_event_and_envelope() {
        let value = response(
            "sign-event",
            json!({
                "secret_key": TEST_SECRET,
                "kind": 1,
                "content": "hello from nostr.el",
                "tags": [["t", "emacs"], ["client", "nostr.el"]],
                "created_at": 1,
                "envelope": true
            }),
        );

        assert_eq!(value["ok"], true);
        assert_eq!(value["event"]["kind"], 1);
        assert_eq!(value["event"]["content"], "hello from nostr.el");
        assert_eq!(value["event"]["created_at"], 1);
        assert!(value["client_message"]
            .as_str()
            .unwrap()
            .starts_with("[\"EVENT\","));

        let verify = response("verify-event", json!({ "event": value["event"].clone() }));
        assert_eq!(verify["ok"], true);
        assert_eq!(verify["valid"], true);
    }

    #[test]
    fn blossom_auth_returns_upload_authorization_event() {
        let value = response(
            "blossom-auth",
            json!({
                "secret_key": TEST_SECRET,
                "server": "https://blossom.example",
                "sha256": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "expiration": 1234
            }),
        );

        assert_eq!(value["ok"], true);
        assert_eq!(value["event"]["kind"], 24242);
        assert_eq!(value["event"]["content"], "");
        assert!(value["authorization"]
            .as_str()
            .unwrap()
            .starts_with("Nostr "));
        assert!(value["event"]["tags"]
            .as_array()
            .unwrap()
            .contains(&json!(["t", "upload"])));
        assert!(value["event"]["tags"]
            .as_array()
            .unwrap()
            .contains(&json!(["server", "https://blossom.example"])));
        assert!(value["event"]["tags"].as_array().unwrap().contains(&json!([
            "x",
            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        ])));
    }

    #[test]
    fn verify_event_reports_invalid_signature_as_valid_false() {
        let signed = response(
            "sign-event",
            json!({
                "secret_key": TEST_SECRET,
                "kind": 1,
                "content": "original",
                "created_at": 1
            }),
        );
        let mut event = signed["event"].clone();
        event["content"] = json!("tampered");

        let verify = response("verify-event", json!({ "event": event }));

        assert_eq!(verify["ok"], true);
        assert_eq!(verify["valid"], false);
    }

    #[test]
    fn nip19_round_trips_basic_entities() {
        let pubkey = response(
            "nip19-encode",
            json!({
                "entity": "pubkey",
                "value": "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"
            }),
        );
        assert_eq!(pubkey["ok"], true);

        let decoded = response(
            "nip19-decode",
            json!({ "value": pubkey["value"].as_str().unwrap() }),
        );
        assert_eq!(decoded["ok"], true);
        assert_eq!(decoded["entity"], "npub");
        assert_eq!(
            decoded["pubkey"],
            "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"
        );
    }

    #[test]
    fn nip19_decodes_nevent_entities() {
        let decoded = response(
            "nip19-decode",
            json!({
                "value": "nevent1qvzqqqqqqypzp978pfzrv6n9xhq5tvenl9e74pklmskh4xw6vxxyp3j8qkke3cezqqsz30njcd6vfl7stvtprak57rqdd36795dr0j25jxvc838qhmqgexgttt6m4"
            }),
        );
        assert_eq!(decoded["ok"], true);
        assert_eq!(decoded["entity"], "nevent");
        assert_eq!(
            decoded["event_id"],
            "28be72c374c4ffd05b1611f6d4f0c0d6c75e2d1a37c954919983c4e0bec08c99"
        );
        assert_eq!(
            decoded["pubkey"],
            "97c70a44366a6535c145b333f973ea86dfdc2d7a99da618c40c64705ad98e322"
        );
        assert_eq!(decoded["kind"], 1);
    }

    #[test]
    fn nip19_decodes_nprofile_entities() {
        let decoded = response(
            "nip19-decode",
            json!({
                "value": "nprofile1qqsrhuxx8l9ex335q7he0f09aej04zpazpl0ne2cgukyawd24mayt8gppemhxue69uhhytnc9e3k7mf0qyt8wumn8ghj7er2vfshxtnnv9jxkc3wvdhk6tclr7lsh"
            }),
        );
        assert_eq!(decoded["ok"], true);
        assert_eq!(decoded["entity"], "nprofile");
        assert_eq!(
            decoded["pubkey"],
            "3bf0c63fcb93463407af97a5e5ee64fa883d107ef9e558472c4eb9aaaefa459d"
        );
        assert_eq!(decoded["relays"][0], "wss://r.x.com/");
    }

    #[test]
    fn nip19_decodes_naddr_entities() {
        let decoded = response(
            "nip19-decode",
            json!({
                "value": "naddr1qqxnzd3exgersv33xymnsve3qgs8suecw4luyht9ekff89x4uacneapk8r5dyk0gmn6uwwurf6u9rusrqsqqqa282m3gxt"
            }),
        );
        assert_eq!(decoded["ok"], true);
        assert_eq!(decoded["entity"], "naddr");
        assert_eq!(decoded["kind"], 30023);
        assert_eq!(
            decoded["pubkey"],
            "787338757fc25d65cd929394d5e7713cf43638e8d259e8dcf5c73b834eb851f2"
        );
        assert_eq!(decoded["identifier"], "1692282117831");
    }

    #[test]
    fn errors_are_structured_and_do_not_echo_secret() {
        let result = handle_command(
            "pubkey",
            &json!({ "secret_key": "not a real secret" }).to_string(),
        );
        let value: Value = serde_json::from_str(&result.body).unwrap();

        assert!(!result.ok);
        assert_eq!(value["ok"], false);
        assert_eq!(value["error"]["code"], "invalid-key");
        assert!(!result.body.contains("not a real secret"));
    }
}
