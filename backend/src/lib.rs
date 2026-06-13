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
            "nip19-encode"
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

    if let Ok(Nip19::Event(event)) = Nip19::from_bech32(value) {
        return Ok(json!({
            "entity": "nevent",
            "event_id": event.event_id.to_hex(),
            "pubkey": event.author.map(|author| author.to_hex()),
            "kind": event.kind.map(|kind| kind.as_u16()),
            "relays": event.relays
                .iter()
                .map(|relay| relay.to_string())
                .collect::<Vec<String>>()
        }));
    }

    if let Ok(public_key) = PublicKey::from_bech32(value) {
        return Ok(json!({
            "entity": "npub",
            "pubkey": public_key.to_hex()
        }));
    }

    if let Ok(secret_key) = SecretKey::from_bech32(value) {
        return Ok(json!({
            "entity": "nsec",
            "secret_key": secret_key.to_secret_hex()
        }));
    }

    if let Ok(event_id) = EventId::from_bech32(value) {
        return Ok(json!({
            "entity": "note",
            "event_id": event_id.to_hex()
        }));
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
