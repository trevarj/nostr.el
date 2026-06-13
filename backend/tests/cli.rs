use serde_json::{json, Value};
use std::io::Write;
use std::process::{Command, Stdio};

const TEST_SECRET: &str = "0000000000000000000000000000000000000000000000000000000000000001";

fn run(command: &str, request: Value) -> (bool, Value, String) {
    let mut child = Command::new(env!("CARGO_BIN_EXE_nostr-el-backend"))
        .arg(command)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn backend");

    child
        .stdin
        .as_mut()
        .expect("stdin")
        .write_all(request.to_string().as_bytes())
        .expect("write request");

    let output = child.wait_with_output().expect("wait backend");
    let stdout = String::from_utf8(output.stdout).expect("stdout utf8");
    let stderr = String::from_utf8(output.stderr).expect("stderr utf8");
    let value = serde_json::from_str(stdout.trim()).expect("stdout JSON");

    (output.status.success(), value, stderr)
}

#[test]
fn binary_reads_secret_from_stdin_and_writes_only_json_to_stdout() {
    let (success, value, stderr) = run("pubkey", json!({ "secret_key": TEST_SECRET }));

    assert!(success);
    assert!(stderr.is_empty());
    assert_eq!(value["ok"], true);
    assert_eq!(
        value["pubkey"],
        "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"
    );
    assert!(!value.to_string().contains(TEST_SECRET));
}

#[test]
fn binary_returns_nonzero_with_structured_json_error() {
    let (success, value, stderr) = run("pubkey", json!({ "secret_key": "bad secret" }));

    assert!(!success);
    assert!(stderr.is_empty());
    assert_eq!(value["ok"], false);
    assert_eq!(value["error"]["code"], "invalid-key");
    assert!(!value.to_string().contains("bad secret"));
}
