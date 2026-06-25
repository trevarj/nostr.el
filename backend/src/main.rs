use clap::Parser;
use std::io::{self, Read};
use std::process::ExitCode;

#[derive(Parser)]
#[command(name = "nostr-el-backend")]
#[command(about = "JSON crypto helper for nostr.el")]
struct Args {
    command: String,
}

fn main() -> ExitCode {
    let args = Args::parse();

    // The relay daemon is long-running and streams stdin line-by-line, so it
    // must not block on reading stdin to EOF like the one-shot commands do.
    if args.command == "relay-daemon" {
        return nostr_el_backend::daemon::run();
    }

    let mut input = String::new();

    if let Err(err) = io::stdin().read_to_string(&mut input) {
        println!(
            "{}",
            nostr_el_backend::io_error_response("stdin-error", err)
        );
        return ExitCode::FAILURE;
    }

    let result = nostr_el_backend::handle_command(&args.command, &input);
    println!("{}", result.body);

    if result.ok {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
