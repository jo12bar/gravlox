mod ast_printer;
mod expr;
mod literal;
mod scanner;
mod token;
mod token_type;

use std::io::{Write, stdin, stdout};

use scanner::Scanner;

const HELP: &str = "\
gravlox

A Lox language interpreter.

USAGE:
  Interpret and run a Lox script file:
    gravlox <SCRIPT>
  Start the Lox REPL:
    gravlox

FLAGS:
  -h, --help    Print help information and exit.

ARGS:
  [SCRIPT]      The Lox script to interpret and run. If not passed, the Lox
                REPL will be started instead.
";

#[derive(Debug)]
struct CliArgs {
    script: Option<std::path::PathBuf>,
}

fn main() {
    let args = match parse_args() {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Error: {e}.");
            eprintln!("Usage: gravlox [SCRIPT]");
            eprintln!("Run `gravlox -h` for more help.");
            std::process::exit(64); // EX_USAGE from sysexits.h
        }
    };

    let lox = Lox::new();

    if let Some(script) = &args.script {
        lox.run_file(script).unwrap_or_else(|e| {
            eprintln!("Error: {e}");
            std::process::exit(1);
        });
    } else {
        lox.run_prompt().unwrap_or_else(|e| {
            eprintln!("Error: {e}");
            std::process::exit(1);
        });
    }
}

fn parse_args() -> Result<CliArgs, pico_args::Error> {
    let mut pargs = pico_args::Arguments::from_env();

    // Handle help first so it has highest priority
    if pargs.contains(["-h", "--help"]) {
        print!("{HELP}");
        std::process::exit(0);
    }

    let args = CliArgs {
        script: match pargs.free_from_str() {
            Ok(v) => Some(v),
            Err(pico_args::Error::MissingArgument) => None,
            Err(e) => return Err(e),
        },
    };

    let remaining = pargs.finish();
    if !remaining.is_empty() {
        eprintln!("Warning: unused arguments left: {remaining:?}");
    }

    Ok(args)
}

#[derive(Default, Debug)]
struct Lox {
    had_error: bool,
}

impl Lox {
    fn new() -> Lox {
        Lox { had_error: false }
    }

    fn run_file(mut self, path: &std::path::Path) -> Result<(), std::io::Error> {
        let script_content = std::fs::read_to_string(path)?;
        self.run(&script_content);
        if self.had_error {
            std::process::exit(65); // EX_DATAERR from sysexits.h
        }
        Ok(())
    }

    fn run_prompt(mut self) -> Result<(), std::io::Error> {
        loop {
            print!("> ");
            stdout().flush()?;

            match stdin().lines().next() {
                Some(Ok(input)) => {
                    self.run(&input);
                    self.had_error = false;
                }
                Some(Err(readline_err)) => return Err(readline_err),
                None => break,
            }
        }

        Ok(())
    }

    fn run(&mut self, source: &str) {
        let mut scanner = Scanner::new(source);

        // For now, just print the tokens
        scanner.scan_tokens(self);

        for token in scanner.iter_tokens() {
            println!("{token}");
        }
    }

    fn error(&mut self, line: LineNum, message: &str) {
        self.report(line, None, message);
    }

    fn report(&mut self, line: LineNum, location: Option<&str>, message: &str) {
        if let Some(location) = location {
            eprintln!("[line {line}] Error {location}: {message}");
        } else {
            eprintln!("[line {line}] Error: {message}");
        }

        self.had_error = true;
    }
}

pub type LineNum = usize;
