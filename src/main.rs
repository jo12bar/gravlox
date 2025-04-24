mod ast;
mod ast_printer;
mod interpreter;
mod literal;
mod parser;
mod runtime_error;
mod scanner;
mod token;
mod token_type;

use std::io::{Write, stdin, stdout};

use interpreter::Interpreter;
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

  -V [VERBOSITY_LEVEL], --verbosity [VERBOSITY_LEVEL]
                Set verbosity level [default: \"warn\"] for logging during
                script parsing and execution. Note that this is the
                interpreter's verbosity, not the script's logging (Lox `print`
                statements are unaffected). This is primarily for debugging
                purposes.
                Possible values in increasing order of verbosity:
                    [off | error | warn | info | debug | trace]
                If any other value is passed in, this flag will assume the
                default.
                Setting higher verbosity levels will probably slow down script
                parsing and execution, so be careful.
                NOTE: You can also use the \"GRAVLOX_LOG\" environment variable
                to set this. If both \"GRAVLOX_LOG\" is set and this CLI flag is
                used, the CLI flag will take precedence.

ARGS:
  [SCRIPT]      The Lox script to interpret and run. If not passed, the Lox
                REPL will be started instead.
";

#[derive(Debug)]
struct CliArgs {
    script: Option<std::path::PathBuf>,
    logging_verbosity: Option<log::LevelFilter>,
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

    init_logging(&args.logging_verbosity);

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
        logging_verbosity: pargs
            .opt_value_from_str(["-V", "--verbosity"])
            .ok()
            .flatten(),

        script: match pargs.free_from_str() {
            Ok(v) => Some(v),
            Err(pico_args::Error::MissingArgument) => None,
            Err(e) => return Err(e),
        },
    };

    let remaining = pargs.finish();
    if !remaining.is_empty() {
        log::warn!("Unused arguments left: {remaining:?}");
    }

    Ok(args)
}

fn init_logging(logging_verbosity: &Option<log::LevelFilter>) {
    use env_logger::Builder;
    use log::LevelFilter;

    let mut builder = Builder::new();

    builder
        .filter_level(LevelFilter::Warn)
        .parse_env("GRAVLOX_LOG");

    if let Some(cli_level_filter) = logging_verbosity {
        builder.filter_level(*cli_level_filter);
    }

    builder.format_timestamp(None).init();
}

#[derive(Debug)]
struct Lox {
    had_error: bool,
    had_runtime_error: bool,
}

impl Lox {
    fn new() -> Lox {
        Lox {
            had_error: false,
            had_runtime_error: false,
        }
    }

    fn run_file(mut self, path: &std::path::Path) -> Result<(), std::io::Error> {
        let script_content = std::fs::read_to_string(path)?;

        let mut interpreter = Interpreter::new();

        self.run(&script_content, &mut interpreter);
        if self.had_error {
            std::process::exit(65); // EX_DATAERR from sysexits.h
        }
        if self.had_runtime_error {
            std::process::exit(70); // EX_SOFTWARE from sysexits.h
        }
        Ok(())
    }

    fn run_prompt(mut self) -> Result<(), std::io::Error> {
        let mut interpreter = Interpreter::new();

        loop {
            print!("> ");
            stdout().flush()?;

            match stdin().lines().next() {
                Some(Ok(input)) => {
                    self.run(&input, &mut interpreter);
                    self.had_error = false;
                }
                Some(Err(readline_err)) => return Err(readline_err),
                None => break,
            }
        }

        Ok(())
    }

    fn run(&mut self, source: &str, interpreter: &mut Interpreter) {
        let mut scanner = Scanner::new(source);

        scanner.scan_tokens(self);
        let tokens = scanner.iter_tokens().cloned().collect();

        let mut parser = parser::Parser::new(tokens);
        let expression = parser.parse(self);

        // Stop if there was a syntax error
        if self.had_error {
            return;
        }

        let expression = expression.expect("parser should report errors to the Lox interpreter properly so they can be detected and handled in a nice way");

        if log::log_enabled!(log::Level::Trace) {
            log::trace!("AST: {}", ast_printer::AstPrinter(&expression).walk_ast());
        }

        interpreter.interpret(&expression, self);
    }

    fn token_error(&mut self, token: &token::Token, message: &str) {
        if token.typ() == token_type::TokenType::Eof {
            self.report(token.line(), Some("at end"), message);
        } else {
            self.report(
                token.line(),
                Some(&format!("at '{}'", token.lexeme())),
                message,
            );
        }
    }

    fn runtime_error(&mut self, err: runtime_error::RuntimeError) {
        eprintln!("{}\n[line {}]", &err.message, err.token.line());
        self.had_runtime_error = true;
    }

    fn error_with_linenum(&mut self, line: LineNum, message: &str) {
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
