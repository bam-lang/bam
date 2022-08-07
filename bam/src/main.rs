use std::fs;

use ansi_term::{Colour, Style};
use anyhow::{anyhow, bail, Context, Error, Result};
use chumsky::Parser;
use clap;

use compiler::Compiler;
use rustyline::{error::ReadlineError, Editor};
use tracing::Level;

mod compiler;
mod eval;
mod lexer;
mod parser;
mod syntax;
mod types;
mod util;

use eval::Loop;
pub use lexer::{LexerBuilder, Token, KEYWORD_MAP};
use types::GlobalTypeEnv;

use crate::eval::Factory;
// use crate::{compiler::Compiler, eval::Factory};
pub use crate::{
    parser::ParserBuilder,
    syntax::{Builtin, Definition, Machine, Program, Statement, Stream, Value},
};

pub type Span = std::ops::Range<usize>;
pub type Spanned<T> = (T, Span);

const BAM: &str = r#"
         ▄▄▄▄    ▄▄▄       ███▄ ▄███▓ ▐██▌ 
        ▓█████▄ ▒████▄    ▓██▒▀█▀ ██▒ ▐██▌ 
        ▒██▒ ▄██▒██  ▀█▄  ▓██    ▓██░ ▐██▌ 
        ▒██░█▀  ░██▄▄▄▄██ ▒██    ▒██  ▓██▒ 
        ░▓█  ▀█▓ ▓█   ▓██▒▒██▒   ░██▒ ▒▄▄  
        ░▒▓███▀▒ ▒▒   ▓▒█░░ ▒░   ░  ░ ░▀▀▒ 
        ▒░▒   ░   ▒   ▒▒ ░░  ░      ░ ░  ░ 
         ░    ░   ░   ▒   ░      ░       ░ 
         ░            ░  ░       ░    ░    
              ░                            
        "Beautifully Assembled Machines"
"#;

const HELP: &str = r#"
> Type in a machine name and feed it values through stdin
> Use the :d command to define a new machine
> Exit all modes, including the REPL, with Ctrl-D
> Happy streaming!
"#;

#[derive(clap::Parser, Debug)]
#[clap(version, about, long_about = None)]
struct Args {
    #[clap(help = "The source file to execute")]
    filename: Option<String>,
    #[clap(long, help = "Enable tracing")]
    trace: bool,
    #[clap(long, help = "Don't run the REPL")]
    no_repl: bool,
}

fn main() -> Result<()> {
    let args: Args = clap::Parser::parse();

    if args.trace {
        tracing_subscriber::fmt()
            .with_max_level(Level::TRACE)
            .init();
    }

    run_repl(args.filename)
}

/// REPL mode.
enum Mode {
    /// If in this mode, accept statements, so things like:
    /// - `let x, y = stream -> Machine`
    /// - `null -> Read -> ToNum -> Sqrt`
    Statement,
    /// If in this mode, accept machine-declarations.
    /// Stores the current line number.
    Definiton(usize),
    /// If in this mode, we're stepping through a stream.
    Streaming(Loop),
}

fn run_repl(filename: Option<String>) -> Result<()> {
    println!("{}", Colour::Purple.bold().paint(BAM));
    println!("{}", Colour::White.bold().paint(HELP));

    // REPL state.

    // Lexer.
    let lexer = LexerBuilder::build();
    // Program and Statement parsers.
    let (pparser, sparser) = ParserBuilder::build();
    // Global typing environment.
    let mut tenv = GlobalTypeEnv::new();
    // Rustyline editor.
    let mut editor = Editor::<()>::new().unwrap();
    // Abstract syntax tree.
    let mut program = Program {
        machines: Vec::new(),
    };
    // Interaction mode.
    let mut mode = Mode::Statement;
    // Buffer for saving lines in :d mode.
    let mut definition_buf = String::new();
    // Execution engine.
    let mut factory = Factory::new(Compiler::new().transform(program.clone()));

    // Methods.

    let mut parse_program = |source: String| -> Result<Vec<Definition>> {
        let tokens = lexer.parse(source).map_err(|errs| {
            anyhow!(
                "{}",
                errs.into_iter()
                    .map(|err| err.to_string())
                    .collect::<Vec<_>>()
                    .join("\n")
            )
        })?;

        let program = pparser.parse(tokens).map_err(|errs| {
            anyhow!(
                "{}",
                errs.into_iter()
                    .map(|err| err.to_string())
                    .collect::<Vec<_>>()
                    .join("\n")
            )
        })?;

        program
            .machines
            .into_iter()
            // FIXME: The typechecker sometimes overflows the stack,
            // probably with the Drain function.
            // .map(|m| match types::check_machine_def(&mut tenv, &m) {
            //     Err(err) => bail!(err),
            //     Ok(()) => Ok(m),
            // })
            .map(|m| Ok(m))
            .collect()
    };

    let _parse_statement = |source: String| -> Result<Stream> {
        let tokens = lexer.parse(source).map_err(|errs| {
            anyhow!(
                "{}",
                errs.into_iter()
                    .map(|err| err.to_string())
                    .collect::<Vec<_>>()
                    .join("\n")
            )
        })?;

        sparser.parse(tokens).map_err(|errs| {
            anyhow!(
                "{}",
                errs.into_iter()
                    .map(|err| err.to_string())
                    .collect::<Vec<_>>()
                    .join("\n")
            )
        })

        // FIXME: no typechecking here :(
    };

    let report = |err: Error| eprintln!("{}", Colour::Red.bold().paint(err.to_string()));

    let mut load_program = |source: String, factory: &mut Factory| match parse_program(source) {
        Ok(machines) => {
            program.machines.extend(machines.into_iter());
            let module = Compiler::new().transform(program.clone());
            *factory = Factory::new(module);
        }
        Err(err) => report(err),
    };

    let prompt = |mode: &mut Mode| match mode {
        Mode::Statement => Colour::Blue.bold().paint("bam> ").to_string(),
        Mode::Streaming(_) => Colour::Purple.bold().paint("BAM!> ").to_string(),
        Mode::Definiton(l) => {
            *l += 1;
            Colour::White.bold().paint(format!("{l} | ")).to_string()
        }
    };

    // Entry point.

    if let Some(filename) = filename {
        let source = fs::read_to_string(&filename)
            .with_context(|| format!("Could not load file `{}`", filename))?;
        load_program(source, &mut factory);
    }

    editor.load_history(".history")?;
    loop {
        match editor.readline(&prompt(&mut mode)) {
            Ok(line) if line.trim().is_empty() => {
                if let Mode::Streaming(ref mut values) = mode {
                    match values.step() {
                        Ok(value) => println!("{}", value),
                        Err(err) => report(err),
                    };
                }
            }
            Ok(line) if line.starts_with(':') => match line.as_str() {
                ":d" | ":define" => {
                    mode = Mode::Definiton(0);
                }
                ":l" | ":load" => {
                    todo!()
                }
                ":r" | ":reload" => {
                    todo!()
                }
                _ => report(anyhow!("Unknown command: `{}`", &line[1..])),
            },
            Ok(line) if matches!(mode, Mode::Definiton(_)) => {
                definition_buf.push_str(&line);
            }
            Ok(line) if matches!(mode, Mode::Statement) => {
                editor.add_history_entry(line.as_str());
                let values = factory.make_loop(&line);
                mode = Mode::Streaming(values);
            }
            Err(ReadlineError::Eof) if matches!(mode, Mode::Streaming(_)) => mode = Mode::Statement,
            Err(ReadlineError::Eof) if matches!(mode, Mode::Definiton(_)) => {
                let lines = definition_buf.drain(..).collect::<String>();
                editor.add_history_entry(lines.as_str());
                load_program(lines, &mut factory);
                mode = Mode::Statement;
            }
            Err(ReadlineError::Interrupted) => {
                // Ctrl+C shouldn't crash the REPL
            }
            Err(ReadlineError::Eof) => {
                println!("{}", Style::new().italic().paint("Exiting ..."));
                editor.save_history(".history")?;
                break;
            }
            error => {
                editor.save_history(".history")?;
                error
                    .map(|_| ())
                    .with_context(|| format!("Unexpected error while reading input"))?;
            }
        }
    }

    editor.save_history(".history")?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_hello_world_from_tokens() {
        // machine Main {
        //     let hello = "Hello, BAM!";
        //     hello{1} -> Print
        // };

        let tokens = vec![
            Token::Machine,
            Token::Ident("Main".to_string()),
            Token::Lbrace,
            Token::Let,
            Token::Ident("hello".to_string()),
            Token::Equals,
            Token::StringLit("Hello, BAM!".to_string()),
            Token::Semicolon,
            Token::Ident("hello".to_string()),
            Token::Lbrace,
            Token::IntLit("1".to_string()),
            Token::Rbrace,
            Token::Pipe,
            Token::Ident("Print".to_string()),
            Token::Rbrace,
        ];

        let result = ParserBuilder::build()
            .0
            .parse(tokens)
            .map_err(|errs| {
                errs.iter().for_each(|e| eprintln!("Error(parse): {:?}", e));
                errs
            })
            .unwrap();

        use Builtin::*;
        use Statement::*;
        use Stream::*;
        use Value::*;
        assert_eq!(
            result,
            Program {
                machines: vec![Definition {
                    name: "Main".to_string(),
                    body: vec![Let(
                        vec!["hello".to_string()],
                        Const(Str("Hello, BAM!".to_string()))
                    )],
                    result: Pipe(
                        Box::new(Take(Box::new(Stream::Var("hello".to_string())), 1)),
                        Box::new(Machine::Builtin(Write))
                    )
                }]
            }
        );
    }

    #[test]
    fn parse_hello_world_from_source() {
        let source = "machine Main {
            let hello = \"Hello, BAM!\";
            hello{1} -> Print
        }";

        let tokens = LexerBuilder::build()
            .parse(source)
            .map_err(|errs| {
                errs.iter().for_each(|e| eprintln!("Error(lex): {:?}", e));
                errs
            })
            .unwrap();

        let result = ParserBuilder::build()
            .0
            .parse(tokens)
            .map_err(|errs| {
                errs.iter().for_each(|e| eprintln!("Error(parse): {:?}", e));
                errs
            })
            .unwrap();

        use Builtin::*;
        use Statement::*;
        use Stream::*;
        use Value::*;
        assert_eq!(
            result,
            Program {
                machines: vec![Definition {
                    name: "Main".to_string(),
                    body: vec![Let(
                        vec!["hello".to_string()],
                        Const(Str("Hello, BAM!".to_string()))
                    )],
                    result: Pipe(
                        Box::new(Take(Box::new(Stream::Var("hello".to_string())), 1)),
                        Box::new(Machine::Builtin(Write))
                    )
                }]
            }
        );
    }
}
