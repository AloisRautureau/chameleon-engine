use crate::board::Board;
use crate::piece::Color;
use crate::r#move::Move;
use crate::search::{Search, SearchFramework, SearchOptions};

use regex::Regex;
use rustyline::Editor;
use std::collections::HashMap;
use std::time::Duration;

pub struct UCI {
    board: Board,
    search_framework: SearchFramework,
    debug_mode: bool,
}

impl UCI {
    pub fn run() {
        let mut instance = UCI::new_instance();
        let mut rl = Editor::<()>::new();

        while let Ok(line) = rl.readline("uci> ") {
            rl.add_history_entry(line.as_str());
            match instance.handle_command(&line) {
                Ok(UCIOkCode::ShouldQuit) => break,
                Err(UCIErrCode::BadCommand(cmd)) => {
                    eprintln!("Unknown or badly formed UCI command: {}", cmd)
                }
                Err(UCIErrCode::BadMove(mv)) => {
                    eprintln!("Badly formatted or illegal move: {}", mv)
                }
                Err(UCIErrCode::MissingArg(arg)) => {
                    eprintln!("Missing an argument: {} {} <- here", line.trim(), arg)
                }
                _ => (),
            }
        }
    }

    fn handle_command(&mut self, line: &str) -> Result<UCIOkCode, UCIErrCode> {
        let args_regex = Self::args_regex();
        let mut args = args_regex.find_iter(line).map(|m| m.as_str());
        let cmd = if let Some(c) = args.next() {
            c
        } else {
            return Err(UCIErrCode::NoCommand);
        };
        match cmd {
            "uci" => {
                Self::send(UCICommand::Id);
                Self::send(UCICommand::UciOk)
            }
            "debug" => self.debug_mode = args.next().unwrap() == "on",
            "isready" => Self::send(UCICommand::ReadyOk),
            "setoption" => (),
            "ucinewgame" => {
                self.board = Board::new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
            }
            "position" => {
                match args.next() {
                    Some("startpos") => {
                        self.board =
                            Board::new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
                    }
                    Some(fen) => self.board = Board::new(&fen.replace('"', "")),
                    None => return Err(UCIErrCode::MissingArg(String::from("<startpos | fen>"))),
                }
                if args.next().is_none() {
                    return Ok(UCIOkCode::OkCommand);
                }
                for mv in args {
                    if self.board.make_from_str(mv).is_err() {
                        return Err(UCIErrCode::BadMove(String::from(mv)));
                    }
                }
            }
            "go" => self
                .search_framework
                .run_search(&self.board, self.parse_go_args(args.map(String::from))),
            "stop" => {
                if let Some(result) = self.search_framework.stop_search() {
                    UCI::send(UCICommand::BestMove(result.best_move.unwrap()))
                }
            }
            "ponderhit" => (),
            "probetable" => {
                if let Some(info) = self.search_framework.probe_table(&self.board) {
                    println!("{}", info)
                } else {
                    println!("no info")
                }
            }
            "show" => println!("{}", self.board),
            "quit" => return Ok(UCIOkCode::ShouldQuit),
            _ => return Err(UCIErrCode::BadCommand(String::from(cmd))),
        }

        Ok(UCIOkCode::OkCommand)
    }

    pub fn send(command: UCICommand) {
        match command {
            UCICommand::Id => println!("id name Chameleon\nid author Aloïs.R"),
            UCICommand::UciOk => println!("uciok"),
            UCICommand::ReadyOk => println!("readyok"),
            UCICommand::BestMove(mv) => println!("bestmove {}", mv),
            UCICommand::Info(search_state) => println!("info {}", search_state),
        }
    }

    fn parse_go_args<I: Iterator<Item = String>>(&self, args: I) -> SearchOptions {
        let valid_args = [
            "searchmoves",
            "ponder",
            "wtime",
            "btime",
            "winc",
            "binc",
            "movestogo",
            "depth",
            "nodes",
            "mate",
            "movetime",
            "infinite",
        ];

        let mut arg_value_map: HashMap<String, String> = HashMap::new();
        let mut current_arg = String::new();
        let mut current_value = String::new();
        for word in args {
            if valid_args.contains(&word.as_str()) {
                arg_value_map.insert(current_arg, String::from(current_value.trim()));
                current_arg = word;
                current_value = String::new();
            } else {
                current_value.push_str(&word);
                current_value.push(' ');
            }
        }
        arg_value_map.insert(current_arg, String::from(current_value.trim()));

        let (clock, increment) = if self.board.side_to_move() == Color::White {
            (
                arg_value_map
                    .get("wtime")
                    .map(|d| Duration::from_millis(d.parse::<u64>().unwrap())),
                arg_value_map
                    .get("winc")
                    .map(|d| Duration::from_millis(d.parse::<u64>().unwrap())),
            )
        } else {
            (
                arg_value_map
                    .get("btime")
                    .map(|d| Duration::from_millis(d.parse::<u64>().unwrap())),
                arg_value_map
                    .get("binc")
                    .map(|d| Duration::from_millis(d.parse::<u64>().unwrap())),
            )
        };

        SearchOptions::from_info(
            arg_value_map.contains_key("infinite"),
            None, // TODO parse this
            clock,
            increment,
            arg_value_map
                .get("movestogo")
                .map(|d| d.parse::<u32>().unwrap()),
            arg_value_map
                .get("depth")
                .map(|d| d.parse::<i32>().unwrap()),
            arg_value_map
                .get("nodes")
                .map(|d| d.parse::<u128>().unwrap()),
            arg_value_map
                .get("movetime")
                .map(|d| Duration::from_millis(d.parse::<u64>().unwrap())),
        )
    }

    fn new_instance() -> UCI {
        UCI {
            board: Board::new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
            search_framework: SearchFramework::new(),
            debug_mode: false,
        }
    }

    fn args_regex() -> Regex {
        Regex::new(r#"(".*?"|[^"\s]+)"#).unwrap()
    }
}

enum UCIOkCode {
    OkCommand,
    ShouldQuit,
}

enum UCIErrCode {
    MissingArg(String),
    NoCommand,
    BadCommand(String),
    BadMove(String),
}

pub enum UCICommand<'a> {
    Id,
    UciOk,
    ReadyOk,
    BestMove(Move),
    Info(&'a Search),
}
