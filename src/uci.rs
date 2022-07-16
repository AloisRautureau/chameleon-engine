use crate::board::Board;
use crate::move_generator::{generate, GenType};
use crate::piece::Color;
use crate::r#move::Move;
use crate::search::{Search, SearchFramework, SearchOptions};

use num_cpus;
use regex::Regex;
use rustyline::config::Configurer;
use rustyline::Editor;
use std::collections::HashMap;
use std::fmt::{Display, Error, Formatter};
use std::time::Duration;

pub struct UCI {
    board: Board,
    search_framework: SearchFramework,
    editor: Editor<()>,
    options: HashMap<String, UCIOption>,
    debug_mode: bool,
}
impl Default for UCI {
    fn default() -> Self {
        let mut editor = Editor::<()>::new();
        editor.set_auto_add_history(true);
        editor.set_check_cursor_position(true);
        UCI {
            board: Board::new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
            search_framework: SearchFramework::new(),
            editor,
            debug_mode: false,
            options: HashMap::from([(
                "ThreadCount".to_string(),
                UCIOption::IntValue {
                    name: "ThreadCount".to_string(),
                    value: 1,
                    default: 1,
                    min: 1,
                    max: num_cpus::get() as i32,
                },
            )]),
        }
    }
}

impl UCI {
    pub fn run(&mut self) {
        while let Ok(line) = self.editor.readline("uci> ") {
            match self.handle_command(&line) {
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
                self.options
                    .iter()
                    .for_each(|o| Self::send(UCICommand::UciOption(o.1)));
                Self::send(UCICommand::UciOk);
            }
            "debug" => self.debug_mode = args.next().unwrap_or("off") == "on",
            "isready" => Self::send(UCICommand::ReadyOk),
            "setoption" => {
                let option_name = if let Some("name") = args.next() {
                    args.next().unwrap_or("")
                } else {
                    ""
                };
                let option_value = match args.skip(1).next() {
                    Some(v) => v,
                    None => {
                        if let Some(UCIOption::Callback { func, .. }) =
                            self.options.get(option_name)
                        {
                            func();
                            return Ok(UCIOkCode::OkCommand);
                        }
                        return Err(UCIErrCode::MissingArg(String::from("<value>")));
                    }
                };
                if let Some(option) = self.options.get_mut(option_name) {
                    option.set_value(option_value);
                }
            }
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
            "go" => self.search_framework.run_search(
                &self.board,
                if let Some(UCIOption::IntValue { value, .. }) = self.options.get("ThreadCount") {
                    *value as u16
                } else {
                    1
                },
                &self.parse_go_args(args.map(String::from)),
            ),
            "stop" => {
                self.search_framework.stop_search();
            }
            "ponderhit" => (),
            // Commands that are not part of the UCI protocol
            "probetable" => {
                println!("{:?}", self.search_framework.probe_table(&self.board))
            }
            "show" => println!("{}", self.board),
            "quit" => return Ok(UCIOkCode::ShouldQuit),
            _ => return Err(UCIErrCode::BadCommand(String::from(cmd))),
        }

        Ok(UCIOkCode::OkCommand)
    }

    pub fn send(command: UCICommand) {
        match command {
            UCICommand::Id => println!("id name Chameleon\nid author A.Rautureau"),
            UCICommand::UciOk => println!("uciok"),
            UCICommand::ReadyOk => println!("readyok"),
            UCICommand::BestMove(mv) => println!("bestmove {}", mv),
            UCICommand::Info(search_state) => println!("info {}", search_state),
            UCICommand::UciOption(option) => println!("option {}", option),
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

        let mut options = SearchOptions::default();
        options
            .set_infinite(arg_value_map.contains_key("infinite"))
            .set_moves_until_time_control(
                arg_value_map
                    .get("movestogo")
                    .map_or(None, |d| d.parse::<u32>().ok()),
            )
            .set_nodes_to_search(
                arg_value_map
                    .get("nodes")
                    .map_or(None, |d| d.parse::<u128>().ok()),
            )
            .set_depth(
                arg_value_map
                    .get("depth")
                    .map_or(None, |d| d.parse::<i8>().ok()),
            );

        if arg_value_map.contains_key("movetime") {
            options.set_time(
                arg_value_map
                    .get("movetime")
                    .map(|d| Duration::from_millis(d.parse::<u64>().unwrap_or(0))),
            );
        } else {
            let (clock, increment) = if self.board.side_to_move() == Color::White {
                (
                    arg_value_map
                        .get("wtime")
                        .map(|d| Duration::from_millis(d.parse::<u64>().unwrap_or(0))),
                    arg_value_map
                        .get("winc")
                        .map(|d| Duration::from_millis(d.parse::<u64>().unwrap_or(0))),
                )
            } else {
                (
                    arg_value_map
                        .get("btime")
                        .map(|d| Duration::from_millis(d.parse::<u64>().unwrap_or(0))),
                    arg_value_map
                        .get("binc")
                        .map(|d| Duration::from_millis(d.parse::<u64>().unwrap_or(0))),
                )
            };
            if let Some(c) = clock {
                options.set_time_from_clock(c, increment);
            }
        }

        if let Some(move_list) = arg_value_map.get("searchmoves") {
            let legal_moves = generate(&self.board, GenType::Legal);
            let mut moves_to_search = vec![];
            for move_str in move_list.split(' ') {
                let (origin, target, maybe_prom) = match Move::parse(move_str) {
                    Some(r) => r,
                    None => continue,
                };
                if let Some(m) = legal_moves.iter().find(|m| {
                    m.origin() == origin
                        && m.target() == target
                        && m.promotion_target() == maybe_prom
                }) {
                    moves_to_search.push(*m)
                }
            }
            options.set_moves_to_search(Some(moves_to_search));
        }

        options
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
    BestMove(&'a Move),
    Info(&'a Search),
    UciOption(&'a UCIOption),
}

pub enum UCIOption {
    BoolValue {
        name: String,
        value: bool,
        default: bool,
    },
    IntValue {
        name: String,
        value: i32,
        default: i32,
        min: i32,
        max: i32,
    },
    Callback {
        name: String,
        func: Box<dyn Fn() -> ()>,
    },
    StringValue {
        name: String,
        value: String,
        default: String,
    },
    StringChoice {
        name: String,
        value: String,
        default: String,
        possible_values: Vec<String>,
    },
}
impl UCIOption {
    pub fn set_value(&mut self, v: &str) {
        match self {
            Self::BoolValue { value, .. } => *value = v.parse::<bool>().unwrap_or(*value),
            Self::IntValue {
                value, min, max, ..
            } => {
                let parsed = v.parse::<i32>().unwrap_or(*value);
                *value = std::cmp::min(std::cmp::max(parsed, *min), *max)
            }
            Self::StringValue { value, .. } => *value = v.to_string(),
            Self::StringChoice {
                value,
                possible_values,
                ..
            } => {
                let set_to = possible_values
                    .iter()
                    .find(|x| x.as_str() == v)
                    .unwrap_or(value);
                *value = set_to.to_string();
            }
            _ => (),
        }
    }
}
impl Display for UCIOption {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Self::BoolValue { name, default, .. } => {
                write!(f, "name {} type check default {}", name, default)
            }
            Self::IntValue {
                name,
                default,
                min,
                max,
                ..
            } => write!(
                f,
                "name {} type spin default {} min {} max {}",
                name, default, min, max
            ),
            Self::Callback { name, .. } => write!(f, "name {} type button", name),
            Self::StringValue { name, default, .. } => {
                write!(f, "name {} type string default {}", name, default)
            }
            Self::StringChoice {
                name,
                default,
                possible_values,
                ..
            } => write!(
                f,
                "name {} type combo default {} {}",
                name,
                default,
                possible_values
                    .iter()
                    .fold(String::new(), |acc, x| format!("{} var {}", acc, x))
            ),
        }
    }
}
