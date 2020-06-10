#[path = "../src/lexer.rs"]
mod lexer;

#[macro_use]
extern crate lazy_static;

use glob::glob;
use regex::Regex;
use std::fs::{read_to_string, File};
use std::io::prelude::*;
use std::io::{BufRead, BufReader, Lines};
use std::iter::Iterator;
use std::path::PathBuf;

lazy_static! {
    static ref filename_match: Regex = Regex::new(r#"#name "([^"]+)""#).unwrap();
}

struct LexerOutput {
    filename: String,
    lines: Lines<BufRead>,
}

impl From<BufRead> for LexerOutput {
    fn from(buf: BufRead) -> LexerOutput {
        let mut lines = buf.lines();
        let first_line = lines.next().unwrap().unwrap();
        let filename = filename_match.captures(first_line).unwrap().get(0).unwrap();

        LexerOutput { filename, lines }
    }
}

impl Iterator for LexerOutput {
    type Item = String;

    fn next(&mut self) -> Option<Result<String>> {
        self.lines.next()
    }
}

#[test]
fn test_lexer() {
    let mut test_cases = PathBuf::from(file!()).parent().unwrap().to_path_buf();
    test_cases.push("lexer-test-cases/*.cool");

    for src_file in glob(test_case.as_str()).unwrap() {
        let mut src_code;
        std::fs::read_to_string(&src_code).unwrap();

        let out_file = src_file.clone() + ".output";
        let f = File::open(out_file).unwrap();
        let out = LexerOutput::from(BufReader::new(f));

        let tokenizer = lexer::Tokenizer::new(src_code);
    }
}
