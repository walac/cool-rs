#[path = "../src/lexer.rs"]
mod lexer;

#[macro_use]
extern crate lazy_static;

use glob::glob;
use regex::Regex;
use std::fs::{read_to_string, File};
use std::io;
use std::io::{BufRead, BufReader, Lines, Read};
use std::iter::Iterator;
use std::path::PathBuf;

lazy_static! {
    static ref FILENAME_MATCH: Regex = Regex::new(r#"#name "([^"]+)""#).unwrap();
}

struct LexerOutput<R: Read> {
    pub filename: String,
    lines: Lines<BufReader<R>>,
}

impl<R: Read> From<BufReader<R>> for LexerOutput<R> {
    fn from(buf: BufReader<R>) -> LexerOutput<R> {
        let mut lines = buf.lines();
        let first_line = lines.next().unwrap().unwrap();
        let filename = FILENAME_MATCH
            .captures(&first_line)
            .unwrap()
            .get(0)
            .unwrap();

        LexerOutput {
            filename: filename.as_str().to_owned(),
            lines,
        }
    }
}

impl<R: Read> Iterator for LexerOutput<R> {
    type Item = io::Result<String>;

    fn next(&mut self) -> Option<io::Result<String>> {
        self.lines.next()
    }
}

#[test]
fn test_lexer() {
    let mut test_cases = PathBuf::from(file!()).parent().unwrap().to_path_buf();
    test_cases.push("lexer-test-cases/*.cool");

    for entry in glob(test_cases.as_path().to_str().unwrap()).unwrap() {
        match entry {
            Ok(src_file) => {
                let src_code = read_to_string(&src_file).unwrap();

                let mut out_file = src_file.clone();
                let mut out_filename = src_file.file_name().unwrap().to_os_string();
                let cur_file = out_filename.to_str().unwrap().to_string();
                out_filename.push(".out");
                out_file.set_file_name(out_filename);
                let f = File::open(out_file).unwrap();
                let out = LexerOutput::from(BufReader::new(f));

                let tokenizer = lexer::Tokenizer::new(&src_code);

                for (expected, token) in out.map(|r| r.unwrap()).zip(tokenizer) {
                    let token_fmt = format!("#{} {:?}", token.line, token.kind);
                    assert_eq!(
                        token_fmt, expected,
                        "{}: {} != {}",
                        &cur_file, token_fmt, expected
                    );
                }
            }
            Err(e) => panic!(e),
        }
    }
}
