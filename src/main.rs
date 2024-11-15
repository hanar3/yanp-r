#[allow(unused)]
#[cfg(test)]
extern crate assert_matches;
use regex::Regex;
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Eq, Clone)]
enum TokenType {
    Number,
    String,
}

#[derive(Debug, Clone)]
struct Token {
    ttype: TokenType,
    value: String,
}

#[derive(Debug)]
struct Tokenizer {
    str: &'static str,
    cursor: usize,
}

impl Tokenizer {
    const SPEC: [(&'static str, TokenType); 3] = [
        // Numbers:
        (r"^\d+", TokenType::Number),
        // Strings:
        (r#"^"[^"]*""#, TokenType::String), // double quote
        (r#"^'[^']*'"#, TokenType::String), // Single quote
    ];
    pub fn new(str: &'static str) -> Self {
        Self { str, cursor: 0 }
    }
    pub fn init(str: &'static str) -> Self {
        Self { str, cursor: 0 }
    }

    pub fn next(&mut self) -> Option<Token> {
        let string = &self.str[(self.cursor as usize)..];

        for (_, spec) in Self::SPEC.iter().enumerate() {
            let regex = Regex::new(spec.0).expect("Invalid regex pattern");
            if let Some(captures) = regex.captures(string) {
                let cap = captures
                    .get(0)
                    .expect("Failed to extract number from regex");
                self.cursor += cap.len();
                return Some(Token {
                    ttype: spec.1.clone(),
                    value: cap.as_str().to_string(),
                });
            }
        }

        return None;
    }
}
#[allow(dead_code)]
#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type", content = "value")]
enum Literal {
    #[serde(rename = "StringLiteral")]
    String(String),

    #[serde(rename = "NumericLiteral")]
    Number(i32),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
enum Statement {
    Literal(Literal),
}

#[derive(Debug, Serialize, Deserialize)]
struct Program {
    #[serde(rename = "type")]
    ptype: &'static str,
    body: Vec<Statement>,
}

#[allow(dead_code)]
struct Parser {
    str: &'static str,
    lookahead: Option<Token>,
    tokenizer: Tokenizer,
}

impl Parser {
    pub fn init(str: &'static str) -> Self {
        let mut tokenizer = Tokenizer::new(str);
        Self {
            str,
            lookahead: tokenizer.next(),
            tokenizer,
        }
    }

    pub fn parse(&mut self) -> Program {
        return Program {
            ptype: "Program",
            body: self.statement_list(),
        };
    }

    pub fn statement_list(&mut self) -> Vec<Statement> {
        let mut statement_list = vec![self.statement()];

        while let Some(_) = self.lookahead {
            statement_list.push(self.statement());
        }

        return statement_list;
    }

    pub fn statement(&mut self) -> Statement {
        if let Some(ref tok) = self.lookahead {
            match tok.ttype {
                TokenType::Number => Statement::Literal(self.numeric_literal()),
                TokenType::String => Statement::Literal(self.string_literal()),
            }
        } else {
            panic!("Unexpected end of input");
        }
    }

    pub fn numeric_literal(&mut self) -> Literal {
        let tok = self.eat(TokenType::Number);
        return Literal::Number(tok.value.parse().expect("Failed to parse number"));
    }

    pub fn string_literal(&mut self) -> Literal {
        let tok = self.eat(TokenType::String);
        return Literal::String(tok.value[1..tok.value.len() - 1].to_string());
    }

    pub fn eat(&mut self, ttype: TokenType) -> Token {
        let lookahead = self
            .lookahead
            .clone()
            .expect(format!("Unexpected end of input, expected {:?}", ttype).as_str()); // FIXME: shouldn't require clone here?

        if lookahead.ttype == ttype {
            self.lookahead = self.tokenizer.next();
            return lookahead;
        } else {
            panic!(
                "Unexpected token, expected: {:?}, but received: {:?}",
                ttype, lookahead.ttype
            );
        }
    }
}

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_a_numeric_literal() {
        let mut parser = Parser::init("42");
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"NumericLiteral\",\"value\":42}]}"
        );
    }

    #[test]
    fn parses_a_string_literal() {
        let mut parser = Parser::init(r#""hello""#);
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"StringLiteral\",\"value\":\"hello\"}]}"
        );
    }

    #[test]
    fn parses_a_singlequote_string_literal() {
        let mut parser = Parser::init(r#"'hello-single'"#);
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"StringLiteral\",\"value\":\"hello-single\"}]}"
        );
    }
}
