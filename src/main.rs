#[allow(unused)]
use regex::Regex;
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Eq, Clone)]
enum TokenType {
    Number,
    String,
    Semicolon,
    Skip,
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
    const SPEC: [(&'static str, TokenType); 5] = [
        // Whitespaces
        (r"^\s+", TokenType::Skip),
        // Numbers:
        (r"^\d+", TokenType::Number),
        // Strings:
        (r#"^"[^"]*""#, TokenType::String), // double quote
        (r#"^'[^']*'"#, TokenType::String), // Single quote
        // Symbols, delimiters
        (r"^;", TokenType::Semicolon),
    ];
    pub fn new(str: &'static str) -> Self {
        Self { str, cursor: 0 }
    }

    pub fn has_more_tokens(&self) -> bool {
        return self.cursor < self.str.len();
    }

    pub fn next(&mut self) -> Option<Token> {
        if !self.has_more_tokens() {
            return None;
        }

        let string = &self.str[(self.cursor as usize)..];

        for (_, spec) in Self::SPEC.iter().enumerate() {
            let regex = Regex::new(spec.0).expect("Invalid regex pattern");

            if let Some(captures) = regex.captures(string) {
                match spec.1 {
                    TokenType::Skip => {
                        let cap = captures.get(0).expect("Failed to extract value from regex");
                        self.cursor += cap.len();
                        return self.next();
                    }
                    _ => {
                        let cap = captures.get(0).expect("Failed to extract value from regex");
                        self.cursor += cap.len();
                        return Some(Token {
                            ttype: spec.1.clone(),
                            value: cap.as_str().to_string(),
                        });
                    }
                }
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
enum Expression {
    Literal(Literal),
}

#[derive(Debug, Serialize, Deserialize)]
struct ExpressionStatement {
    #[serde(rename = "type")]
    _type: &'static str,
    expression: Expression,
}

#[derive(Debug, Serialize, Deserialize)]
struct Program {
    #[serde(rename = "type")]
    ptype: &'static str,
    body: Vec<ExpressionStatement>,
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

    pub fn statement_list(&mut self) -> Vec<ExpressionStatement> {
        let mut statement_list = vec![self.statement()];

        while !self.lookahead.is_none() {
            statement_list.push(self.statement());
        }

        return statement_list;
    }

    pub fn statement(&mut self) -> ExpressionStatement {
        if let Some(_) = self.lookahead {
            return self.expression_statement();
        } else {
            panic!("Unexpected end of input parsing statement");
        }
    }

    /*
     * ExpressionStatement
     *   : Expression ';'
     *   ;
     * */
    pub fn expression_statement(&mut self) -> ExpressionStatement {
        let expression = self.expression();
        self.eat(TokenType::Semicolon);
        ExpressionStatement {
            _type: "ExpressionStatement",
            expression,
        }
    }

    /*
     * Expression
     *   : Literal
     *   ;
     * */
    pub fn expression(&mut self) -> Expression {
        let la = self.lookahead.clone().unwrap(); // TODO: should not clone!
        match la.ttype {
            TokenType::Number => Expression::Literal(self.numeric_literal()),
            TokenType::String => Expression::Literal(self.string_literal()),
            _ => panic!("Unexpected token type: {:?}", la.ttype),
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
        let mut parser = Parser::init("42;");
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"NumericLiteral\",\"value\":42}}]}"
        );
    }

    #[test]
    fn parses_a_string_literal() {
        let mut parser = Parser::init(r#""hello";"#);
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"StringLiteral\",\"value\":\"hello\"}}]}"
        );
    }

    #[test]
    fn parses_a_singlequote_string_literal() {
        let mut parser = Parser::init(r#"'hello-single';"#);
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"StringLiteral\",\"value\":\"hello-single\"}}]}"
        );
    }

    #[test]
    fn skips_spaces() {
        let mut parser = Parser::init(r#"    'hello-single';    "#);
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"StringLiteral\",\"value\":\"hello-single\"}}]}"
        );
    }
}
