#![allow(unused)]
use core::fmt;
use std::fmt::write;

use regex::Regex;
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Eq, Clone)]
enum TokenType {
    Number,
    String,
    Semicolon,
    OpenCurly,
    CloseCurly,
    Skip,
    AdditiveOp,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
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
    const SPEC: [(&'static str, TokenType); 10] = [
        // Whitespaces
        (r"^\s+", TokenType::Skip),
        // Numbers:
        (r"^\d+", TokenType::Number),
        // Strings:
        (r#"^"[^"]*""#, TokenType::String), // double quote
        (r#"^'[^']*'"#, TokenType::String), // Single quote
        // Comments
        (r"^\/\/.*", TokenType::Skip),           // Single-line
        (r"^\/\*[\s\S]*?\*\/", TokenType::Skip), // Multi-line
        // Symbols, delimiters
        (r"^;", TokenType::Semicolon),
        (r"^\{", TokenType::OpenCurly),
        (r"^\}", TokenType::CloseCurly),
        // Math operators
        (r"^[+\-]", TokenType::AdditiveOp),
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
#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(tag = "type", content = "value")]
enum Literal {
    #[serde(rename = "StringLiteral")]
    String(String),

    #[serde(rename = "NumericLiteral")]
    Number(i32),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
enum BinaryExpressionNode {
    Literal(Literal),
    Binary(Box<BinaryExpr>),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct BinaryExpr {
    #[serde(rename = "type")]
    _type: String,
    operator: String,
    left: BinaryExpressionNode,
    right: BinaryExpressionNode,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
enum Expression {
    Literal(Literal),
    Binary(BinaryExpr),
}

#[derive(Debug, Serialize, Deserialize)]
struct ExpressionStatement {
    #[serde(rename = "type")]
    _type: String,
    expression: Expression,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
enum Statement {
    Expression(ExpressionStatement),
    Block(BlockStatement),
}

#[derive(Debug, Serialize, Deserialize)]
struct BlockStatement {
    #[serde(rename = "type")]
    _type: String,
    body: Vec<Statement>,
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
            body: self.statement_list(None),
        };
    }

    pub fn statement_list(&mut self, stop_lookahead: Option<TokenType>) -> Vec<Statement> {
        let mut statement_list = vec![self.statement()];

        while let Some(ref tok) = self.lookahead {
            if let Some(ref stop_ttype) = stop_lookahead {
                // Pause when we find the stop lookahead?
                if *stop_ttype == tok.ttype {
                    break;
                }
            }
            statement_list.push(self.statement());
        }

        return statement_list;
    }

    pub fn statement(&mut self) -> Statement {
        if let Some(ref tok) = self.lookahead {
            match tok.ttype {
                TokenType::OpenCurly => Statement::Block(self.block_statement()),
                _ => Statement::Expression(self.expression_statement()),
            }
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
            _type: "ExpressionStatement".into(),
            expression,
        }
    }

    pub fn block_statement(&mut self) -> BlockStatement {
        self.eat(TokenType::OpenCurly);
        if let Some(ref tok) = self.lookahead {
            let body = match tok.ttype {
                TokenType::CloseCurly => {
                    vec![] // Empty block
                }
                _ => self.statement_list(Some(TokenType::CloseCurly)),
            };
            self.eat(TokenType::CloseCurly);

            return BlockStatement {
                _type: "BlockStatement".into(),
                body,
            };
        } else {
            panic!("Unexpected end of input parsing block statement");
        }
    }

    /*
     * Expression
     *   : Literal
     *   ;
     * */
    pub fn expression(&mut self) -> Expression {
        let la = self.lookahead.clone().unwrap(); // TODO: should not clone!
        self.additive_expression()
    }

    pub fn additive_expression(&mut self) -> Expression {
        let lookahead = self.peek();
        let mut left = self.literal();
        let mut expr: Expression = Expression::Literal(left.clone());

        // ugh...
        while (self.lookahead.as_ref().unwrap().ttype.to_string()
            == TokenType::AdditiveOp.to_string())
        {
            println!("{left:?}, {lookahead:?}");
            let operator = self.eat(TokenType::AdditiveOp);
            let right = self.additive_expression();
            match right {
                Expression::Binary(right_expr) => {
                    expr = Expression::Binary(BinaryExpr {
                        _type: "BinaryExpression".into(),
                        operator: "+".into(),
                        left: BinaryExpressionNode::Literal(left.clone()),
                        right: BinaryExpressionNode::Binary(Box::new(right_expr)),
                    });
                }
                Expression::Literal(lit) => {
                    expr = Expression::Binary(BinaryExpr {
                        _type: "BinaryExpression".into(),
                        operator: "+".into(),
                        left: BinaryExpressionNode::Literal(left.clone()),
                        right: BinaryExpressionNode::Literal(lit),
                    });
                }
            };
        }

        return expr;
    }

    pub fn peek(&mut self) -> Token {
        return self.lookahead.clone().expect("Unexpected end of input");
    }

    // TODO: there's a better way to do this...
    pub fn literal(&mut self) -> Literal {
        let la = self
            .lookahead
            .clone()
            .expect("Unexpected end of input while trying to parse literal");
        match la.ttype {
            TokenType::Number => self.numeric_literal(),
            TokenType::String => self.string_literal(),
            _ => panic!("Unexpected token: {:?}", la.ttype),
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
    #[test]
    fn parses_block_statement() {
        let mut parser = Parser::init(
            r#"
            {
                42;
                "hello";
            }
        "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"BlockStatement\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"NumericLiteral\",\"value\":42}},{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"StringLiteral\",\"value\":\"hello\"}}]}]}"

        );
    }
    #[test]
    fn skips_single_line_comments() {
        let mut parser = Parser::init(
            r#"
            // Hello world!
            {
                42;
                "hello";
            }
        "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"BlockStatement\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"NumericLiteral\",\"value\":42}},{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"StringLiteral\",\"value\":\"hello\"}}]}]}"

        );
    }

    #[test]
    fn skips_multiline_comments() {
        let mut parser = Parser::init(
            r#"
            /*
             * Hello
             * This is a doc
             * 
             * */
            {
                42;
                "hello";
            }
        "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"BlockStatement\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"NumericLiteral\",\"value\":42}},{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"StringLiteral\",\"value\":\"hello\"}}]}]}"

        );
    }
    #[test]
    fn parses_a_simple_sum_expression() {
        let mut parser = Parser::init(
            r#"
                42 + 41;
        "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"BlockStatement\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"NumericLiteral\",\"value\":42}},{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"StringLiteral\",\"value\":\"hello\"}}]}]}"

        );
    }
}
