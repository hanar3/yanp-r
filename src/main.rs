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
    OpenParen,
    CloseParen,
    Skip,
    AdditiveOp,
    MultiplicativeOp,
    Identifier,
    SimpleAssign,
    ComplexAssign,
    Comma,
    Let,
    If,
    Else,
    RelationalOp,
    EqualityOp,
    True,
    False,
    Nil,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    While,
    Do,
    For,
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
    const SPEC: [(&'static str, TokenType); 31] = [
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
        (r"^\(", TokenType::OpenParen),
        (r"^\)", TokenType::CloseParen),
        (r"^\,", TokenType::Comma),
        // Keywords
        (r"^\blet\b", TokenType::Let),
        (r"^\bif\b", TokenType::If),
        (r"^\belse\b", TokenType::Else),
        (r"^\btrue\b", TokenType::True),
        (r"^\bfalse\b", TokenType::False),
        (r"^\bnil\b", TokenType::Nil),
        (r"^\bwhile\b", TokenType::While),
        (r"^\bdo\b", TokenType::Do),
        (r"^\bfor\b", TokenType::For),
        // Equality operators
        (r"^[=!]=", TokenType::EqualityOp),
        // Assignment Operators
        (r"^=", TokenType::SimpleAssign),
        (r"^[\*\/\+\-]=", TokenType::ComplexAssign),
        // Math operators
        (r"^[+\-]", TokenType::AdditiveOp),
        (r"^[*\/]", TokenType::MultiplicativeOp),
        (r"^[><]=?", TokenType::RelationalOp),
        // Logical Operator
        (r"^&&?", TokenType::LogicalAnd),
        (r"^\|\|", TokenType::LogicalOr),
        (r"^!", TokenType::LogicalNot),
        // Identifier
        (r"^\w+", TokenType::Identifier),
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

    #[serde(rename = "BooleanLiteral")]
    Boolean(bool),

    #[serde(rename = "NilLiteral")]
    Nil(()),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct Identifier {
    #[serde(rename = "type")]
    _type: String,
    name: String,
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
    left: Box<Expression>,
    right: Box<Expression>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct LogicalExpr {
    #[serde(rename = "type")]
    _type: String,
    operator: String,
    left: Box<Expression>,
    right: Box<Expression>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct UnaryExpr {
    #[serde(rename = "type")]
    _type: String,
    operator: String,
    argument: Box<Expression>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct AssignmentExpression {
    #[serde(rename = "type")]
    _type: String,
    operator: String,
    left: Box<Expression>,
    right: Box<Expression>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged)]
enum Expression {
    Literal(Literal),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Logical(LogicalExpr),
    AssignmentExpression(AssignmentExpression),
    LeftHandSideExpression(Identifier),
}

#[derive(Debug, Serialize, Deserialize)]
struct ExpressionStatement {
    #[serde(rename = "type")]
    _type: String,
    expression: Expression,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct VariableDeclarationStatement {
    #[serde(rename = "type")]
    _type: String,
    declarations: Vec<VariableDeclarator>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct VariableDeclarator {
    #[serde(rename = "type")]
    _type: String,
    id: Identifier,
    init: Option<Expression>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
enum Statement {
    Expression(ExpressionStatement),
    Block(BlockStatement),
    VariableDecl(VariableDeclarationStatement),
    If(Box<IfStatement>),
    While(Box<WhileStatement>),
    For(Box<ForStatement>),
    DoWhile(Box<DoWhileStatement>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
enum ForStatementInit {
    VariableDeclaration(VariableDeclarationStatement),
    AssignmentExpression(Expression),
}

#[derive(Debug, Serialize, Deserialize)]
struct IfStatement {
    #[serde(rename = "type")]
    _type: String,
    test: Expression,
    consequent: Statement,
    alternate: Option<Statement>,
}

#[derive(Debug, Serialize, Deserialize)]
struct WhileStatement {
    #[serde(rename = "type")]
    _type: String,
    test: Expression,
    body: Statement,
}

#[derive(Debug, Serialize, Deserialize)]
struct DoWhileStatement {
    #[serde(rename = "type")]
    _type: String,
    test: Expression,
    body: Statement,
}

#[derive(Debug, Serialize, Deserialize)]
struct ForStatement {
    #[serde(rename = "type")]
    _type: String,
    init: Option<ForStatementInit>,
    test: Option<Expression>,
    update: Option<Expression>,
    body: Statement,
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
                TokenType::Let => Statement::VariableDecl(self.variable_declaration_statement()),
                TokenType::If => Statement::If(Box::new(self.if_statement())),
                TokenType::While | TokenType::Do | TokenType::For => self.iteration_statement(),
                _ => Statement::Expression(self.expression_statement()),
            }
        } else {
            panic!("Unexpected end of input parsing statement");
        }
    }

    pub fn iteration_statement(&mut self) -> Statement {
        match self.lookahead().ttype {
            TokenType::While => Statement::While(Box::new(self.while_statement())),
            TokenType::For => Statement::For(Box::new(self.for_statement())),
            TokenType::Do => Statement::DoWhile(Box::new(self.do_while_statement())),
            _ => panic!("Lookahead is not an iteration statement"),
        }
    }

    pub fn while_statement(&mut self) -> WhileStatement {
        self.eat(TokenType::While);
        self.eat(TokenType::OpenParen);
        let test = self.expression();
        self.eat(TokenType::CloseParen);
        let body = self.statement();

        return WhileStatement {
            _type: "WhileStatement".to_string(),
            test,
            body,
        };
    }

    pub fn for_statement(&mut self) -> ForStatement {
        self.eat(TokenType::For);
        self.eat(TokenType::OpenParen);

        let init = if self.lookahead().ttype != TokenType::Semicolon {
            Some(self.for_statement_init())
        } else {
            None
        };
        self.eat(TokenType::Semicolon);

        let test = if self.lookahead().ttype != TokenType::Semicolon {
            Some(self.expression())
        } else {
            None
        };
        self.eat(TokenType::Semicolon);

        let update = if self.lookahead().ttype != TokenType::CloseParen {
            Some(self.expression())
        } else {
            None
        };
        self.eat(TokenType::CloseParen);

        let body = self.statement();

        return ForStatement {
            _type: "ForStatement".into(),
            init,
            test,
            update,
            body,
        };
    }

    pub fn for_statement_init(&mut self) -> ForStatementInit {
        if self.lookahead().ttype == TokenType::Let {
            return ForStatementInit::VariableDeclaration(self.variable_statement_init());
        } else {
            return ForStatementInit::AssignmentExpression(self.expression());
        }
    }

    pub fn do_while_statement(&mut self) -> DoWhileStatement {
        self.eat(TokenType::Do);
        let body = self.statement();
        self.eat(TokenType::While);
        self.eat(TokenType::OpenParen);
        let test = self.expression();
        self.eat(TokenType::CloseParen);
        self.eat(TokenType::Semicolon);

        DoWhileStatement {
            _type: "DoWhileStatement".to_string(),
            test,
            body,
        }
    }

    pub fn if_statement(&mut self) -> IfStatement {
        let lookahead = self.lookahead.clone();
        self.eat(TokenType::If);
        self.eat(TokenType::OpenParen);
        let test = self.expression();
        self.eat(TokenType::CloseParen);

        let consequent = self.statement();

        let alternate: Option<Statement> = if let Some(ref lookahead) = self.lookahead {
            if lookahead.ttype == TokenType::Else {
                self.eat(TokenType::Else);
                Some(self.statement())
            } else {
                None
            }
        } else {
            None
        };

        IfStatement {
            _type: "IfStatement".into(),
            test,
            consequent,
            alternate,
        }
    }

    pub fn variable_statement_init(&mut self) -> VariableDeclarationStatement {
        self.eat(TokenType::Let);
        return VariableDeclarationStatement {
            _type: "VariableDeclaration".into(),
            declarations: self.variable_declaration_list(),
        };
    }

    pub fn variable_declaration_statement(&mut self) -> VariableDeclarationStatement {
        let statement = self.variable_statement_init();
        self.eat(TokenType::Semicolon);
        return statement;
    }

    pub fn variable_declaration_list(&mut self) -> Vec<VariableDeclarator> {
        let mut declarations = vec![];
        loop {
            declarations.push(self.variable_declaration());

            if self.lookahead().ttype != TokenType::Comma {
                break;
            }

            self.eat(TokenType::Comma);
        }
        return declarations;
    }

    pub fn variable_declaration(&mut self) -> VariableDeclarator {
        let id = self.identifier();

        let init = if self.lookahead().ttype != TokenType::Comma
            && self.lookahead().ttype != TokenType::Semicolon
        {
            Some(self.variable_initializer())
        } else {
            None
        };

        VariableDeclarator {
            _type: "VariableDeclarator".into(),
            id,
            init,
        }
    }

    pub fn variable_initializer(&mut self) -> Expression {
        self.eat(TokenType::SimpleAssign);
        return self.assignment_expression();
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

    pub fn lookahead(&self) -> &Token {
        return self.lookahead.as_ref().expect("Unexpected end of input");
    }

    pub fn expression(&mut self) -> Expression {
        self.assignment_expression()
    }

    pub fn is_assignment_operator(&mut self, lookahead: &Token) -> bool {
        match lookahead.ttype {
            TokenType::SimpleAssign | TokenType::ComplexAssign => true,
            _ => false,
        }
    }

    pub fn is_valid_left(&mut self, left: &Expression) -> bool {
        match left {
            Expression::LeftHandSideExpression(_) => true,
            _ => false,
        }
    }

    pub fn assignment_expression(&mut self) -> Expression {
        let left = self.logical_or_expression();

        if !self.is_assignment_operator(&self.lookahead().clone()) {
            return left;
        }

        if !self.is_valid_left(&left) {
            panic!("Expected LeftHandSideExpression but got {:?}", left);
        }

        return Expression::AssignmentExpression(AssignmentExpression {
            _type: "AssignmentExpression".into(),
            operator: self.assignment_operator().value,
            left: Box::new(left),
            right: Box::new(self.assignment_expression()),
        });
    }

    pub fn logical_and_expression(&mut self) -> Expression {
        let mut left = self.equality_expression();

        while (self.lookahead().ttype == TokenType::LogicalAnd) {
            let operator = self.eat(TokenType::LogicalAnd);
            let right = self.equality_expression();
            left = Expression::Logical(LogicalExpr {
                _type: "LogicalExpression".into(),
                operator: operator.value,
                left: Box::new(left),
                right: Box::new(right),
            });
        }

        return left;
    }

    pub fn logical_or_expression(&mut self) -> Expression {
        let mut left = self.logical_and_expression();

        while (self.lookahead().ttype == TokenType::LogicalOr) {
            let operator = self.eat(TokenType::LogicalOr);
            let right = self.logical_and_expression();
            left = Expression::Logical(LogicalExpr {
                _type: "LogicalExpression".into(),
                operator: operator.value,
                left: Box::new(left),
                right: Box::new(right),
            });
        }

        return left;
    }

    pub fn equality_expression(&mut self) -> Expression {
        let mut left = self.relational_expression();

        while (self.lookahead().ttype == TokenType::EqualityOp) {
            let operator = self.eat(TokenType::EqualityOp);
            let right = self.relational_expression();
            left = Expression::Binary(BinaryExpr {
                _type: "BinaryExpression".into(),
                operator: operator.value,
                left: Box::new(left),
                right: Box::new(right),
            });
        }

        return left;
    }
    pub fn relational_expression(&mut self) -> Expression {
        let mut left = self.additive_expression();

        while (self.lookahead().ttype == TokenType::RelationalOp) {
            let operator = self.eat(TokenType::RelationalOp);
            let right = self.additive_expression();
            left = Expression::Binary(BinaryExpr {
                _type: "BinaryExpression".into(),
                operator: operator.value,
                left: Box::new(left),
                right: Box::new(right),
            });
        }

        return left;
    }

    pub fn assignment_operator(&mut self) -> Token {
        let lookahead = self.lookahead();
        match lookahead.ttype {
            TokenType::SimpleAssign => self.eat(TokenType::SimpleAssign),
            TokenType::ComplexAssign => self.eat(TokenType::ComplexAssign),
            _ => panic!("Expected assignment operator, instead got {:?}", lookahead),
        }
    }

    pub fn is_literal(&self, token_type: TokenType) -> bool {
        return token_type == TokenType::Number
            || token_type == TokenType::String
            || token_type == TokenType::True
            || token_type == TokenType::False
            || token_type == TokenType::Nil;
    }

    pub fn unary_expression(&mut self) -> Expression {
        let lookahead = self.lookahead();
        let operator = match lookahead.ttype {
            TokenType::AdditiveOp => Some(self.eat(TokenType::AdditiveOp).value),
            TokenType::LogicalNot => Some(self.eat(TokenType::LogicalNot).value),
            _ => None,
        };

        if let Some(ref op) = operator {
            return Expression::Unary(UnaryExpr {
                _type: "UnaryExpression".into(),
                operator: op.to_string(),
                argument: Box::new(self.unary_expression()),
            });
        }

        return self.left_hand_side_expression();
    }

    pub fn primary_expression(&mut self) -> Expression {
        let lookahead = self.lookahead();
        if self.is_literal(lookahead.ttype.clone()) {
            return Expression::Literal(self.literal());
        }

        match lookahead.ttype {
            TokenType::String | TokenType::Number => Expression::Literal(self.literal()),
            TokenType::OpenParen => self.parse_parenthesized_expression(),
            TokenType::Identifier => Expression::LeftHandSideExpression(self.identifier()),
            _ => self.left_hand_side_expression(),
        }
    }

    pub fn left_hand_side_expression(&mut self) -> Expression {
        return self.primary_expression();
    }

    pub fn identifier(&mut self) -> Identifier {
        return Identifier {
            _type: "Identifier".into(),
            name: self.eat(TokenType::Identifier).value,
        };
    }

    pub fn multiplicative_expression(&mut self) -> Expression {
        let mut left = self.unary_expression();

        while (self.lookahead().ttype == TokenType::MultiplicativeOp) {
            let operator = self.eat(TokenType::MultiplicativeOp);
            let right = self.unary_expression();
            left = Expression::Binary(BinaryExpr {
                _type: "BinaryExpression".into(),
                operator: operator.value,
                left: Box::new(left),
                right: Box::new(right),
            });
        }

        return left;
    }

    pub fn additive_expression(&mut self) -> Expression {
        let mut left = self.multiplicative_expression();

        while (self.lookahead().ttype == TokenType::AdditiveOp) {
            let operator = self.eat(TokenType::AdditiveOp);
            let right = self.multiplicative_expression();
            left = Expression::Binary(BinaryExpr {
                _type: "BinaryExpression".into(),
                operator: operator.value,
                left: Box::new(left),
                right: Box::new(right),
            });
        }

        return left;
    }

    pub fn parse_parenthesized_expression(&mut self) -> Expression {
        self.eat(TokenType::OpenParen);
        let expr = self.expression();
        self.eat(TokenType::CloseParen);
        return expr;
    }

    pub fn literal(&mut self) -> Literal {
        let la = self.lookahead();
        match self.lookahead().ttype {
            TokenType::Number => self.numeric_literal(),
            TokenType::String => self.string_literal(),
            TokenType::True | TokenType::False => self.boolean_literal(),
            TokenType::Nil => self.nil_literal(),
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

    pub fn nil_literal(&mut self) -> Literal {
        let token = self.eat(TokenType::Nil);
        return Literal::Nil(());
    }

    pub fn boolean_literal(&mut self) -> Literal {
        let lookahead = self.lookahead.clone();
        let literal = if let Some(ref lookahead_token) = lookahead {
            match lookahead_token.ttype {
                TokenType::True => {
                    self.eat(TokenType::True);
                    Literal::Boolean(true)
                }
                TokenType::False => {
                    self.eat(TokenType::False);
                    Literal::Boolean(lookahead_token.value.eq("true"))
                }
                _ => panic!("Expected BooleanLiteral, instead found {:?}", lookahead),
            }
        } else {
            panic!("Expected BooleanLiteral, instead found end of input");
        };

        return literal;
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
                "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"BinaryExpression\",\"operator\":\"+\",\"left\":{\"type\":\"NumericLiteral\",\"value\":42},\"right\":{\"type\":\"NumericLiteral\",\"value\":41}}}]}"
            );
    }
    #[test]
    fn parses_a_chained_binary_expression() {
        let mut parser = Parser::init(
            r#"
                    2 + 3 - 1;
            "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
                ast,

                "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"BinaryExpression\",\"operator\":\"-\",\"left\":{\"type\":\"BinaryExpression\",\"operator\":\"+\",\"left\":{\"type\":\"NumericLiteral\",\"value\":2},\"right\":{\"type\":\"NumericLiteral\",\"value\":3}},\"right\":{\"type\":\"NumericLiteral\",\"value\":1}}}]}"

            );
    }
    #[test]
    fn parses_a_parenthesized_binary_expression() {
        let mut parser = Parser::init(
            r#"
                    2 + (3 - 1);
            "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
                ast,
                 "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"BinaryExpression\",\"operator\":\"+\",\"left\":{\"type\":\"NumericLiteral\",\"value\":2},\"right\":{\"type\":\"BinaryExpression\",\"operator\":\"-\",\"left\":{\"type\":\"NumericLiteral\",\"value\":3},\"right\":{\"type\":\"NumericLiteral\",\"value\":1}}}}]}"

            );
    }
    #[test]
    fn parses_a_parenthesized_binary_expression_2() {
        let mut parser = Parser::init(
            r#"
                    (2 + 3) - 1;
            "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
                ast,
                "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"BinaryExpression\",\"operator\":\"-\",\"left\":{\"type\":\"BinaryExpression\",\"operator\":\"+\",\"left\":{\"type\":\"NumericLiteral\",\"value\":2},\"right\":{\"type\":\"NumericLiteral\",\"value\":3}},\"right\":{\"type\":\"NumericLiteral\",\"value\":1}}}]}"

            );
    }
    #[test]
    fn parses_a_multiplicative_expression() {
        let mut parser = Parser::init(
            r#"
                    3 * 2;
            "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
                ast,
                "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"BinaryExpression\",\"operator\":\"*\",\"left\":{\"type\":\"NumericLiteral\",\"value\":3},\"right\":{\"type\":\"NumericLiteral\",\"value\":2}}}]}"

            );
    }
    #[test]
    fn parses_a_chained_multiplication_with_correct_operator_precedence() {
        let mut parser = Parser::init(
            r#"
                    3 + 2 * 3;
            "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
                ast,
                "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"BinaryExpression\",\"operator\":\"+\",\"left\":{\"type\":\"NumericLiteral\",\"value\":3},\"right\":{\"type\":\"BinaryExpression\",\"operator\":\"*\",\"left\":{\"type\":\"NumericLiteral\",\"value\":2},\"right\":{\"type\":\"NumericLiteral\",\"value\":3}}}}]}"
            );
    }

    #[test]
    fn parses_an_assignment() {
        let mut parser = Parser::init(
            r#"
                    x = 3;
            "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
                ast,
                "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"AssignmentExpression\",\"operator\":\"=\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":3}}}]}"

            );
    }

    #[test]
    fn parses_chained_variable_assignment() {
        let mut parser = Parser::init(
            r#"
                    x = y = 3;
            "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
                ast,
                "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"AssignmentExpression\",\"operator\":\"=\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"AssignmentExpression\",\"operator\":\"=\",\"left\":{\"type\":\"Identifier\",\"name\":\"y\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":3}}}}]}"

            );
    }

    #[test]
    pub fn parses_variable_assignment_with_binary_expressions() {
        let mut parser = Parser::init(
            r#"
                    x = y + 3;
            "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
                ast,
                "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"AssignmentExpression\",\"operator\":\"=\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"BinaryExpression\",\"operator\":\"+\",\"left\":{\"type\":\"Identifier\",\"name\":\"y\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":3}}}}]}"

            );
    }

    #[test]
    pub fn parses_variable_declaration() {
        let mut parser = Parser::init(
            r#"
                    let x;
            "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
                ast,
                "{\"type\":\"Program\",\"body\":[{\"type\":\"VariableDeclaration\",\"declarations\":[{\"type\":\"VariableDeclarator\",\"id\":{\"type\":\"Identifier\",\"name\":\"x\"},\"init\":null}]}]}"

        );
    }

    #[test]
    pub fn parses_a_variable_declaration_with_assignment() {
        let mut parser = Parser::init(
            r#"
                let x = 42;
        "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"VariableDeclaration\",\"declarations\":[{\"type\":\"VariableDeclarator\",\"id\":{\"type\":\"Identifier\",\"name\":\"x\"},\"init\":{\"type\":\"NumericLiteral\",\"value\":42}}]}]}"

        );
    }
    #[test]
    pub fn parses_multiple_variable_declarations() {
        let mut parser = Parser::init(
            r#"
                let x, y, z;
        "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"VariableDeclaration\",\"declarations\":[{\"type\":\"VariableDeclarator\",\"id\":{\"type\":\"Identifier\",\"name\":\"x\"},\"init\":null},{\"type\":\"VariableDeclarator\",\"id\":{\"type\":\"Identifier\",\"name\":\"y\"},\"init\":null},{\"type\":\"VariableDeclarator\",\"id\":{\"type\":\"Identifier\",\"name\":\"z\"},\"init\":null}]}]}"

        );
    }
    #[test]
    pub fn parses_chained_assignment_inside_variable_decl() {
        let mut parser = Parser::init(
            r#"
                let foo = bar = 10;
        "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"VariableDeclaration\",\"declarations\":[{\"type\":\"VariableDeclarator\",\"id\":{\"type\":\"Identifier\",\"name\":\"foo\"},\"init\":{\"type\":\"AssignmentExpression\",\"operator\":\"=\",\"left\":{\"type\":\"Identifier\",\"name\":\"bar\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":10}}}]}]}"

        );
    }
    #[test]
    pub fn parses_complex_assignment() {
        let mut parser = Parser::init(
            r#"
             x += 42;
        "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"AssignmentExpression\",\"operator\":\"+=\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":42}}}]}"

        );
    }

    #[test]
    pub fn parses_ifelse_statement() {
        let mut parser = Parser::init(
            r#"
            if (x) {
                x = 1;
            } else {
                x = 2;
            }
        "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"IfStatement\",\"test\":{\"type\":\"Identifier\",\"name\":\"x\"},\"consequent\":{\"type\":\"BlockStatement\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"AssignmentExpression\",\"operator\":\"=\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":1}}}]},\"alternate\":{\"type\":\"BlockStatement\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"AssignmentExpression\",\"operator\":\"=\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":2}}}]}}]}"

        );
    }
    #[test]
    // no else
    pub fn parses_if_statement() {
        let mut parser = Parser::init(
            r#"
            if (x) {
                x = 1;
            }
        "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"IfStatement\",\"test\":{\"type\":\"Identifier\",\"name\":\"x\"},\"consequent\":{\"type\":\"BlockStatement\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"AssignmentExpression\",\"operator\":\"=\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":1}}}]},\"alternate\":null}]}"
        );
    }

    #[test]
    // no else
    pub fn parses_relational_op_gt() {
        let mut parser = Parser::init(r#"x > 10;"#);
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"BinaryExpression\",\"operator\":\">\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":10}}}]}"
        );
    }

    #[test]
    // no else
    pub fn parses_relational_op_lt() {
        let mut parser = Parser::init(r#"x < 10;"#);
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"BinaryExpression\",\"operator\":\"<\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":10}}}]}"
        );
    }

    #[test]
    pub fn parses_ifelse_with_relational_op() {
        let mut parser = Parser::init(
            r#"
            if (x > 10) {
                x = 1;
            } else {
                x = 2;
            }
        "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"IfStatement\",\"test\":{\"type\":\"BinaryExpression\",\"operator\":\">\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":10}},\"consequent\":{\"type\":\"BlockStatement\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"AssignmentExpression\",\"operator\":\"=\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":1}}}]},\"alternate\":{\"type\":\"BlockStatement\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"AssignmentExpression\",\"operator\":\"=\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":2}}}]}}]}"
        );
    }

    #[test]
    pub fn parses_equality_op() {
        let mut parser = Parser::init(r#"x + 5 > 10 == true;"#);
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"BinaryExpression\",\"operator\":\"==\",\"left\":{\"type\":\"BinaryExpression\",\"operator\":\">\",\"left\":{\"type\":\"BinaryExpression\",\"operator\":\"+\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":5}},\"right\":{\"type\":\"NumericLiteral\",\"value\":10}},\"right\":{\"type\":\"BooleanLiteral\",\"value\":true}}}]}"
        );
    }
    #[test]
    pub fn parses_neq() {
        let mut parser = Parser::init(r#"x + 5 > 10 != nil;"#);
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"BinaryExpression\",\"operator\":\"!=\",\"left\":{\"type\":\"BinaryExpression\",\"operator\":\">\",\"left\":{\"type\":\"BinaryExpression\",\"operator\":\"+\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":5}},\"right\":{\"type\":\"NumericLiteral\",\"value\":10}},\"right\":{\"type\":\"NilLiteral\",\"value\":null}}}]}"
        );
    }

    #[test]
    pub fn parses_logical() {
        let mut parser = Parser::init(r#"x && y;"#);
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"LogicalExpression\",\"operator\":\"&&\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"Identifier\",\"name\":\"y\"}}}]}"
        );
    }

    #[test]
    pub fn parses_logical_and_with_binary() {
        let mut parser = Parser::init(r#"x > 0 && y < 1;"#);
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"LogicalExpression\",\"operator\":\"&&\",\"left\":{\"type\":\"BinaryExpression\",\"operator\":\">\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":0}},\"right\":{\"type\":\"BinaryExpression\",\"operator\":\"<\",\"left\":{\"type\":\"Identifier\",\"name\":\"y\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":1}}}}]}"
        );
    }

    #[test]
    pub fn parses_logical_or_with_binary() {
        let mut parser = Parser::init(r#"x > 0 || y < 1;"#);
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"LogicalExpression\",\"operator\":\"||\",\"left\":{\"type\":\"BinaryExpression\",\"operator\":\">\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":0}},\"right\":{\"type\":\"BinaryExpression\",\"operator\":\"<\",\"left\":{\"type\":\"Identifier\",\"name\":\"y\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":1}}}}]}"

        );
    }

    #[test]
    pub fn parses_basic_unary_expression() {
        let mut parser = Parser::init(r#"-x;"#);
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,

            "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"UnaryExpression\",\"operator\":\"-\",\"argument\":{\"type\":\"Identifier\",\"name\":\"x\"}}}]}"

        );
    }

    #[test]
    pub fn parses_chained_unary_expression() {
        let mut parser = Parser::init(r#"-!x;"#);
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"UnaryExpression\",\"operator\":\"-\",\"argument\":{\"type\":\"UnaryExpression\",\"operator\":\"!\",\"argument\":{\"type\":\"Identifier\",\"name\":\"x\"}}}}]}"
        );
    }

    #[test]
    pub fn parses_while_statement() {
        let mut parser = Parser::init(
            r#"
            while(x < 10) {
                x += 1;
            }
        "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"WhileStatement\",\"test\":{\"type\":\"BinaryExpression\",\"operator\":\"<\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":10}},\"body\":{\"type\":\"BlockStatement\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"AssignmentExpression\",\"operator\":\"+=\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":1}}}]}}]}"
        );
    }

    #[test]
    pub fn parses_do_while_statement() {
        let mut parser = Parser::init(
            r#"
            do {
                x += 1;
            } while(x < 10);

        "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"DoWhileStatement\",\"test\":{\"type\":\"BinaryExpression\",\"operator\":\"<\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":10}},\"body\":{\"type\":\"BlockStatement\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"AssignmentExpression\",\"operator\":\"+=\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":1}}}]}}]}"
        );
    }

    #[test]
    pub fn parses_for_statement() {
        let mut parser = Parser::init(
            r#"
            for (let i = 0; i < 10; i += 1) {
                x += 1;
            }

        "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"ForStatement\",\"init\":{\"type\":\"VariableDeclaration\",\"declarations\":[{\"type\":\"VariableDeclarator\",\"id\":{\"type\":\"Identifier\",\"name\":\"i\"},\"init\":{\"type\":\"NumericLiteral\",\"value\":0}}]},\"test\":{\"type\":\"BinaryExpression\",\"operator\":\"<\",\"left\":{\"type\":\"Identifier\",\"name\":\"i\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":10}},\"update\":{\"type\":\"AssignmentExpression\",\"operator\":\"+=\",\"left\":{\"type\":\"Identifier\",\"name\":\"i\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":1}},\"body\":{\"type\":\"BlockStatement\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"AssignmentExpression\",\"operator\":\"+=\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":1}}}]}}]}"
        );
    }

    #[test]
    pub fn parses_infinite_for_statement() {
        let mut parser = Parser::init(
            r#"
            for (;;) {
                x += 1;
            }

        "#,
        );
        let ast = parser.parse();
        let ast = serde_json::to_string(&ast).unwrap();
        assert_eq!(
            ast,
            "{\"type\":\"Program\",\"body\":[{\"type\":\"ForStatement\",\"init\":null,\"test\":null,\"update\":null,\"body\":{\"type\":\"BlockStatement\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"AssignmentExpression\",\"operator\":\"+=\",\"left\":{\"type\":\"Identifier\",\"name\":\"x\"},\"right\":{\"type\":\"NumericLiteral\",\"value\":1}}}]}}]}"
        );
    }
}
