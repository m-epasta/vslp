#![allow(dead_code)]

use crate::lexer::{Lexer, Token, TokenKind};
use text_size::{TextRange, TextSize};

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct ParseResult<T> {
    pub value: T,
    pub errors: Vec<ParseError>,
}

impl<T> ParseResult<T> {
    pub fn new(value: T) -> Self {
        Self {
            value,
            errors: Vec::new(),
        }
    }

    pub fn with_errors(value: T, errors: Vec<ParseError>) -> Self {
        Self { value, errors }
    }
}

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub items: Vec<Item>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub enum Item {
    Function(Function),
    Class(Class),
    VariableDeclaration(VariableDeclaration),
    Import(Import),
    Export(Export),
    Statement(Statement),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Identifier,
    pub params: Vec<Parameter>,
    pub body: Block,
    pub is_async: bool,
    pub decorators: Vec<Decorator>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: Identifier,
    pub super_class: Option<Identifier>,
    pub methods: Vec<Function>,
    pub fields: Vec<VariableDeclaration>,
    pub decorators: Vec<Decorator>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub value: Option<Expression>,
    pub is_mutable: bool,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub path: String,
    pub alias: Option<String>,
    pub items: Vec<String>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct Export {
    pub item: Box<Item>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: Identifier,
    pub default_value: Option<Expression>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct Decorator {
    pub name: String,
    pub args: Vec<Expression>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    VariableDeclaration(VariableDeclaration),
    If(IfStatement),
    While(WhileStatement),
    For(ForStatement),
    Return(ReturnStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    Try(TryStatement),
    Block(Block),
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_branch: Box<Statement>,
    pub else_branch: Option<Box<Statement>>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Box<Statement>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct ForStatement {
    pub init: Option<Box<Statement>>,
    pub condition: Option<Expression>,
    pub update: Option<Expression>,
    pub body: Box<Statement>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub value: Option<Expression>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct BreakStatement {
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct ContinueStatement {
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct TryStatement {
    pub try_block: Block,
    pub catch_block: Option<CatchBlock>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct CatchBlock {
    pub parameter: Option<Identifier>,
    pub body: Block,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    Call(CallExpression),
    Member(MemberExpression),
    Index(IndexExpression),
    Assignment(AssignmentExpression),
    Array(ArrayExpression),
    Object(ObjectExpression),
    Function(FunctionExpression),
    Await(AwaitExpression),
    This(ThisExpression),
    Super(SuperExpression),
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
}

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub operand: Box<Expression>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Minus,
    Not,
    PreIncrement,
    PostIncrement,
    PreDecrement,
    PostDecrement,
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct MemberExpression {
    pub object: Box<Expression>,
    pub property: Identifier,
    pub computed: bool,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct IndexExpression {
    pub object: Box<Expression>,
    pub index: Box<Expression>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct AssignmentExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct ArrayExpression {
    pub elements: Vec<Expression>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct ObjectExpression {
    pub properties: Vec<ObjectProperty>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct ObjectProperty {
    pub key: ObjectPropertyKey,
    pub value: Expression,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub enum ObjectPropertyKey {
    Identifier(Identifier),
    String(String),
}

#[derive(Debug, Clone)]
pub struct FunctionExpression {
    pub params: Vec<Parameter>,
    pub body: Block,
    pub is_async: bool,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct AwaitExpression {
    pub argument: Box<Expression>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct ThisExpression {
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct SuperExpression {
    pub range: TextRange,
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let tokens = lexer.tokenize();
        Self {
            tokens,
            current: 0,
            errors: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> ParseResult<SourceFile> {
        let start = if self.tokens.is_empty() {
            TextSize::new(0)
        } else {
            self.current_token().range.start()
        };
        let mut items = Vec::new();

        while !self.is_at_end() {
            if matches!(
                self.current_token().kind,
                TokenKind::Whitespace | TokenKind::Comment | TokenKind::Newline
            ) {
                self.advance();
                continue;
            }

            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(error) => {
                    self.errors.push(error);
                    self.synchronize();
                }
            }
        }

        let end = if items.is_empty() {
            start
        } else {
            self.previous_token().range.end()
        };

        let source_file = SourceFile {
            items,
            range: TextRange::new(start, end),
        };

        ParseResult::with_errors(source_file, self.errors.clone())
    }

    fn parse_item(&mut self) -> Result<Item, ParseError> {
        let mut decorators = Vec::new();
        while self.check(&TokenKind::AtBracket) {
            decorators.push(self.parse_decorator()?);
        }

        match self.current_token().kind {
            TokenKind::Fn => {
                let mut func = self.parse_function()?;
                if let Item::Function(ref mut f) = func {
                    f.decorators = decorators;
                }
                Ok(func)
            }
            TokenKind::Class => {
                let mut class = self.parse_class()?;
                if let Item::Class(ref mut c) = class {
                    c.decorators = decorators;
                }
                Ok(class)
            }
            TokenKind::Let => Ok(Item::VariableDeclaration(
                self.parse_variable_declaration()?,
            )),
            TokenKind::Import => Ok(Item::Import(self.parse_import()?)),
            TokenKind::Export => Ok(Item::Export(self.parse_export()?)),
            _ => Ok(Item::Statement(self.parse_statement()?)),
        }
    }

    fn parse_decorator(&mut self) -> Result<Decorator, ParseError> {
        let start = self.current_token().range.start();
        self.consume(&TokenKind::AtBracket, "Expected '@['")?;

        let name = self.consume_identifier("Expected decorator name")?.name;
        let mut args = Vec::new();

        if self.check(&TokenKind::LeftParen) {
            self.advance();
            while !self.check(&TokenKind::RightParen) && !self.is_at_end() {
                args.push(self.parse_expression()?);
                if !self.check(&TokenKind::RightParen) {
                    self.consume(&TokenKind::Comma, "Expected ','")?;
                }
            }
            self.consume(&TokenKind::RightParen, "Expected ')'")?;
        }

        self.consume(&TokenKind::RightBracket, "Expected ']'")?;

        let end = self.previous_token().range.end();
        Ok(Decorator {
            name,
            args,
            range: TextRange::new(start, end),
        })
    }

    fn parse_function(&mut self) -> Result<Item, ParseError> {
        let start = self.current_token().range.start();
        let is_async = if self.check(&TokenKind::Async) {
            self.advance();
            true
        } else {
            false
        };

        self.consume(&TokenKind::Fn, "Expected 'fn'")?;
        let name = self.consume_identifier("Expected function name")?;

        self.consume(&TokenKind::LeftParen, "Expected '('")?;
        let mut params = Vec::new();

        while !self.check(&TokenKind::RightParen) && !self.is_at_end() {
            params.push(self.parse_parameter()?);
            if !self.check(&TokenKind::RightParen) {
                self.consume(&TokenKind::Comma, "Expected ','")?;
            }
        }

        self.consume(&TokenKind::RightParen, "Expected ')'")?;
        let body = self.parse_block()?;
        let end = body.range.end();

        Ok(Item::Function(Function {
            name,
            params,
            body,
            is_async,
            decorators: Vec::new(),
            range: TextRange::new(start, end),
        }))
    }

    fn parse_class(&mut self) -> Result<Item, ParseError> {
        let start = self.current_token().range.start();
        self.consume(&TokenKind::Class, "Expected 'class'")?;
        let name = self.consume_identifier("Expected class name")?;

        let super_class = if self.check(&TokenKind::Less) {
            self.advance();
            Some(self.consume_identifier("Expected superclass name")?)
        } else {
            None
        };

        self.consume(&TokenKind::LeftBrace, "Expected '{'")?;

        let mut methods = Vec::new();
        let mut fields = Vec::new();

        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            if matches!(
                self.current_token().kind,
                TokenKind::Whitespace | TokenKind::Comment | TokenKind::Newline
            ) {
                self.advance();
                continue;
            }

            if self.check(&TokenKind::Fn) {
                if let Item::Function(func) = self.parse_function()? {
                    methods.push(func);
                }
            } else if self.check(&TokenKind::Let) {
                fields.push(self.parse_variable_declaration()?);
            } else {
                return Err(ParseError {
                    message: "Expected method or field declaration".to_string(),
                    range: self.current_token().range,
                });
            }
        }

        self.consume(&TokenKind::RightBrace, "Expected '}'")?;
        let end = self.previous_token().range.end();

        Ok(Item::Class(Class {
            name,
            super_class,
            methods,
            fields,
            decorators: Vec::new(),
            range: TextRange::new(start, end),
        }))
    }

    fn parse_variable_declaration(&mut self) -> Result<VariableDeclaration, ParseError> {
        let start = self.current_token().range.start();
        self.consume(&TokenKind::Let, "Expected 'let'")?;
        let name = self.consume_identifier("Expected variable name")?;

        let value = if self.check(&TokenKind::Equal) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        let end = self.previous_token().range.end();
        Ok(VariableDeclaration {
            name,
            value,
            is_mutable: true,
            range: TextRange::new(start, end),
        })
    }

    fn parse_import(&mut self) -> Result<Import, ParseError> {
        let start = self.current_token().range.start();
        self.consume(&TokenKind::Import, "Expected 'import'")?;

        let path = if self.check(&TokenKind::String) {
            self.advance();
            self.previous_token().text.trim_matches('"').to_string()
        } else {
            return Err(ParseError {
                message: "Expected import path".to_string(),
                range: self.current_token().range,
            });
        };

        let end = self.previous_token().range.end();
        Ok(Import {
            path,
            alias: None,
            items: Vec::new(),
            range: TextRange::new(start, end),
        })
    }

    fn parse_export(&mut self) -> Result<Export, ParseError> {
        let start = self.current_token().range.start();
        self.consume(&TokenKind::Export, "Expected 'export'")?;

        let item = Box::new(self.parse_item()?);
        let end = self.previous_token().range.end();

        Ok(Export {
            item,
            range: TextRange::new(start, end),
        })
    }

    fn parse_parameter(&mut self) -> Result<Parameter, ParseError> {
        let start = self.current_token().range.start();
        let name = self.consume_identifier("Expected parameter name")?;

        let default_value = if self.check(&TokenKind::Equal) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        let end = self.previous_token().range.end();
        Ok(Parameter {
            name,
            default_value,
            range: TextRange::new(start, end),
        })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.current_token().kind {
            TokenKind::If => Ok(Statement::If(self.parse_if_statement()?)),
            TokenKind::While => Ok(Statement::While(self.parse_while_statement()?)),
            TokenKind::For => Ok(Statement::For(self.parse_for_statement()?)),
            TokenKind::Return => Ok(Statement::Return(self.parse_return_statement()?)),
            TokenKind::LeftBrace => Ok(Statement::Block(self.parse_block()?)),
            TokenKind::Let => Ok(Statement::VariableDeclaration(
                self.parse_variable_declaration()?,
            )),
            _ => Ok(Statement::Expression(self.parse_expression()?)),
        }
    }

    fn parse_if_statement(&mut self) -> Result<IfStatement, ParseError> {
        let start = self.current_token().range.start();
        self.consume(&TokenKind::If, "Expected 'if'")?;

        let condition = self.parse_expression()?;
        let then_branch = Box::new(self.parse_statement()?);

        let else_branch = if self.check(&TokenKind::Else) {
            self.advance();
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        let end = self.previous_token().range.end();
        Ok(IfStatement {
            condition,
            then_branch,
            else_branch,
            range: TextRange::new(start, end),
        })
    }

    fn parse_while_statement(&mut self) -> Result<WhileStatement, ParseError> {
        let start = self.current_token().range.start();
        self.consume(&TokenKind::While, "Expected 'while'")?;

        let condition = self.parse_expression()?;
        let body = Box::new(self.parse_statement()?);

        let end = self.previous_token().range.end();
        Ok(WhileStatement {
            condition,
            body,
            range: TextRange::new(start, end),
        })
    }

    fn parse_for_statement(&mut self) -> Result<ForStatement, ParseError> {
        let start = self.current_token().range.start();
        self.consume(&TokenKind::For, "Expected 'for'")?;

        let init = if self.check(&TokenKind::Semicolon) {
            None
        } else {
            Some(Box::new(self.parse_statement()?))
        };

        if !self.check(&TokenKind::Semicolon) {
            self.consume(&TokenKind::Semicolon, "Expected ';'")?;
        } else {
            self.advance();
        }

        let condition = if self.check(&TokenKind::Semicolon) {
            None
        } else {
            Some(self.parse_expression()?)
        };

        self.consume(&TokenKind::Semicolon, "Expected ';'")?;

        let update = if self.check(&TokenKind::LeftBrace) {
            None
        } else {
            Some(self.parse_expression()?)
        };

        let body = Box::new(self.parse_statement()?);
        let end = self.previous_token().range.end();

        Ok(ForStatement {
            init,
            condition,
            update,
            body,
            range: TextRange::new(start, end),
        })
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, ParseError> {
        let start = self.current_token().range.start();
        self.consume(&TokenKind::Return, "Expected 'return'")?;

        let value = if self.check(&TokenKind::Semicolon)
            || self.check(&TokenKind::Newline)
            || self.check(&TokenKind::RightBrace)
        {
            None
        } else {
            Some(self.parse_expression()?)
        };

        let end = self.previous_token().range.end();
        Ok(ReturnStatement {
            value,
            range: TextRange::new(start, end),
        })
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let start = self.current_token().range.start();
        self.consume(&TokenKind::LeftBrace, "Expected '{'")?;

        let mut statements = Vec::new();
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            if matches!(
                self.current_token().kind,
                TokenKind::Whitespace | TokenKind::Comment | TokenKind::Newline
            ) {
                self.advance();
                continue;
            }

            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(error) => {
                    self.errors.push(error);
                    self.synchronize();
                }
            }
        }

        self.consume(&TokenKind::RightBrace, "Expected '}'")?;
        let end = self.previous_token().range.end();

        Ok(Block {
            statements,
            range: TextRange::new(start, end),
        })
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expression, ParseError> {
        let expr = self.parse_logical_or()?;

        if self.check(&TokenKind::Equal) {
            self.advance();
            let right = self.parse_assignment()?;
            return Ok(Expression::Assignment(AssignmentExpression {
                left: Box::new(expr),
                right: Box::new(right),
                range: TextRange::new(TextSize::new(0), TextSize::new(0)),
            }));
        }

        Ok(expr)
    }

    fn parse_logical_or(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_logical_and()?;

        while self.check(&TokenKind::Or) || self.check(&TokenKind::PipePipe) {
            let _operator = self.advance();
            let right = self.parse_logical_and()?;
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator: BinaryOperator::Or,
                right: Box::new(right),
                range: TextRange::new(TextSize::new(0), TextSize::new(0)),
            });
        }

        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_equality()?;

        while self.check(&TokenKind::And) || self.check(&TokenKind::AmpersandAmpersand) {
            let _operator = self.advance();
            let right = self.parse_equality()?;
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator: BinaryOperator::And,
                right: Box::new(right),
                range: TextRange::new(TextSize::new(0), TextSize::new(0)),
            });
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_comparison()?;

        while matches!(
            self.current_token().kind,
            TokenKind::BangEqual | TokenKind::EqualEqual
        ) {
            let operator_kind = self.current_token().kind;
            self.advance();
            let right = self.parse_comparison()?;
            let operator = match operator_kind {
                TokenKind::BangEqual => BinaryOperator::NotEqual,
                TokenKind::EqualEqual => BinaryOperator::Equal,
                _ => unreachable!(),
            };
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                range: TextRange::new(TextSize::new(0), TextSize::new(0)),
            });
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_term()?;

        while matches!(
            self.current_token().kind,
            TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual
        ) {
            let operator_kind = self.current_token().kind;
            self.advance();
            let right = self.parse_term()?;
            let operator = match operator_kind {
                TokenKind::Greater => BinaryOperator::Greater,
                TokenKind::GreaterEqual => BinaryOperator::GreaterEqual,
                TokenKind::Less => BinaryOperator::Less,
                TokenKind::LessEqual => BinaryOperator::LessEqual,
                _ => unreachable!(),
            };
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                range: TextRange::new(TextSize::new(0), TextSize::new(0)),
            });
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_factor()?;

        while matches!(
            self.current_token().kind,
            TokenKind::Minus | TokenKind::Plus
        ) {
            let operator_kind = self.current_token().kind;
            self.advance();
            let right = self.parse_factor()?;
            let operator = match operator_kind {
                TokenKind::Minus => BinaryOperator::Subtract,
                TokenKind::Plus => BinaryOperator::Add,
                _ => unreachable!(),
            };
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                range: TextRange::new(TextSize::new(0), TextSize::new(0)),
            });
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_unary()?;

        while matches!(
            self.current_token().kind,
            TokenKind::Slash | TokenKind::Star | TokenKind::Percent
        ) {
            let operator_kind = self.current_token().kind;
            self.advance();
            let right = self.parse_unary()?;
            let operator = match operator_kind {
                TokenKind::Slash => BinaryOperator::Divide,
                TokenKind::Star => BinaryOperator::Multiply,
                TokenKind::Percent => BinaryOperator::Modulo,
                _ => unreachable!(),
            };
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                range: TextRange::new(TextSize::new(0), TextSize::new(0)),
            });
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expression, ParseError> {
        if matches!(
            self.current_token().kind,
            TokenKind::Bang | TokenKind::Minus
        ) {
            let operator_kind = self.current_token().kind;
            self.advance();
            let right = self.parse_unary()?;
            let operator = match operator_kind {
                TokenKind::Bang => UnaryOperator::Not,
                TokenKind::Minus => UnaryOperator::Minus,
                _ => unreachable!(),
            };
            return Ok(Expression::Unary(UnaryExpression {
                operator,
                operand: Box::new(right),
                range: TextRange::new(TextSize::new(0), TextSize::new(0)),
            }));
        }

        self.parse_call()
    }

    fn parse_call(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_primary()?;

        loop {
            match self.current_token().kind {
                TokenKind::LeftParen => {
                    self.advance();
                    let mut arguments = Vec::new();
                    while !self.check(&TokenKind::RightParen) && !self.is_at_end() {
                        arguments.push(self.parse_expression()?);
                        if !self.check(&TokenKind::RightParen) {
                            self.consume(&TokenKind::Comma, "Expected ','")?;
                        }
                    }
                    self.consume(&TokenKind::RightParen, "Expected ')'")?;
                    expr = Expression::Call(CallExpression {
                        callee: Box::new(expr),
                        arguments,
                        range: TextRange::new(TextSize::new(0), TextSize::new(0)),
                    });
                }
                TokenKind::Dot => {
                    self.advance();
                    let name = self.consume_identifier("Expected property name")?;
                    expr = Expression::Member(MemberExpression {
                        object: Box::new(expr),
                        property: name,
                        computed: false,
                        range: TextRange::new(TextSize::new(0), TextSize::new(0)),
                    });
                }
                TokenKind::LeftBracket => {
                    self.advance();
                    let index = self.parse_expression()?;
                    self.consume(&TokenKind::RightBracket, "Expected ']'")?;
                    expr = Expression::Index(IndexExpression {
                        object: Box::new(expr),
                        index: Box::new(index),
                        range: TextRange::new(TextSize::new(0), TextSize::new(0)),
                    });
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        match self.current_token().kind {
            TokenKind::True => {
                self.advance();
                Ok(Expression::Literal(Literal::Boolean(true)))
            }
            TokenKind::False => {
                self.advance();
                Ok(Expression::Literal(Literal::Boolean(false)))
            }
            TokenKind::Nil => {
                self.advance();
                Ok(Expression::Literal(Literal::Nil))
            }
            TokenKind::Number => {
                let token = self.advance();
                let value = token.text.parse::<f64>().unwrap_or(0.0);
                Ok(Expression::Literal(Literal::Number(value)))
            }
            TokenKind::String => {
                let token = self.advance();
                let value = token.text.trim_matches('"').to_string();
                Ok(Expression::Literal(Literal::String(value)))
            }
            TokenKind::Identifier => {
                let token = self.advance();
                Ok(Expression::Identifier(Identifier {
                    name: token.text.clone(),
                    range: token.range,
                }))
            }
            TokenKind::This => {
                let token = self.advance();
                Ok(Expression::This(ThisExpression { range: token.range }))
            }
            TokenKind::Super => {
                let token = self.advance();
                Ok(Expression::Super(SuperExpression { range: token.range }))
            }
            TokenKind::LeftParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.consume(&TokenKind::RightParen, "Expected ')'")?;
                Ok(expr)
            }
            TokenKind::LeftBracket => {
                let start = self.current_token().range.start();
                self.advance();
                let mut elements = Vec::new();
                while !self.check(&TokenKind::RightBracket) && !self.is_at_end() {
                    elements.push(self.parse_expression()?);
                    if !self.check(&TokenKind::RightBracket) {
                        self.consume(&TokenKind::Comma, "Expected ','")?;
                    }
                }
                self.consume(&TokenKind::RightBracket, "Expected ']'")?;
                let end = self.previous_token().range.end();
                Ok(Expression::Array(ArrayExpression {
                    elements,
                    range: TextRange::new(start, end),
                }))
            }
            _ => Err(ParseError {
                message: format!("Unexpected token: {:?}", self.current_token().kind),
                range: self.current_token().range,
            }),
        }
    }

    // Helper methods
    fn current_token(&self) -> &Token {
        if self.current >= self.tokens.len() {
            &self.tokens[self.tokens.len() - 1] // EOF token
        } else {
            &self.tokens[self.current]
        }
    }

    fn previous_token(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous_token()
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len() || self.current_token().kind == TokenKind::Eof
    }

    fn check(&self, kind: &TokenKind) -> bool {
        if self.is_at_end() {
            false
        } else {
            &self.current_token().kind == kind
        }
    }

    fn consume(&mut self, kind: &TokenKind, message: &str) -> Result<&Token, ParseError> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            Err(ParseError {
                message: message.to_string(),
                range: self.current_token().range,
            })
        }
    }

    fn consume_identifier(&mut self, message: &str) -> Result<Identifier, ParseError> {
        if self.check(&TokenKind::Identifier) {
            let token = self.advance();
            Ok(Identifier {
                name: token.text.clone(),
                range: token.range,
            })
        } else {
            Err(ParseError {
                message: message.to_string(),
                range: self.current_token().range,
            })
        }
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous_token().kind == TokenKind::Semicolon {
                return;
            }

            match self.current_token().kind {
                TokenKind::Class
                | TokenKind::Fn
                | TokenKind::Let
                | TokenKind::For
                | TokenKind::If
                | TokenKind::While
                | TokenKind::Return => return,
                _ => {}
            }

            self.advance();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_creation() {
        let input = "let x = 42";
        let lexer = Lexer::new(input);
        let _parser = Parser::new(lexer);
        // Just test that we can create a parser
        assert!(true);
    }
}
