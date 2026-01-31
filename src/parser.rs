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
    Struct(Struct),
    Enum(Enum),
    Statement(Statement),
    Empty,
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: Identifier,
    pub value: Option<Expression>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Identifier,
    pub params: Vec<Identifier>,
    pub body: Block,
    pub is_async: bool,
    pub attributes: Vec<Attribute>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: Identifier,
    pub methods: Vec<Function>,
    pub attributes: Vec<Attribute>,
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
    pub path: Token,
    pub alias: Option<Identifier>,
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
pub struct Struct {
    pub name: Identifier,
    pub fields: Vec<StructField>,
    pub attributes: Vec<Attribute>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Identifier,
    pub type_name: Identifier,
    pub initializer: Option<Expression>,
    pub attributes: Vec<Attribute>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: Identifier,
    pub variants: Vec<EnumVariant>,
    pub attributes: Vec<Attribute>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: Identifier,
    pub params: Vec<Identifier>,
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
    Function(Box<Function>),
    Class(Box<Class>),
    Struct(Box<Struct>),
    Enum(Box<Enum>),
    If(IfStatement),
    While(WhileStatement),
    For(ForStatement),
    Return(ReturnStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    Try(TryStatement),
    Block(Block),
    Import(Import),
    Export(Export),
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
    pub initializer: Option<Box<Statement>>,
    pub condition: Option<Expression>,
    pub increment: Option<Expression>,
    pub body: Box<Statement>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub keyword: Token,
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
    pub try_body: Block,
    pub catch_var: Identifier,
    pub catch_body: Block,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    Postfix(PostfixExpression),
    Literal(LiteralExpression),
    Grouping(Box<Expression>),
    Identifier(Identifier),
    Assignment(AssignmentExpression),
    Call(CallExpression),
    Array(ArrayExpression),
    Map(MapExpression),
    Index(IndexExpression),
    AssignIndex(AssignIndexExpression),
    Function(FunctionExpression),
    Get(GetExpression),
    Set(SetExpression),
    This(ThisExpression),
    Match(MatchExpression),
    Await(AwaitExpression),
    InterpolatedString(InterpolatedStringExpression),
    Logical(LogicalExpression),
}

impl Expression {
    pub fn range(&self) -> TextRange {
        match self {
            Expression::Binary(e) => e.range,
            Expression::Unary(e) => e.range,
            Expression::Postfix(e) => e.range,
            Expression::Literal(e) => e.range,
            Expression::Grouping(e) => e.range(),
            Expression::Identifier(e) => e.range,
            Expression::Assignment(e) => e.range,
            Expression::Call(e) => e.range,
            Expression::Array(e) => e.range,
            Expression::Map(e) => e.range,
            Expression::Index(e) => e.range,
            Expression::AssignIndex(e) => e.range,
            Expression::Function(e) => e.range,
            Expression::Get(e) => e.range,
            Expression::Set(e) => e.range,
            Expression::This(e) => e.range,
            Expression::Match(e) => e.range,
            Expression::Await(e) => e.range,
            Expression::InterpolatedString(e) => e.range,
            Expression::Logical(e) => e.range,
        }
    }
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
pub struct LiteralExpression {
    pub value: Literal,
    pub range: TextRange,
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
}

#[derive(Debug, Clone)]
pub struct PostfixExpression {
    pub left: Box<Expression>,
    pub operator: PostfixOperator,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub enum PostfixOperator {
    Increment,
    Decrement,
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct GetExpression {
    pub object: Box<Expression>,
    pub name: Identifier,
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
    pub name: Identifier,
    pub value: Box<Expression>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct SetExpression {
    pub object: Box<Expression>,
    pub name: Identifier,
    pub value: Box<Expression>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct AssignIndexExpression {
    pub object: Box<Expression>,
    pub index: Box<Expression>,
    pub value: Box<Expression>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct ArrayExpression {
    pub elements: Vec<Expression>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct MapExpression {
    pub keys: Vec<Expression>,
    pub values: Vec<Expression>,
    pub range: TextRange,
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
pub struct MatchExpression {
    pub target: Box<Expression>,
    pub arms: Vec<MatchArm>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Box<Expression>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Variant(VariantPattern),
    Literal(Literal),
    Identifier(Identifier),
}

#[derive(Debug, Clone)]
pub struct VariantPattern {
    pub enum_name: Option<Identifier>,
    pub variant: Identifier,
    pub params: Vec<Identifier>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct InterpolatedStringExpression {
    pub parts: Vec<Expression>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct LogicalExpression {
    pub left: Box<Expression>,
    pub operator: Token,
    pub right: Box<Expression>,
    pub range: TextRange,
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let tokens = lexer
            .tokenize()
            .into_iter()
            .filter(|t| {
                !matches!(
                    t.kind,
                    TokenKind::Whitespace | TokenKind::Comment | TokenKind::Newline
                )
            })
            .collect();
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
            if self.match_token(TokenKind::Semicolon) {
                continue;
            }

            match self.declaration() {
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

        ParseResult::with_errors(
            SourceFile {
                items,
                range: TextRange::new(start, end),
            },
            self.errors.clone(),
        )
    }

    fn declaration(&mut self) -> Result<Item, ParseError> {
        let mut attributes = Vec::new();
        while self.match_token(TokenKind::AtBracket) {
            attributes.extend(self.parse_attributes()?);
            while self.match_token(TokenKind::Semicolon) {}
        }

        if self.match_token(TokenKind::Class) {
            self.class_declaration(attributes)
        } else if self.match_token(TokenKind::Async) {
            self.consume(&TokenKind::Fn, "Expect 'fn' after 'async'")?;
            self.function_with_async(attributes, true)
        } else if self.match_token(TokenKind::Fn) {
            self.function_with_async(attributes, false)
        } else if self.match_token(TokenKind::Struct) {
            self.struct_declaration(attributes)
        } else if self.match_token(TokenKind::Enum) {
            self.enum_declaration(attributes)
        } else if self.match_token(TokenKind::Let) {
            if !attributes.is_empty() {
                return Err(ParseError {
                    message: "Cannot use attributes on variable declarations".to_string(),
                    range: self.previous_token().range,
                });
            }
            self.var_declaration()
        } else {
            if !attributes.is_empty() {
                return Err(ParseError {
                    message: "Unexpected attributes before statement".to_string(),
                    range: self.previous_token().range,
                });
            }
            Ok(Item::Statement(self.statement()?))
        }
    }

    fn parse_attributes(&mut self) -> Result<Vec<Attribute>, ParseError> {
        let mut attributes = Vec::new();
        while self.match_token(TokenKind::Semicolon) {}

        if !self.check(&TokenKind::RightBracket) {
            loop {
                while self.match_token(TokenKind::Semicolon) {}
                let name = self.consume_identifier("Expect attribute name")?;
                let mut value = None;

                if self.match_token(TokenKind::Colon) {
                    value = Some(self.parse_expression()?);
                } else if self.match_token(TokenKind::LeftParen) {
                    value = Some(self.parse_expression()?);
                    self.consume(
                        &TokenKind::RightParen,
                        "Expect ')' after attribute argument",
                    )?;
                }

                let range = TextRange::new(name.range.start(), self.previous_token().range.end());
                attributes.push(Attribute { name, value, range });

                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        while self.match_token(TokenKind::Semicolon) {}
        self.consume(&TokenKind::RightBracket, "Expect ']' after attributes")?;

        Ok(attributes)
    }

    fn var_declaration(&mut self) -> Result<Item, ParseError> {
        let start = self.previous_token().range.start(); // From 'let'
        let name = self.consume_identifier("Expect variable name")?;
        let mut initializer = Expression::Literal(LiteralExpression {
            value: Literal::Nil,
            range: name.range,
        });

        if self.match_token(TokenKind::Equal) {
            initializer = self.parse_expression()?;
        }

        // ASI check mirroring parser.v:141-157
        if !self.match_token(TokenKind::Semicolon) {
            if !self.is_at_end()
                && !self.check(&TokenKind::RightBrace)
                && !self.check(&TokenKind::RightBracket)
            {
                let next = self.current_token();
                if matches!(
                    next.kind,
                    TokenKind::Identifier
                        | TokenKind::Number
                        | TokenKind::String
                        | TokenKind::LeftParen
                        | TokenKind::LeftBrace
                        | TokenKind::LeftBracket
                        | TokenKind::True
                        | TokenKind::False
                        | TokenKind::Nil
                        | TokenKind::Return
                        | TokenKind::If
                        | TokenKind::While
                        | TokenKind::For
                        | TokenKind::Fn
                        | TokenKind::Class
                        | TokenKind::Struct
                        | TokenKind::Enum
                        | TokenKind::Match
                        | TokenKind::Try
                        | TokenKind::Import
                        | TokenKind::Async
                        | TokenKind::Let
                ) {
                    return Err(ParseError {
                        message: "Expect ; after variable declaration".to_string(),
                        range: next.range,
                    });
                }
            }
        }

        let end = self.previous_token().range.end();
        Ok(Item::VariableDeclaration(VariableDeclaration {
            name,
            value: Some(initializer),
            is_mutable: true,
            range: TextRange::new(start, end),
        }))
    }

    fn function_with_async(
        &mut self,
        attributes: Vec<Attribute>,
        is_async: bool,
    ) -> Result<Item, ParseError> {
        let start = if is_async {
            // "async" was consumed before "fn" in declaration()
            self.tokens[self.current - 2].range.start()
        } else {
            self.previous_token().range.start()
        };

        let name = self.consume_identifier("Expect function name")?;
        self.consume(&TokenKind::LeftParen, "Expect ( after function name")?;

        let mut params = Vec::new();
        if !self.check(&TokenKind::RightParen) {
            loop {
                params.push(self.consume_identifier("Expect parameter name")?);
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.consume(&TokenKind::RightParen, "Expect ) after parameters")?;
        self.consume(&TokenKind::LeftBrace, "Expect { before function body")?;
        let body = self.block()?;
        let end = self.previous_token().range.end();
        Ok(Item::Function(Function {
            name,
            params,
            body: Block {
                statements: body,
                range: TextRange::new(start, end),
            },
            is_async,
            attributes,
            range: TextRange::new(start, end),
        }))
    }

    fn class_declaration(&mut self, attributes: Vec<Attribute>) -> Result<Item, ParseError> {
        let start = self.previous_token().range.start();
        let name = self.consume_identifier("Expect class name")?;
        self.consume(&TokenKind::LeftBrace, "Expect '{' before class body")?;

        let mut methods = Vec::new();
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            while self.match_token(TokenKind::Semicolon) {}
            if self.check(&TokenKind::RightBrace) {
                break;
            }
            methods.push(self.method()?);
        }

        self.consume(&TokenKind::RightBrace, "Expect '}' after class body")?;
        let end = self.previous_token().range.end();

        Ok(Item::Class(Class {
            name,
            methods,
            attributes,
            range: TextRange::new(start, end),
        }))
    }

    fn struct_declaration(&mut self, attributes: Vec<Attribute>) -> Result<Item, ParseError> {
        let start = self.previous_token().range.start();
        let name = self.consume_identifier("Expect struct name")?;
        self.consume(&TokenKind::LeftBrace, "Expect '{' before struct body")?;

        let mut fields = Vec::new();
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            while self.match_token(TokenKind::Semicolon) {}
            if self.check(&TokenKind::RightBrace) {
                break;
            }

            let mut f_attrs = Vec::new();
            while self.match_token(TokenKind::AtBracket) {
                f_attrs.extend(self.parse_attributes()?);
                while self.match_token(TokenKind::Semicolon) {}
            }

            let f_start = self.current_token().range.start();
            let f_name = self.consume_identifier("Expect field name")?;
            let f_type = self.consume_identifier("Expect field type")?;

            let mut initializer = None;
            if self.match_token(TokenKind::Equal) {
                initializer = Some(self.parse_expression()?);
            }

            let f_end = self.previous_token().range.end();
            fields.push(StructField {
                name: f_name,
                type_name: f_type,
                initializer,
                attributes: f_attrs,
                range: TextRange::new(f_start, f_end),
            });

            if !self.match_token(TokenKind::Comma) {
                let _ = self.match_token(TokenKind::Semicolon);
            }
            while self.match_token(TokenKind::Semicolon) {}
        }

        self.consume(&TokenKind::RightBrace, "Expect '}' after struct body")?;
        let end = self.previous_token().range.end();

        Ok(Item::Struct(Struct {
            name,
            fields,
            attributes,
            range: TextRange::new(start, end),
        }))
    }

    fn enum_declaration(&mut self, attributes: Vec<Attribute>) -> Result<Item, ParseError> {
        let start = self.previous_token().range.start();
        self.consume(&TokenKind::Enum, "Expected 'enum'")?; // This line was missing in the provided snippet
        let name = self.consume_identifier("Expect enum name")?;
        self.consume(&TokenKind::LeftBrace, "Expect '{' before enum body")?;

        let mut variants = Vec::new();
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            while self.match_token(TokenKind::Semicolon) {}
            if self.check(&TokenKind::RightBrace) {
                break;
            }

            let v_start = self.current_token().range.start();
            let v_name = self.consume_identifier("Expect variant name")?;
            let mut params = Vec::new();

            if self.match_token(TokenKind::LeftParen) {
                if !self.check(&TokenKind::RightParen) {
                    loop {
                        params.push(self.consume_identifier("Expect parameter type name")?);
                        if !self.match_token(TokenKind::Comma) {
                            break;
                        }
                    }
                }
                self.consume(
                    &TokenKind::RightParen,
                    "Expect ) after enum variant parameters",
                )?;
            }

            let v_end = self.previous_token().range.end();
            variants.push(EnumVariant {
                name: v_name,
                params,
                range: TextRange::new(v_start, v_end),
            });

            if !self.match_token(TokenKind::Comma) {
                let _ = self.match_token(TokenKind::Semicolon);
            }
            while self.match_token(TokenKind::Semicolon) {}
        }

        self.consume(&TokenKind::RightBrace, "Expect '}' after enum body")?;
        let end = self.previous_token().range.end();

        Ok(Item::Enum(Enum {
            name,
            variants,
            attributes,
            range: TextRange::new(start, end),
        }))
    }

    fn method(&mut self) -> Result<Function, ParseError> {
        let mut attributes = Vec::new();
        while self.match_token(TokenKind::AtBracket) {
            attributes.extend(self.parse_attributes()?);
            while self.match_token(TokenKind::Semicolon) {}
        }

        let mut is_async = false;
        if self.match_token(TokenKind::Async) {
            is_async = true;
        }

        let start = self.current_token().range.start();
        let name = self.consume_identifier("Expect method name")?;
        self.consume(&TokenKind::LeftParen, "Expect '(' after method name")?;

        let mut params = Vec::new();
        if !self.check(&TokenKind::RightParen) {
            loop {
                params.push(self.consume_identifier("Expect parameter name")?);
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.consume(&TokenKind::RightParen, "Expect ')' after parameters")?;
        self.consume(&TokenKind::LeftBrace, "Expect '{' before method body")?;
        let body = self.block()?;

        let end = self.previous_token().range.end();
        Ok(Function {
            name,
            params,
            body: Block {
                statements: body,
                range: TextRange::new(start, end),
            },
            attributes,
            is_async,
            range: TextRange::new(start, end),
        })
    }

    fn statement(&mut self) -> Result<Statement, ParseError> {
        if self.match_token(TokenKind::For) {
            return self.for_statement();
        }
        if self.match_token(TokenKind::If) {
            return self.if_statement();
        }
        if self.match_token(TokenKind::Try) {
            return self.try_statement();
        }
        if self.match_token(TokenKind::Import) {
            return Ok(Statement::Import(self.import_statement()?));
        }
        if self.match_token(TokenKind::Return) {
            return self.return_statement();
        }
        if self.match_token(TokenKind::While) {
            return self.while_statement();
        }
        if self.match_token(TokenKind::LeftBrace) {
            let start = self.previous_token().range.start(); // '{' token
            let statements = self.block()?;
            let end = self.previous_token().range.end(); // '}' token
            return Ok(Statement::Block(Block {
                statements,
                range: TextRange::new(start, end),
            }));
        }

        self.expression_statement()
    }

    fn for_statement(&mut self) -> Result<Statement, ParseError> {
        let start = self.previous_token().range.start();
        self.consume(&TokenKind::LeftParen, "Expect ( after for")?;

        let mut initializer = None;
        if self.match_token(TokenKind::Semicolon) {
            initializer = None;
        } else if self.match_token(TokenKind::Let) {
            if let Item::VariableDeclaration(var) = self.var_declaration()? {
                initializer = Some(Box::new(Statement::VariableDeclaration(var)));
            }
        } else {
            initializer = Some(Box::new(self.expression_statement()?));
        }

        let mut condition = None;
        if !self.check(&TokenKind::Semicolon) {
            condition = Some(self.parse_expression()?);
        }
        self.consume(&TokenKind::Semicolon, "Expect ; after loop condition")?;

        let mut increment = None;
        if !self.check(&TokenKind::RightParen) {
            increment = Some(self.parse_expression()?);
        }
        self.consume(&TokenKind::RightParen, "Expect ) after for clauses")?;

        let body = Box::new(self.statement()?);
        let end = self.previous_token().range.end();

        Ok(Statement::For(ForStatement {
            initializer,
            condition,
            increment,
            body,
            range: TextRange::new(start, end),
        }))
    }

    fn if_statement(&mut self) -> Result<Statement, ParseError> {
        let start = self.previous_token().range.start();
        self.consume(&TokenKind::LeftParen, "Expect ( after if")?;
        let condition = self.parse_expression()?;
        self.consume(&TokenKind::RightParen, "Expect ) after condition")?;

        let then_branch = Box::new(self.statement()?);
        let mut else_branch = None;
        if self.match_token(TokenKind::Else) {
            else_branch = Some(Box::new(self.statement()?));
        }

        let end = self.previous_token().range.end();
        Ok(Statement::If(IfStatement {
            condition,
            then_branch,
            else_branch,
            range: TextRange::new(start, end),
        }))
    }

    fn return_statement(&mut self) -> Result<Statement, ParseError> {
        let start = self.previous_token().range.start();
        let keyword = self.previous_token().clone();
        let mut value = None;
        if !self.check(&TokenKind::Semicolon) {
            value = Some(self.parse_expression()?);
        }

        let _ = self.match_token(TokenKind::Semicolon);
        let end = self.previous_token().range.end();

        Ok(Statement::Return(ReturnStatement {
            keyword,
            value,
            range: TextRange::new(start, end),
        }))
    }

    fn while_statement(&mut self) -> Result<Statement, ParseError> {
        let start = self.previous_token().range.start();
        self.consume(&TokenKind::LeftParen, "Expect ( after while")?;
        let condition = self.parse_expression()?;
        self.consume(&TokenKind::RightParen, "Expect ) after condition")?;
        let body = Box::new(self.statement()?);

        let end = self.previous_token().range.end();
        Ok(Statement::While(WhileStatement {
            condition,
            body,
            range: TextRange::new(start, end),
        }))
    }

    fn try_statement(&mut self) -> Result<Statement, ParseError> {
        let start = self.previous_token().range.start();
        self.consume(&TokenKind::LeftBrace, "Expect '{' before try body")?;
        let try_body_start = self.previous_token().range.start();
        let try_body_statements = self.block()?;
        let try_body_end = self.previous_token().range.end();
        let try_body = Block {
            statements: try_body_statements,
            range: TextRange::new(try_body_start, try_body_end),
        };

        self.consume(&TokenKind::Catch, "Expect 'catch' after try block")?;
        self.consume(&TokenKind::LeftParen, "Expect '(' after catch")?;
        let catch_var = self.consume_identifier("Expect identifier for catch error")?;
        self.consume(&TokenKind::RightParen, "Expect ')' after catch identifier")?;

        self.consume(&TokenKind::LeftBrace, "Expect '{' before catch body")?;
        let catch_body_start = self.previous_token().range.start();
        let catch_body_statements = self.block()?;
        let catch_body_end = self.previous_token().range.end();
        let catch_body = Block {
            statements: catch_body_statements,
            range: TextRange::new(catch_body_start, catch_body_end),
        };

        let end = self.previous_token().range.end();
        Ok(Statement::Try(TryStatement {
            try_body,
            catch_var,
            catch_body,
            range: TextRange::new(start, end),
        }))
    }

    fn import_statement(&mut self) -> Result<Import, ParseError> {
        let start = self.previous_token().range.start();
        let path: Token;

        if self.check(&TokenKind::String) {
            path = self.advance().clone();
        } else if self.check(&TokenKind::Identifier) {
            let mut parts = vec![self.advance().text.clone()];
            while self.match_token(TokenKind::Colon) {
                parts.push(
                    self.consume_identifier("Expect identifier after : in import path")?
                        .name,
                );
            }
            let path_str = parts.join("/") + ".vs";
            path = Token::new(
                TokenKind::String,
                format!("\"{}\"", path_str),
                self.previous_token().range,
            );
        } else {
            return Err(ParseError {
                message: "Expect string or identifier execution path after import".to_string(),
                range: self.current_token().range,
            });
        }

        let mut alias = None;
        if self.check(&TokenKind::Identifier) && self.current_token().text == "as" {
            self.advance();
            alias = Some(self.consume_identifier("Expect alias name after as")?);
        }

        let _ = self.match_token(TokenKind::Semicolon);
        let end = self.previous_token().range.end();

        Ok(Import {
            path,
            alias,
            range: TextRange::new(start, end),
        })
    }

    fn expression_statement(&mut self) -> Result<Statement, ParseError> {
        let _start = self.current_token().range.start();
        let expr = self.parse_expression()?;

        // ASI check mirroring parser.v:536-551
        if !self.match_token(TokenKind::Semicolon) {
            if !self.is_at_end()
                && !self.check(&TokenKind::RightBrace)
                && !self.check(&TokenKind::RightBracket)
            {
                let next = self.current_token();
                if matches!(
                    next.kind,
                    TokenKind::Identifier
                        | TokenKind::Number
                        | TokenKind::String
                        | TokenKind::LeftParen
                        | TokenKind::LeftBrace
                        | TokenKind::LeftBracket
                        | TokenKind::True
                        | TokenKind::False
                        | TokenKind::Nil
                        | TokenKind::Return
                        | TokenKind::If
                        | TokenKind::While
                        | TokenKind::For
                        | TokenKind::Fn
                        | TokenKind::Class
                        | TokenKind::Struct
                        | TokenKind::Enum
                        | TokenKind::Match
                        | TokenKind::Try
                        | TokenKind::Import
                        | TokenKind::Async
                        | TokenKind::Let
                ) {
                    return Err(ParseError {
                        message: "Expect ; after expression".to_string(),
                        range: next.range,
                    });
                }
            }
        }

        Ok(Statement::Expression(expr))
    }

    fn block(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut statements = Vec::new();
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            while self.match_token(TokenKind::Semicolon) {}
            if self.check(&TokenKind::RightBrace) {
                break;
            }
            match self.declaration()? {
                Item::Statement(stmt) => statements.push(stmt),
                Item::VariableDeclaration(var) => {
                    statements.push(Statement::VariableDeclaration(var))
                }
                Item::Function(func) => statements.push(Statement::Function(Box::new(func))),
                Item::Class(cls) => statements.push(Statement::Class(Box::new(cls))),
                Item::Struct(s) => statements.push(Statement::Struct(Box::new(s))),
                Item::Enum(e) => statements.push(Statement::Enum(Box::new(e))),
                _ => {}
            }
        }

        self.consume(&TokenKind::RightBrace, "Expect '}' after block")?;
        Ok(statements)
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expression, ParseError> {
        let expr = self.parse_logical_or()?;

        if self.match_token(TokenKind::Equal) {
            let value = self.parse_assignment()?;
            let end = value.range().end();
            let expr_start = expr.range().start();

            match expr {
                Expression::Identifier(id) => {
                    return Ok(Expression::Assignment(AssignmentExpression {
                        name: id,
                        value: Box::new(value),
                        range: TextRange::new(expr_start, end),
                    }));
                }
                Expression::Get(get) => {
                    return Ok(Expression::Set(SetExpression {
                        object: get.object,
                        name: get.name,
                        value: Box::new(value),
                        range: TextRange::new(expr_start, end),
                    }));
                }
                Expression::Index(idx) => {
                    return Ok(Expression::AssignIndex(AssignIndexExpression {
                        object: idx.object,
                        index: idx.index,
                        value: Box::new(value),
                        range: TextRange::new(expr_start, end),
                    }));
                }
                _ => {
                    return Err(ParseError {
                        message: "Invalid assignment target".to_string(),
                        range: self.previous_token().range,
                    });
                }
            }
        }

        Ok(expr)
    }

    fn parse_logical_or(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_logical_and()?;

        while self.match_token(TokenKind::Or) || self.match_token(TokenKind::PipePipe) {
            let operator = self.previous_token().clone();
            let right = self.parse_logical_and()?;
            let start = expr.range().start();
            let end = right.range().end();
            expr = Expression::Logical(LogicalExpression {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                range: TextRange::new(start, end),
            });
        }

        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_equality()?;

        while self.match_token(TokenKind::And) || self.match_token(TokenKind::AmpersandAmpersand) {
            let operator = self.previous_token().clone();
            let right = self.parse_equality()?;
            let start = expr.range().start();
            let end = right.range().end();
            expr = Expression::Logical(LogicalExpression {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                range: TextRange::new(start, end),
            });
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_comparison()?;

        while self.match_token(TokenKind::BangEqual) || self.match_token(TokenKind::EqualEqual) {
            let operator_kind = self.previous_token().kind;
            let operator = match operator_kind {
                TokenKind::BangEqual => BinaryOperator::NotEqual,
                TokenKind::EqualEqual => BinaryOperator::Equal,
                _ => unreachable!(),
            };
            let right = self.parse_comparison()?;
            let start = expr.range().start();
            let end = right.range().end();
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                range: TextRange::new(start, end),
            });
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_term()?;

        while self.match_token(TokenKind::Greater)
            || self.match_token(TokenKind::GreaterEqual)
            || self.match_token(TokenKind::Less)
            || self.match_token(TokenKind::LessEqual)
        {
            let operator_kind = self.previous_token().kind;
            let operator = match operator_kind {
                TokenKind::Greater => BinaryOperator::Greater,
                TokenKind::GreaterEqual => BinaryOperator::GreaterEqual,
                TokenKind::Less => BinaryOperator::Less,
                TokenKind::LessEqual => BinaryOperator::LessEqual,
                _ => unreachable!(),
            };
            let right = self.parse_term()?;
            let start = expr.range().start();
            let end = right.range().end();
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                range: TextRange::new(start, end),
            });
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_factor()?;

        while self.match_token(TokenKind::Minus) || self.match_token(TokenKind::Plus) {
            let operator_kind = self.previous_token().kind;
            let operator = match operator_kind {
                TokenKind::Minus => BinaryOperator::Subtract,
                TokenKind::Plus => BinaryOperator::Add,
                _ => unreachable!(),
            };
            let right = self.parse_factor()?;
            let start = expr.range().start();
            let end = right.range().end();
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                range: TextRange::new(start, end),
            });
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_unary()?;

        while self.match_token(TokenKind::Slash)
            || self.match_token(TokenKind::Star)
            || self.match_token(TokenKind::Percent)
        {
            let operator_kind = self.previous_token().kind;
            let operator = match operator_kind {
                TokenKind::Slash => BinaryOperator::Divide,
                TokenKind::Star => BinaryOperator::Multiply,
                TokenKind::Percent => BinaryOperator::Modulo,
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;
            let start = expr.range().start();
            let end = right.range().end();
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                range: TextRange::new(start, end),
            });
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expression, ParseError> {
        if self.match_token(TokenKind::Bang) || self.match_token(TokenKind::Minus) {
            let operator_kind = self.previous_token().kind;
            let start = self.previous_token().range.start();
            let operator = match operator_kind {
                TokenKind::Bang => UnaryOperator::Not,
                TokenKind::Minus => UnaryOperator::Minus,
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;
            let end = right.range().end();
            return Ok(Expression::Unary(UnaryExpression {
                operator,
                operand: Box::new(right),
                range: TextRange::new(start, end),
            }));
        }

        if self.match_token(TokenKind::Await) {
            let start = self.previous_token().range.start();
            let value = self.parse_unary()?;
            let end = value.range().end();
            return Ok(Expression::Await(AwaitExpression {
                argument: Box::new(value),
                range: TextRange::new(start, end),
            }));
        }

        self.parse_call()
    }

    fn parse_call(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.match_token(TokenKind::LeftParen) {
                expr = self.finish_call(expr)?;
            } else if self.match_token(TokenKind::Dot) {
                let name = self.consume_identifier("Expect property name after '.'")?;
                let start = expr.range().start();
                let end = name.range.end();
                expr = Expression::Get(GetExpression {
                    object: Box::new(expr),
                    name,
                    range: TextRange::new(start, end),
                });
            } else if self.match_token(TokenKind::LeftBracket) {
                let index = self.parse_expression()?;
                self.consume(&TokenKind::RightBracket, "Expect ] after index")?;
                let start = expr.range().start();
                let end = self.previous_token().range.end();
                expr = Expression::Index(IndexExpression {
                    object: Box::new(expr),
                    index: Box::new(index),
                    range: TextRange::new(start, end),
                });
            } else if self.match_token(TokenKind::PlusPlus)
                || self.match_token(TokenKind::MinusMinus)
            {
                let operator_kind = self.previous_token().kind;
                let operator = match operator_kind {
                    TokenKind::PlusPlus => PostfixOperator::Increment,
                    TokenKind::MinusMinus => PostfixOperator::Decrement,
                    _ => unreachable!(),
                };
                let start = expr.range().start();
                let end = self.previous_token().range.end();
                expr = Expression::Postfix(PostfixExpression {
                    left: Box::new(expr),
                    operator,
                    range: TextRange::new(start, end),
                });
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expression) -> Result<Expression, ParseError> {
        let mut arguments = Vec::new();
        if !self.check(&TokenKind::RightParen) {
            loop {
                arguments.push(self.parse_expression()?);
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.consume(&TokenKind::RightParen, "Expect ')' after arguments")?;
        let start = callee.range().start();
        let end = self.previous_token().range.end();

        Ok(Expression::Call(CallExpression {
            callee: Box::new(callee),
            arguments,
            range: TextRange::new(start, end),
        }))
    }

    fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        if self.match_token(TokenKind::False) {
            return Ok(Expression::Literal(LiteralExpression {
                value: Literal::Boolean(false),
                range: self.previous_token().range,
            }));
        }
        if self.match_token(TokenKind::True) {
            return Ok(Expression::Literal(LiteralExpression {
                value: Literal::Boolean(true),
                range: self.previous_token().range,
            }));
        }
        if self.match_token(TokenKind::Nil) {
            return Ok(Expression::Literal(LiteralExpression {
                value: Literal::Nil,
                range: self.previous_token().range,
            }));
        }

        if self.match_token(TokenKind::Number) {
            let token = self.previous_token();
            let value = token.text.parse::<f64>().unwrap_or(0.0);
            return Ok(Expression::Literal(LiteralExpression {
                value: Literal::Number(value),
                range: token.range,
            }));
        }

        if self.match_token(TokenKind::String) {
            let token = self.previous_token();
            let value = token.text.trim_matches('"').to_string();
            return Ok(Expression::Literal(LiteralExpression {
                value: Literal::String(value),
                range: token.range,
            }));
        }

        if self.match_token(TokenKind::StringInterpStart) {
            return self.interpolated_string();
        }

        if self.match_token(TokenKind::This) {
            return Ok(Expression::This(ThisExpression {
                range: self.previous_token().range,
            }));
        }

        if self.match_token(TokenKind::Identifier) {
            let token = self.previous_token();
            return Ok(Expression::Identifier(Identifier {
                name: token.text.clone(),
                range: token.range,
            }));
        }

        if self.match_token(TokenKind::LeftParen) {
            let _start = self.previous_token().range.start();
            let expr = self.parse_expression()?;
            self.consume(&TokenKind::RightParen, "Expect ')' after expression")?;
            let _end = self.previous_token().range.end();
            return Ok(Expression::Grouping(Box::new(expr)));
        }

        if self.match_token(TokenKind::LeftBracket) {
            return self.array_literal();
        }

        if self.match_token(TokenKind::LeftBrace) {
            return self.map_literal();
        }

        if self.match_token(TokenKind::Async) {
            self.consume(&TokenKind::Fn, "Expect 'fn' after 'async'")?;
            return self.function_expression_with_async(true);
        }

        if self.match_token(TokenKind::Fn) {
            return self.function_expression_with_async(false);
        }

        if self.match_token(TokenKind::Match) {
            return self.match_expression();
        }

        Err(ParseError {
            message: "Expect expression".to_string(),
            range: self.current_token().range,
        })
    }

    fn interpolated_string(&mut self) -> Result<Expression, ParseError> {
        let start_pos = self.previous_token().range.start();
        let mut parts = Vec::new();
        // First segment (trim starting " and trailing ${)
        let initial_token = self.previous_token().clone();
        let initial_text = initial_token
            .text
            .trim_start_matches('"')
            .trim_end_matches("${")
            .to_string();
        parts.push(Expression::Literal(LiteralExpression {
            value: Literal::String(initial_text),
            range: initial_token.range,
        }));

        loop {
            // Interpolated expression
            parts.push(self.parse_expression()?);

            if self.match_token(TokenKind::StringInterpMiddle) {
                let mid_token = self.previous_token().clone();
                // Trim leading } and trailing ${
                let mid_text = mid_token
                    .text
                    .trim_start_matches('}')
                    .trim_end_matches("${")
                    .to_string();
                parts.push(Expression::Literal(LiteralExpression {
                    value: Literal::String(mid_text),
                    range: mid_token.range,
                }));
            } else if self.match_token(TokenKind::StringInterpEnd) {
                let end_token = self.previous_token().clone();
                // Trim leading } and trailing "
                let end_text = end_token
                    .text
                    .trim_start_matches('}')
                    .trim_end_matches('"')
                    .to_string();
                parts.push(Expression::Literal(LiteralExpression {
                    value: Literal::String(end_text),
                    range: end_token.range,
                }));
                break;
            } else {
                return Err(ParseError {
                    message: "Expect '}' or more string segments after interpolation expression"
                        .to_string(),
                    range: self.current_token().range,
                });
            }
        }

        let end_pos = self.previous_token().range.end();
        Ok(Expression::InterpolatedString(
            InterpolatedStringExpression {
                parts,
                range: TextRange::new(start_pos, end_pos),
            },
        ))
    }

    fn array_literal(&mut self) -> Result<Expression, ParseError> {
        let start = self.previous_token().range.start();
        let mut elements = Vec::new();

        if !self.check(&TokenKind::RightBracket) {
            loop {
                elements.push(self.parse_expression()?);
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.consume(&TokenKind::RightBracket, "Expect ']' after array elements")?;
        let end = self.previous_token().range.end();

        Ok(Expression::Array(ArrayExpression {
            elements,
            range: TextRange::new(start, end),
        }))
    }

    fn map_literal(&mut self) -> Result<Expression, ParseError> {
        let start = self.previous_token().range.start();
        let mut keys = Vec::new();
        let mut values = Vec::new();

        if !self.check(&TokenKind::RightBrace) {
            loop {
                keys.push(self.parse_expression()?);
                self.consume(&TokenKind::Colon, "Expect ':' after map key")?;
                values.push(self.parse_expression()?);
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.consume(&TokenKind::RightBrace, "Expect '}' after map elements")?;
        let end = self.previous_token().range.end();

        Ok(Expression::Map(MapExpression {
            keys,
            values,
            range: TextRange::new(start, end),
        }))
    }

    fn function_expression_with_async(&mut self, is_async: bool) -> Result<Expression, ParseError> {
        let start = if is_async {
            self.tokens[self.current - 2].range.start()
        } else {
            self.previous_token().range.start()
        };

        self.consume(&TokenKind::LeftParen, "Expect '(' after 'fn'")?;
        let mut params = Vec::new();
        if !self.check(&TokenKind::RightParen) {
            loop {
                let name = self.consume_identifier("Expect parameter name")?;
                params.push(Parameter {
                    name,
                    default_value: None,
                    range: self.previous_token().range,
                });
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenKind::RightParen, "Expect ')' after parameters")?;

        self.consume(&TokenKind::LeftBrace, "Expect '{' before function body")?;
        let body = self.block()?;
        let end = self.previous_token().range.end();

        Ok(Expression::Function(FunctionExpression {
            params,
            body: Block {
                statements: body,
                range: TextRange::new(start, end),
            },
            is_async,
            range: TextRange::new(start, end),
        }))
    }

    fn match_expression(&mut self) -> Result<Expression, ParseError> {
        let start = self.previous_token().range.start();
        let target = self.parse_expression()?;

        self.consume(&TokenKind::LeftBrace, "Expect '{' after match target")?;
        let mut arms = Vec::new();

        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            let arm_start = self.current_token().range.start();
            let pattern = self.parse_pattern()?;
            self.consume(&TokenKind::FatArrow, "Expect '=>' after pattern")?;
            let body = self.parse_expression()?;
            let arm_end = body.range().end();
            arms.push(MatchArm {
                pattern,
                body: Box::new(body),
                range: TextRange::new(arm_start, arm_end),
            });

            // Optional comma
            self.match_token(TokenKind::Comma);
        }

        self.consume(&TokenKind::RightBrace, "Expect '}' after match arms")?;
        let end = self.previous_token().range.end();

        Ok(Expression::Match(MatchExpression {
            target: Box::new(target),
            arms,
            range: TextRange::new(start, end),
        }))
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        let start_pos = self.current_token().range.start(); // Initialize start_pos

        if self.match_token(TokenKind::False) {
            return Ok(Pattern::Literal(Literal::Boolean(false)));
        }
        if self.match_token(TokenKind::True) {
            return Ok(Pattern::Literal(Literal::Boolean(true)));
        }
        if self.match_token(TokenKind::Nil) {
            return Ok(Pattern::Literal(Literal::Nil));
        }
        if self.match_token(TokenKind::Number) {
            let value = self.previous_token().text.parse::<f64>().unwrap_or(0.0);
            return Ok(Pattern::Literal(Literal::Number(value)));
        }
        if self.match_token(TokenKind::String) {
            let value = self.previous_token().text.trim_matches('"').to_string();
            return Ok(Pattern::Literal(Literal::String(value)));
        }

        // Variant or Identifier pattern
        let name_ref = self.consume_identifier("Expect pattern")?; // Renamed to name_ref

        if self.match_token(TokenKind::Dot) {
            // EnumName.Variant
            let variant = self.consume_identifier("Expect variant name after '.'")?;
            let mut params = Vec::new();
            if self.match_token(TokenKind::LeftParen) {
                if !self.check(&TokenKind::RightParen) {
                    loop {
                        params.push(self.consume_identifier("Expect parameter name")?);
                        if !self.match_token(TokenKind::Comma) {
                            break;
                        }
                    }
                }
                self.consume(
                    &TokenKind::RightParen,
                    "Expect ')' after variant parameters",
                )?;
            }
            let end = self.previous_token().range.end();
            return Ok(Pattern::Variant(VariantPattern {
                enum_name: Some(name_ref), // Use name_ref directly
                variant,
                params,
                range: TextRange::new(start_pos, end), // Use start_pos
            }));
        }

        // Just Variant or Identifier
        // If it's a variant like `red(x, y)`, it's a VariantPattern without enum_name
        if self.match_token(TokenKind::LeftParen) {
            let mut params = Vec::new();
            if !self.check(&TokenKind::RightParen) {
                loop {
                    params.push(self.consume_identifier("Expect parameter name")?);
                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                }
            }
            self.consume(
                &TokenKind::RightParen,
                "Expect ')' after variant parameters",
            )?;
            return Ok(Pattern::Variant(VariantPattern {
                enum_name: None,
                variant: name_ref.clone(),
                params,
                range: TextRange::new(name_ref.range.start(), self.previous_token().range.end()),
            }));
        }

        // Check if it's capitalized - likely a variant name
        if name_ref
            .name
            .chars()
            .next()
            .map_or(false, |c| c.is_uppercase())
        {
            return Ok(Pattern::Variant(VariantPattern {
                enum_name: None,
                variant: name_ref.clone(),
                params: Vec::new(),
                range: name_ref.range,
            }));
        }

        Ok(Pattern::Identifier(name_ref))
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

    fn match_token(&mut self, kind: TokenKind) -> bool {
        if self.check(&kind) {
            self.advance();
            true
        } else {
            false
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
    fn test_parse_multiple_statements_no_semicolon() {
        let input = "let x = 10\nlet y = 20";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let result = parser.parse();

        assert_eq!(
            result.errors.len(),
            0,
            "Expected no parse errors, but got: {:?}",
            result.errors
        );
        assert_eq!(result.value.items.len(), 2);
    }

    #[test]
    fn test_parse_if_else_no_semicolon() {
        let input = "if (true) x = 1\nelse x = 2";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let result = parser.parse();

        assert_eq!(
            result.errors.len(),
            0,
            "Expected no parse errors, but got: {:?}",
            result.errors
        );
        assert_eq!(result.value.items.len(), 1);
    }
}
