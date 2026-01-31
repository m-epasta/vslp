#![allow(deprecated, dead_code)]

use crate::lexer::{Lexer, TokenKind};
use crate::parser::{Expression, Item, Parser, SourceFile, Statement};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, DocumentSymbol, Position, Range, SemanticToken, SymbolKind,
};
use std::collections::HashMap;
use text_size::TextRange;
use url::Url;

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub range: TextRange,
    pub uri: Url,
    pub definition_range: TextRange,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub symbols: HashMap<String, Symbol>,
    pub parent: Option<Box<Scope>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            parent: None,
        }
    }

    pub fn with_parent(parent: Scope) -> Self {
        Self {
            symbols: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn insert(&mut self, name: String, symbol: Symbol) {
        self.symbols.insert(name, symbol);
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.symbols
            .get(name)
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.lookup(name)))
    }
}

#[derive(Debug)]
pub struct VScriptAnalyzer {
    global_scope: Scope,
}

impl VScriptAnalyzer {
    pub fn new() -> Self {
        let mut global_scope = Scope::new();

        // Add built-in functions
        for builtin in &[
            "print",
            "println",
            "len",
            "push",
            "pop",
            "slice",
            "to_string",
            "to_number",
        ] {
            global_scope.insert(
                builtin.to_string(),
                Symbol {
                    name: builtin.to_string(),
                    kind: SymbolKind::FUNCTION,
                    range: TextRange::new(0.into(), 0.into()),
                    uri: Url::parse("builtin://").unwrap(),
                    definition_range: TextRange::new(0.into(), 0.into()),
                },
            );
        }

        Self { global_scope }
    }

    pub async fn get_diagnostics(&self, content: &str, uri: &Url) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        let lexer = Lexer::new(content);
        let mut parser = Parser::new(lexer);
        let parse_result = parser.parse();

        // Convert parse errors to diagnostics
        for error in parse_result.errors {
            diagnostics.push(Diagnostic {
                range: self.text_range_to_lsp_range(error.range, content),
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("vscript".to_string()),
                message: error.message,
                related_information: None,
                tags: None,
                data: None,
            });
        }

        // Semantic analysis
        let mut scope = Scope::with_parent(self.global_scope.clone());
        self.analyze_source_file(
            &parse_result.value,
            &mut scope,
            uri,
            content,
            &mut diagnostics,
        );

        diagnostics
    }

    fn analyze_source_file(
        &self,
        source_file: &SourceFile,
        scope: &mut Scope,
        uri: &Url,
        content: &str,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        for item in &source_file.items {
            self.analyze_item(item, scope, uri, content, diagnostics);
        }
    }

    fn analyze_item(
        &self,
        item: &Item,
        scope: &mut Scope,
        uri: &Url,
        content: &str,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        match item {
            Item::Function(func) => {
                // Add function to scope
                scope.insert(
                    func.name.name.clone(),
                    Symbol {
                        name: func.name.name.clone(),
                        kind: SymbolKind::FUNCTION,
                        range: func.range,
                        uri: uri.clone(),
                        definition_range: func.name.range,
                    },
                );

                // Analyze function body with new scope
                let mut func_scope = Scope::with_parent(scope.clone());

                // Add parameters to function scope
                for param in &func.params {
                    func_scope.insert(
                        param.name.name.clone(),
                        Symbol {
                            name: param.name.name.clone(),
                            kind: SymbolKind::VARIABLE,
                            range: param.range,
                            uri: uri.clone(),
                            definition_range: param.name.range,
                        },
                    );
                }

                self.analyze_block(&func.body, &mut func_scope, uri, content, diagnostics);
            }
            Item::Class(class) => {
                // Add class to scope
                scope.insert(
                    class.name.name.clone(),
                    Symbol {
                        name: class.name.name.clone(),
                        kind: SymbolKind::CLASS,
                        range: class.range,
                        uri: uri.clone(),
                        definition_range: class.name.range,
                    },
                );

                // Analyze class methods
                for method in &class.methods {
                    let method_item = Item::Function(method.clone());
                    self.analyze_item(&method_item, scope, uri, content, diagnostics);
                }
            }
            Item::Struct(strct) => {
                scope.insert(
                    strct.name.name.clone(),
                    Symbol {
                        name: strct.name.name.clone(),
                        kind: SymbolKind::STRUCT,
                        range: strct.range,
                        uri: uri.clone(),
                        definition_range: strct.name.range,
                    },
                );
            }
            Item::Enum(enm) => {
                scope.insert(
                    enm.name.name.clone(),
                    Symbol {
                        name: enm.name.name.clone(),
                        kind: SymbolKind::ENUM,
                        range: enm.range,
                        uri: uri.clone(),
                        definition_range: enm.name.range,
                    },
                );
            }
            Item::VariableDeclaration(var_decl) => {
                scope.insert(
                    var_decl.name.name.clone(),
                    Symbol {
                        name: var_decl.name.name.clone(),
                        kind: SymbolKind::VARIABLE,
                        range: var_decl.range,
                        uri: uri.clone(),
                        definition_range: var_decl.name.range,
                    },
                );

                if let Some(ref value) = var_decl.value {
                    self.analyze_expression(value, scope, uri, content, diagnostics);
                }
            }
            Item::Statement(stmt) => {
                self.analyze_statement(stmt, scope, uri, content, diagnostics);
            }
            _ => {} // Handle other items as needed
        }
    }

    fn analyze_block(
        &self,
        block: &crate::parser::Block,
        scope: &mut Scope,
        uri: &Url,
        content: &str,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        let mut block_scope = Scope::with_parent(scope.clone());
        for stmt in &block.statements {
            self.analyze_statement(stmt, &mut block_scope, uri, content, diagnostics);
        }
    }

    fn analyze_statement(
        &self,
        stmt: &Statement,
        scope: &mut Scope,
        uri: &Url,
        content: &str,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        match stmt {
            Statement::Expression(expr) => {
                self.analyze_expression(expr, scope, uri, content, diagnostics);
            }
            Statement::VariableDeclaration(var_decl) => {
                scope.insert(
                    var_decl.name.name.clone(),
                    Symbol {
                        name: var_decl.name.name.clone(),
                        kind: SymbolKind::VARIABLE,
                        range: var_decl.range,
                        uri: uri.clone(),
                        definition_range: var_decl.name.range,
                    },
                );

                if let Some(ref value) = var_decl.value {
                    self.analyze_expression(value, scope, uri, content, diagnostics);
                }
            }
            Statement::If(if_stmt) => {
                self.analyze_expression(&if_stmt.condition, scope, uri, content, diagnostics);
                self.analyze_statement(&if_stmt.then_branch, scope, uri, content, diagnostics);
                if let Some(ref else_branch) = if_stmt.else_branch {
                    self.analyze_statement(else_branch, scope, uri, content, diagnostics);
                }
            }
            Statement::While(while_stmt) => {
                self.analyze_expression(&while_stmt.condition, scope, uri, content, diagnostics);
                self.analyze_statement(&while_stmt.body, scope, uri, content, diagnostics);
            }
            Statement::For(for_stmt) => {
                let mut for_scope = Scope::with_parent(scope.clone());
                if let Some(ref init) = for_stmt.init {
                    self.analyze_statement(init, &mut for_scope, uri, content, diagnostics);
                }
                if let Some(ref condition) = for_stmt.condition {
                    self.analyze_expression(condition, &mut for_scope, uri, content, diagnostics);
                }
                if let Some(ref update) = for_stmt.update {
                    self.analyze_expression(update, &mut for_scope, uri, content, diagnostics);
                }
                self.analyze_statement(&for_stmt.body, &mut for_scope, uri, content, diagnostics);
            }
            Statement::Return(ret_stmt) => {
                if let Some(ref value) = ret_stmt.value {
                    self.analyze_expression(value, scope, uri, content, diagnostics);
                }
            }
            Statement::Block(block) => {
                self.analyze_block(block, scope, uri, content, diagnostics);
            }
            _ => {} // Handle other statements
        }
    }

    fn analyze_expression(
        &self,
        expr: &Expression,
        scope: &mut Scope,
        uri: &Url,
        content: &str,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        match expr {
            Expression::Identifier(id) => {
                // Check if identifier is defined
                if scope.lookup(&id.name).is_none() {
                    diagnostics.push(Diagnostic {
                        range: self.text_range_to_lsp_range(id.range, content),
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some("vscript".to_string()),
                        message: format!("Undefined variable '{}'", id.name),
                        related_information: None,
                        tags: None,
                        data: None,
                    });
                }
            }
            Expression::Binary(binary) => {
                self.analyze_expression(&binary.left, scope, uri, content, diagnostics);
                self.analyze_expression(&binary.right, scope, uri, content, diagnostics);
            }
            Expression::Unary(unary) => {
                self.analyze_expression(&unary.operand, scope, uri, content, diagnostics);
            }
            Expression::Call(call) => {
                self.analyze_expression(&call.callee, scope, uri, content, diagnostics);
                for arg in &call.arguments {
                    self.analyze_expression(arg, scope, uri, content, diagnostics);
                }
            }
            Expression::Member(member) => {
                self.analyze_expression(&member.object, scope, uri, content, diagnostics);
            }
            Expression::Index(index) => {
                self.analyze_expression(&index.object, scope, uri, content, diagnostics);
                self.analyze_expression(&index.index, scope, uri, content, diagnostics);
            }
            Expression::Assignment(assign) => {
                self.analyze_expression(&assign.left, scope, uri, content, diagnostics);
                self.analyze_expression(&assign.right, scope, uri, content, diagnostics);
            }
            Expression::Array(array) => {
                for element in &array.elements {
                    self.analyze_expression(element, scope, uri, content, diagnostics);
                }
            }
            Expression::Object(object) => {
                for prop in &object.properties {
                    self.analyze_expression(&prop.value, scope, uri, content, diagnostics);
                }
            }
            Expression::Function(func_expr) => {
                let mut func_scope = Scope::with_parent(scope.clone());
                for param in &func_expr.params {
                    func_scope.insert(
                        param.name.name.clone(),
                        Symbol {
                            name: param.name.name.clone(),
                            kind: SymbolKind::VARIABLE,
                            range: param.range,
                            uri: uri.clone(),
                            definition_range: param.name.range,
                        },
                    );
                }
                self.analyze_block(&func_expr.body, &mut func_scope, uri, content, diagnostics);
            }
            Expression::Await(await_expr) => {
                self.analyze_expression(&await_expr.argument, scope, uri, content, diagnostics);
            }
            _ => {} // Handle other expressions
        }
    }

    pub async fn get_document_symbols(&self, content: &str, _uri: &Url) -> Vec<DocumentSymbol> {
        let lexer = Lexer::new(content);
        let mut parser = Parser::new(lexer);
        let parse_result = parser.parse();

        let mut symbols = Vec::new();
        for item in &parse_result.value.items {
            if let Some(symbol) = self.item_to_document_symbol(item, content) {
                symbols.push(symbol);
            }
        }

        symbols
    }

    fn item_to_document_symbol(&self, item: &Item, content: &str) -> Option<DocumentSymbol> {
        match item {
            Item::Function(func) => Some(DocumentSymbol {
                name: func.name.name.clone(),
                detail: Some("function".to_string()),
                kind: SymbolKind::FUNCTION,
                tags: None,
                deprecated: None,
                range: self.text_range_to_lsp_range(func.range, content),
                selection_range: self.text_range_to_lsp_range(func.name.range, content),
                children: None,
            }),
            Item::Class(class) => {
                let mut children = Vec::new();
                for method in &class.methods {
                    if let Some(child) =
                        self.item_to_document_symbol(&Item::Function(method.clone()), content)
                    {
                        children.push(child);
                    }
                }

                Some(DocumentSymbol {
                    name: class.name.name.clone(),
                    detail: Some("class".to_string()),
                    kind: SymbolKind::CLASS,
                    tags: None,
                    deprecated: None,
                    range: self.text_range_to_lsp_range(class.range, content),
                    selection_range: self.text_range_to_lsp_range(class.name.range, content),
                    children: if children.is_empty() {
                        None
                    } else {
                        Some(children)
                    },
                })
            }
            Item::VariableDeclaration(var_decl) => Some(DocumentSymbol {
                name: var_decl.name.name.clone(),
                detail: Some("variable".to_string()),
                kind: SymbolKind::VARIABLE,
                tags: None,
                deprecated: None,
                range: self.text_range_to_lsp_range(var_decl.range, content),
                selection_range: self.text_range_to_lsp_range(var_decl.name.range, content),
                children: None,
            }),
            Item::Struct(strct) => Some(DocumentSymbol {
                name: strct.name.name.clone(),
                detail: Some("struct".to_string()),
                kind: SymbolKind::STRUCT,
                tags: None,
                deprecated: None,
                range: self.text_range_to_lsp_range(strct.range, content),
                selection_range: self.text_range_to_lsp_range(strct.name.range, content),
                children: None,
            }),
            Item::Enum(enm) => Some(DocumentSymbol {
                name: enm.name.name.clone(),
                detail: Some("enum".to_string()),
                kind: SymbolKind::ENUM,
                tags: None,
                deprecated: None,
                range: self.text_range_to_lsp_range(enm.range, content),
                selection_range: self.text_range_to_lsp_range(enm.name.range, content),
                children: None,
            }),
            _ => None,
        }
    }

    pub async fn get_semantic_tokens(&self, content: &str) -> Vec<SemanticToken> {
        let lexer = Lexer::new(content);
        let mut parser = Parser::new(lexer);
        let _parse_result = parser.parse();

        // Re-tokenize for semantic highlighting
        let mut lexer = Lexer::new(content);
        let tokens = lexer.tokenize();

        let mut semantic_tokens = Vec::new();
        let mut prev_line = 0;
        let mut prev_col = 0;

        for token in tokens {
            let range = self.text_range_to_lsp_range(token.range, content);
            let token_type = self.token_kind_to_semantic_type(&token.kind);
            let token_modifiers = 0; // No modifiers for now

            let delta_line = range.start.line - prev_line;
            let delta_col = if delta_line == 0 {
                range.start.character - prev_col
            } else {
                range.start.character
            };

            semantic_tokens.push(SemanticToken {
                delta_line,
                delta_start: delta_col,
                length: range.end.character - range.start.character,
                token_type,
                token_modifiers_bitset: token_modifiers,
            });

            prev_line = range.start.line;
            prev_col = range.start.character;
        }

        semantic_tokens
    }

    fn token_kind_to_semantic_type(&self, kind: &TokenKind) -> u32 {
        match kind {
            TokenKind::Fn
            | TokenKind::Class
            | TokenKind::Let
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::While
            | TokenKind::For
            | TokenKind::Return
            | TokenKind::This
            | TokenKind::Super
            | TokenKind::Import
            | TokenKind::Export
            | TokenKind::Async
            | TokenKind::Await
            | TokenKind::Try
            | TokenKind::Catch
            | TokenKind::Match
            | TokenKind::Struct
            | TokenKind::Enum
            | TokenKind::And
            | TokenKind::Or => 0, // KEYWORD
            TokenKind::String => 1,     // STRING
            TokenKind::Comment => 2,    // COMMENT
            TokenKind::Number => 3,     // NUMBER
            TokenKind::Identifier => 6, // VARIABLE (will be refined based on context)
            _ => 9,                     // OPERATOR
        }
    }

    fn text_range_to_lsp_range(&self, range: TextRange, content: &str) -> Range {
        let start_offset = usize::from(range.start());
        let end_offset = usize::from(range.end());

        let start_pos = self.offset_to_position(start_offset, content);
        let end_pos = self.offset_to_position(end_offset, content);

        Range {
            start: start_pos,
            end: end_pos,
        }
    }

    fn offset_to_position(&self, offset: usize, content: &str) -> Position {
        let mut line = 0;
        let mut col = 0;

        for (i, ch) in content.char_indices() {
            if i >= offset {
                break;
            }
            if ch == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }

        Position {
            line,
            character: col,
        }
    }

    #[allow(dead_code)]
    pub fn lookup_symbol<'a>(&self, _name: &str, scope: &'a Scope) -> Option<&'a Symbol> {
        // This would need to be implemented properly
        // For now, return None to avoid compilation error
        let _ = scope;
        None
    }
}
