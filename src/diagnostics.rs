#![allow(dead_code)]

use crate::lexer::{Lexer, TokenKind};
use crate::parser::{ParseError, Parser};
use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};
use std::collections::HashMap;
use text_size::TextRange;

pub struct DiagnosticProvider {
    syntax_errors: Vec<ParseError>,
    semantic_errors: Vec<SemanticError>,
}

#[derive(Debug, Clone)]
pub struct SemanticError {
    pub message: String,
    pub range: TextRange,
    pub severity: DiagnosticSeverity,
    pub code: Option<String>,
}

impl DiagnosticProvider {
    pub fn new() -> Self {
        Self {
            syntax_errors: Vec::new(),
            semantic_errors: Vec::new(),
        }
    }

    pub fn analyze_document(&mut self, content: &str) -> Vec<Diagnostic> {
        self.syntax_errors.clear();
        self.semantic_errors.clear();

        // Lexical analysis
        self.check_lexical_errors(content);

        // Syntax analysis
        self.check_syntax_errors(content);

        // Semantic analysis
        self.check_semantic_errors(content);

        // Convert to LSP diagnostics
        self.to_lsp_diagnostics(content)
    }

    fn check_lexical_errors(&mut self, content: &str) {
        let mut lexer = Lexer::new(content);
        let tokens = lexer.tokenize();

        for token in tokens {
            if token.kind == TokenKind::Error {
                self.syntax_errors.push(ParseError {
                    message: format!("Lexical error: {}", token.text),
                    range: token.range,
                });
            }
        }
    }

    fn check_syntax_errors(&mut self, content: &str) {
        let lexer = Lexer::new(content);
        let mut parser = Parser::new(lexer);
        let parse_result = parser.parse();

        self.syntax_errors.extend(parse_result.errors);
    }

    fn check_semantic_errors(&mut self, content: &str) {
        let lexer = Lexer::new(content);
        let mut parser = Parser::new(lexer);
        let parse_result = parser.parse();

        if parse_result.errors.is_empty() {
            let mut checker = SemanticChecker::new();
            checker.check_source_file(&parse_result.value);
            self.semantic_errors.extend(checker.errors);
        }
    }

    fn to_lsp_diagnostics(&self, content: &str) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        // Add syntax errors
        for error in &self.syntax_errors {
            diagnostics.push(Diagnostic {
                range: self.text_range_to_lsp_range(error.range, content),
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("vscript".to_string()),
                message: error.message.clone(),
                related_information: None,
                tags: None,
                data: None,
            });
        }

        // Add semantic errors
        for error in &self.semantic_errors {
            diagnostics.push(Diagnostic {
                range: self.text_range_to_lsp_range(error.range, content),
                severity: Some(error.severity),
                code: error
                    .code
                    .clone()
                    .map(|c| lsp_types::NumberOrString::String(c)),
                code_description: None,
                source: Some("vscript".to_string()),
                message: error.message.clone(),
                related_information: None,
                tags: None,
                data: None,
            });
        }

        diagnostics
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
}

struct SemanticChecker {
    errors: Vec<SemanticError>,
    scopes: Vec<HashMap<String, SymbolInfo>>,
    current_function: Option<String>,
    loop_depth: usize,
}

#[derive(Debug, Clone)]
struct SymbolInfo {
    name: String,
    symbol_type: SymbolType,
    range: TextRange,
    is_used: bool,
}

#[derive(Debug, Clone, PartialEq)]
enum SymbolType {
    Variable,
    Function,
    Class,
    Parameter,
}

impl SemanticChecker {
    fn new() -> Self {
        Self {
            errors: Vec::new(),
            scopes: vec![HashMap::new()], // Global scope
            current_function: None,
            loop_depth: 0,
        }
    }

    fn check_source_file(&mut self, source_file: &crate::parser::SourceFile) {
        // Add built-in functions to global scope
        self.add_builtins();

        for item in &source_file.items {
            self.check_item(item);
        }

        // Check for unused variables
        self.check_unused_variables();
    }

    fn add_builtins(&mut self) {
        let builtins = vec![
            "print",
            "println",
            "len",
            "push",
            "pop",
            "slice",
            "to_string",
            "to_number",
            "type_of",
            "is_array",
            "is_string",
            "is_number",
            "is_function",
        ];

        for builtin in builtins {
            self.scopes[0].insert(
                builtin.to_string(),
                SymbolInfo {
                    name: builtin.to_string(),
                    symbol_type: SymbolType::Function,
                    range: TextRange::new(0.into(), 0.into()),
                    is_used: false,
                },
            );
        }
    }

    fn check_item(&mut self, item: &crate::parser::Item) {
        match item {
            crate::parser::Item::Function(func) => {
                self.add_symbol(&func.name.name, SymbolType::Function, func.name.range);
                self.check_function(func);
            }
            crate::parser::Item::Class(class) => {
                self.add_symbol(&class.name.name, SymbolType::Class, class.name.range);
                self.check_class(class);
            }
            crate::parser::Item::VariableDeclaration(var_decl) => {
                self.add_symbol(
                    &var_decl.name.name,
                    SymbolType::Variable,
                    var_decl.name.range,
                );
                if let Some(ref value) = var_decl.value {
                    self.check_expression(value);
                }
            }
            crate::parser::Item::Statement(stmt) => {
                self.check_statement(stmt);
            }
            _ => {}
        }
    }

    fn check_function(&mut self, func: &crate::parser::Function) {
        let old_function = self.current_function.clone();
        self.current_function = Some(func.name.name.clone());

        self.push_scope();

        // Add parameters to scope
        for param in &func.params {
            self.add_symbol(&param.name.name, SymbolType::Parameter, param.name.range);
        }

        self.check_block(&func.body);

        // Check that async functions use await
        if func.is_async && !self.has_await_in_block(&func.body) {
            self.add_warning(
                "Async function should use 'await'".to_string(),
                func.range,
                Some("async_no_await".to_string()),
            );
        }

        self.pop_scope();
        self.current_function = old_function;
    }

    fn check_class(&mut self, class: &crate::parser::Class) {
        self.push_scope();

        // Check for constructor
        let _has_init = class.methods.iter().any(|m| m.name.name == "init");

        for method in &class.methods {
            self.check_function(method);

            // Check that init method doesn't return a value
            if method.name.name == "init" && self.has_return_value_in_block(&method.body) {
                self.add_error(
                    "Constructor 'init' should not return a value".to_string(),
                    method.range,
                    Some("constructor_return".to_string()),
                );
            }
        }

        self.pop_scope();
    }

    fn check_block(&mut self, block: &crate::parser::Block) {
        self.push_scope();

        for stmt in &block.statements {
            self.check_statement(stmt);
        }

        self.pop_scope();
    }

    fn check_statement(&mut self, stmt: &crate::parser::Statement) {
        match stmt {
            crate::parser::Statement::Expression(expr) => {
                self.check_expression(expr);
            }
            crate::parser::Statement::VariableDeclaration(var_decl) => {
                if let Some(ref value) = var_decl.value {
                    self.check_expression(value);
                }
                self.add_symbol(
                    &var_decl.name.name,
                    SymbolType::Variable,
                    var_decl.name.range,
                );
            }
            crate::parser::Statement::If(if_stmt) => {
                self.check_expression(&if_stmt.condition);
                self.check_statement(&if_stmt.then_branch);
                if let Some(ref else_branch) = if_stmt.else_branch {
                    self.check_statement(else_branch);
                }
            }
            crate::parser::Statement::While(while_stmt) => {
                self.loop_depth += 1;
                self.check_expression(&while_stmt.condition);
                self.check_statement(&while_stmt.body);
                self.loop_depth -= 1;
            }
            crate::parser::Statement::For(for_stmt) => {
                self.loop_depth += 1;
                self.push_scope();

                if let Some(ref init) = for_stmt.init {
                    self.check_statement(init);
                }
                if let Some(ref condition) = for_stmt.condition {
                    self.check_expression(condition);
                }
                if let Some(ref update) = for_stmt.update {
                    self.check_expression(update);
                }
                self.check_statement(&for_stmt.body);

                self.pop_scope();
                self.loop_depth -= 1;
            }
            crate::parser::Statement::Return(ret_stmt) => {
                if self.current_function.is_none() {
                    self.add_error(
                        "Return statement outside of function".to_string(),
                        ret_stmt.range,
                        Some("return_outside_function".to_string()),
                    );
                }
                if let Some(ref value) = ret_stmt.value {
                    self.check_expression(value);
                }
            }
            crate::parser::Statement::Break(_) | crate::parser::Statement::Continue(_) => {
                if self.loop_depth == 0 {
                    self.add_error(
                        "Break/continue outside of loop".to_string(),
                        match stmt {
                            crate::parser::Statement::Break(s) => s.range,
                            crate::parser::Statement::Continue(s) => s.range,
                            _ => unreachable!(),
                        },
                        Some("break_continue_outside_loop".to_string()),
                    );
                }
            }
            crate::parser::Statement::Block(block) => {
                self.check_block(block);
            }
            _ => {}
        }
    }

    fn check_expression(&mut self, expr: &crate::parser::Expression) {
        match expr {
            crate::parser::Expression::Identifier(id) => {
                if !self.is_symbol_defined(&id.name) {
                    self.add_error(
                        format!("Undefined variable '{}'", id.name),
                        id.range,
                        Some("undefined_variable".to_string()),
                    );
                } else {
                    self.mark_symbol_used(&id.name);
                }
            }
            crate::parser::Expression::Binary(binary) => {
                self.check_expression(&binary.left);
                self.check_expression(&binary.right);

                // Check for potential division by zero
                if matches!(
                    binary.operator,
                    crate::parser::BinaryOperator::Divide | crate::parser::BinaryOperator::Modulo
                ) {
                    if let crate::parser::Expression::Literal(crate::parser::Literal::Number(n)) =
                        binary.right.as_ref()
                    {
                        if *n == 0.0 {
                            self.add_warning(
                                "Division by zero".to_string(),
                                binary.range,
                                Some("division_by_zero".to_string()),
                            );
                        }
                    }
                }
            }
            crate::parser::Expression::Unary(unary) => {
                self.check_expression(&unary.operand);
            }
            crate::parser::Expression::Call(call) => {
                self.check_expression(&call.callee);
                for arg in &call.arguments {
                    self.check_expression(arg);
                }
            }
            crate::parser::Expression::Member(member) => {
                self.check_expression(&member.object);
            }
            crate::parser::Expression::Index(index) => {
                self.check_expression(&index.object);
                self.check_expression(&index.index);
            }
            crate::parser::Expression::Assignment(assign) => {
                self.check_expression(&assign.left);
                self.check_expression(&assign.right);
            }
            crate::parser::Expression::Array(array) => {
                for element in &array.elements {
                    self.check_expression(element);
                }
            }
            crate::parser::Expression::Object(object) => {
                for prop in &object.properties {
                    self.check_expression(&prop.value);
                }
            }
            crate::parser::Expression::Function(func_expr) => {
                self.push_scope();
                for param in &func_expr.params {
                    self.add_symbol(&param.name.name, SymbolType::Parameter, param.name.range);
                }
                self.check_block(&func_expr.body);
                self.pop_scope();
            }
            crate::parser::Expression::Await(await_expr) => {
                if let Some(ref _func_name) = self.current_function {
                    // Check if current function is async
                    // This would require more context to properly implement
                }
                self.check_expression(&await_expr.argument);
            }
            _ => {}
        }
    }

    fn add_symbol(&mut self, name: &str, symbol_type: SymbolType, range: TextRange) {
        if let Some(current_scope) = self.scopes.last_mut() {
            if current_scope.contains_key(name) {
                self.add_error(
                    format!("Variable '{}' is already declared", name),
                    range,
                    Some("duplicate_declaration".to_string()),
                );
            } else {
                current_scope.insert(
                    name.to_string(),
                    SymbolInfo {
                        name: name.to_string(),
                        symbol_type,
                        range,
                        is_used: false,
                    },
                );
            }
        }
    }

    fn is_symbol_defined(&self, name: &str) -> bool {
        for scope in self.scopes.iter().rev() {
            if scope.contains_key(name) {
                return true;
            }
        }
        false
    }

    fn mark_symbol_used(&mut self, name: &str) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(symbol) = scope.get_mut(name) {
                symbol.is_used = true;
                break;
            }
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn check_unused_variables(&mut self) {
        let mut unused_vars = Vec::new();

        for scope in &self.scopes {
            for (name, symbol) in scope {
                if !symbol.is_used
                    && symbol.symbol_type == SymbolType::Variable
                    && !name.starts_with('_')
                {
                    unused_vars.push((name.clone(), symbol.range, "unused_variable".to_string()));
                }
            }
        }

        for (name, range, code) in unused_vars {
            self.add_warning(
                format!("Variable '{}' is declared but never used", name),
                range,
                Some(code),
            );
        }
    }

    fn has_await_in_block(&self, block: &crate::parser::Block) -> bool {
        for stmt in &block.statements {
            if self.has_await_in_statement(stmt) {
                return true;
            }
        }
        false
    }

    fn has_await_in_statement(&self, stmt: &crate::parser::Statement) -> bool {
        match stmt {
            crate::parser::Statement::Expression(expr) => self.has_await_in_expression(expr),
            crate::parser::Statement::If(if_stmt) => {
                self.has_await_in_expression(&if_stmt.condition)
                    || self.has_await_in_statement(&if_stmt.then_branch)
                    || if_stmt
                        .else_branch
                        .as_ref()
                        .map_or(false, |b| self.has_await_in_statement(b))
            }
            crate::parser::Statement::While(while_stmt) => {
                self.has_await_in_expression(&while_stmt.condition)
                    || self.has_await_in_statement(&while_stmt.body)
            }
            crate::parser::Statement::Block(block) => self.has_await_in_block(block),
            _ => false,
        }
    }

    fn has_await_in_expression(&self, expr: &crate::parser::Expression) -> bool {
        match expr {
            crate::parser::Expression::Await(_) => true,
            crate::parser::Expression::Binary(binary) => {
                self.has_await_in_expression(&binary.left)
                    || self.has_await_in_expression(&binary.right)
            }
            crate::parser::Expression::Call(call) => {
                self.has_await_in_expression(&call.callee)
                    || call
                        .arguments
                        .iter()
                        .any(|arg| self.has_await_in_expression(arg))
            }
            _ => false,
        }
    }

    fn has_return_value_in_block(&self, block: &crate::parser::Block) -> bool {
        for stmt in &block.statements {
            if let crate::parser::Statement::Return(ret) = stmt {
                if ret.value.is_some() {
                    return true;
                }
            }
        }
        false
    }

    fn add_error(&mut self, message: String, range: TextRange, code: Option<String>) {
        self.errors.push(SemanticError {
            message,
            range,
            severity: DiagnosticSeverity::ERROR,
            code,
        });
    }

    fn add_warning(&mut self, message: String, range: TextRange, code: Option<String>) {
        self.errors.push(SemanticError {
            message,
            range,
            severity: DiagnosticSeverity::WARNING,
            code,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_diagnostic_provider_creation() {
        let _provider = DiagnosticProvider::new();
        // Just test that we can create the provider
        assert!(true);
    }
}
