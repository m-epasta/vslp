use crate::lexer::{Lexer, TokenKind};
use crate::parser::{Expression, Item, Parser, Statement};
use crate::workspace::WorkspaceManager;
use lsp_types::{Location, Position, Range};
use std::collections::HashMap;
use text_size::TextRange;
use url::Url;

pub async fn get_definitions(
    content: &str,
    position: Position,
    workspace: &WorkspaceManager,
    current_uri: &Url,
) -> Option<Vec<Location>> {
    let offset = position_to_offset(content, position);
    let identifier = find_identifier_at_position(content, offset)?;

    let mut definitions = Vec::new();

    // First, check the current document
    if let Some(def) = find_definition_in_document(content, &identifier, current_uri).await {
        definitions.push(def);
    }

    // Then check other documents in the workspace
    for doc in workspace.documents.values() {
        if doc.uri != *current_uri {
            if let Some(def) =
                find_definition_in_document(&doc.content, &identifier, &doc.uri).await
            {
                definitions.push(def);
            }
        }
    }

    // Check for built-in definitions
    if let Some(def) = find_builtin_definition(&identifier) {
        definitions.push(def);
    }

    if definitions.is_empty() {
        None
    } else {
        Some(definitions)
    }
}

pub async fn get_references(
    content: &str,
    position: Position,
    workspace: &WorkspaceManager,
    current_uri: &Url,
) -> Option<Vec<Location>> {
    let offset = position_to_offset(content, position);
    let identifier = find_identifier_at_position(content, offset)?;

    let mut references = Vec::new();

    // Find references in all documents
    for doc in workspace.documents.values() {
        references.extend(find_references_in_document(&doc.content, &identifier, &doc.uri).await);
    }

    // Also include the current document if not already processed
    if !workspace.documents.contains_key(current_uri) {
        references.extend(find_references_in_document(content, &identifier, current_uri).await);
    }

    if references.is_empty() {
        None
    } else {
        Some(references)
    }
}

async fn find_definition_in_document(
    content: &str,
    identifier: &str,
    uri: &Url,
) -> Option<Location> {
    let lexer = Lexer::new(content);
    let mut parser = Parser::new(lexer);
    let parse_result = parser.parse();

    if !parse_result.errors.is_empty() {
        return None;
    }

    let mut finder = DefinitionFinder::new(identifier, uri.clone(), content);
    finder.visit_source_file(&parse_result.value);

    finder.definition
}

async fn find_references_in_document(content: &str, identifier: &str, uri: &Url) -> Vec<Location> {
    let lexer = Lexer::new(content);
    let mut parser = Parser::new(lexer);
    let parse_result = parser.parse();

    if !parse_result.errors.is_empty() {
        return Vec::new();
    }

    let mut finder = ReferenceFinder::new(identifier, uri.clone(), content);
    finder.visit_source_file(&parse_result.value);

    finder.references
}

fn find_identifier_at_position(content: &str, offset: usize) -> Option<String> {
    if offset >= content.len() {
        return None;
    }

    let mut lexer = Lexer::new(content);
    let tokens = lexer.tokenize();

    for token in tokens {
        let start = usize::from(token.range.start());
        let token_end = usize::from(token.range.end());

        if offset >= start && offset <= token_end && token.kind == TokenKind::Identifier {
            return Some(token.text);
        }
    }

    None
}

fn find_builtin_definition(identifier: &str) -> Option<Location> {
    let builtins = [
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

    if builtins.contains(&identifier) {
        // Return a builtin:// URI for built-in functions
        if let Ok(uri) = Url::parse(&format!("builtin://{}.vs", identifier)) {
            return Some(Location {
                uri,
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end: Position {
                        line: 0,
                        character: identifier.len() as u32,
                    },
                },
            });
        }
    }

    None
}

struct DefinitionFinder {
    target: String,
    uri: Url,
    content: String,
    definition: Option<Location>,
    scopes: Vec<HashMap<String, TextRange>>,
}

impl DefinitionFinder {
    fn new(target: &str, uri: Url, content: &str) -> Self {
        Self {
            target: target.to_string(),
            uri,
            content: content.to_string(),
            definition: None,
            scopes: vec![HashMap::new()], // Global scope
        }
    }

    fn visit_source_file(&mut self, source_file: &crate::parser::SourceFile) {
        for item in &source_file.items {
            self.visit_item(item);
            if self.definition.is_some() {
                break;
            }
        }
    }

    fn visit_item(&mut self, item: &Item) {
        match item {
            Item::Function(func) => {
                if func.name.name == self.target {
                    self.definition = Some(self.create_location(func.name.range));
                    return;
                }

                self.push_scope();

                // Add parameters to scope
                for param in &func.params {
                    self.add_to_scope(&param.name, param.range);
                }

                self.visit_block(&func.body);
                self.pop_scope();
            }
            Item::Class(class) => {
                if class.name.name == self.target {
                    self.definition = Some(self.create_location(class.name.range));
                    return;
                }

                self.push_scope();
                for method in &class.methods {
                    self.visit_item(&Item::Function(method.clone()));
                    if self.definition.is_some() {
                        break;
                    }
                }
                self.pop_scope();
            }
            Item::VariableDeclaration(var_decl) => {
                if var_decl.name.name == self.target {
                    self.definition = Some(self.create_location(var_decl.name.range));
                    return;
                }

                self.add_to_scope(&var_decl.name.name, var_decl.name.range);

                if let Some(ref value) = var_decl.value {
                    self.visit_expression(value);
                }
            }
            Item::Statement(stmt) => {
                self.visit_statement(stmt);
            }
            _ => {}
        }
    }

    fn visit_block(&mut self, block: &crate::parser::Block) {
        self.push_scope();
        for stmt in &block.statements {
            self.visit_statement(stmt);
            if self.definition.is_some() {
                break;
            }
        }
        self.pop_scope();
    }

    fn visit_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Expression(expr) => {
                self.visit_expression(expr);
            }
            Statement::VariableDeclaration(var_decl) => {
                if var_decl.name.name == self.target {
                    self.definition = Some(self.create_location(var_decl.name.range));
                    return;
                }

                self.add_to_scope(&var_decl.name.name, var_decl.name.range);

                if let Some(ref value) = var_decl.value {
                    self.visit_expression(value);
                }
            }
            Statement::If(if_stmt) => {
                self.visit_expression(&if_stmt.condition);
                if self.definition.is_some() {
                    return;
                }
                self.visit_statement(&if_stmt.then_branch);
                if self.definition.is_some() {
                    return;
                }
                if let Some(ref else_branch) = if_stmt.else_branch {
                    self.visit_statement(else_branch);
                }
            }
            Statement::While(while_stmt) => {
                self.visit_expression(&while_stmt.condition);
                if self.definition.is_some() {
                    return;
                }
                self.visit_statement(&while_stmt.body);
            }
            Statement::For(for_stmt) => {
                self.push_scope();
                if let Some(ref init) = for_stmt.initializer {
                    self.visit_statement(init);
                }
                if self.definition.is_some() {
                    self.pop_scope();
                    return;
                }
                if let Some(ref condition) = for_stmt.condition {
                    self.visit_expression(condition);
                }
                if self.definition.is_some() {
                    self.pop_scope();
                    return;
                }
                if let Some(ref update) = for_stmt.increment {
                    self.visit_expression(update);
                }
                if self.definition.is_some() {
                    self.pop_scope();
                    return;
                }
                self.visit_statement(&for_stmt.body);
                self.pop_scope();
            }
            Statement::Return(ret_stmt) => {
                if let Some(ref value) = ret_stmt.value {
                    self.visit_expression(value);
                }
            }
            Statement::Block(block) => {
                self.visit_block(block);
            }
            _ => {}
        }
    }

    fn visit_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Identifier(id) => {
                if id.name == self.target {
                    // Check if this identifier is defined in current scopes
                    if let Some(def_range) = self.lookup_in_scopes(&id.name) {
                        self.definition = Some(self.create_location(def_range));
                    }
                }
            }
            Expression::Binary(binary) => {
                self.visit_expression(&binary.left);
                if self.definition.is_some() {
                    return;
                }
                self.visit_expression(&binary.right);
            }
            Expression::Unary(unary) => {
                self.visit_expression(&unary.operand);
            }
            Expression::Call(call) => {
                self.visit_expression(&call.callee);
                if self.definition.is_some() {
                    return;
                }
                for arg in &call.arguments {
                    self.visit_expression(arg);
                    if self.definition.is_some() {
                        return;
                    }
                }
            }
            Expression::Get(get) => {
                self.visit_expression(&get.object);
            }
            Expression::Set(set) => {
                self.visit_expression(&set.object);
                if self.definition.is_some() {
                    return;
                }
                self.visit_expression(&set.value);
            }
            Expression::Index(idx) => {
                self.visit_expression(&idx.object);
                if self.definition.is_some() {
                    return;
                }
                self.visit_expression(&idx.index);
            }
            Expression::AssignIndex(idx) => {
                self.visit_expression(&idx.object);
                if self.definition.is_some() {
                    return;
                }
                self.visit_expression(&idx.index);
                if self.definition.is_some() {
                    return;
                }
                self.visit_expression(&idx.value);
            }
            Expression::Assignment(assign) => {
                if assign.name.name == self.target {
                    if let Some(def_range) = self.lookup_in_scopes(&assign.name.name) {
                        self.definition = Some(self.create_location(def_range));
                    }
                }
                if self.definition.is_some() {
                    return;
                }
                self.visit_expression(&assign.value);
            }
            Expression::Logical(logical) => {
                self.visit_expression(&logical.left);
                if self.definition.is_some() {
                    return;
                }
                self.visit_expression(&logical.right);
            }
            Expression::Array(array) => {
                for element in &array.elements {
                    self.visit_expression(element);
                    if self.definition.is_some() {
                        return;
                    }
                }
            }
            Expression::Map(map) => {
                for key in &map.keys {
                    self.visit_expression(key);
                    if self.definition.is_some() {
                        return;
                    }
                }
                for value in &map.values {
                    self.visit_expression(value);
                    if self.definition.is_some() {
                        return;
                    }
                }
            }
            Expression::Grouping(expr) => {
                self.visit_expression(expr);
            }
            Expression::Postfix(postfix) => {
                self.visit_expression(&postfix.left);
            }
            Expression::InterpolatedString(interp) => {
                for part in &interp.parts {
                    self.visit_expression(part);
                    if self.definition.is_some() {
                        return;
                    }
                }
            }
            Expression::Match(match_expr) => {
                self.visit_expression(&match_expr.target);
                if self.definition.is_some() {
                    return;
                }
                for arm in &match_expr.arms {
                    self.visit_expression(&arm.body);
                    if self.definition.is_some() {
                        return;
                    }
                }
            }
            Expression::Function(func_expr) => {
                self.push_scope();
                for param in &func_expr.params {
                    self.add_to_scope(&param.name.name, param.name.range);
                }
                self.visit_block(&func_expr.body);
                self.pop_scope();
            }
            Expression::Await(await_expr) => {
                self.visit_expression(&await_expr.argument);
            }
            Expression::This(_this) => {}
            Expression::Literal(_lit) => {}
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn add_to_scope(&mut self, name: &str, range: TextRange) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name.to_string(), range);
        }
    }

    fn lookup_in_scopes(&self, name: &str) -> Option<TextRange> {
        for scope in self.scopes.iter().rev() {
            if let Some(range) = scope.get(name) {
                return Some(*range);
            }
        }
        None
    }

    fn create_location(&self, range: TextRange) -> Location {
        Location {
            uri: self.uri.clone(),
            range: self.text_range_to_lsp_range(range),
        }
    }

    fn text_range_to_lsp_range(&self, range: TextRange) -> Range {
        let start_offset = usize::from(range.start());
        let end_offset = usize::from(range.end());

        let start_pos = self.offset_to_position(start_offset);
        let end_pos = self.offset_to_position(end_offset);

        Range {
            start: start_pos,
            end: end_pos,
        }
    }

    fn offset_to_position(&self, offset: usize) -> Position {
        let mut line = 0;
        let mut col = 0;

        for (i, ch) in self.content.char_indices() {
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

struct ReferenceFinder {
    target: String,
    uri: Url,
    content: String,
    references: Vec<Location>,
}

impl ReferenceFinder {
    fn new(target: &str, uri: Url, content: &str) -> Self {
        Self {
            target: target.to_string(),
            uri,
            content: content.to_string(),
            references: Vec::new(),
        }
    }

    fn visit_source_file(&mut self, source_file: &crate::parser::SourceFile) {
        for item in &source_file.items {
            self.visit_item(item);
        }
    }

    fn visit_item(&mut self, item: &Item) {
        match item {
            Item::Function(func) => {
                if func.name.name == self.target {
                    self.references.push(self.create_location(func.name.range));
                }

                for param in &func.params {
                    if param.name == self.target {
                        self.references.push(self.create_location(param.range));
                    }
                }

                self.visit_block(&func.body);
            }
            Item::Class(class) => {
                if class.name.name == self.target {
                    self.references.push(self.create_location(class.name.range));
                }

                for method in &class.methods {
                    self.visit_item(&Item::Function(method.clone()));
                }
            }
            Item::VariableDeclaration(var_decl) => {
                if var_decl.name.name == self.target {
                    self.references
                        .push(self.create_location(var_decl.name.range));
                }

                if let Some(ref value) = var_decl.value {
                    self.visit_expression(value);
                }
            }
            Item::Statement(stmt) => {
                self.visit_statement(stmt);
            }
            _ => {}
        }
    }

    fn visit_block(&mut self, block: &crate::parser::Block) {
        for stmt in &block.statements {
            self.visit_statement(stmt);
        }
    }

    fn visit_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Expression(expr) => {
                self.visit_expression(expr);
            }
            Statement::VariableDeclaration(var_decl) => {
                if var_decl.name.name == self.target {
                    self.references
                        .push(self.create_location(var_decl.name.range));
                }

                if let Some(ref value) = var_decl.value {
                    self.visit_expression(value);
                }
            }
            Statement::If(if_stmt) => {
                self.visit_expression(&if_stmt.condition);
                self.visit_statement(&if_stmt.then_branch);
                if let Some(ref else_branch) = if_stmt.else_branch {
                    self.visit_statement(else_branch);
                }
            }
            Statement::While(while_stmt) => {
                self.visit_expression(&while_stmt.condition);
                self.visit_statement(&while_stmt.body);
            }
            Statement::For(for_stmt) => {
                if let Some(ref init) = for_stmt.initializer {
                    self.visit_statement(init);
                }
                if let Some(ref condition) = for_stmt.condition {
                    self.visit_expression(condition);
                }
                if let Some(ref update) = for_stmt.increment {
                    self.visit_expression(update);
                }
                self.visit_statement(&for_stmt.body);
            }
            Statement::Return(ret_stmt) => {
                if let Some(ref value) = ret_stmt.value {
                    self.visit_expression(value);
                }
            }
            Statement::Block(block) => {
                self.visit_block(block);
            }
            _ => {}
        }
    }

    fn visit_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Identifier(id) => {
                if id.name == self.target {
                    self.references.push(self.create_location(id.range));
                }
            }
            Expression::Binary(binary) => {
                self.visit_expression(&binary.left);
                self.visit_expression(&binary.right);
            }
            Expression::Unary(unary) => {
                self.visit_expression(&unary.operand);
            }
            Expression::Call(call) => {
                self.visit_expression(&call.callee);
                for arg in &call.arguments {
                    self.visit_expression(arg);
                }
            }
            Expression::Literal(_lit) => {}
            Expression::Get(get) => {
                self.visit_expression(&get.object);
            }
            Expression::Set(set) => {
                self.visit_expression(&set.object);
                self.visit_expression(&set.value);
            }
            Expression::Index(idx) => {
                self.visit_expression(&idx.object);
                self.visit_expression(&idx.index);
            }
            Expression::AssignIndex(idx) => {
                self.visit_expression(&idx.object);
                self.visit_expression(&idx.index);
                self.visit_expression(&idx.value);
            }
            Expression::Assignment(assign) => {
                if assign.name.name == self.target {
                    self.references
                        .push(self.create_location(assign.name.range));
                }
                self.visit_expression(&assign.value);
            }
            Expression::Logical(logical) => {
                self.visit_expression(&logical.left);
                self.visit_expression(&logical.right);
            }
            Expression::Array(array) => {
                for element in &array.elements {
                    self.visit_expression(element);
                }
            }
            Expression::Map(map) => {
                for key in &map.keys {
                    self.visit_expression(key);
                }
                for value in &map.values {
                    self.visit_expression(value);
                }
            }
            Expression::Grouping(expr) => {
                self.visit_expression(expr);
            }
            Expression::Postfix(postfix) => {
                self.visit_expression(&postfix.left);
            }
            Expression::InterpolatedString(interp) => {
                for part in &interp.parts {
                    self.visit_expression(part);
                }
            }
            Expression::Match(match_expr) => {
                self.visit_expression(&match_expr.target);
                for arm in &match_expr.arms {
                    self.visit_expression(&arm.body);
                }
            }
            Expression::Function(func_expr) => {
                for param in &func_expr.params {
                    if param.name.name == self.target {
                        self.references.push(self.create_location(param.name.range));
                    }
                }
                self.visit_block(&func_expr.body);
            }
            Expression::Await(await_expr) => {
                self.visit_expression(&await_expr.argument);
            }
            Expression::This(_this) => {}
        }
    }

    fn create_location(&self, range: TextRange) -> Location {
        Location {
            uri: self.uri.clone(),
            range: self.text_range_to_lsp_range(range),
        }
    }

    fn text_range_to_lsp_range(&self, range: TextRange) -> Range {
        let start_offset = usize::from(range.start());
        let end_offset = usize::from(range.end());

        let start_pos = self.offset_to_position(start_offset);
        let end_pos = self.offset_to_position(end_offset);

        Range {
            start: start_pos,
            end: end_pos,
        }
    }

    fn offset_to_position(&self, offset: usize) -> Position {
        let mut line = 0;
        let mut col = 0;

        for (i, ch) in self.content.char_indices() {
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

fn position_to_offset(content: &str, position: Position) -> usize {
    let mut offset = 0;
    let mut line = 0;
    let mut col = 0;

    for ch in content.chars() {
        if line == position.line && col == position.character {
            break;
        }

        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
        offset += ch.len_utf8();
    }

    offset
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_identifier_at_position() {
        let content = "let hello = world;";
        let offset = 12; // Position of 'w' in 'world'
        let result = find_identifier_at_position(content, offset);
        assert_eq!(result, Some("world".to_string()));
    }

    #[test]
    fn test_position_to_offset() {
        let content = "line1\nline2\nline3";
        let position = Position {
            line: 1,
            character: 2,
        };
        let offset = position_to_offset(content, position);
        assert_eq!(offset, 8); // "line1\nli"
    }
}
