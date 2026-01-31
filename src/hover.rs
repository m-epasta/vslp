// Remove unused imports
use crate::lexer::{Lexer, TokenKind};
use crate::parser::{Expression, Item, Parser, Statement};
use crate::workspace::WorkspaceManager;
use lsp_types::{Hover, HoverContents, MarkedString, Position, Range};
use std::collections::HashMap;
use text_size::TextRange;

pub async fn get_hover(
    content: &str,
    position: Position,
    workspace: &WorkspaceManager,
) -> Option<Hover> {
    let offset = position_to_offset(content, position);
    let identifier = find_identifier_at_position(content, offset)?;

    // Try to find hover information for the identifier
    if let Some(hover_info) = find_hover_info(&identifier, content, workspace).await {
        return Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(hover_info)),
            range: Some(get_identifier_range(content, offset)?),
        });
    }

    // Check for built-in functions
    if let Some(builtin_info) = get_builtin_hover(&identifier) {
        return Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(builtin_info)),
            range: Some(get_identifier_range(content, offset)?),
        });
    }

    None
}

async fn find_hover_info(
    identifier: &str,
    content: &str,
    workspace: &WorkspaceManager,
) -> Option<String> {
    // First, check the current document
    if let Some(info) = find_hover_in_document(content, identifier).await {
        return Some(info);
    }

    // Then check other documents in the workspace
    for doc in workspace.documents.values() {
        if let Some(info) = find_hover_in_document(&doc.content, identifier).await {
            return Some(info);
        }
    }

    None
}

async fn find_hover_in_document(content: &str, identifier: &str) -> Option<String> {
    let lexer = Lexer::new(content);
    let mut parser = Parser::new(lexer);
    let parse_result = parser.parse();

    if !parse_result.errors.is_empty() {
        return None;
    }

    let mut finder = HoverInfoFinder::new(identifier);
    finder.visit_source_file(&parse_result.value);

    finder.hover_info
}

fn find_identifier_at_position(content: &str, offset: usize) -> Option<String> {
    if offset >= content.len() {
        return None;
    }

    let mut lexer = Lexer::new(content);
    let tokens = lexer.tokenize();

    for token in tokens {
        let token_start = usize::from(token.range.start());
        let token_end = usize::from(token.range.end());

        if offset >= token_start && offset <= token_end && token.kind == TokenKind::Identifier {
            return Some(token.text);
        }
    }

    None
}

fn get_identifier_range(content: &str, offset: usize) -> Option<Range> {
    let mut lexer = Lexer::new(content);
    let tokens = lexer.tokenize();

    for token in tokens {
        let token_start = usize::from(token.range.start());
        let token_end = usize::from(token.range.end());

        if offset >= token_start && offset <= token_end && token.kind == TokenKind::Identifier {
            let start_pos = offset_to_position(content, token_start);
            let end_pos = offset_to_position(content, token_end);

            return Some(Range {
                start: start_pos,
                end: end_pos,
            });
        }
    }

    None
}

fn get_builtin_hover(identifier: &str) -> Option<String> {
    match identifier {
        "print" => Some(format!(
            "```vscript\nfn print(value: any) -> nil\n```\n\nPrint a value to stdout without a newline."
        )),
        "println" => Some(format!(
            "```vscript\nfn println(value: any) -> nil\n```\n\nPrint a value to stdout with a newline."
        )),
        "len" => Some(format!(
            "```vscript\nfn len(collection: array | string | object) -> number\n```\n\nGet the length of a collection (array, string, or object)."
        )),
        "push" => Some(format!(
            "```vscript\nfn push(array: array, item: any) -> nil\n```\n\nAdd an item to the end of an array."
        )),
        "pop" => Some(format!(
            "```vscript\nfn pop(array: array) -> any\n```\n\nRemove and return the last item from an array."
        )),
        "slice" => Some(format!(
            "```vscript\nfn slice(collection: array | string, start: number, end?: number) -> array | string\n```\n\nExtract a section of an array or string."
        )),
        "to_string" => Some(format!(
            "```vscript\nfn to_string(value: any) -> string\n```\n\nConvert any value to its string representation."
        )),
        "to_number" => Some(format!(
            "```vscript\nfn to_number(value: any) -> number\n```\n\nConvert a value to a number. Returns NaN if conversion fails."
        )),
        "type_of" => Some(format!(
            "```vscript\nfn type_of(value: any) -> string\n```\n\nGet the type of a value as a string."
        )),
        "is_array" => Some(format!(
            "```vscript\nfn is_array(value: any) -> boolean\n```\n\nCheck if a value is an array."
        )),
        "is_string" => Some(format!(
            "```vscript\nfn is_string(value: any) -> boolean\n```\n\nCheck if a value is a string."
        )),
        "is_number" => Some(format!(
            "```vscript\nfn is_number(value: any) -> boolean\n```\n\nCheck if a value is a number."
        )),
        "is_function" => Some(format!(
            "```vscript\nfn is_function(value: any) -> boolean\n```\n\nCheck if a value is a function."
        )),
        // Array methods
        "map" => Some(format!(
            "```vscript\nfn map(callback: function) -> array\n```\n\nCreate a new array with the results of calling a function for every array element."
        )),
        "filter" => Some(format!(
            "```vscript\nfn filter(callback: function) -> array\n```\n\nCreate a new array with all elements that pass the test implemented by the callback function."
        )),
        "reduce" => Some(format!(
            "```vscript\nfn reduce(callback: function, initial?: any) -> any\n```\n\nReduce the array to a single value by calling the callback function for each element."
        )),
        "forEach" => Some(format!(
            "```vscript\nfn forEach(callback: function) -> nil\n```\n\nExecute a function for each array element."
        )),
        "find" => Some(format!(
            "```vscript\nfn find(callback: function) -> any\n```\n\nFind the first element in the array that satisfies the callback function."
        )),
        "indexOf" => Some(format!(
            "```vscript\nfn indexOf(searchElement: any, fromIndex?: number) -> number\n```\n\nFind the first index of an element in the array, or -1 if not found."
        )),
        "join" => Some(format!(
            "```vscript\nfn join(separator?: string) -> string\n```\n\nJoin all elements of an array into a string, separated by the specified separator."
        )),
        // String methods
        "split" => Some(format!(
            "```vscript\nfn split(separator: string, limit?: number) -> array\n```\n\nSplit a string into an array of substrings."
        )),
        "replace" => Some(format!(
            "```vscript\nfn replace(search: string, replacement: string) -> string\n```\n\nReplace occurrences of a substring with another string."
        )),
        "toLowerCase" => Some(format!(
            "```vscript\nfn toLowerCase() -> string\n```\n\nConvert the string to lowercase."
        )),
        "toUpperCase" => Some(format!(
            "```vscript\nfn toUpperCase() -> string\n```\n\nConvert the string to uppercase."
        )),
        "trim" => Some(format!(
            "```vscript\nfn trim() -> string\n```\n\nRemove whitespace from both ends of the string."
        )),
        "charAt" => Some(format!(
            "```vscript\nfn charAt(index: number) -> string\n```\n\nGet the character at the specified index."
        )),
        "substring" => Some(format!(
            "```vscript\nfn substring(start: number, end?: number) -> string\n```\n\nExtract characters from the string between two indices."
        )),
        // Object methods
        "keys" => Some(format!(
            "```vscript\nfn keys() -> array\n```\n\nGet an array of the object's property names."
        )),
        "values" => Some(format!(
            "```vscript\nfn values() -> array\n```\n\nGet an array of the object's property values."
        )),
        "hasOwnProperty" => Some(format!(
            "```vscript\nfn hasOwnProperty(property: string) -> boolean\n```\n\nCheck if the object has the specified property."
        )),
        _ => None,
    }
}

struct HoverInfoFinder {
    target: String,
    hover_info: Option<String>,
    scopes: Vec<HashMap<String, SymbolInfo>>,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
struct SymbolInfo {
    name: String,
    symbol_type: SymbolType,
    signature: Option<String>,
    documentation: Option<String>,
    range: TextRange,
}

#[derive(Debug, Clone, PartialEq)]
enum SymbolType {
    Variable,
    Function,
    Class,
    Parameter,
    Method,
}

impl HoverInfoFinder {
    fn new(target: &str) -> Self {
        Self {
            target: target.to_string(),
            hover_info: None,
            scopes: vec![HashMap::new()], // Global scope
        }
    }

    fn visit_source_file(&mut self, source_file: &crate::parser::SourceFile) {
        for item in &source_file.items {
            self.visit_item(item);
            if self.hover_info.is_some() {
                break;
            }
        }

        // If not found in items, check if it's in any scope
        if self.hover_info.is_none() {
            self.check_scopes();
        }
    }

    fn visit_item(&mut self, item: &Item) {
        match item {
            Item::Function(func) => {
                let params = func
                    .params
                    .iter()
                    .map(|p| p.name.clone())
                    .collect::<Vec<_>>()
                    .join(", ");

                let signature = format!("fn {}({})", func.name.name, params);

                let info = SymbolInfo {
                    name: func.name.name.clone(),
                    symbol_type: SymbolType::Function,
                    signature: Some(signature.clone()),
                    documentation: if func.attributes.is_empty() {
                        None
                    } else {
                        Some(format!(
                            "Attributes: {:?}",
                            func.attributes
                                .iter()
                                .map(|d| &d.name.name)
                                .collect::<Vec<_>>()
                        ))
                    },
                    range: func.name.range,
                };

                if func.name.name == self.target {
                    self.hover_info = Some(self.format_symbol_info(&info));
                    return;
                }

                self.add_to_scope(&func.name.name, info);

                self.push_scope();
                for param in &func.params {
                    let param_info = SymbolInfo {
                        name: param.name.clone(),
                        symbol_type: SymbolType::Parameter,
                        signature: Some(format!("{}: parameter", param.name)),
                        documentation: None,
                        range: param.range,
                    };
                    self.add_to_scope(&param.name, param_info);
                }

                self.visit_block(&func.body);
                self.pop_scope();
            }
            Item::Class(class) => {
                let info = SymbolInfo {
                    name: class.name.name.clone(),
                    symbol_type: SymbolType::Class,
                    signature: Some(format!("class {}", class.name.name)),
                    documentation: None,
                    range: class.name.range,
                };

                if class.name.name == self.target {
                    self.hover_info = Some(self.format_symbol_info(&info));
                    return;
                }

                self.add_to_scope(&class.name.name, info);

                self.push_scope();
                for method in &class.methods {
                    let params = method
                        .params
                        .iter()
                        .map(|p| p.name.clone())
                        .collect::<Vec<_>>()
                        .join(", ");

                    let method_info = SymbolInfo {
                        name: method.name.name.clone(),
                        symbol_type: SymbolType::Method,
                        signature: Some(format!("fn {}({})", method.name.name, params)),
                        documentation: if method.name.name == "init" {
                            Some("Constructor method".to_string())
                        } else {
                            None
                        },
                        range: method.name.range,
                    };

                    if method.name.name == self.target {
                        self.hover_info = Some(self.format_symbol_info(&method_info));
                        return;
                    }

                    self.add_to_scope(&method.name.name, method_info);
                    self.visit_item(&Item::Function(method.clone()));
                    if self.hover_info.is_some() {
                        break;
                    }
                }
                self.pop_scope();
            }
            Item::VariableDeclaration(var_decl) => {
                let var_type = self.infer_variable_type(var_decl.value.as_ref());
                let info = SymbolInfo {
                    name: var_decl.name.name.clone(),
                    symbol_type: SymbolType::Variable,
                    signature: Some(format!("let {}: {}", var_decl.name.name, var_type)),
                    documentation: None,
                    range: var_decl.name.range,
                };

                if var_decl.name.name == self.target {
                    self.hover_info = Some(self.format_symbol_info(&info));
                    return;
                }

                self.add_to_scope(&var_decl.name.name, info);

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
            if self.hover_info.is_some() {
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
                let var_type = self.infer_variable_type(var_decl.value.as_ref());
                let info = SymbolInfo {
                    name: var_decl.name.name.clone(),
                    symbol_type: SymbolType::Variable,
                    signature: Some(format!("let {}: {}", var_decl.name.name, var_type)),
                    documentation: None,
                    range: var_decl.name.range,
                };

                if var_decl.name.name == self.target {
                    self.hover_info = Some(self.format_symbol_info(&info));
                    return;
                }

                self.add_to_scope(&var_decl.name.name, info);

                if let Some(ref value) = var_decl.value {
                    self.visit_expression(value);
                }
            }
            Statement::If(if_stmt) => {
                self.visit_expression(&if_stmt.condition);
                if self.hover_info.is_some() {
                    return;
                }
                self.visit_statement(&if_stmt.then_branch);
                if self.hover_info.is_some() {
                    return;
                }
                if let Some(ref else_branch) = if_stmt.else_branch {
                    self.visit_statement(else_branch);
                }
            }
            Statement::While(while_stmt) => {
                self.visit_expression(&while_stmt.condition);
                if self.hover_info.is_some() {
                    return;
                }
                self.visit_statement(&while_stmt.body);
            }
            Statement::For(for_stmt) => {
                self.push_scope();
                if let Some(ref init) = for_stmt.initializer {
                    self.visit_statement(init);
                }
                if self.hover_info.is_some() {
                    self.pop_scope();
                    return;
                }
                if let Some(ref condition) = for_stmt.condition {
                    self.visit_expression(condition);
                }
                if self.hover_info.is_some() {
                    self.pop_scope();
                    return;
                }
                if let Some(ref update) = for_stmt.increment {
                    self.visit_expression(update);
                }
                if self.hover_info.is_some() {
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
                    if let Some(info) = self.lookup_in_scopes(&id.name) {
                        self.hover_info = Some(self.format_symbol_info(info));
                    }
                }
            }
            Expression::Binary(binary) => {
                self.visit_expression(&binary.left);
                if self.hover_info.is_some() {
                    return;
                }
                self.visit_expression(&binary.right);
            }
            Expression::Unary(unary) => {
                self.visit_expression(&unary.operand);
            }
            Expression::Call(call) => {
                self.visit_expression(&call.callee);
                if self.hover_info.is_some() {
                    return;
                }
                for arg in &call.arguments {
                    self.visit_expression(arg);
                    if self.hover_info.is_some() {
                        return;
                    }
                }
            }
            Expression::Get(get) => {
                self.visit_expression(&get.object);
            }
            Expression::Set(set) => {
                self.visit_expression(&set.object);
                if self.hover_info.is_some() {
                    return;
                }
                self.visit_expression(&set.value);
            }
            Expression::Index(idx) => {
                self.visit_expression(&idx.object);
                if self.hover_info.is_some() {
                    return;
                }
                self.visit_expression(&idx.index);
            }
            Expression::AssignIndex(idx) => {
                self.visit_expression(&idx.object);
                if self.hover_info.is_some() {
                    return;
                }
                self.visit_expression(&idx.index);
                if self.hover_info.is_some() {
                    return;
                }
                self.visit_expression(&idx.value);
            }
            Expression::Assignment(assign) => {
                self.visit_expression(&assign.value);
            }
            Expression::Logical(logical) => {
                self.visit_expression(&logical.left);
                if self.hover_info.is_some() {
                    return;
                }
                self.visit_expression(&logical.right);
            }
            Expression::Array(array) => {
                for element in &array.elements {
                    self.visit_expression(element);
                    if self.hover_info.is_some() {
                        return;
                    }
                }
            }
            Expression::Map(map) => {
                for key in &map.keys {
                    self.visit_expression(key);
                    if self.hover_info.is_some() {
                        return;
                    }
                }
                for value in &map.values {
                    self.visit_expression(value);
                    if self.hover_info.is_some() {
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
                    if self.hover_info.is_some() {
                        return;
                    }
                }
            }
            Expression::Match(match_expr) => {
                self.visit_expression(&match_expr.target);
                if self.hover_info.is_some() {
                    return;
                }
                for arm in &match_expr.arms {
                    self.visit_expression(&arm.body);
                    if self.hover_info.is_some() {
                        return;
                    }
                }
            }
            Expression::Function(func_expr) => {
                self.push_scope();
                for param in &func_expr.params {
                    let param_info = SymbolInfo {
                        name: param.name.name.clone(),
                        symbol_type: SymbolType::Parameter,
                        signature: Some(format!("{}: parameter", param.name.name)),
                        documentation: None,
                        range: param.name.range,
                    };
                    self.add_to_scope(&param.name.name, param_info);
                }
                self.visit_block(&func_expr.body);
                self.pop_scope();
            }
            Expression::Await(await_expr) => {
                self.visit_expression(&await_expr.argument);
            }
            _ => {}
        }
    }

    fn infer_variable_type(&self, value: Option<&Expression>) -> String {
        match value {
            Some(Expression::Literal(literal)) => match literal.value {
                crate::parser::Literal::String(_) => "string".to_string(),
                crate::parser::Literal::Number(_) => "number".to_string(),
                crate::parser::Literal::Boolean(_) => "boolean".to_string(),
                crate::parser::Literal::Nil => "nil".to_string(),
            },
            Some(Expression::Array(_)) => "array".to_string(),
            Some(Expression::Map(_)) => "map".to_string(),
            Some(Expression::Function(_)) => "function".to_string(),
            Some(Expression::Call(_)) => "any".to_string(),
            _ => "any".to_string(),
        }
    }

    fn format_symbol_info(&self, info: &SymbolInfo) -> String {
        let mut result = String::new();

        if let Some(ref signature) = info.signature {
            result.push_str(&format!("```vscript\n{}\n```", signature));
        }

        if let Some(ref documentation) = info.documentation {
            if !result.is_empty() {
                result.push_str("\n\n");
            }
            result.push_str(documentation);
        }

        // Add symbol type information
        let type_info = match info.symbol_type {
            SymbolType::Variable => "Variable",
            SymbolType::Function => "Function",
            SymbolType::Class => "Class",
            SymbolType::Parameter => "Parameter",
            SymbolType::Method => "Method",
        };

        if result.is_empty() {
            result = format!("**{}**: {}", type_info, info.name);
        } else {
            result.push_str(&format!("\n\n**{}**", type_info));
        }

        result
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn add_to_scope(&mut self, name: &str, info: SymbolInfo) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name.to_string(), info);
        }
    }

    fn lookup_in_scopes(&self, name: &str) -> Option<&SymbolInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(name) {
                return Some(info);
            }
        }
        None
    }

    fn check_scopes(&mut self) {
        if let Some(info) = self.lookup_in_scopes(&self.target) {
            self.hover_info = Some(self.format_symbol_info(info));
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

fn offset_to_position(content: &str, offset: usize) -> Position {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_builtin_hover() {
        let hover_info = get_builtin_hover("print");
        assert!(hover_info.is_some());
        assert!(hover_info.unwrap().contains("Print a value to stdout"));
    }

    #[test]
    fn test_find_identifier_at_position() {
        let content = "let hello = world;";
        let offset = 4; // Position of 'h' in 'hello'
        let result = find_identifier_at_position(content, offset);
        assert_eq!(result, Some("hello".to_string()));
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
