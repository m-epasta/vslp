use crate::lexer::Lexer;
use crate::parser::{Item, Parser, Statement};
use crate::workspace::WorkspaceManager;
use lsp_types::{CompletionItem, CompletionItemKind, Position};
use std::collections::HashSet;

pub async fn get_completions(
    content: &str,
    position: Position,
    workspace: &WorkspaceManager,
) -> Vec<CompletionItem> {
    let mut completions = Vec::new();

    // Get the character offset from position
    let offset = position_to_offset(content, position);
    let (prefix, context) = get_completion_context(content, offset);

    match context {
        CompletionContext::MemberAccess(object_name) => {
            completions.extend(get_member_completions(&object_name, workspace));
        }
        CompletionContext::FunctionCall => {
            completions.extend(get_function_completions(workspace).await);
        }
        CompletionContext::Variable => {
            completions.extend(get_variable_completions(workspace).await);
            completions.extend(get_keyword_completions());
            completions.extend(get_builtin_completions());
        }
        CompletionContext::Import => {
            completions.extend(get_import_completions(workspace).await);
        }
        CompletionContext::Decorator => {
            completions.extend(get_decorator_completions());
        }
        CompletionContext::Type => {
            completions.extend(get_type_completions());
        }
    }

    // Filter completions based on prefix
    if !prefix.is_empty() {
        completions.retain(|item| {
            item.label
                .to_lowercase()
                .starts_with(&prefix.to_lowercase())
        });
    }

    completions
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
enum CompletionContext {
    MemberAccess(String),
    FunctionCall,
    Variable,
    Import,
    Decorator,
    Type,
}

fn get_completion_context(content: &str, offset: usize) -> (String, CompletionContext) {
    let before_cursor = &content[..offset.min(content.len())];

    // Look for the last meaningful token before cursor
    let mut chars = before_cursor.chars().rev();
    let mut current_word = String::new();
    let in_word = true;

    // Collect the current word being typed
    for ch in &mut chars {
        if ch.is_alphanumeric() || ch == '_' {
            if in_word {
                current_word.insert(0, ch);
            }
        } else if in_word {
            if ch == '.' {
                // Member access - find the object name
                let mut object_name = String::new();
                let collecting_object = true;

                for ch in chars {
                    if ch.is_alphanumeric() || ch == '_' {
                        if collecting_object {
                            object_name.insert(0, ch);
                        }
                    } else if collecting_object && !object_name.is_empty() {
                        break;
                    } else if ch.is_whitespace() {
                        continue;
                    } else {
                        break;
                    }
                }

                return (current_word, CompletionContext::MemberAccess(object_name));
            } else if ch == '@' {
                return (current_word, CompletionContext::Decorator);
            }
            break;
        } else if !ch.is_whitespace() {
            break;
        }
    }

    // Check for import context
    if before_cursor.trim_end().ends_with("import") {
        return (current_word, CompletionContext::Import);
    }

    // Check if we're after 'fn' keyword for type annotations
    if before_cursor.contains("fn ") && before_cursor.trim_end().ends_with(":") {
        return (current_word, CompletionContext::Type);
    }

    // Default to variable/general context
    (current_word, CompletionContext::Variable)
}

fn get_member_completions(object_name: &str, _workspace: &WorkspaceManager) -> Vec<CompletionItem> {
    let mut completions = Vec::new();

    // Built-in methods for common types
    match object_name.to_lowercase().as_str() {
        "array" | "arr" => {
            completions.extend(vec![
                create_method_completion("push", "push(item)", "Add an item to the end of the array"),
                create_method_completion("pop", "pop()", "Remove and return the last item from the array"),
                create_method_completion("length", "length", "Get the length of the array"),
                create_method_completion("slice", "slice(start, end)", "Extract a section of the array"),
                create_method_completion("map", "map(callback)", "Create a new array with the results of calling a function for every array element"),
                create_method_completion("filter", "filter(callback)", "Create a new array with all elements that pass a test"),
                create_method_completion("reduce", "reduce(callback, initial)", "Reduce the array to a single value"),
                create_method_completion("forEach", "forEach(callback)", "Execute a function for each array element"),
            ]);
        }
        "string" | "str" => {
            completions.extend(vec![
                create_method_completion("length", "length", "Get the length of the string"),
                create_method_completion(
                    "slice",
                    "slice(start, end)",
                    "Extract a section of the string",
                ),
                create_method_completion(
                    "split",
                    "split(separator)",
                    "Split the string into an array",
                ),
                create_method_completion(
                    "replace",
                    "replace(search, replacement)",
                    "Replace occurrences in the string",
                ),
                create_method_completion(
                    "toLowerCase",
                    "toLowerCase()",
                    "Convert string to lowercase",
                ),
                create_method_completion(
                    "toUpperCase",
                    "toUpperCase()",
                    "Convert string to uppercase",
                ),
                create_method_completion("trim", "trim()", "Remove whitespace from both ends"),
                create_method_completion(
                    "indexOf",
                    "indexOf(searchValue)",
                    "Find the index of a substring",
                ),
            ]);
        }
        "object" | "obj" => {
            completions.extend(vec![
                create_method_completion("keys", "keys()", "Get an array of the object's keys"),
                create_method_completion(
                    "values",
                    "values()",
                    "Get an array of the object's values",
                ),
                create_method_completion(
                    "hasOwnProperty",
                    "hasOwnProperty(prop)",
                    "Check if object has a property",
                ),
            ]);
        }
        _ => {
            // Look up the object type in the workspace and provide its methods
            // This would require more sophisticated type inference
        }
    }

    completions
}

async fn get_function_completions(workspace: &WorkspaceManager) -> Vec<CompletionItem> {
    let mut completions = Vec::new();

    // Add functions from all documents in the workspace
    for doc in workspace.documents.values() {
        let lexer = Lexer::new(&doc.content);
        let mut parser = Parser::new(lexer);
        let parse_result = parser.parse();

        for item in &parse_result.value.items {
            if let Item::Function(func) = item {
                let params = func
                    .params
                    .iter()
                    .map(|p| p.name.clone())
                    .collect::<Vec<_>>()
                    .join(", ");

                completions.push(CompletionItem {
                    label: func.name.name.clone(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some(format!("fn {}({})", func.name.name, params)),
                    documentation: None,
                    insert_text: Some(format!(
                        "{}({})",
                        func.name.name,
                        if func.params.is_empty() { "" } else { "$0" }
                    )),
                    insert_text_format: Some(lsp_types::InsertTextFormat::SNIPPET),
                    ..Default::default()
                });
            }
        }
    }

    completions
}

async fn get_variable_completions(workspace: &WorkspaceManager) -> Vec<CompletionItem> {
    let mut completions = Vec::new();
    let mut seen_vars = HashSet::new();

    // Add variables from all documents in the workspace
    for doc in workspace.documents.values() {
        let lexer = Lexer::new(&doc.content);
        let mut parser = Parser::new(lexer);
        let parse_result = parser.parse();

        collect_variables_from_items(&parse_result.value.items, &mut seen_vars);
    }

    for var_name in seen_vars {
        completions.push(CompletionItem {
            label: var_name.clone(),
            kind: Some(CompletionItemKind::VARIABLE),
            detail: Some("variable".to_string()),
            ..Default::default()
        });
    }

    completions
}

fn collect_variables_from_items(items: &[Item], seen_vars: &mut HashSet<String>) {
    for item in items {
        match item {
            Item::VariableDeclaration(var_decl) => {
                seen_vars.insert(var_decl.name.name.clone());
            }
            Item::Function(func) => {
                for param in &func.params {
                    seen_vars.insert(param.name.clone());
                }
                collect_variables_from_statements(&func.body.statements, seen_vars);
            }
            Item::Class(class) => {
                for method in &class.methods {
                    for param in &method.params {
                        seen_vars.insert(param.name.clone());
                    }
                    collect_variables_from_statements(&method.body.statements, seen_vars);
                }
            }
            Item::Statement(stmt) => {
                collect_variables_from_statement(stmt, seen_vars);
            }
            _ => {}
        }
    }
}

fn collect_variables_from_statements(statements: &[Statement], seen_vars: &mut HashSet<String>) {
    for stmt in statements {
        collect_variables_from_statement(stmt, seen_vars);
    }
}

fn collect_variables_from_statement(stmt: &Statement, seen_vars: &mut HashSet<String>) {
    match stmt {
        Statement::VariableDeclaration(var_decl) => {
            seen_vars.insert(var_decl.name.name.clone());
        }
        Statement::Block(block) => {
            collect_variables_from_statements(&block.statements, seen_vars);
        }
        Statement::If(if_stmt) => {
            collect_variables_from_statement(&if_stmt.then_branch, seen_vars);
            if let Some(else_branch) = &if_stmt.else_branch {
                collect_variables_from_statement(else_branch, seen_vars);
            }
        }
        Statement::While(while_stmt) => {
            collect_variables_from_statement(&while_stmt.body, seen_vars);
        }
        Statement::For(for_stmt) => {
            if let Some(init) = &for_stmt.initializer {
                collect_variables_from_statement(init, seen_vars);
            }
            collect_variables_from_statement(&for_stmt.body, seen_vars);
        }
        _ => {}
    }
}

fn get_keyword_completions() -> Vec<CompletionItem> {
    let keywords = vec![
        ("fn", "Function declaration"),
        ("class", "Class declaration"),
        ("let", "Variable declaration"),
        ("if", "Conditional statement"),
        ("else", "Else clause"),
        ("while", "While loop"),
        ("for", "For loop"),
        ("return", "Return statement"),
        ("this", "Current instance reference"),
        ("import", "Import statement"),
        ("export", "Export statement"),
        ("async", "Async function modifier"),
        ("await", "Await expression"),
        ("try", "Try statement"),
        ("catch", "Catch clause"),
        ("true", "Boolean true"),
        ("false", "Boolean false"),
        ("nil", "Null value"),
    ];

    keywords
        .into_iter()
        .map(|(keyword, description)| CompletionItem {
            label: keyword.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(description.to_string()),
            insert_text: Some(keyword.to_string()),
            ..Default::default()
        })
        .collect()
}

fn get_builtin_completions() -> Vec<CompletionItem> {
    let builtins = vec![
        ("print", "print(value)", "Print a value to stdout"),
        (
            "println",
            "println(value)",
            "Print a value with newline to stdout",
        ),
        ("len", "len(collection)", "Get the length of a collection"),
        ("push", "push(array, item)", "Add an item to an array"),
        (
            "pop",
            "pop(array)",
            "Remove and return the last item from an array",
        ),
        (
            "slice",
            "slice(collection, start, end)",
            "Extract a section of a collection",
        ),
        ("to_string", "to_string(value)", "Convert a value to string"),
        ("to_number", "to_number(value)", "Convert a value to number"),
        ("type_of", "type_of(value)", "Get the type of a value"),
        ("is_array", "is_array(value)", "Check if value is an array"),
        (
            "is_string",
            "is_string(value)",
            "Check if value is a string",
        ),
        (
            "is_number",
            "is_number(value)",
            "Check if value is a number",
        ),
        (
            "is_function",
            "is_function(value)",
            "Check if value is a function",
        ),
        ("clock", "clock()", "Get the current time in seconds"),
    ];

    builtins
        .into_iter()
        .map(|(name, signature, description)| CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some(signature.to_string()),
            documentation: Some(lsp_types::Documentation::String(description.to_string())),
            insert_text: Some(format!("{}($0)", name)),
            insert_text_format: Some(lsp_types::InsertTextFormat::SNIPPET),
            ..Default::default()
        })
        .collect()
}

async fn get_import_completions(workspace: &WorkspaceManager) -> Vec<CompletionItem> {
    let mut completions = Vec::new();

    // Core modules
    let core_modules = vec![
        ("core:os", "Operating system utilities"),
        ("core:fs", "File system operations"),
        ("core:json", "JSON parsing and serialization"),
        ("core:http", "HTTP client utilities"),
        ("core:math", "Mathematical functions"),
        ("core:time", "Time and date utilities"),
        ("core:crypto", "Cryptographic functions"),
    ];

    for (module, description) in core_modules {
        completions.push(CompletionItem {
            label: module.to_string(),
            kind: Some(CompletionItemKind::MODULE),
            detail: Some(description.to_string()),
            insert_text: Some(format!("{} as ", module)),
            insert_text_format: Some(lsp_types::InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    }

    // Available modules from v.mod dependencies
    for v_mod in workspace.get_all_v_mods() {
        for dep in &v_mod.dependencies {
            completions.push(CompletionItem {
                label: dep.clone(),
                kind: Some(CompletionItemKind::MODULE),
                detail: Some("External dependency".to_string()),
                ..Default::default()
            });
        }
    }

    // Local files
    for doc in workspace.documents.values() {
        if let Ok(path) = doc.uri.to_file_path() {
            if let Some(file_stem) = path.file_stem().and_then(|s| s.to_str()) {
                if file_stem != "main" && file_stem != "index" {
                    completions.push(CompletionItem {
                        label: format!("./{}.vs", file_stem),
                        kind: Some(CompletionItemKind::FILE),
                        detail: Some("Local file".to_string()),
                        insert_text: Some(format!("./{}", file_stem)),
                        ..Default::default()
                    });
                }
            }
        }
    }

    completions
}

fn get_decorator_completions() -> Vec<CompletionItem> {
    let decorators = vec![
        ("cached", "Cache function results"),
        ("memoize", "Memoize function calls"),
        ("debug", "Add debug logging"),
        ("time", "Measure execution time"),
        ("validate", "Validate function arguments"),
        ("deprecated", "Mark as deprecated"),
        ("async", "Make function asynchronous"),
        ("pure", "Mark function as pure"),
    ];

    decorators
        .into_iter()
        .map(|(name, description)| CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::TEXT),
            detail: Some(description.to_string()),
            insert_text: Some(format!("@[{}]", name)),
            ..Default::default()
        })
        .collect()
}

fn get_type_completions() -> Vec<CompletionItem> {
    let types = vec![
        "string", "number", "boolean", "array", "object", "function", "nil",
    ];

    types
        .into_iter()
        .map(|type_name| CompletionItem {
            label: type_name.to_string(),
            kind: Some(CompletionItemKind::TYPE_PARAMETER),
            detail: Some("Type".to_string()),
            ..Default::default()
        })
        .collect()
}

fn create_method_completion(name: &str, signature: &str, description: &str) -> CompletionItem {
    CompletionItem {
        label: name.to_string(),
        kind: Some(CompletionItemKind::METHOD),
        detail: Some(signature.to_string()),
        documentation: Some(lsp_types::Documentation::String(description.to_string())),
        insert_text: Some(if signature.contains('(') {
            format!("{}($0)", name)
        } else {
            name.to_string()
        }),
        insert_text_format: Some(lsp_types::InsertTextFormat::SNIPPET),
        ..Default::default()
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
    fn test_completion_context() {
        let content = "obj.";
        let offset = content.len();
        let (_prefix, context) = get_completion_context(content, offset);

        match context {
            CompletionContext::MemberAccess(obj) => assert_eq!(obj, "obj"),
            _ => panic!("Expected MemberAccess context"),
        }
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
