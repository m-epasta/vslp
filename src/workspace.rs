#![allow(dead_code)]

use anyhow::Result;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use tokio::fs;
use url::Url;
use walkdir::WalkDir;

#[derive(Debug, Clone)]
pub struct VModFile {
    pub name: String,
    pub version: String,
    pub description: Option<String>,
    pub dependencies: Vec<String>,
    pub path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct WorkspaceDocument {
    pub uri: Url,
    pub content: String,
    pub version: i32,
}

#[derive(Debug)]
pub struct WorkspaceManager {
    pub root_folders: Vec<Url>,
    pub documents: HashMap<Url, WorkspaceDocument>,
    pub v_mod_files: HashMap<PathBuf, VModFile>,
    pub symbols: HashMap<String, Vec<crate::analyzer::Symbol>>,
}

impl WorkspaceManager {
    pub fn new() -> Self {
        Self {
            root_folders: Vec::new(),
            documents: HashMap::new(),
            v_mod_files: HashMap::new(),
            symbols: HashMap::new(),
        }
    }

    pub async fn add_folder(&mut self, uri: Url) {
        tracing::info!("Adding workspace folder: {}", uri);

        if !self.root_folders.contains(&uri) {
            self.root_folders.push(uri.clone());
            self.scan_workspace_folder(&uri).await;
        }
    }

    async fn scan_workspace_folder(&mut self, uri: &Url) {
        if let Ok(path) = uri.to_file_path() {
            self.scan_for_v_mod_files(&path).await;
            self.scan_for_vscript_files(&path).await;
        }
    }

    async fn scan_for_v_mod_files(&mut self, root_path: &Path) {
        for entry in WalkDir::new(root_path)
            .follow_links(true)
            .into_iter()
            .filter_map(|e| e.ok())
        {
            let path = entry.path();
            if path.file_name().and_then(|n| n.to_str()) == Some("v.mod") {
                if let Ok(v_mod) = self.parse_v_mod_file(path).await {
                    self.v_mod_files.insert(path.to_path_buf(), v_mod);
                }
            }
        }
    }

    async fn scan_for_vscript_files(&mut self, root_path: &Path) {
        for entry in WalkDir::new(root_path)
            .follow_links(true)
            .into_iter()
            .filter_map(|e| e.ok())
        {
            let path = entry.path();
            if let Some(extension) = path.extension() {
                if extension == "vs" {
                    if let Ok(uri) = Url::from_file_path(path) {
                        if let Ok(content) = fs::read_to_string(path).await {
                            self.documents.insert(
                                uri.clone(),
                                WorkspaceDocument {
                                    uri: uri.clone(),
                                    content,
                                    version: 1,
                                },
                            );
                        }
                    }
                }
            }
        }
    }

    async fn parse_v_mod_file(&self, path: &Path) -> Result<VModFile> {
        let content = fs::read_to_string(path).await?;

        // Simple v.mod parser - this is a basic implementation
        // In a real implementation, you'd want a proper parser
        let mut name = String::new();
        let mut version = String::new();
        let mut description = None;
        let mut dependencies = Vec::new();

        for line in content.lines() {
            let line = line.trim();
            if line.starts_with("name:") {
                name = line
                    .split(':')
                    .nth(1)
                    .unwrap_or("")
                    .trim()
                    .trim_matches('\'')
                    .trim_matches('"')
                    .to_string();
            } else if line.starts_with("version:") {
                version = line
                    .split(':')
                    .nth(1)
                    .unwrap_or("")
                    .trim()
                    .trim_matches('\'')
                    .trim_matches('"')
                    .to_string();
            } else if line.starts_with("description:") {
                description = Some(
                    line.split(':')
                        .nth(1)
                        .unwrap_or("")
                        .trim()
                        .trim_matches('\'')
                        .trim_matches('"')
                        .to_string(),
                );
            } else if line.starts_with("dependencies:") {
                // Parse dependencies array - simplified
                if line.contains('[') && line.contains(']') {
                    let deps_str = line
                        .split('[')
                        .nth(1)
                        .unwrap_or("")
                        .split(']')
                        .next()
                        .unwrap_or("");
                    for dep in deps_str.split(',') {
                        let dep = dep.trim().trim_matches('\'').trim_matches('"');
                        if !dep.is_empty() {
                            dependencies.push(dep.to_string());
                        }
                    }
                }
            }
        }

        Ok(VModFile {
            name,
            version,
            description,
            dependencies,
            path: path.to_path_buf(),
        })
    }

    pub async fn add_document(&mut self, uri: Url, content: &str) {
        let doc = WorkspaceDocument {
            uri: uri.clone(),
            content: content.to_string(),
            version: 1,
        };
        self.documents.insert(uri, doc);
    }

    pub async fn update_document(&mut self, uri: Url, content: &str) {
        if let Some(doc) = self.documents.get_mut(&uri) {
            doc.content = content.to_string();
            doc.version += 1;
        } else {
            self.add_document(uri, content).await;
        }
    }

    pub fn get_document(&self, uri: &Url) -> Option<&WorkspaceDocument> {
        self.documents.get(uri)
    }

    pub fn find_v_mod_for_path(&self, file_path: &Path) -> Option<&VModFile> {
        // Find the closest v.mod file by traversing up the directory tree
        let mut current = file_path.parent();

        while let Some(dir) = current {
            let v_mod_path = dir.join("v.mod");
            if let Some(v_mod) = self.v_mod_files.get(&v_mod_path) {
                return Some(v_mod);
            }
            current = dir.parent();
        }

        None
    }

    pub fn get_all_v_mods(&self) -> Vec<&VModFile> {
        self.v_mod_files.values().collect()
    }

    pub fn resolve_import(&self, import_path: &str, current_file: &Url) -> Option<Url> {
        // Handle different import types
        if import_path.starts_with("core:") {
            // Built-in core library import
            return self.resolve_core_import(import_path);
        }

        if import_path.starts_with("./") || import_path.starts_with("../") {
            // Relative import
            return self.resolve_relative_import(import_path, current_file);
        }

        if import_path.contains(':') {
            // Module import (e.g., "module:submodule")
            return self.resolve_module_import(import_path);
        }

        // Default to relative import
        self.resolve_relative_import(import_path, current_file)
    }

    fn resolve_core_import(&self, import_path: &str) -> Option<Url> {
        // Core library imports are built-in, return a special URL
        Some(Url::parse(&format!("builtin://{}", import_path)).ok()?)
    }

    fn resolve_relative_import(&self, import_path: &str, current_file: &Url) -> Option<Url> {
        if let Ok(current_path) = current_file.to_file_path() {
            if let Some(current_dir) = current_path.parent() {
                let resolved_path = current_dir.join(import_path);

                // Try with .vs extension if not present
                let resolved_path = if resolved_path.extension().is_none() {
                    resolved_path.with_extension("vs")
                } else {
                    resolved_path
                };

                if resolved_path.exists() {
                    return Url::from_file_path(resolved_path).ok();
                }
            }
        }
        None
    }

    fn resolve_module_import(&self, import_path: &str) -> Option<Url> {
        let parts: Vec<&str> = import_path.split(':').collect();
        if parts.len() < 2 {
            return None;
        }

        let module_name = parts[0];
        let submodule = parts[1];

        // Look for the module in v.mod dependencies
        for v_mod in self.v_mod_files.values() {
            if v_mod
                .dependencies
                .iter()
                .any(|dep| dep.contains(module_name))
            {
                // In a real implementation, you'd resolve the dependency path
                // For now, return a placeholder
                return Some(Url::parse(&format!("module://{}/{}", module_name, submodule)).ok()?);
            }
        }

        None
    }

    pub async fn reload(&mut self) {
        tracing::info!("Reloading workspace");

        // Clear current state
        self.documents.clear();
        self.v_mod_files.clear();
        self.symbols.clear();

        // Rescan all folders
        let folders = self.root_folders.clone();
        for folder in folders {
            self.scan_workspace_folder(&folder).await;
        }
    }

    pub fn get_workspace_symbols(&self, query: &str) -> Vec<crate::analyzer::Symbol> {
        let mut results = Vec::new();
        let query_lower = query.to_lowercase();

        for symbols in self.symbols.values() {
            for symbol in symbols {
                if symbol.name.to_lowercase().contains(&query_lower) {
                    results.push(symbol.clone());
                }
            }
        }

        results
    }

    pub fn add_symbols(&mut self, uri: &Url, symbols: Vec<crate::analyzer::Symbol>) {
        self.symbols.insert(uri.to_string(), symbols);
    }

    pub fn get_file_symbols(&self, uri: &Url) -> Option<&Vec<crate::analyzer::Symbol>> {
        self.symbols.get(&uri.to_string())
    }

    pub fn is_vscript_file(&self, uri: &Url) -> bool {
        if let Ok(path) = uri.to_file_path() {
            if let Some(extension) = path.extension() {
                return extension == "vs";
            }
        }
        false
    }

    pub fn get_project_root(&self, file_uri: &Url) -> Option<&Url> {
        if let Ok(file_path) = file_uri.to_file_path() {
            for root in &self.root_folders {
                if let Ok(root_path) = root.to_file_path() {
                    if file_path.starts_with(&root_path) {
                        return Some(root);
                    }
                }
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;
    use tokio::fs;

    #[tokio::test]
    async fn test_v_mod_parsing() {
        let dir = tempdir().unwrap();
        let v_mod_path = dir.path().join("v.mod");

        let v_mod_content = r#"Module {
    name: 'test_project'
    description: 'A test VScript project'
    version: '0.1.0'
    license: 'MIT'
    dependencies: []
}"#;

        fs::write(&v_mod_path, v_mod_content).await.unwrap();

        let workspace = WorkspaceManager::new();
        let v_mod = workspace.parse_v_mod_file(&v_mod_path).await.unwrap();

        assert_eq!(v_mod.name, "test_project");
        assert_eq!(v_mod.version, "0.1.0");
    }

    #[tokio::test]
    async fn test_workspace_scanning() {
        let dir = tempdir().unwrap();
        let vs_file = dir.path().join("test.vs");
        fs::write(&vs_file, "fn main() { print('hello'); }")
            .await
            .unwrap();

        let mut workspace = WorkspaceManager::new();
        let uri = Url::from_file_path(dir.path()).unwrap();

        workspace.add_folder(uri).await;

        let vs_uri = Url::from_file_path(&vs_file).unwrap();
        assert!(workspace.get_document(&vs_uri).is_some());
    }
}
