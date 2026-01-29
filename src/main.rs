use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

mod analyzer;
mod completion;
mod diagnostics;
mod goto_definition;
mod hover;
mod lexer;
mod parser;
mod workspace;

use crate::analyzer::VScriptAnalyzer;
use crate::workspace::WorkspaceManager;
use dashmap::DashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use url::Url;

#[derive(Debug)]
struct VScriptLanguageServer {
    client: Client,
    workspace: Arc<RwLock<WorkspaceManager>>,
    documents: Arc<DashMap<Url, String>>,
    analyzer: Arc<VScriptAnalyzer>,
}

impl VScriptLanguageServer {
    fn new(client: Client) -> Self {
        Self {
            client,
            workspace: Arc::new(RwLock::new(WorkspaceManager::new())),
            documents: Arc::new(DashMap::new()),
            analyzer: Arc::new(VScriptAnalyzer::new()),
        }
    }

    async fn publish_diagnostics(&self, uri: Url, content: &str) {
        let diagnostics = self.analyzer.get_diagnostics(content, &uri).await;

        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for VScriptLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        tracing::info!("VScript LSP server initializing...");

        if let Some(workspace_folders) = params.workspace_folders {
            let mut workspace = self.workspace.write().await;
            for folder in workspace_folders {
                workspace.add_folder(folder.uri.clone()).await;
            }
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(true),
                    trigger_characters: Some(vec![".".to_string(), "@".to_string()]),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: SemanticTokensLegend {
                                token_types: vec![
                                    SemanticTokenType::KEYWORD,
                                    SemanticTokenType::STRING,
                                    SemanticTokenType::COMMENT,
                                    SemanticTokenType::NUMBER,
                                    SemanticTokenType::FUNCTION,
                                    SemanticTokenType::CLASS,
                                    SemanticTokenType::VARIABLE,
                                    SemanticTokenType::PARAMETER,
                                    SemanticTokenType::PROPERTY,
                                    SemanticTokenType::METHOD,
                                    SemanticTokenType::TYPE,
                                    SemanticTokenType::OPERATOR,
                                ],
                                token_modifiers: vec![
                                    SemanticTokenModifier::DECLARATION,
                                    SemanticTokenModifier::DEFINITION,
                                    SemanticTokenModifier::READONLY,
                                    SemanticTokenModifier::STATIC,
                                    SemanticTokenModifier::ASYNC,
                                ],
                            },
                            range: Some(true),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            ..Default::default()
                        },
                    ),
                ),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "vscript-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        tracing::info!("VScript LSP server initialized");

        self.client
            .log_message(MessageType::INFO, "VScript LSP server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        tracing::info!("VScript LSP server shutting down");
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let content = params.text_document.text.clone();

        self.documents.insert(uri.clone(), content.clone());

        // Update workspace with new document
        {
            let mut workspace = self.workspace.write().await;
            workspace.add_document(uri.clone(), &content).await;
        }

        self.publish_diagnostics(uri, &content).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();

        if let Some(change) = params.content_changes.into_iter().next() {
            let content = change.text;
            self.documents.insert(uri.clone(), content.clone());

            // Update workspace
            {
                let mut workspace = self.workspace.write().await;
                workspace.update_document(uri.clone(), &content).await;
            }

            self.publish_diagnostics(uri, &content).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents.remove(&params.text_document.uri);
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        if let Some(content) = self.documents.get(&uri) {
            let workspace = self.workspace.read().await;
            let completions = completion::get_completions(&content, position, &workspace).await;
            return Ok(Some(CompletionResponse::Array(completions)));
        }

        Ok(None)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        if let Some(content) = self.documents.get(&uri) {
            let workspace = self.workspace.read().await;
            return Ok(hover::get_hover(&content, position, &workspace).await);
        }

        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        if let Some(content) = self.documents.get(&uri) {
            let workspace = self.workspace.read().await;
            let definitions =
                goto_definition::get_definitions(&content, position, &workspace, &uri).await;
            return Ok(definitions.map(GotoDefinitionResponse::Array));
        }

        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        if let Some(content) = self.documents.get(&uri) {
            let workspace = self.workspace.read().await;
            return Ok(goto_definition::get_references(&content, position, &workspace, &uri).await);
        }

        Ok(None)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;

        if let Some(content) = self.documents.get(&uri) {
            let symbols = self.analyzer.get_document_symbols(&content, &uri).await;
            return Ok(Some(DocumentSymbolResponse::Nested(symbols)));
        }

        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;

        if let Some(content) = self.documents.get(&uri) {
            let tokens = self.analyzer.get_semantic_tokens(&content).await;
            return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: tokens,
            })));
        }

        Ok(None)
    }
}

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| VScriptLanguageServer::new(client)).finish();

    tracing::info!("Starting VScript LSP server");
    Server::new(stdin, stdout, socket).serve(service).await;
}
