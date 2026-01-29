# VScript Language Server Protocol (LSP)

A high-performance Language Server Protocol implementation for the VScript programming language, built in Rust.

## Features

- 🚀 **Fast and Reliable**: Built in Rust for maximum performance
- 🎯 **Go-to Definition**: Navigate to symbol definitions across files
- 💡 **Intelligent Completion**: Context-aware code completion
- 🔍 **Hover Information**: Rich type and documentation information
- 🚨 **Real-time Diagnostics**: Syntax and semantic error detection
- 🎨 **Semantic Highlighting**: Advanced syntax highlighting
- 📁 **Workspace Management**: Full project and module support
- 🔄 **Module Resolution**: Smart import resolution with v.mod support

## Installation

### From Source

```bash
# Clone the repository
git clone https://github.com/vscript/vscript-lsp.git
cd vscript-lsp

# Build the LSP server
cargo build --release

# The binary will be available at target/release/vscript-lsp
```

### Using Cargo

```bash
cargo install vscript-lsp
```

## Usage

### Command Line

Start the LSP server:

```bash
vscript-lsp
```

The server communicates via stdin/stdout using the Language Server Protocol.

### Editor Integration

The LSP server is designed to work with any editor that supports LSP. See the [Zed extension](../zed-vscript) for a complete integration example.

#### Manual Configuration

For editors that support LSP, configure them to run `vscript-lsp` for `.vs` files.

Example configuration for various editors:

**VS Code (settings.json):**
```json
{
  "vscript.lsp.serverPath": "vscript-lsp"
}
```

**Vim/Neovim with coc.nvim:**
```json
{
  "languageserver": {
    "vscript": {
      "command": "vscript-lsp",
      "filetypes": ["vscript"]
    }
  }
}
```

## Configuration

The LSP server supports configuration through workspace settings:

```json
{
  "vscript": {
    "diagnostics": {
      "enable": true,
      "unusedVariables": "warning"
    },
    "completion": {
      "enable": true,
      "autoImport": true
    },
    "hover": {
      "enable": true,
      "showDocumentation": true
    }
  }
}
```

## VScript Language Features

### Supported Constructs

- ✅ Functions and methods
- ✅ Classes and inheritance
- ✅ Variables and parameters
- ✅ Control flow (if/else, loops)
- ✅ Arrays and objects
- ✅ String interpolation
- ✅ Async/await
- ✅ Decorators (@[decorator])
- ✅ Imports and exports
- ✅ Try/catch blocks

### Module System

The LSP understands VScript's module system:

```vscript
// Import core modules
import core:os as os
import core:json as json

// Import local files
import "./utils.vs" as utils

// Module resolution follows v.mod files
```

### Built-in Functions

The LSP provides hover information and completion for built-in functions:

- `print()`, `println()` - Output functions
- `len()`, `push()`, `pop()` - Collection functions
- `to_string()`, `to_number()` - Conversion functions
- Array methods: `map()`, `filter()`, `reduce()`, `forEach()`
- String methods: `split()`, `replace()`, `trim()`
- And many more...

## Architecture

The VScript LSP is structured as follows:

```
src/
├── main.rs          # LSP server entry point
├── analyzer.rs      # Semantic analysis engine
├── completion.rs    # Code completion provider
├── diagnostics.rs   # Error and warning detection
├── goto_definition.rs # Go-to-definition implementation
├── hover.rs         # Hover information provider
├── lexer.rs         # VScript tokenizer
├── parser.rs        # VScript parser and AST
└── workspace.rs     # Workspace and project management
```

### Key Components

- **Lexer**: Fast tokenization of VScript source code
- **Parser**: Builds Abstract Syntax Trees (AST) from tokens
- **Analyzer**: Performs semantic analysis and symbol resolution
- **Workspace Manager**: Handles project files and v.mod resolution

## Development

### Building

```bash
cargo build
```

### Testing

```bash
cargo test
```

### Debug Logging

Enable debug logging:

```bash
RUST_LOG=debug vscript-lsp
```

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Add tests for your changes
5. Run tests (`cargo test`)
6. Commit your changes (`git commit -am 'Add amazing feature'`)
7. Push to the branch (`git push origin feature/amazing-feature`)
8. Open a Pull Request

## Protocol Support

This LSP server implements the following LSP features:

- `textDocument/completion` - Code completion
- `textDocument/hover` - Hover information
- `textDocument/definition` - Go to definition
- `textDocument/references` - Find references
- `textDocument/documentSymbol` - Document outline
- `workspace/symbol` - Workspace symbol search
- `textDocument/publishDiagnostics` - Error reporting
- `textDocument/semanticTokens` - Semantic highlighting

## Performance

The VScript LSP is designed for performance:

- **Fast Parsing**: Incremental parsing with error recovery
- **Efficient Memory Usage**: Smart caching and cleanup
- **Concurrent Analysis**: Multi-threaded semantic analysis
- **Lazy Loading**: On-demand file processing

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Related Projects

- [VScript Compiler](../vscript) - The main VScript compiler
- [Zed VScript Extension](../zed-vscript) - Zed editor integration
- [Tree-sitter VScript](https://github.com/vscript/tree-sitter-vscript) - Syntax highlighting grammar

## Support

- 📖 [Documentation](https://vscript.dev/docs)
- 🐛 [Issue Tracker](https://github.com/vscript/vscript-lsp/issues)
- 💬 [Discussions](https://github.com/vscript/vscript-lsp/discussions)