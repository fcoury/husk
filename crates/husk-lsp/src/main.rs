//! Husk Language Server Protocol implementation.
//!
//! This binary provides LSP support for Husk, enabling IDE features like
//! diagnostics, hover, and go-to-definition in editors like VSCode and Neovim.

use tower_lsp::{LspService, Server};

mod backend;
mod diagnostics;
mod document;

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(backend::HuskBackend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
