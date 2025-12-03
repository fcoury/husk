//! Husk Language Server Protocol implementation.
//!
//! This binary provides LSP support for Husk, enabling IDE features like
//! diagnostics, hover, and go-to-definition in editors like VSCode and Neovim.
//!
//! # Enabling Debug Logging
//!
//! Set the `HUSK_LSP_LOG` environment variable to enable tracing:
//!
//! ```bash
//! HUSK_LSP_LOG=debug husk-lsp
//! ```
//!
//! Log levels: error, warn, info, debug, trace

use tower_lsp::{LspService, Server};
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

mod backend;
mod diagnostics;
mod document;

fn init_tracing() {
    // Use HUSK_LSP_LOG env var, defaulting to "info" if not set
    let filter = EnvFilter::try_from_env("HUSK_LSP_LOG")
        .unwrap_or_else(|_| EnvFilter::new("info"));

    tracing_subscriber::registry()
        .with(filter)
        .with(
            fmt::layer()
                .with_writer(std::io::stderr)
                .with_ansi(false)
                .with_target(true)
                .with_level(true),
        )
        .init();
}

#[tokio::main]
async fn main() {
    init_tracing();

    tracing::info!("Starting husk-lsp v{}", env!("CARGO_PKG_VERSION"));

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(backend::HuskBackend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
