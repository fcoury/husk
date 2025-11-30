import * as path from 'path';
import * as fs from 'fs';
import { workspace, ExtensionContext, window } from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export function activate(context: ExtensionContext) {
    // Determine the path to the husk-lsp binary
    const serverPath = getServerPath(context);

    if (!serverPath) {
        window.showErrorMessage(
            'Husk LSP server not found. Please install husk-lsp or configure the path.'
        );
        return;
    }

    // Server options: how to start the language server
    const serverOptions: ServerOptions = {
        run: {
            command: serverPath,
            transport: TransportKind.stdio
        },
        debug: {
            command: serverPath,
            transport: TransportKind.stdio
        }
    };

    // Client options: what documents the client is interested in
    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: 'file', language: 'husk' }
        ],
        synchronize: {
            // Notify the server about file changes to .hk files
            fileEvents: workspace.createFileSystemWatcher('**/*.hk')
        }
    };

    // Create the language client
    client = new LanguageClient(
        'huskLanguageServer',
        'Husk Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client (and server)
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

function getServerPath(context: ExtensionContext): string | undefined {
    // Check user configuration first
    const config = workspace.getConfiguration('husk');
    const configuredPath = config.get<string>('serverPath');

    if (configuredPath && configuredPath.trim() !== '') {
        if (fs.existsSync(configuredPath)) {
            return configuredPath;
        }
        window.showWarningMessage(
            `Configured husk-lsp path not found: ${configuredPath}. Falling back to bundled server.`
        );
    }

    // Check bundled server
    const bundledPath = path.join(context.extensionPath, 'server', getBinaryName());
    if (fs.existsSync(bundledPath)) {
        return bundledPath;
    }

    // Check PATH
    const pathEnv = process.env.PATH || '';
    const pathDirs = pathEnv.split(path.delimiter);

    for (const dir of pathDirs) {
        const candidate = path.join(dir, getBinaryName());
        if (fs.existsSync(candidate)) {
            return candidate;
        }
    }

    return undefined;
}

function getBinaryName(): string {
    return process.platform === 'win32' ? 'husk-lsp.exe' : 'husk-lsp';
}
