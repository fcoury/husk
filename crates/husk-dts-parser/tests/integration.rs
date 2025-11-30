//! Integration tests for husk-dts-parser using real-world .d.ts samples.

use husk_dts_parser::{parse, generate, CodegenOptions};

/// Test parsing a simple hand-crafted .d.ts file.
#[test]
fn test_simple_express_like() {
    let dts = r#"
        export interface Request {
            url: string;
            method: string;
            params: Record<string, string>;
        }

        export interface Response {
            status(code: number): Response;
            send(body: string): Response;
            json(data: any): Response;
        }

        export type RequestHandler = (req: Request, res: Response) => void;

        export function createApp(): Application;

        export interface Application {
            get(path: string, handler: RequestHandler): void;
            post(path: string, handler: RequestHandler): void;
            listen(port: number, callback?: () => void): void;
        }
    "#;

    let file = parse(dts).expect("Failed to parse express-like .d.ts");

    // Verify we parsed the expected items
    assert!(file.items.len() >= 4, "Expected at least 4 items");

    // Generate Husk code
    let result = generate(&file, &CodegenOptions {
        module_name: Some("express".to_string()),
        verbose: false,
    });

    // Check generated code
    assert!(result.code.contains("mod express;"), "Should include module import");
    assert!(result.code.contains("struct Request;"), "Should include Request struct");
    assert!(result.code.contains("struct Response;"), "Should include Response struct");
    assert!(result.code.contains("struct Application;"), "Should include Application struct");
    assert!(result.code.contains("fn createApp() -> Application;"), "Should include createApp function");

    // Check impl blocks
    assert!(result.code.contains("impl Response {"), "Should include Response impl");
    assert!(result.code.contains("impl Application {"), "Should include Application impl");
}

/// Test parsing a node path-like module.
#[test]
fn test_node_path_like() {
    let dts = r#"
        interface ParsedPath {
            root: string;
            dir: string;
            base: string;
            ext: string;
            name: string;
        }

        interface Path {
            normalize(path: string): string;
            join(...paths: string[]): string;
            resolve(...paths: string[]): string;
            isAbsolute(path: string): boolean;
            relative(from: string, to: string): string;
            dirname(path: string): string;
            basename(path: string, suffix?: string): string;
            extname(path: string): string;
            parse(path: string): ParsedPath;
        }

        declare const path: Path;
        export = path;
    "#;

    let file = parse(dts).expect("Failed to parse path-like .d.ts");
    let result = generate(&file, &CodegenOptions::default());

    // Check interfaces become structs with impl blocks
    assert!(result.code.contains("struct ParsedPath;"));
    assert!(result.code.contains("struct Path;"));
    assert!(result.code.contains("impl Path {"));

    // Check methods
    assert!(result.code.contains("fn normalize(self, path: String) -> String;"));
    assert!(result.code.contains("fn join(self, paths: JsArray<String>) -> String;"));
    assert!(result.code.contains("fn isAbsolute(self, path: String) -> bool;"));
    assert!(result.code.contains("fn basename(self, path: String, suffix: Option<String>) -> String;"));
}

/// Test parsing Promise-based async APIs.
#[test]
fn test_async_api() {
    let dts = r#"
        interface ReadResult {
            bytesRead: number;
            buffer: Buffer;
        }

        interface FileHandle {
            read(buffer: Buffer, offset: number, length: number): Promise<ReadResult>;
            write(data: string | Buffer): Promise<void>;
            close(): Promise<void>;
        }

        declare function open(path: string, mode?: string): Promise<FileHandle>;
    "#;

    let file = parse(dts).expect("Failed to parse async .d.ts");
    let result = generate(&file, &CodegenOptions::default());

    // Check Promise handling
    assert!(result.code.contains("fn open(path: String, mode: Option<String>) -> JsPromise<FileHandle>;"));
    assert!(result.code.contains("fn read(self, buffer: Buffer, offset: f64, length: f64) -> JsPromise<ReadResult>;"));
    assert!(result.code.contains("fn close(self) -> JsPromise<()>;"));
}

/// Test parsing callback-based APIs.
#[test]
fn test_callback_api() {
    let dts = r#"
        declare function setTimeout(callback: () => void, ms: number): number;
        declare function setInterval(callback: () => void, ms: number): number;
        declare function clearTimeout(id: number): void;
        declare function clearInterval(id: number): void;

        interface EventEmitter {
            on(event: string, listener: (data: any) => void): void;
            once(event: string, listener: (data: any) => void): void;
            emit(event: string, ...args: any[]): boolean;
            removeListener(event: string, listener: (data: any) => void): void;
        }
    "#;

    let file = parse(dts).expect("Failed to parse callback .d.ts");
    let result = generate(&file, &CodegenOptions::default());

    // Check callback functions
    assert!(result.code.contains("fn setTimeout(callback: fn() -> (), ms: f64) -> f64;"));
    assert!(result.code.contains("fn setInterval(callback: fn() -> (), ms: f64) -> f64;"));

    // Check methods with callbacks - these have parameters so use JsFn
    assert!(result.code.contains("fn on(self, event: String, listener: fn(JsValue) -> ());"));
}

/// Test parsing generic types.
#[test]
fn test_generic_types() {
    let dts = r#"
        declare function identity<T>(x: T): T;
        declare function map<T, U>(arr: T[], fn: (item: T) => U): U[];
        declare function filter<T>(arr: T[], predicate: (item: T) => boolean): T[];

        interface Box<T> {
            value: T;
            map<U>(fn: (x: T) => U): Box<U>;
            flatMap<U>(fn: (x: T) => Box<U>): Box<U>;
        }
    "#;

    let file = parse(dts).expect("Failed to parse generic .d.ts");
    let result = generate(&file, &CodegenOptions::default());

    // Check generic functions
    assert!(result.code.contains("fn identity<T>(x: T) -> T;"));
    assert!(result.code.contains("fn map<T, U>(arr: JsArray<T>, fn_: fn(T) -> U) -> JsArray<U>;"));
    assert!(result.code.contains("fn filter<T>(arr: JsArray<T>, predicate: fn(T) -> bool) -> JsArray<T>;"));

    // Check generic interface
    assert!(result.code.contains("struct Box;"));
}

/// Test parsing optional and nullable types.
#[test]
fn test_optional_nullable() {
    let dts = r#"
        declare function find(id: string): User | null;
        declare function findOrCreate(id: string): User | undefined;
        declare function process(data?: string): void;

        interface Config {
            host: string;
            port?: number;
            timeout?: number | null;
        }
    "#;

    let file = parse(dts).expect("Failed to parse optional .d.ts");
    let result = generate(&file, &CodegenOptions::default());

    // Check nullable returns become Option
    assert!(result.code.contains("fn find(id: String) -> Option<User>;"));
    assert!(result.code.contains("fn findOrCreate(id: String) -> Option<User>;"));

    // Check optional params
    assert!(result.code.contains("fn process(data: Option<String>);"));
}

/// Test parsing classes with constructors.
#[test]
fn test_class_with_constructor() {
    let dts = r#"
        declare class Buffer {
            constructor(size: number);
            constructor(data: string, encoding?: string);

            length: number;
            toString(encoding?: string): string;
            slice(start?: number, end?: number): Buffer;

            static alloc(size: number): Buffer;
            static from(data: string, encoding?: string): Buffer;
        }
    "#;

    let file = parse(dts).expect("Failed to parse class .d.ts");
    let result = generate(&file, &CodegenOptions::default());

    // Check struct and impl
    assert!(result.code.contains("struct Buffer;"));
    assert!(result.code.contains("impl Buffer {"));

    // Check methods
    assert!(result.code.contains("fn toString(self, encoding: Option<String>) -> String;"));
    assert!(result.code.contains("fn slice(self, start: Option<f64>, end: Option<f64>) -> Buffer;"));
}

/// Test that reserved keywords are escaped.
#[test]
fn test_keyword_escaping() {
    let dts = r#"
        declare function type(mod: string): void;
        declare function struct(fn: number): boolean;

        interface Match {
            match(pattern: string): boolean;
        }
    "#;

    let file = parse(dts).expect("Failed to parse keyword .d.ts");
    let result = generate(&file, &CodegenOptions::default());

    // Check escaped function names
    assert!(result.code.contains("fn type_(mod_: String);"));
    assert!(result.code.contains("fn struct_(fn_: f64) -> bool;"));

    // Check escaped method names
    assert!(result.code.contains("fn match_(self, pattern: String) -> bool;"));
}

/// Test module name handling.
#[test]
fn test_module_name_variants() {
    let dts = "declare function init(): void;";
    let file = parse(dts).expect("Failed to parse");

    // Simple module name
    let result = generate(&file, &CodegenOptions {
        module_name: Some("express".to_string()),
        verbose: false,
    });
    assert!(result.code.contains("mod express;"));

    // Hyphenated module name
    let result = generate(&file, &CodegenOptions {
        module_name: Some("lodash-es".to_string()),
        verbose: false,
    });
    assert!(result.code.contains(r#"mod "lodash-es" as lodash_es;"#));

    // Scoped module name
    let result = generate(&file, &CodegenOptions {
        module_name: Some("@types/node".to_string()),
        verbose: false,
    });
    assert!(result.code.contains(r#"mod "@types/node" as node;"#));
}
