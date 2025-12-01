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

/// Test property getters and setters for interfaces.
#[test]
fn test_property_getters_and_setters() {
    let dts = r#"
        interface Config {
            host: string;
            port?: number;
            readonly version: string;
        }
    "#;

    let file = parse(dts).expect("Failed to parse");
    let result = generate(&file, &CodegenOptions::default());

    // Required property - getter and setter
    assert!(result.code.contains("fn host(self) -> String;"),
        "Should generate host getter");
    assert!(result.code.contains("fn set_host(self, value: String);"),
        "Should generate host setter");

    // Optional property - getter and setter with Option
    assert!(result.code.contains("fn port(self) -> Option<f64>;"),
        "Should generate optional port getter");
    assert!(result.code.contains("fn set_port(self, value: Option<f64>);"),
        "Should generate optional port setter");

    // Readonly property - getter only, NO setter
    assert!(result.code.contains("fn version(self) -> String;"),
        "Should generate version getter");
    assert!(!result.code.contains("fn set_version"),
        "Readonly property should not have setter");
}

/// Test class properties generate getters and setters.
#[test]
fn test_class_properties_with_setters() {
    let dts = r#"
        declare class Buffer {
            readonly length: number;
            static BYTES_PER_ELEMENT: number;
            data: Uint8Array;
            private _internal: any;
        }
    "#;

    let file = parse(dts).expect("Failed to parse");
    let result = generate(&file, &CodegenOptions::default());

    // Instance readonly property - getter only
    assert!(result.code.contains("fn length(self) -> f64;"),
        "Should generate length getter");
    assert!(!result.code.contains("fn set_length"),
        "Readonly property should not have setter");

    // Static property - getter and setter (no self parameter)
    assert!(result.code.contains("fn BYTES_PER_ELEMENT() -> f64;"),
        "Should generate static getter without self");
    assert!(result.code.contains("fn set_BYTES_PER_ELEMENT(value: f64);"),
        "Should generate static setter without self");

    // Mutable instance property - getter and setter
    assert!(result.code.contains("fn data(self) -> Uint8Array;"),
        "Should generate data getter");
    assert!(result.code.contains("fn set_data(self, value: Uint8Array);"),
        "Should generate data setter");

    // Private property should be skipped entirely
    assert!(!result.code.contains("_internal"),
        "Private properties should be skipped");
}

/// Test that properties in existing tests now generate getters.
#[test]
fn test_interface_properties_generate_getters() {
    let dts = r#"
        interface Request {
            url: string;
            method: string;
            body: any;
            readonly headers: Headers;
        }
    "#;

    let file = parse(dts).expect("Failed to parse");
    let result = generate(&file, &CodegenOptions::default());

    // Check property getters are generated
    assert!(result.code.contains("fn url(self) -> String;"),
        "Should generate url getter");
    assert!(result.code.contains("fn method(self) -> String;"),
        "Should generate method getter");
    assert!(result.code.contains("fn body(self) -> JsValue;"),
        "Should generate body getter with JsValue for any");
    assert!(result.code.contains("fn headers(self) -> Headers;"),
        "Should generate readonly headers getter");

    // Check setters for non-readonly properties
    assert!(result.code.contains("fn set_url(self, value: String);"),
        "Should generate url setter");
    assert!(result.code.contains("fn set_method(self, value: String);"),
        "Should generate method setter");
    assert!(result.code.contains("fn set_body(self, value: JsValue);"),
        "Should generate body setter");

    // Readonly should not have setter
    assert!(!result.code.contains("fn set_headers"),
        "Readonly headers should not have setter");
}

/// Test parsing interface with keyword property names.
#[test]
fn test_keyword_as_property_name() {
    let dts = r#"
        interface Options {
            static: boolean;
            readonly: string;
            public: number;
            private: boolean;
            class: string;
            function: () => void;
            new: string;
            default: boolean;
        }
    "#;

    let file = parse(dts).expect("Should parse keywords as property names");

    // Verify all properties are present
    assert_eq!(file.items.len(), 1);

    let result = generate(&file, &CodegenOptions::default());

    // Keywords should be escaped in generated code
    assert!(result.code.contains("fn static_(self) -> bool;"),
        "Should generate escaped static getter");
    assert!(result.code.contains("fn readonly_(self) -> String;"),
        "Should generate escaped readonly getter");
    assert!(result.code.contains("fn public_(self) -> f64;"),
        "Should generate escaped public getter");
    assert!(result.code.contains("fn private_(self) -> bool;"),
        "Should generate escaped private getter");
    assert!(result.code.contains("fn class_(self) -> String;"),
        "Should generate escaped class getter");
    assert!(result.code.contains("fn new_(self) -> String;"),
        "Should generate escaped new getter");
    assert!(result.code.contains("fn default_(self) -> bool;"),
        "Should generate escaped default getter");
}

/// Test disambiguating modifiers from property names.
#[test]
fn test_modifier_vs_property_name() {
    let dts = r#"
        interface Mixed {
            readonly: boolean;
            readonly name: string;
            readonly readonly: boolean;
        }
    "#;

    let file = parse(dts).expect("Should disambiguate modifiers");

    let result = generate(&file, &CodegenOptions::default());

    // "readonly" alone is a property named "readonly"
    assert!(result.code.contains("fn readonly_(self) -> bool;"),
        "Should have readonly_ getter for property named readonly");

    // "readonly name" means "name" is the property with readonly modifier
    assert!(result.code.contains("fn name(self) -> String;"),
        "Should have name getter (readonly modifier)");
    assert!(!result.code.contains("fn set_name"),
        "Readonly property 'name' should not have setter");

    // "readonly readonly" means property named "readonly" with readonly modifier
    // The second one should generate a getter for the readonly property
}

/// Test variable declarations with keyword names.
#[test]
fn test_var_with_keyword_name() {
    let dts = r#"
        declare var static: RequestHandler;
        declare let readonly: boolean;
        declare const public: string;
    "#;

    let file = parse(dts).expect("Should parse var with keyword name");

    // All three should parse as variables
    assert_eq!(file.items.len(), 3);

    let result = generate(&file, &CodegenOptions::default());

    // Keywords should be escaped in function names
    assert!(result.code.contains("fn static_() -> RequestHandler;"),
        "Should generate escaped static constant");
    assert!(result.code.contains("fn readonly_() -> bool;"),
        "Should generate escaped readonly constant");
    assert!(result.code.contains("fn public_() -> String;"),
        "Should generate escaped public constant");
}

/// Test method names can be keywords.
#[test]
fn test_method_with_keyword_name() {
    let dts = r#"
        interface HttpClient {
            delete(): Promise<Response>;
            get(url: string): Promise<Response>;
            static(value: boolean): void;
            readonly(flag: boolean): boolean;
        }
    "#;

    let file = parse(dts).expect("Should parse methods with keyword names");

    let result = generate(&file, &CodegenOptions::default());

    // Methods with keyword names
    assert!(result.code.contains("fn delete(self) -> JsPromise<Response>;"),
        "Should have delete method (not a keyword in Husk)");
    assert!(result.code.contains("fn get(self, url: String) -> JsPromise<Response>;"),
        "Should have get method");
    assert!(result.code.contains("fn static_(self, value: bool);"),
        "Should have escaped static_ method");
    assert!(result.code.contains("fn readonly_(self, flag: bool) -> bool;"),
        "Should have escaped readonly_ method");
}

/// Test that `new(...)` in interfaces can be parsed as a method named "new".
/// Note: In TypeScript, `new(...)` at the start of a member is a construct signature,
/// but our parser now treats it as a method when followed by `(`, making it more flexible.
#[test]
fn test_new_as_method_name() {
    let dts = r#"
        interface Factory {
            new(config: Config): Product;
        }
    "#;

    let file = parse(dts).expect("Should parse");

    let result = generate(&file, &CodegenOptions::default());

    // `new(...)` is now parsed as a method named "new"
    // The name gets escaped to `new_` in the generated code
    assert!(result.code.contains("fn new_"),
        "new(...) should be parsed as method and escaped to new_");
}

/// Test class properties with keyword names.
#[test]
fn test_class_keyword_properties() {
    let dts = r#"
        declare class Config {
            static: boolean;
            readonly: string;
            static static: number;
            readonly readonly: string;
        }
    "#;

    let file = parse(dts).expect("Should parse class with keyword properties");

    let result = generate(&file, &CodegenOptions::default());

    // Instance properties named with keywords
    assert!(result.code.contains("fn static_(self) -> bool;"),
        "Should have instance static_ getter");
    assert!(result.code.contains("fn readonly_(self) -> String;"),
        "Should have instance readonly_ getter");

    // Static property named "static" - "static static: number" means static property named "static"
    assert!(result.code.contains("fn static_() -> f64;"),
        "Should have static static_ getter (no self)");
}

/// Test object type literals with keyword property names.
#[test]
fn test_object_type_keyword_properties() {
    let dts = r#"
        type Options = {
            static: boolean;
            readonly: string;
            function(): void;
            new(): Options;
        };
    "#;

    let file = parse(dts).expect("Should parse object type with keyword properties");

    // Should parse successfully without errors
    assert_eq!(file.items.len(), 1);
}

/// Test parsing the actual Express static property issue.
#[test]
fn test_express_static_property() {
    let dts = r#"
        declare namespace e {
            var json: any;
            var raw: any;
            var static: RequestHandler;
            var urlencoded: any;
        }
    "#;

    let file = parse(dts).expect("Should parse Express namespace with 'var static'");

    // Should have namespace with 4 variables
    assert_eq!(file.items.len(), 1);
}

/// Test parsing the actual better-sqlite3 readonly property issue.
#[test]
fn test_better_sqlite3_readonly_property() {
    let dts = r#"
        interface Statement {
            database: Database;
            source: string;
            reader: boolean;
            readonly: boolean;
            busy: boolean;
        }
    "#;

    let file = parse(dts).expect("Should parse interface with 'readonly' as property name");

    // Verify we have the interface with all properties
    assert_eq!(file.items.len(), 1);

    let result = generate(&file, &CodegenOptions::default());

    // The property named "readonly" should generate an escaped getter
    assert!(result.code.contains("fn readonly_(self) -> bool;"),
        "Should have readonly_ getter for property named 'readonly'");
}

/// Test generated code escapes TypeScript keywords.
#[test]
fn test_keyword_property_codegen() {
    let dts = r#"
        interface Config {
            static: boolean;
            readonly: string;
        }
    "#;

    let file = parse(dts).expect("Should parse");
    let result = generate(&file, &CodegenOptions::default());

    // Verify keywords are escaped in generated Husk code
    assert!(result.code.contains("static_"),
        "static should be escaped to static_");
    assert!(result.code.contains("readonly_"),
        "readonly should be escaped to readonly_");
}

/// Test simple qualified type name (single dot).
#[test]
fn test_qualified_type_name_simple() {
    let dts = r#"
        interface Statement {
            run(): Database.RunResult;
        }
    "#;
    let file = parse(dts).expect("Should parse qualified type name");
    let result = generate(&file, &CodegenOptions::default());

    assert!(result.code.contains("fn run(self) -> RunResult;"),
        "Should use simple name from qualified type");
}

/// Test nested qualified type name (multiple dots).
#[test]
fn test_qualified_type_name_nested() {
    let dts = r#"
        declare function create(): express.core.Application;
    "#;
    let file = parse(dts).expect("Should parse nested qualified type");
    let result = generate(&file, &CodegenOptions::default());

    assert!(result.code.contains("fn create() -> Application;"),
        "Should use last segment from deeply nested qualified type");
}

/// Test qualified type with generic arguments.
#[test]
fn test_qualified_type_with_generics() {
    let dts = r#"
        declare function getEntries(): Namespace.Entry<string, number>;
    "#;
    let file = parse(dts).expect("Should parse qualified type with generics");
    let result = generate(&file, &CodegenOptions::default());

    assert!(result.code.contains("Entry<String, f64>"),
        "Should handle generics on qualified types");
}

/// Test qualified types in various positions (properties, parameters, return types).
#[test]
fn test_qualified_type_in_various_positions() {
    let dts = r#"
        interface Foo {
            prop: Namespace.Type;
            method(param: Other.Param): Return.Result;
        }
    "#;
    let file = parse(dts).expect("Should parse qualified types in all positions");

    let result = generate(&file, &CodegenOptions::default());
    assert!(result.code.contains("fn prop(self) -> Type;"),
        "Property should use simple name from qualified type");
    assert!(result.code.contains("fn method(self, param: Param) -> Result;"),
        "Method params and return should use simple names");
}

/// Test Express-like qualified type pattern.
#[test]
fn test_express_core_application() {
    let dts = r#"
        declare namespace express {
            interface Application extends core.Application {}
        }
    "#;
    let file = parse(dts).expect("Should parse Express-like qualified type");

    // Just verify it parses - the extends clause should work with qualified type
    assert_eq!(file.items.len(), 1);
}

/// Test better-sqlite3-like qualified type pattern.
#[test]
fn test_better_sqlite3_database_runresult() {
    let dts = r#"
        interface Statement {
            database: Database;
            source: string;
            reader: boolean;
            readonly: boolean;
            run(): Database.RunResult;
        }
    "#;
    let file = parse(dts).expect("Should parse better-sqlite3-like qualified type");

    let result = generate(&file, &CodegenOptions::default());
    assert!(result.code.contains("fn run(self) -> RunResult;"),
        "Should use RunResult (last segment) from Database.RunResult");
}

/// Test qualified type as array element.
#[test]
fn test_qualified_type_array() {
    let dts = r#"
        declare function getItems(): Namespace.Item[];
    "#;
    let file = parse(dts).expect("Should parse qualified type in array");
    let result = generate(&file, &CodegenOptions::default());

    assert!(result.code.contains("JsArray<Item>"),
        "Should handle qualified type inside array");
}

/// Test backward compatibility - simple types still work.
#[test]
fn test_simple_type_still_works() {
    let dts = r#"
        declare function simple(): SimpleType;
        declare function withGenerics(): Array<SimpleType>;
    "#;
    let file = parse(dts).expect("Simple types should still parse");
    let result = generate(&file, &CodegenOptions::default());

    assert!(result.code.contains("fn simple() -> SimpleType;"),
        "Simple types should work unchanged");
    assert!(result.code.contains("fn withGenerics() -> JsArray<SimpleType>;"),
        "Simple types in generics should work");
}
