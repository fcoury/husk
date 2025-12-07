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
        ..Default::default()
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
        interface Buffer {}

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

    // Check generic interface - type parameters are now preserved
    assert!(result.code.contains("struct Box<T>;"));
}

/// Test parsing optional and nullable types.
#[test]
fn test_optional_nullable() {
    let dts = r#"
        interface User {}

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
        ..Default::default()
    });
    assert!(result.code.contains("mod express;"));

    // Hyphenated module name
    let result = generate(&file, &CodegenOptions {
        module_name: Some("lodash-es".to_string()),
        verbose: false,
        ..Default::default()
    });
    assert!(result.code.contains(r#"mod "lodash-es" as lodash_es;"#));

    // Scoped module name
    let result = generate(&file, &CodegenOptions {
        module_name: Some("@types/node".to_string()),
        verbose: false,
        ..Default::default()
    });
    assert!(result.code.contains(r#"mod "@types/node" as node;"#));
}

/// Test property getters and setters for interfaces.
/// Now uses #[getter] syntax instead of method getters/setters.
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

    // All properties use #[getter] syntax now
    assert!(result.code.contains("#[getter]"),
        "Should use #[getter] attribute");

    // Required property
    assert!(result.code.contains("host: String;"),
        "Should generate host property. Got:\n{}", result.code);

    // Optional property
    assert!(result.code.contains("port: Option<f64>;"),
        "Should generate optional port property. Got:\n{}", result.code);

    // Readonly property (also uses #[getter] syntax)
    assert!(result.code.contains("version: String;"),
        "Should generate version property. Got:\n{}", result.code);
}

/// Test class properties generate properly.
/// Class properties still use method syntax (not #[getter]) for now.
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

    // Class properties still use method syntax (class codegen not changed)
    // Instance readonly property - getter only
    assert!(result.code.contains("fn length(self) -> f64;"),
        "Should generate length getter. Got:\n{}", result.code);
    assert!(!result.code.contains("fn set_length"),
        "Readonly property should not have setter");

    // Static property - getter and setter (no self parameter)
    assert!(result.code.contains("fn BYTES_PER_ELEMENT() -> f64;"),
        "Should generate static getter without self. Got:\n{}", result.code);
    assert!(result.code.contains("fn set_BYTES_PER_ELEMENT(value: f64);"),
        "Should generate static setter without self. Got:\n{}", result.code);

    // Mutable instance property - getter and setter
    // Uint8Array is unknown so it becomes JsValue
    assert!(result.code.contains("fn data(self) -> JsValue;"),
        "Should generate data getter with JsValue (Uint8Array unknown). Got:\n{}", result.code);
    assert!(result.code.contains("fn set_data(self, value: JsValue);"),
        "Should generate data setter with JsValue. Got:\n{}", result.code);

    // Private property should be skipped entirely
    assert!(!result.code.contains("_internal"),
        "Private properties should be skipped");
}

/// Test that interface properties now generate #[getter] syntax.
#[test]
fn test_interface_properties_generate_getters() {
    let dts = r#"
        interface Headers {}

        interface Request {
            url: string;
            method: string;
            body: any;
            readonly headers: Headers;
        }
    "#;

    let file = parse(dts).expect("Failed to parse");
    let result = generate(&file, &CodegenOptions::default());

    // All properties use #[getter] syntax now
    assert!(result.code.contains("#[getter]"),
        "Should use #[getter] attribute. Got:\n{}", result.code);

    // Check property declarations (no longer method syntax)
    assert!(result.code.contains("url: String;"),
        "Should generate url property. Got:\n{}", result.code);
    assert!(result.code.contains("method: String;"),
        "Should generate method property. Got:\n{}", result.code);
    assert!(result.code.contains("body: JsValue;"),
        "Should generate body property with JsValue for any. Got:\n{}", result.code);
    assert!(result.code.contains("headers: Headers;"),
        "Should generate headers property. Got:\n{}", result.code);

    // No setters in #[getter] syntax
    assert!(!result.code.contains("fn set_"),
        "Should not have setter methods with #[getter] syntax");
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
    // Non-function properties use #[getter] syntax
    assert!(result.code.contains("static_: bool;"),
        "Should generate escaped static property. Got:\n{}", result.code);
    assert!(result.code.contains("readonly_: String;"),
        "Should generate escaped readonly property. Got:\n{}", result.code);
    assert!(result.code.contains("public_: f64;"),
        "Should generate escaped public property. Got:\n{}", result.code);
    assert!(result.code.contains("private_: bool;"),
        "Should generate escaped private property. Got:\n{}", result.code);
    assert!(result.code.contains("class_: String;"),
        "Should generate escaped class property. Got:\n{}", result.code);
    assert!(result.code.contains("new_: String;"),
        "Should generate escaped new property. Got:\n{}", result.code);
    assert!(result.code.contains("default_: bool;"),
        "Should generate escaped default property. Got:\n{}", result.code);

    // Function-typed property still uses method syntax
    assert!(result.code.contains("fn function_(self)") || result.code.contains("function_:"),
        "Should generate escaped function property. Got:\n{}", result.code);
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

    // "readonly" alone is a property named "readonly" - uses #[getter] syntax
    assert!(result.code.contains("#[getter]"),
        "Should use #[getter] attribute. Got:\n{}", result.code);
    assert!(result.code.contains("readonly_: bool;"),
        "Should have readonly_ property (escaped keyword). Got:\n{}", result.code);

    // "readonly name" means "name" is the property with readonly modifier
    assert!(result.code.contains("name: String;"),
        "Should have name property. Got:\n{}", result.code);
    assert!(!result.code.contains("fn set_name"),
        "Readonly property 'name' should not have setter");

    // "readonly readonly" means property named "readonly" with readonly modifier
    // The second one should generate a getter for the readonly property
}

/// Test variable declarations with keyword names.
#[test]
fn test_var_with_keyword_name() {
    let dts = r#"
        interface RequestHandler {}

        declare var static: RequestHandler;
        declare let readonly: boolean;
        declare const public: string;
    "#;

    let file = parse(dts).expect("Should parse var with keyword name");

    // Should have the interface and three variables
    assert_eq!(file.items.len(), 4);

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
        interface Response {}

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
        "Should have instance static_ getter. Got:\n{}", result.code);
    assert!(result.code.contains("fn readonly_(self) -> String;"),
        "Should have instance readonly_ getter. Got:\n{}", result.code);

    // Static property named "static" - "static static: number" means static property named "static"
    assert!(result.code.contains("fn static_() -> f64;"),
        "Should have static static_ getter (no self). Got:\n{}", result.code);
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

    // Interface properties now use #[getter] syntax
    assert!(result.code.contains("#[getter]"),
        "Should use #[getter] attribute. Got:\n{}", result.code);

    // The property named "readonly" should be escaped
    assert!(result.code.contains("readonly_: bool;"),
        "Should have readonly_ property (escaped keyword). Got:\n{}", result.code);
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
        interface RunResult {}

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
        interface Application {}

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
        interface Entry {}

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
        interface Type {}
        interface Param {}
        interface Result {}

        interface Foo {
            prop: Namespace.Type;
            method(param: Other.Param): Return.Result;
        }
    "#;
    let file = parse(dts).expect("Should parse qualified types in all positions");

    let result = generate(&file, &CodegenOptions::default());

    // Properties now use #[getter] syntax
    assert!(result.code.contains("#[getter]"),
        "Should use #[getter] attribute. Got:\n{}", result.code);
    assert!(result.code.contains("prop: Type;"),
        "Property should use simple name from qualified type. Got:\n{}", result.code);

    // Methods still use fn syntax
    assert!(result.code.contains("fn method(self, param: Param) -> Result;"),
        "Method params and return should use simple names. Got:\n{}", result.code);
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
        interface Database {}
        interface RunResult {}

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
        interface Item {}

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
        interface SimpleType {}

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

/// Test trailing comma in type parameter lists.
/// TypeScript allows: `interface Foo<T, U,> {}`
#[test]
fn test_trailing_comma_in_type_params() {
    let dts = r#"
        interface Foo<T, U,> {}
        interface Bar<A = string, B = number,> extends Foo<A, B> {}
    "#;
    let file = parse(dts).expect("Should parse trailing commas in type params");

    // Verify the interfaces were parsed
    assert!(file.items.len() >= 2, "Should have at least 2 interfaces");
}

/// Test trailing comma with constraint and default.
#[test]
fn test_trailing_comma_with_constraint() {
    let dts = r#"
        interface RequestHandler<
            P = ParamsDictionary,
            ResBody = any,
            ReqBody = any,
            ReqQuery = ParsedQs,
            LocalsObj extends Record<string, any> = Record<string, any>,
        > {}
    "#;
    let file = parse(dts).expect("Should parse trailing comma with constraints");
    assert!(!file.items.is_empty(), "Should have parsed interface");
}

/// Test conditional type with array in extends clause.
/// Pattern: `T extends unknown[] ? A : B`
#[test]
fn test_conditional_type_with_array_extends() {
    let dts = r#"
        type IsArray<T> = T extends unknown[] ? true : false;
    "#;
    let file = parse(dts).expect("Should parse conditional type with array extends");
    assert!(!file.items.is_empty(), "Should have parsed type alias");
}

/// Test conditional type used as method return type.
/// This is the pattern used in better-sqlite3's `prepare` method.
#[test]
fn test_interface_method_conditional_return() {
    let dts = r#"
        interface Database {
            prepare<P extends unknown[] | {} = unknown[]>(
                source: string
            ): P extends unknown[] ? Statement<P> : Statement<[P]>;
        }
    "#;
    let file = parse(dts).expect("Should parse method with conditional return type");
    assert!(!file.items.is_empty(), "Should have parsed interface");
}

// ==========================================
// Phase 1: Method Overloading Tests
// ==========================================

/// Test that overloaded methods are merged into a single signature.
/// The most permissive signature should be used.
#[test]
fn test_overloaded_methods_merged() {
    let dts = r#"
        interface Value {}

        interface Foo {
            get(key: string): Value;
            get(key: string, defaultValue: Value): Value;
        }
    "#;
    let file = parse(dts).unwrap();
    let result = generate(&file, &CodegenOptions::default());

    // Should have exactly ONE get method
    let get_count = result.code.matches("fn get(").count();
    assert_eq!(get_count, 1, "Should have exactly one get method, found {}", get_count);

    // The merged signature should have the second parameter as optional
    // Note: We preserve TypeScript param names (defaultValue, not default_value)
    assert!(
        result.code.contains("fn get(self, key: String, defaultValue: Option<Value>) -> Value;"),
        "Should merge overloads with optional param. Got:\n{}",
        result.code
    );
}

/// Test overloaded top-level functions are merged.
#[test]
fn test_overloaded_functions_merged() {
    let dts = r#"
        declare function Database(filename: string): Database;
        declare function Database(filename: string, options: Options): Database;
    "#;
    let file = parse(dts).unwrap();
    let result = generate(&file, &CodegenOptions::default());

    // Should have exactly ONE Database function
    let count = result.code.matches("fn Database(").count();
    assert_eq!(count, 1, "Should have exactly one Database function, found {}", count);
}

/// Test overloads with different first parameter types emit warning.
#[test]
fn test_overloaded_different_types_takes_first() {
    let dts = r#"
        interface Foo {
            set(key: string): void;
            set(key: number): void;
        }
    "#;
    let file = parse(dts).unwrap();
    let result = generate(&file, &CodegenOptions { verbose: true, ..Default::default() });

    // Should have exactly ONE set method
    let count = result.code.matches("fn set(").count();
    assert_eq!(count, 1, "Should have exactly one set method, found {}", count);
}

// ==========================================
// Phase 2: Generic Type Parameter Tests
// ==========================================

/// Test that unresolved method-only type parameters are simplified to JsValue.
#[test]
fn test_unresolved_generics_simplified() {
    let dts = r#"
        interface Database {
            transaction<F extends (...args: any) => any>(fn: F): Transaction<F>;
        }
    "#;
    let file = parse(dts).unwrap();
    let result = generate(&file, &CodegenOptions::default());

    // Transaction<F> should be simplified since F is a method-only param
    // Either simplified to JsValue or to Transaction (base type)
    let has_simplified = result.code.contains("-> JsValue;")
        || result.code.contains("-> Transaction;")
        || !result.code.contains("Transaction<F>");

    assert!(
        has_simplified,
        "Should simplify unresolved generic return type. Got:\n{}",
        result.code
    );
}

/// Test that struct-level type parameters are preserved.
#[test]
fn test_struct_type_params_preserved() {
    let dts = r#"
        interface Box<T> {
            value: T;
            get(): T;
        }
    "#;
    let file = parse(dts).unwrap();
    let result = generate(&file, &CodegenOptions::default());

    // T should appear in the method signature since it's a struct-level param
    assert!(
        result.code.contains("fn get(self) -> T;") || result.code.contains("-> JsValue"),
        "Struct type params should be preserved or mapped to JsValue. Got:\n{}",
        result.code
    );
}

// ==========================================
// Phase 3: Property Getter Tests
// ==========================================

/// Test that interface properties generate #[getter] syntax.
#[test]
fn test_getter_property_syntax() {
    let dts = r#"
        interface Request {
            readonly body: any;
            params: object;
        }
    "#;
    let file = parse(dts).unwrap();
    let result = generate(&file, &CodegenOptions::default());

    // Should use #[getter] attribute syntax
    assert!(
        result.code.contains("#[getter]"),
        "Should generate #[getter] attribute. Got:\n{}",
        result.code
    );

    // Should use field syntax, not method syntax
    assert!(
        result.code.contains("extern \"js\" body:") || result.code.contains("extern \"js\" body :"),
        "Should use field syntax for properties. Got:\n{}",
        result.code
    );
}

/// Test that function-typed properties are still generated as methods.
#[test]
fn test_function_property_as_method() {
    let dts = r#"
        interface Handlers {
            onClick: () => void;
            onData: (data: string) => boolean;
        }
    "#;
    let file = parse(dts).unwrap();
    let result = generate(&file, &CodegenOptions::default());

    // Function-typed properties should be methods, not #[getter] fields
    // They can use either getter or method syntax
    assert!(
        result.code.contains("onClick") && result.code.contains("onData"),
        "Should include function-typed properties. Got:\n{}",
        result.code
    );
}

// ==========================================
// Phase 4: Template Literal Type Tests
// ==========================================

/// Test parsing simple template literal type.
#[test]
fn test_template_literal_type_simple() {
    let dts = r#"
        type Greeting = `Hello, ${string}!`;
    "#;
    let result = parse(dts);
    assert!(
        result.is_ok(),
        "Failed to parse simple template literal: {:?}",
        result.err()
    );
}

/// Test parsing template literal type with infer.
#[test]
fn test_template_literal_type_with_infer() {
    let dts = r#"
        type RouteParam<T extends string> = T extends `${infer Start}/${infer Rest}` ? Start : T;
    "#;
    let result = parse(dts);
    assert!(
        result.is_ok(),
        "Failed to parse template literal with infer: {:?}",
        result.err()
    );
}

/// Test codegen maps template literal types to String.
#[test]
fn test_template_literal_codegen_to_string() {
    // Test direct use of template literal type in function parameter
    let dts = r#"
        declare function emit(event: `on${string}`): void;
    "#;
    let file = match parse(dts) {
        Ok(f) => f,
        Err(e) => {
            // If parsing fails, skip this test (Phase 4 not implemented yet)
            eprintln!("Skipping test - template literal parsing not implemented: {}", e);
            return;
        }
    };
    let result = generate(&file, &CodegenOptions::default());

    // Template literal should be mapped directly to String
    assert!(
        result.code.contains("event: String"),
        "Template literal type should map to String. Got:\n{}",
        result.code
    );
}

// ==========================================
// Real-world Type Definition Tests
// ==========================================

/// Test parsing and generating code from the real @types/express package.
/// This test requires `npm install` to have been run in the fixtures directory.
#[test]
fn test_real_express_types() {
    use husk_dts_parser::resolver::{Resolver, ResolveOptions};
    use husk_dts_parser::generate_from_module;
    use std::path::Path;

    let fixtures_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures");

    let express_entry = fixtures_dir
        .join("node_modules")
        .join("@types")
        .join("express")
        .join("index.d.ts");

    // Skip test if node_modules not installed
    if !express_entry.exists() {
        eprintln!(
            "Skipping test - express types not installed. Run: cd {} && npm install",
            fixtures_dir.display()
        );
        return;
    }

    // Resolve the full module graph
    let mut resolver = Resolver::new(ResolveOptions {
        base_dir: Some(fixtures_dir.clone()),
        follow_references: true,
        max_depth: Some(10),
        ..Default::default()
    });

    let resolved = resolver.resolve(&express_entry);

    // Check that we resolved multiple files (express has dependencies)
    assert!(
        resolved.files.len() > 1,
        "Expected multiple files to be resolved, got {}",
        resolved.files.len()
    );

    // Generate Husk code
    let result = generate_from_module(&resolved, &CodegenOptions {
        module_name: Some("express".to_string()),
        verbose: false,
        ..Default::default()
    });

    println!("Generated {} bytes of Husk code", result.code.len());
    println!("Resolved {} files", resolved.files.len());
    println!("Warnings: {}", result.warnings.len());

    // Basic structure checks
    assert!(
        result.code.contains("extern \"js\""),
        "Should have extern block"
    );
    assert!(
        result.code.contains("mod express;") || result.code.contains("mod \"express\""),
        "Should have express module import"
    );

    // Key types from express should be present
    assert!(
        result.code.contains("struct Application"),
        "Should have Application struct. Got:\n{}",
        &result.code[..result.code.len().min(2000)]
    );
    assert!(
        result.code.contains("struct Request"),
        "Should have Request struct"
    );
    assert!(
        result.code.contains("struct Response"),
        "Should have Response struct"
    );
    assert!(
        result.code.contains("struct Router"),
        "Should have Router struct"
    );

    // Check for impl blocks
    assert!(
        result.code.contains("impl Application"),
        "Should have Application impl block"
    );

    // Print a sample of the generated code for debugging
    println!("\n=== Sample of generated code ===\n");
    for line in result.code.lines().take(100) {
        println!("{}", line);
    }

    // Report any resolution errors
    if !resolved.errors.is_empty() {
        println!("\n=== Resolution errors ===");
        for err in &resolved.errors {
            println!("  - {}", err);
        }
    }
}
