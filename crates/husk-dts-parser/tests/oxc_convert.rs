use husk_dts_parser::{OxcDtsParser, convert_oxc_program};

#[test]
fn converts_basic_declarations() {
    let src = r#"
        declare interface User {
            id: number;
            name?: string;
        }

        declare function greet(user: User, shout?: boolean): string;

        type Id = string | number;
    "#;

    let parser = OxcDtsParser::new();
    let parsed = parser.parse(src);
    assert!(
        parsed.errors.is_empty(),
        "Oxc parse errors: {:?}",
        parsed.errors
    );

    let file = convert_oxc_program(&parsed.program).expect("convert_oxc_program failed");

    assert_eq!(file.items.len(), 3);
    // interface
    let iface = match &file.items[0] {
        husk_dts_parser::DtsItem::Interface(i) => i,
        other => panic!("expected interface, got {other:?}"),
    };
    assert_eq!(iface.name, "User");
    assert_eq!(iface.members.len(), 2);

    // function
    let func = match &file.items[1] {
        husk_dts_parser::DtsItem::Function(f) => f,
        other => panic!("expected function, got {other:?}"),
    };
    assert_eq!(func.name, "greet");
    assert_eq!(func.params.len(), 2);

    // type alias
    let alias = match &file.items[2] {
        husk_dts_parser::DtsItem::TypeAlias(a) => a,
        other => panic!("expected type alias, got {other:?}"),
    };
    assert_eq!(alias.name, "Id");
}
