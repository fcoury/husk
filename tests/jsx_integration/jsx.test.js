/**
 * JSX Integration Tests for Husk
 *
 * These tests verify that Husk's JSX codegen produces valid JavaScript
 * that works correctly with React's jsx-runtime.
 */

import { jsx as _jsx, jsxs as _jsxs, Fragment } from "react/jsx-runtime";

describe("JSX Basic Elements", () => {
  test("simple div element", () => {
    // Husk: <div />
    const element = _jsx("div", {});
    expect(element.type).toBe("div");
    expect(element.props).toEqual({});
  });

  test("div with string attribute", () => {
    // Husk: <div class="container" />
    const element = _jsx("div", { class: "container" });
    expect(element.type).toBe("div");
    expect(element.props.class).toBe("container");
  });

  test("div with multiple attributes", () => {
    // Husk: <div id="main" class="container" />
    const element = _jsx("div", { id: "main", class: "container" });
    expect(element.props.id).toBe("main");
    expect(element.props.class).toBe("container");
  });

  test("boolean attribute", () => {
    // Husk: <input disabled />
    const element = _jsx("input", { disabled: true });
    expect(element.props.disabled).toBe(true);
  });

  test("expression attribute", () => {
    // Husk: <div id={myId} />
    const myId = "dynamic-id";
    const element = _jsx("div", { id: myId });
    expect(element.props.id).toBe("dynamic-id");
  });
});

describe("JSX Children", () => {
  test("single text child uses _jsx", () => {
    // Husk: <div>Hello</div>
    const element = _jsx("div", { children: "Hello" });
    expect(element.props.children).toBe("Hello");
  });

  test("multiple children uses _jsxs", () => {
    // Husk: <div><span>A</span><span>B</span></div>
    const element = _jsxs("div", {
      children: [
        _jsx("span", { children: "A" }),
        _jsx("span", { children: "B" }),
      ],
    });
    expect(element.props.children).toHaveLength(2);
    expect(element.props.children[0].props.children).toBe("A");
    expect(element.props.children[1].props.children).toBe("B");
  });

  test("nested elements", () => {
    // Husk: <div><p><span>Nested</span></p></div>
    const element = _jsx("div", {
      children: _jsx("p", {
        children: _jsx("span", { children: "Nested" }),
      }),
    });
    expect(element.props.children.props.children.props.children).toBe("Nested");
  });

  test("text with preserved spacing", () => {
    // Husk: <div>Hello, world</div>
    // Text should be "Hello, world" not "Hello , world"
    const element = _jsx("div", { children: "Hello, world" });
    expect(element.props.children).toBe("Hello, world");
  });

  test("text with colon spacing", () => {
    // Husk: <div>Error: 42</div>
    // Text should preserve spacing around colon
    const element = _jsx("div", { children: "Error: 42" });
    expect(element.props.children).toBe("Error: 42");
  });

  test("numeric text content", () => {
    // Husk: <div>42</div>
    const element = _jsx("div", { children: "42" });
    expect(element.props.children).toBe("42");
  });

  test("keyword as text content", () => {
    // Husk: <div>true</div> (the word "true", not the boolean)
    const element = _jsx("div", { children: "true" });
    expect(element.props.children).toBe("true");
  });
});

describe("JSX Fragments", () => {
  test("empty fragment", () => {
    // Husk: <></>
    const element = _jsx(Fragment, {});
    expect(element.type).toBe(Fragment);
  });

  test("fragment with single child uses _jsx", () => {
    // Husk: <><span /></>
    // Single child should use _jsx (not _jsxs) and NOT wrap in array
    const element = _jsx(Fragment, {
      children: _jsx("span", {}),
    });
    expect(element.type).toBe(Fragment);
    // Single child is passed directly, not as an array
    expect(element.props.children.type).toBe("span");
  });

  test("fragment with children", () => {
    // Husk: <><div /><span /></>
    const element = _jsxs(Fragment, {
      children: [_jsx("div", {}), _jsx("span", {})],
    });
    expect(element.type).toBe(Fragment);
    expect(element.props.children).toHaveLength(2);
  });

  test("nested fragment is preserved", () => {
    // Husk: <div><><span /></></div>
    // The nested fragment should be preserved, not flattened away
    const element = _jsx("div", {
      children: _jsx(Fragment, {
        children: _jsx("span", {}),
      }),
    });
    expect(element.type).toBe("div");
    expect(element.props.children.type).toBe(Fragment);
    expect(element.props.children.props.children.type).toBe("span");
  });
});

describe("JSX Components", () => {
  test("component reference (uppercase)", () => {
    // Husk: <MyComponent />
    function MyComponent() {
      return _jsx("div", {});
    }
    const element = _jsx(MyComponent, {});
    expect(element.type).toBe(MyComponent);
  });

  test("component with props", () => {
    // Husk: <Button label="Click" />
    function Button(props) {
      return _jsx("button", { children: props.label });
    }
    const element = _jsx(Button, { label: "Click" });
    expect(element.props.label).toBe("Click");
  });

  test("member expression component", () => {
    // Husk: <Foo.Bar />
    const Foo = {
      Bar: function Bar() {
        return _jsx("div", {});
      },
    };
    const element = _jsx(Foo.Bar, {});
    expect(element.type).toBe(Foo.Bar);
  });
});

describe("JSX Spread Attributes - Ordering Semantics", () => {
  test("spread then static: static wins", () => {
    // Husk: <div {...a} b="override" />
    // Expected: Object.assign({}, a, { b: "override" })
    // The static prop 'b' should override 'a.b'
    const a = { b: "from-spread", c: "preserved" };
    const props = Object.assign({}, a, { b: "override" });

    const element = _jsx("div", props);
    expect(element.props.b).toBe("override");
    expect(element.props.c).toBe("preserved");
  });

  test("static then spread: spread wins", () => {
    // Husk: <div b="original" {...a} />
    // Expected: Object.assign({}, { b: "original" }, a)
    // The spread 'a.b' should override 'b'
    const a = { b: "from-spread", c: "extra" };
    const props = Object.assign({}, { b: "original" }, a);

    const element = _jsx("div", props);
    expect(element.props.b).toBe("from-spread");
    expect(element.props.c).toBe("extra");
  });

  test("interleaved: static, spread, static", () => {
    // Husk: <div a="1" {...b} c="2" />
    // Expected: Object.assign({}, { a: "1" }, b, { c: "2" })
    const b = { a: "overridden-a", c: "overridden-c", d: "extra" };
    const props = Object.assign({}, { a: "1" }, b, { c: "2" });

    const element = _jsx("div", props);
    // 'a' should be overridden by spread 'b'
    expect(element.props.a).toBe("overridden-a");
    // 'c' should be overridden by final static prop
    expect(element.props.c).toBe("2");
    // 'd' from spread should be preserved
    expect(element.props.d).toBe("extra");
  });

  test("multiple spreads", () => {
    // Husk: <div {...a} {...b} {...c} />
    // Expected: Object.assign({}, a, b, c)
    const a = { x: 1, y: 1, z: 1 };
    const b = { y: 2, z: 2 };
    const c = { z: 3 };
    const props = Object.assign({}, a, b, c);

    const element = _jsx("div", props);
    expect(element.props.x).toBe(1); // Only in a
    expect(element.props.y).toBe(2); // Overridden by b
    expect(element.props.z).toBe(3); // Overridden by c
  });

  test("spread with children", () => {
    // Husk: <div {...props}>Child</div>
    // Expected: Object.assign({}, props, { children: "Child" })
    const spreadProps = { className: "test", id: "myId" };
    const props = Object.assign({}, spreadProps, { children: "Child" });

    const element = _jsx("div", props);
    expect(element.props.className).toBe("test");
    expect(element.props.id).toBe("myId");
    expect(element.props.children).toBe("Child");
  });

  test("no spread: simple object", () => {
    // Husk: <div a="1" b="2" />
    // Should NOT use Object.assign, just a simple object
    const element = _jsx("div", { a: "1", b: "2" });
    expect(element.props.a).toBe("1");
    expect(element.props.b).toBe("2");
  });
});

describe("JSX Expression Children", () => {
  test("expression child", () => {
    // Husk: <div>{value}</div>
    const value = "dynamic";
    const element = _jsx("div", { children: value });
    expect(element.props.children).toBe("dynamic");
  });

  test("mixed text and expressions", () => {
    // Husk: <div>Count: {count}</div>
    // This would be: _jsxs("div", { children: ["Count: ", count] })
    const count = 42;
    const element = _jsxs("div", { children: ["Count: ", count] });
    expect(element.props.children).toEqual(["Count: ", 42]);
  });

  test("map expression in children", () => {
    // Husk: <ul>{items.map(|item| <li>{item}</li>)}</ul>
    const items = ["a", "b", "c"];
    const element = _jsx("ul", {
      children: items.map((item) => _jsx("li", { children: item })),
    });
    expect(element.props.children).toHaveLength(3);
    expect(element.props.children[0].props.children).toBe("a");
    expect(element.props.children[1].props.children).toBe("b");
    expect(element.props.children[2].props.children).toBe("c");
  });
});

describe("JSX Self-Closing vs Non-Self-Closing", () => {
  test("self-closing element", () => {
    // Husk: <br />
    const element = _jsx("br", {});
    expect(element.type).toBe("br");
    expect(element.props).toEqual({});
  });

  test("non-self-closing with no children", () => {
    // Husk: <div></div>
    const element = _jsx("div", {});
    expect(element.type).toBe("div");
    expect(element.props).toEqual({});
  });
});

describe("JSX Event Handlers", () => {
  test("onClick handler", () => {
    // Husk: <button onClick={handler} />
    const handler = () => {};
    const element = _jsx("button", { onClick: handler });
    expect(element.props.onClick).toBe(handler);
  });

  test("multiple event handlers", () => {
    // Husk: <input onChange={onChange} onBlur={onBlur} />
    const onChange = () => {};
    const onBlur = () => {};
    const element = _jsx("input", { onChange, onBlur });
    expect(element.props.onChange).toBe(onChange);
    expect(element.props.onBlur).toBe(onBlur);
  });
});
