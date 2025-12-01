# Express.js + SQLite Todo API

A complete REST API example written in Husk, demonstrating:

- **Property getters**: `req.body()`, `req.params()` generate correct JS property access (`req.body`, `req.params`)
- **JsValue field extraction**: `jsvalue_getString()`, `jsvalue_getNumber()` safely extract typed values
- **JsObject construction**: `JsObject_new().setString().toJsValue()` builds response objects
- **npm package integration**: `extern "js" { mod "package-name"; }` syntax

## Quick Start

```bash
# Install dependencies
npm install

# Build with Husk compiler
npm run build

# Start the server
npm start
```

Server runs at http://localhost:3000

## API Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | /todos | List all todos |
| GET | /todos/:id | Get a single todo |
| POST | /todos | Create a new todo |
| PUT | /todos/:id | Update a todo |
| DELETE | /todos/:id | Delete a todo |

## Example Requests

```bash
# Create a todo
curl -X POST http://localhost:3000/todos \
  -H "Content-Type: application/json" \
  -d '{"title": "Learn Husk"}'

# List all todos
curl http://localhost:3000/todos

# Update a todo
curl -X PUT http://localhost:3000/todos/1 \
  -H "Content-Type: application/json" \
  -d '{"title": "Learn Husk", "completed": true}'

# Delete a todo
curl -X DELETE http://localhost:3000/todos/1
```

## Project Structure

```
examples/express_sqlite/
├── src/
│   ├── main.hk        # Application entry point
│   ├── express.hk     # Express.js bindings
│   └── database.hk    # better-sqlite3 bindings
├── package.json
└── README.md
```

## Key Patterns

### Property Getters

Express properties like `req.body` are accessed via getter methods that generate property access:

```husk
impl Request {
    extern "js" fn body(self) -> JsValue;   // Generates: req.body
    extern "js" fn params(self) -> JsValue; // Generates: req.params
}
```

### JsValue Field Extraction

Extract typed values from JavaScript objects:

```husk
let body = req.body();
match jsvalue_getString(body, "title") {
    Some(title) => { /* use title */ }
    None => { /* handle missing field */ }
}
```

### Object Construction

Build JavaScript objects for responses:

```husk
let response = JsObject_new()
    .setString("error", "Not found")
    .setNumber("code", 404.0)
    .toJsValue();
res.json(response);
```
