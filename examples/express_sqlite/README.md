# Express.js + SQLite Todo API

A REST API example written in Husk that demonstrates the recommended npm
interop pattern:

- direct npm module imports with `extern "js" mod`
- generated d.ts bindings managed by `huskc dts`
- curated wrapper modules for app-friendly types
- explicit `JsValue` boundaries for JSON request/response data

## Quick Start

```bash
npm install
npm run build
npm start
```

The server runs at `http://localhost:3000`.

## API Endpoints

| Method | Endpoint | Description |
| --- | --- | --- |
| GET | `/todos` | List all todos |
| GET | `/todos/:id` | Get a single todo |
| POST | `/todos` | Create a new todo |
| PUT | `/todos/:id` | Update a todo |
| DELETE | `/todos/:id` | Delete a todo |

## Binding Workflow

`husk.toml` configures d.ts imports for Express and better-sqlite3:

```bash
huskc dts update --follow-imports --report
```

With `generation_gap = true`, raw generated declarations belong in
`*.gen.hk` files and the stable wrapper API stays in:

- `src/express.hk`
- `src/database.hk`

The app imports the wrapper API instead of declaring raw externs in `main.hk`.

## Wrapper Pattern

The wrappers expose named types such as `ExpressApp`, `Request`, `Response`,
`Database`, and `TodoStatements`. They keep low-level statement handles and
field extraction inside the interop modules.

```husk
let app = create_app();
app.use_middleware(json_middleware());

app.post("/todos", |req: Request, res: Response| {
    let title = request_body_string(req, "title");
    // app logic uses String/f64/bool where possible
});
```

`JsValue` still appears where JavaScript is genuinely dynamic: request bodies,
JSON responses, and values returned from broad TypeScript declarations.

## Example Requests

```bash
curl -X POST http://localhost:3000/todos \
  -H "Content-Type: application/json" \
  -d '{"title": "Learn Husk"}'

curl http://localhost:3000/todos

curl -X PUT http://localhost:3000/todos/1 \
  -H "Content-Type: application/json" \
  -d '{"title": "Learn Husk", "completed": true}'

curl -X DELETE http://localhost:3000/todos/1
```
