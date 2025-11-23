// Example Node host for a Husk-compiled Express-style app.
//
// This script expects:
//   1. `express` to be installed as a dependency:
//        npm install express
//   2. The Husk example `examples/interop_express_minimal.hk` to be compiled to:
//        target/interop_express_minimal.js
//      e.g.:
//        cargo run --bin huskc -- compile examples/interop_express_minimal.hk \
//          > target/interop_express_minimal.js
//
// When run with Node:
//   node examples/express_host.js
//
// it will:
//   - Create a shared Express `app`.
//   - Expose a factory on `globalThis.express` so the compiled Husk code can call it.
//   - Require the compiled Husk JS, which will run `main()` and register a route.
//   - Start the HTTP server on port 3000.

const express = require("express");

let app = null;

// Provide a factory compatible with the Husk extern `fn express() -> ExpressApp;`.
globalThis.express = function () {
  if (!app) {
    app = express();
  }
  return app;
};

// Require the compiled Husk module. Its `main()` will call `express()`,
// obtain the shared app, and register at least one route (e.g., "/hello").
require("../target/interop_express_minimal.js");

if (!app) {
  app = express();
}

const port = process.env.PORT || 3000;
app.listen(port, () => {
  console.log(`Husk + Express server listening on http://localhost:${port}`);
});

