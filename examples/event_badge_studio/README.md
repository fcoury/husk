# Event Badge Studio

This example builds a static event microsite and standalone SVG badges from a
pipe-delimited schedule file.

It is intentionally not a thin npm demo. Husk owns the domain model,
`Result`-based parsing, `Option` rendering fallbacks, enums, methods, and
sectioned application flow. Npm libraries handle the ecosystem work:

- `chroma-js` derives badge colors from session kind accents.
- `date-fns` parses and formats schedule times.
- `qrcode-svg` embeds session links in each generated badge.
- `slugify` creates stable badge filenames.
- `node:fs` writes `dist/index.html` and `dist/badges/*.svg`.

Run it from this directory:

```bash
npm install
npm run build
npm start
```

Open `dist/index.html` in a browser after running the generator.
