#!/usr/bin/env node

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const mainJsPath = path.join(__dirname, 'dist', 'main.js');
const bindingsPath = path.join(__dirname, 'extern-bindings.json');

// Read the bindings configuration
const bindings = JSON.parse(fs.readFileSync(bindingsPath, 'utf8'));

// Read the current main.js
let mainJs = fs.readFileSync(mainJsPath, 'utf8');

// Find where to insert the bindings (after the fs import)
const fsImportRegex = /import fs from 'fs';\nimport path from 'path';/;
const bindingsCode = Object.entries(bindings.bindings)
  .map(([name, binding]) => `const ${name} = ${binding};`)
  .join('\n');

const replacement = `import fs from 'fs';
import path from 'path';

// Extern function bindings
${bindingsCode}`;

mainJs = mainJs.replace(fsImportRegex, replacement);

// Write back the modified file
fs.writeFileSync(mainJsPath, mainJs);

console.log('✓ Added extern function bindings to dist/main.js');