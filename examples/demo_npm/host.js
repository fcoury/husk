#!/usr/bin/env node
/**
 * Host script for Husk npm Demo
 *
 * This script:
 * 1. Requires npm packages (chalk, nanoid, validator)
 * 2. Exposes wrapper functions via globalThis
 * 3. Loads the compiled Husk module from dist/
 * 4. Calls the main() function
 *
 * Usage:
 *   npm install
 *   npm run build   # or: huskc build
 *   npm start       # or: node host.js
 */

import chalk from 'chalk';
import { nanoid } from 'nanoid';
import validator from 'validator';

// === Expose npm package functions via globalThis ===

// nanoid - unique ID generation
globalThis.nanoid = () => nanoid();

// validator - string validation
globalThis.is_email = (s) => validator.isEmail(s);
globalThis.is_alpha = (s) => validator.isAlpha(s);
globalThis.is_length = (s, min, max) => validator.isLength(s, { min, max });

// chalk - terminal colors
globalThis.chalk_green = (s) => chalk.green(s);
globalThis.chalk_red = (s) => chalk.red(s);
globalThis.chalk_blue = (s) => chalk.blue(s);
globalThis.chalk_gray = (s) => chalk.gray(s);
globalThis.chalk_bold = (s) => chalk.bold(s);

// console output
globalThis.println = (s) => console.log(s);

// === Load and run compiled Husk code ===
// Note: The Husk build outputs to dist/main.js with --lib mode disabled,
// so main() is auto-called. We just need to import it.
await import('./dist/main.js');
