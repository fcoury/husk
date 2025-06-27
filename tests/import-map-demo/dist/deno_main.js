function println(...args) { console.log(...args); }
async function __husk_await_bridge(promise) {
  try {
    const value = await promise;
    // Check if the resolved value is already a Husk Result
    if (value && typeof value === 'object' && (value.type === 'Ok' || value.type === 'Err')) {
      return value; // It's already a Result, pass it through
    }
    return { type: 'Ok', value }; // Wrap the success value
  } catch (error) {
    return __husk_map_error(error);
  }
}
function __husk_map_error(error) {
  if (error && typeof error === 'object' && (error.type === 'Ok' || error.type === 'Err')) {
    return error; // Already a Husk Result
  }
  
  let errorPayload;
  if (error instanceof Error) {
    errorPayload = {
      name: error.name,
      message: error.message,
      stack: error.stack || null
    };
  } else if (error instanceof DOMException) {
    errorPayload = {
      name: error.name,
      message: error.message,
      code: error.code
    };
  } else if (typeof error === 'string') {
    errorPayload = {
      message: error,
      stack: (new Error()).stack || null
    };
  } else {
    errorPayload = {
      value: error,
      stack: (new Error()).stack || null
    };
  }
  
  return { type: 'Err', value: errorPayload };
}
function __husk_safe_call(fn, ...args) {
  try {
    const result = fn(...args);
    return { type: 'Ok', value: result };
  } catch (error) {
    return __husk_map_error(error);
  }
}
function __format__(formatStr, ...args) {
  let result = '';
  let argIndex = 0;
  let i = 0;
  while (i < formatStr.length) {
    if (formatStr[i] === '{' && i + 1 < formatStr.length) {
      if (formatStr[i + 1] === '{') {
        result += '{';
        i += 2;
      } else if (formatStr[i + 1] === '}') {
        if (argIndex < args.length) {
          result += String(args[argIndex++]);
        }
        i += 2;
      } else {
        result += formatStr[i];
        i++;
      }
    } else if (formatStr[i] === '}' && i + 1 < formatStr.length && formatStr[i + 1] === '}') {
      result += '}';
      i += 2;
    } else {
      result += formatStr[i];
      i++;
    }
  }
  return result;
}
function __husk_enum_equals(a, b) {
  // Handle null/undefined
  if (a === b) return true;
  if (a == null || b == null) return false;
  
  // Handle Option/Result object literals
  if (typeof a === 'object' && typeof b === 'object' && 
      'type' in a && 'type' in b) {
    if (a.type !== b.type) return false;
    // Deep equality for values
    if (a.value === b.value) return true;
    if (typeof a.value === 'object' && typeof b.value === 'object') {
      return JSON.stringify(a.value) === JSON.stringify(b.value);
    }
    return false;
  }
  
  // Handle class-based enums
  if (typeof a.equals === 'function') {
    return a.equals(b);
  }
  
  // Fallback to regular equality
  return a === b;
}

// File I/O functions
import fs from 'fs';
import path from 'path';

function read_file(filePath) {
  try {
    return { type: 'Ok', value: fs.readFileSync(filePath, 'utf8') };
  } catch (e) {
    return { type: 'Err', value: e.message };
  }
}

function read_file_bytes(filePath) {
  try {
    const buffer = fs.readFileSync(filePath);
    return { type: 'Ok', value: Array.from(buffer) };
  } catch (e) {
    return { type: 'Err', value: e.message };
  }
}

function read_lines(filePath) {
  try {
    const content = fs.readFileSync(filePath, 'utf8');
    const lines = content.split('\n');
    // Remove last empty line if file ends with newline
    if (lines[lines.length - 1] === '') lines.pop();
    return { type: 'Ok', value: lines };
  } catch (e) {
    return { type: 'Err', value: e.message };
  }
}

function write_file(filePath, contents) {
  try {
    fs.writeFileSync(filePath, contents);
    return undefined; // unit
  } catch (e) {
    throw new Error(e.message);
  }
}

function write_file_bytes(filePath, data) {
  try {
    const buffer = Buffer.from(data);
    fs.writeFileSync(filePath, buffer);
    return undefined; // unit
  } catch (e) {
    throw new Error(e.message);
  }
}

function append_file(filePath, contents) {
  try {
    fs.appendFileSync(filePath, contents);
    return undefined; // unit
  } catch (e) {
    throw new Error(e.message);
  }
}

function exists(filePath) {
  try {
    fs.accessSync(filePath);
    return true;
  } catch {
    return false;
  }
}

function is_file(filePath) {
  try {
    return fs.statSync(filePath).isFile();
  } catch {
    return false;
  }
}

function is_dir(filePath) {
  try {
    return fs.statSync(filePath).isDirectory();
  } catch {
    return false;
  }
}

function create_dir(dirPath) {
  try {
    fs.mkdirSync(dirPath);
    return { type: 'Ok', value: null };
  } catch (error) {
    if (error.code === 'EEXIST') {
      return { type: 'Err', error: `Directory already exists: ${dirPath}` };
    } else if (error.code === 'EACCES') {
      return { type: 'Err', error: `Permission denied: ${dirPath}` };
    } else if (error.code === 'ENOENT') {
      return { type: 'Err', error: `Parent directory not found: ${dirPath}` };
    } else {
      return { type: 'Err', error: `IO error creating directory ${dirPath}: ${error.message}` };
    }
  }
}

function create_dir_all(dirPath) {
  try {
    fs.mkdirSync(dirPath, { recursive: true });
    return { type: 'Ok', value: null };
  } catch (error) {
    if (error.code === 'EACCES') {
      return { type: 'Err', error: `Permission denied: ${dirPath}` };
    } else {
      return { type: 'Err', error: `IO error creating directories ${dirPath}: ${error.message}` };
    }
  }
}

function remove_dir(dirPath) {
  try {
    fs.rmdirSync(dirPath);
    return { type: 'Ok', value: null };
  } catch (error) {
    if (error.code === 'ENOENT') {
      return { type: 'Err', error: `Directory not found: ${dirPath}` };
    } else if (error.code === 'EACCES') {
      return { type: 'Err', error: `Permission denied: ${dirPath}` };
    } else if (error.code === 'ENOTEMPTY') {
      return { type: 'Err', error: `Directory not empty: ${dirPath}` };
    } else {
      return { type: 'Err', error: `IO error removing directory ${dirPath}: ${error.message}` };
    }
  }
}

function remove_dir_all(dirPath) {
  try {
    fs.rmSync(dirPath, { recursive: true });
    return { type: 'Ok', value: null };
  } catch (error) {
    if (error.code === 'ENOENT') {
      return { type: 'Err', error: `Directory not found: ${dirPath}` };
    } else if (error.code === 'EACCES') {
      return { type: 'Err', error: `Permission denied: ${dirPath}` };
    } else {
      return { type: 'Err', error: `IO error removing directory ${dirPath}: ${error.message}` };
    }
  }
}

function read_dir(dirPath) {
  try {
    const entries = fs.readdirSync(dirPath, { withFileTypes: true });
    const result = entries.map(entry => ({
      name: entry.name,
      is_file: entry.isFile(),
      is_dir: entry.isDirectory()
    }));
    return { type: 'Ok', value: result };
  } catch (error) {
    if (error.code === 'ENOENT') {
      return { type: 'Err', error: `Directory not found: ${dirPath}` };
    } else if (error.code === 'EACCES') {
      return { type: 'Err', error: `Permission denied: ${dirPath}` };
    } else if (error.code === 'ENOTDIR') {
      return { type: 'Err', error: `Not a directory: ${dirPath}` };
    } else {
      return { type: 'Err', error: `IO error reading directory ${dirPath}: ${error.message}` };
    }
  }
}

// Console I/O functions
import readline from 'readline';

function read_line() {
  try {
    const rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout
    });
    return new Promise((resolve) => {
      rl.question('', (answer) => {
        rl.close();
        resolve({ type: 'Ok', value: answer });
      });
    });
  } catch (error) {
    return { type: 'Err', value: error.message };
  }
}

// Async File I/O functions
import { promises as fsPromises } from 'fs';

async function read_file_async(filePath) {
  try {
    const content = await fsPromises.readFile(filePath, 'utf8');
    return { type: 'Ok', value: content };
  } catch (e) {
    return { type: 'Err', value: e.message };
  }
}

async function read_file_bytes_async(filePath) {
  try {
    const buffer = await fsPromises.readFile(filePath);
    return { type: 'Ok', value: Array.from(buffer) };
  } catch (e) {
    return { type: 'Err', value: e.message };
  }
}

async function read_lines_async(filePath) {
  try {
    const content = await fsPromises.readFile(filePath, 'utf8');
    const lines = content.split('\n');
    if (lines[lines.length - 1] === '') lines.pop();
    return { type: 'Ok', value: lines };
  } catch (e) {
    return { type: 'Err', value: e.message };
  }
}

async function write_file_async(filePath, contents) {
  try {
    await fsPromises.writeFile(filePath, contents);
    return { type: 'Ok', value: undefined };
  } catch (e) {
    return { type: 'Err', value: e.message };
  }
}

async function write_file_bytes_async(filePath, data) {
  try {
    const buffer = Buffer.from(data);
    await fsPromises.writeFile(filePath, buffer);
    return { type: 'Ok', value: undefined };
  } catch (e) {
    return { type: 'Err', value: e.message };
  }
}

async function append_file_async(filePath, contents) {
  try {
    await fsPromises.appendFile(filePath, contents);
    return { type: 'Ok', value: undefined };
  } catch (e) {
    return { type: 'Err', value: e.message };
  }
}

import 'https://deno.land/x/express@v1.0.0/mod.ts';
import { Application, Router } from 'https://deno.land/x/oak/mod.ts';
function main() {
  void (console.log("Deno import maps demo"));
  void (console.log("Express and Oak frameworks mapped to Deno URLs"));
}
