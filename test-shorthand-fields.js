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
function Point(x, y) {this.x = x;this.y = y;};
function Person(name, age) {this.name = name;this.age = age;};
class Status {
  constructor(variant, value) {
    this.variant = variant;
    this.value = value;
  }
}
Status.Working = class extends Status {constructor(value) { super('Working', value); }};
Status.Done = class extends Status {constructor(value) { super('Done', value); }};
;
function main() {
let x = 10;let y = 20;let p1 = (function() {const __INSTANCE__ = Object.create(Point.prototype);__INSTANCE__.x = x;__INSTANCE__.y = y;return __INSTANCE__;})();let p2 = (function() {const __INSTANCE__ = Object.create(Point.prototype);__INSTANCE__.x = 30;__INSTANCE__.y = 40;return __INSTANCE__;})();let name = "Alice";let age = 25;let person = (function() {const __INSTANCE__ = Object.create(Person.prototype);__INSTANCE__.name = name;__INSTANCE__.age = age;return __INSTANCE__;})();let p3 = (function() {const __INSTANCE__ = Object.create(Point.prototype);__INSTANCE__.x = x;__INSTANCE__.y = 50;return __INSTANCE__;})();let task = "coding";let progress = 75;let status = (function() {const __INSTANCE__ = Object.create(Status.Working.prototype);__INSTANCE__.task = task;__INSTANCE__.progress = progress;return __INSTANCE__;})();void (println(`Point1: (${p1.x}, ${p1.y})`));void (println(`Person: ${person.name} is ${person.age} years old`));
};

main();
