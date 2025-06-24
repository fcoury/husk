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
function Config(host, port, debug, timeout) {this.host = host;this.port = port;this.debug = debug;this.timeout = timeout;};
class Request {
  constructor(variant, value) {
    this.variant = variant;
    this.value = value;
  }
}
Request.Get = class extends Request {constructor(value) { super('Get', value); }};
Request.Post = class extends Request {constructor(value) { super('Post', value); }};
;
function main() {
let config = (function() {const __INSTANCE__ = Object.create(Config.prototype);__INSTANCE__.host = "localhost";__INSTANCE__.port = 8080;__INSTANCE__.debug = true;__INSTANCE__.timeout = 30;return __INSTANCE__;})();(() => {
const _matched = config;
if (_matched && typeof _matched === 'object') {
const host = _matched.host;
const port = _matched.port;
void (println(`Server at ${host}:${port}`));
}
})();let req = (function() {const __INSTANCE__ = Object.create(Request.Get.prototype);__INSTANCE__.url = "/api/data";__INSTANCE__.headers = "Content-Type: json";__INSTANCE__.body = "";return __INSTANCE__;})();return (() => {
const _matched = req;
if (_matched instanceof Request.Get) {
const url = _matched.url;
void (println(`GET request to ${url}`));
} else if (_matched instanceof Request.Post) {
const url = _matched.url;
const data = _matched.data;
void (println(`POST ${url} with data: ${data}`));
}
})();
};

main();
