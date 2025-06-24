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
function main() {
let pairs = [[1, "one"], [2, "two"], [3, "three"]];for (const [number, word] of pairs) {
void (println(`${number}: ${word}`))
};let coordinates = [[0, 0], [1, 2], [3, 4]];for (const [x, y] of coordinates) {
void (println(`Point at (${x}, ${y})`))
};
};

main();
