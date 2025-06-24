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
class Command {
  constructor(variant, value) {
    this.variant = variant;
    this.value = value;
  }
}
Command.Process = class extends Command {constructor(value) { super('Process', value); }};
Command.Process = new Command.Process();
Command.Help = class extends Command {constructor(value) { super('Help', value); }};
Command.Help = new Command.Help();
;
function parse_args(args) {
(() => {
  if ((args.length < 2)) {
    return { type: 'Err', value: "Not enough arguments" };
  }
})();return (() => {
const _matched = args[1];
if (true) {
(() => {
  if ((args.length < 4)) {
    return { type: 'Err', value: "Process command requires input and output paths" };
  }
})();return { type: 'Ok', value: (function() {const __INSTANCE__ = Object.create(Command::Process.prototype);__INSTANCE__.input = args[2];__INSTANCE__.output = args[3];return __INSTANCE__;})() };
} else if (true) {
return { type: 'Ok', value: Command.Help };
} else if (true) {
return { type: 'Err', value: `Unknown command: ${args[1]}` };
}
})();
};
function process_command(cmd) {
return (() => {
const _matched = cmd;
if (true) {
(() => {
  if ((input.length === 0)) {
    return { type: 'Err', value: "Input path cannot be empty" };
  }
})();(() => {
  if ((output.length === 0)) {
    return { type: 'Err', value: "Output path cannot be empty" };
  }
})();void (println(`Processing from ${input} to ${output}`));return { type: 'Ok', value: void 0 };
} else if (_matched === Command.Help) {
void (println("Usage: cli <command> [args]"));void (println("Commands:"));void (println("  process <input> <output>  - Process a file"));void (println("  help                      - Show this help"));return { type: 'Ok', value: void 0 };
}
})();
};
function main() {
let args = ["cli", "process", "input.txt", "output.txt"];let result = parse_args(args);return (() => {
const _matched = result;
if ((_matched && typeof _matched === 'object' && 'Ok' in _matched)) {
const cmd = _matched.Ok;
let process_result = process_command(cmd);return (() => {
const _matched = process_result;
if ((_matched && typeof _matched === 'object' && 'Ok' in _matched)) {
void (println("Command completed successfully"));
} else if ((_matched && typeof _matched === 'object' && 'Err' in _matched)) {
const e = _matched.Err;
void (println(`Error: ${e}`));
}
})();
} else if ((_matched && typeof _matched === 'object' && 'Err' in _matched)) {
const msg = _matched.Err;
void (println(`Error: ${msg}`));
}
})();
};

