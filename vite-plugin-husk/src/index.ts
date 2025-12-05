/**
 * vite-plugin-husk v0.1.0
 *
 * Minimal Vite plugin for Husk language support.
 * Uses shell-out architecture: compiles .hk files by calling native `huskc`.
 *
 * Future versions may use WASM for in-process compilation.
 */

import { execFileSync } from "node:child_process";
import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join, relative, resolve } from "node:path";
import type { Plugin, ResolvedConfig } from "vite";

export interface HuskPluginOptions {
  /**
   * Path to huskc executable. Defaults to "huskc" (must be in PATH).
   */
  huskc?: string;

  /**
   * Enable JSX support. Defaults to true.
   */
  jsx?: boolean;

  /**
   * Target module format: "esm" or "cjs". Defaults to "esm".
   */
  target?: "esm" | "cjs";

  /**
   * Enable source maps. Defaults to false.
   * Note: Source maps are not yet supported in the shell-out architecture.
   * This option is reserved for future WASM-based compilation.
   */
  sourceMaps?: boolean;

  /**
   * Additional arguments to pass to huskc.
   */
  huskArgs?: string[];
}

interface CompilationResult {
  code: string;
  map?: string;
  errors: string[];
}

/**
 * Check if huskc is available
 */
function checkHuskc(huskc: string): boolean {
  try {
    execFileSync(huskc, ["--version"], { stdio: "ignore" });
    return true;
  } catch {
    return false;
  }
}

/**
 * Compile a .hk file using huskc
 */
function compileHusk(
  filePath: string,
  options: Required<HuskPluginOptions>
): CompilationResult {
  const args: string[] = [
    "compile",
    filePath,
    "--target",
    options.target,
  ];

  // Note: huskc requires --output when using --source-map.
  // Since we're using stdout capture, source maps are not yet supported.
  // Future versions may use temp files or WASM for source map support.

  if (options.huskArgs) {
    args.push(...options.huskArgs);
  }

  try {
    // Use huskc to compile the file
    const output = execFileSync(options.huskc, args, {
      encoding: "utf-8",
      cwd: dirname(filePath),
    });

    // For now, huskc outputs to stdout
    // Source maps would require --output to write to a temp file
    return {
      code: output,
      errors: [],
    };
  } catch (error: any) {
    const message = error.stderr?.toString() || error.message;
    return {
      code: "",
      errors: [message],
    };
  }
}

/**
 * Add JSX runtime import to compiled code
 */
function injectJsxRuntime(code: string): string {
  // Check if the code uses JSX functions
  // Use more specific patterns to avoid false positives on unrelated identifiers
  const usesJsx = code.includes("_jsx(") || code.includes("_jsxs(");
  const usesFragment = code.includes("Fragment,") || code.includes("Fragment)");
  if (usesJsx || usesFragment) {
    const importStatement =
      'import { jsx as _jsx, jsxs as _jsxs, Fragment } from "react/jsx-runtime";\n';
    return importStatement + code;
  }
  return code;
}

export default function huskPlugin(options: HuskPluginOptions = {}): Plugin {
  const resolvedOptions: Required<HuskPluginOptions> = {
    huskc: options.huskc ?? "huskc",
    jsx: options.jsx ?? true,
    target: options.target ?? "esm",
    sourceMaps: options.sourceMaps ?? false, // Not yet supported in shell-out mode
    huskArgs: options.huskArgs ?? [],
  };

  let config: ResolvedConfig;
  let huskcAvailable = false;

  return {
    name: "vite-plugin-husk",

    configResolved(resolvedConfig) {
      config = resolvedConfig;
    },

    buildStart() {
      // Check if huskc is available
      huskcAvailable = checkHuskc(resolvedOptions.huskc);
      if (!huskcAvailable) {
        this.warn(
          `huskc not found at "${resolvedOptions.huskc}". ` +
            "Husk files will not be compiled. " +
            "Install huskc or specify the path in plugin options."
        );
      }

      // Warn if sourceMaps is explicitly enabled (not yet supported)
      if (options.sourceMaps === true) {
        this.warn(
          "sourceMaps option is not yet supported in shell-out mode. " +
            "Source maps will not be generated. " +
            "This feature is reserved for future WASM-based compilation."
        );
      }
    },

    resolveId(id, importer) {
      // Handle .hk imports
      if (id.endsWith(".hk")) {
        if (importer) {
          const resolved = resolve(dirname(importer), id);
          if (existsSync(resolved)) {
            return resolved;
          }
        }
        // Try as absolute path
        if (existsSync(id)) {
          return id;
        }
      }
      return null;
    },

    load(id) {
      // Only handle .hk files
      if (!id.endsWith(".hk")) {
        return null;
      }

      if (!huskcAvailable) {
        this.error(
          `Cannot compile ${id}: huskc is not available. ` +
            "Please install huskc and ensure it's in your PATH."
        );
        return null;
      }

      const result = compileHusk(id, resolvedOptions);

      if (result.errors.length > 0) {
        this.error(result.errors.join("\n"));
        return null;
      }

      let code = result.code;

      // Inject JSX runtime if enabled
      if (resolvedOptions.jsx) {
        code = injectJsxRuntime(code);
      }

      return {
        code,
        map: result.map ? JSON.parse(result.map) : null,
      };
    },

    handleHotUpdate({ file, server }) {
      // Trigger HMR for .hk files
      if (file.endsWith(".hk")) {
        const module = server.moduleGraph.getModuleById(file);
        if (module) {
          server.moduleGraph.invalidateModule(module);
          return [module];
        }
      }
    },
  };
}

// Named export for ESM
export { huskPlugin };
