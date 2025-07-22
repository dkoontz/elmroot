import { defineConfig } from "vite";
import elm from "vite-plugin-elm";

export default defineConfig(({ mode }) => ({
  plugins: [
    elm({
      // Disable optimization in development mode to enable Debug.log
      optimize: mode !== "development",
    }),
  ],
  build: {
    // Build as library for Node.js
    lib: {
      entry: "examples/Main.elm",
      name: "Main",
      formats: ["es"],
      fileName: (format) => `main.${format === "es" ? "mjs" : "js"}`,
    },
    // Target Node.js environment
    target: "node18",
    // Externalize Node.js built-ins and dependencies
    rollupOptions: {
      external: ["http", "fs", "uuid", "path", "url"],
    },
    // Output to dist directory
    outDir: "dist",
  },
}));
