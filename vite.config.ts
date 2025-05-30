import { defineConfig } from "vite";
// @ts-ignore
import elm from "vite-plugin-elm";

export default defineConfig({
  plugins: [elm({ optimize: undefined })],
});
