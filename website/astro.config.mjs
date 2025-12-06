// @ts-check
import { defineConfig } from 'astro/config';
import sitemap from '@astrojs/sitemap';
import tailwindcss from '@tailwindcss/vite';
import huskDark from './src/themes/husk-dark.json';
import huskLight from './src/themes/husk-light.json';

// https://astro.build/config
export default defineConfig({
  site: 'https://husk-lang.org',
  trailingSlash: 'always',
  integrations: [sitemap()],
  vite: {
    plugins: [tailwindcss()]
  },
  markdown: {
    shikiConfig: {
      themes: {
        light: huskLight,
        dark: huskDark,
      },
      defaultColor: false,
      langAlias: {
        husk: 'rust',
      },
    },
  },
});