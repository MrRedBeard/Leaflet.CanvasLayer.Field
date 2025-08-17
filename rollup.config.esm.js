import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import json from '@rollup/plugin-json';
import polyfills from 'rollup-plugin-node-polyfills';
import alias from '@rollup/plugin-alias';
import postcss from 'rollup-plugin-postcss';
import postcssBanner from 'postcss-banner';
import pkg from './package.json' assert { type: 'json' };
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

export default
{
  input: 'src/index.js',
  output: {
    file: 'dist/leaflet.canvaslayer.field.esm.js',
    format: 'esm',
    name: 'LeafletCanvasLayerField',
    banner: `/*! LeafletCanvasLayerField v${pkg.version} */`,
    sourcemap: true,
    inlineDynamicImports: true,
    globals:
    {
      leaflet: 'L'
    },
  },
  plugins: [
    alias(
    {
      entries: [
        { find: 'events', replacement: 'rollup-plugin-node-polyfills/polyfills/events.js' }
      ]
    }),
    resolve(
      { 
        browser: true, 
        preferBuiltins: false 
      }),
      commonjs({
        include: /node_modules/,
        transformMixedEsModules: true
      }),
      json(),
      polyfills(),
      postcss({
        plugins: [
          postcssBanner({
            banner: `LeafletCanvasLayerField v${pkg.version}`,
            inline: false
          })
        ],
        extract: path.resolve(__dirname, 'dist/leaflet.canvaslayer.field.css'),
        minimize: false,
        sourceMap: false
      })
  ],
  external: ['leaflet']
};