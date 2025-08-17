import L from 'leaflet';

// expose base classes/types on window.L for IIFE consumers
import Vector from './Vector.js';
import Cell from './Cell.js';
import Field from './Field.js';
import ScalarField from './ScalarField.js';
import VectorField from './VectorField.js';

if (typeof window !== 'undefined' && window.L)
{
  window.L.Vector = Vector;
  window.L.Cell = Cell;
  window.L.Field = Field;
  window.L.ScalarField = ScalarField;
  window.L.VectorField = VectorField;
  window.L.CanvasLayer = window.L.CanvasLayer || L.CanvasLayer;
  // non-conflicting name to avoid clobbering other plugins
  window.L.CanvasLayer.FieldClass = CanvasFieldLayer;
}

import './layer/L.CanvasLayer.js';
import './layer/L.CanvasLayer.SimpleLonLat.js';
import './layer/L.CanvasLayer.Field.js';
import './layer/L.CanvasLayer.ScalarField.js';
import './layer/L.CanvasLayer.VectorFieldAnim.js';

// controls (also side‑effect files)
import './control/L.Control.ColorBar.js';
import './control/L.Control.LayersPlayer.js';

// export default CanvasFieldLayer;

export { Cell } from './Cell.js'
export { CanvasLayer } from './layer/L.CanvasLayer.js';
export { Field }       from './Field.js';
export { ScalarField } from './ScalarField.js';
export { VectorField } from './VectorField.js';
export { VectorFieldAnim } from './layer/L.CanvasLayer.VectorFieldAnim.js';
export { ColorBarControl } from './control/L.Control.ColorBar.js';
export { LayersPlayerControl } from './control/L.Control.LayersPlayer.js';