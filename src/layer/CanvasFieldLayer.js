/**
 * Abstract base for a raster-like field (ScalarField | VectorField) on a canvas.
 * Subclasses must implement onDrawLayer(viewInfo).
 *
 * Responsibilities:
 * - Manage visibility and pointer/identify events
 * - Apply opacity and value filters to the underlying field
 * - Report geographic bounds based on the field's extent
 *
 * @template TField
 */
export class CanvasFieldLayer extends L.CanvasLayer
{
    /**
     * @param {TField} field
     * @param {{
     *   mouseMoveCursor?: { value:string, noValue:string },
     *   opacity?: number,
     *   onClick?: (e:any)=>void,
     *   onMouseMove?: (e:any)=>void,
     *   inFilter?: (v:any)=>boolean,
     *   pane?: string,
     *   zIndex?: number,
     *   mixBlendMode?: string|null,
     *   pointerEvents?: 'auto'|'none'
     * }} [options]
     */
    constructor(field = null, options = {})
    {
        super(options);

        /** @private */ this._visible = true;
        /** @private */ this._field = null;

        if (field)
        {
            this.setData(field);
        }
    }

    // ----- Leaflet lifecycle -------------------------------------------------

    /**
     * Merge base events and add zoom visibility tweaks.
     * @returns {Record<string, Function>}
     */
    getEvents()
    {
        const events = (super.getEvents && super.getEvents()) || {};
        return {
            ...events,
            zoomstart: this._hideCanvas.bind(this),
            zoomend: this._showCanvas.bind(this)
        };
    }

     /** Called by base CanvasLayer after the canvas is mounted */
    onLayerDidMount()
    {
        this._ensureCanvasAlignment();
        this._updateOpacity();
        this._applyPointerEvents();
        this._applyBlendMode();
        this._enableIdentify();
    }

    /** Called by base CanvasLayer before unmount */
    onLayerWillUnmount()
    {
        this._disableIdentify();
    }

    // ----- Public API --------------------------------------------------------

    /** Show canvas & re-enable identify */
    show()
    {
        this._visible = true;
        this._showCanvas();
        this._enableIdentify();
        return this;
    }

    /** Hide canvas & disable identify */
    hide()
    {
        this._visible = false;
        this._hideCanvas();
        this._disableIdentify();
        return this;
    }

    /** @returns {boolean} */
    isVisible()
    {
        return this._visible;
    }

    /** Request a redraw (only if map & field are set) */
    needRedraw()
    {
        if (this._map && this._field)
        {
            super.needRedraw();
        }
    }

    /**
     * Set/replace the underlying field object.
     * @param {TField} field
     */
    setData(field)
    {
        if (this.options.inFilter && field?.setFilter)
        {
            field.setFilter(this.options.inFilter);
        }
        this._field = field;
        this.needRedraw();
        this.fire('load');
        return this;
    }

    /**
     * Set a value filter to apply at the Field level.
     * @param {(v:any)=>boolean} f
     */
    setFilter(f)
    {
        this.options.inFilter = f;
        if (this._field?.setFilter)
        {
            this._field.setFilter(f);
        }
        this.needRedraw();

        return this;
    }

    /**
     * Set canvas opacity.
     * @param {number} opacity
     * @returns {this}
     */
    setOpacity(opacity)
    {
        this.options.opacity = opacity;
        if (this._canvas)
        {
            this._updateOpacity();
        }
        return this;
    }

    /**
     * @returns {L.LatLngBounds}
     */
    getBounds()
    {
        if (!this._field) return L.latLngBounds(); // empty
        const [xmin, ymin, xmax, ymax] = this._field.extent();
        return L.latLngBounds(L.latLng(ymin, xmin), L.latLng(ymax, xmax));
    }

    // ----- Abstract drawing hook --------------------------------------------

    /**
     * Subclasses must implement the drawing routine using the provided viewInfo.
     * @param {object} viewInfo
     * @abstract
     */
    /* eslint-disable-next-line class-methods-use-this */
    onDrawLayer(viewInfo)
    {
        throw new TypeError('onDrawLayer(viewInfo) must be implemented by subclass');
    }

    // ----- Private helpers ---------------------------------------------------

    /** Keep canvas aligned with (0,0) container corner */
    _ensureCanvasAlignment()
    {
        if (!this._map || !this._canvas) return;
        const topLeft = this._map.containerPointToLayerPoint([0, 0]);
        L.DomUtil.setPosition(this._canvas, topLeft);
    }

    _showCanvas()
    {
        if (this._canvas && this._visible)
        {
            this._canvas.style.visibility = 'visible';
            this._canvas.style.display = '';
        }

        this.options.visible = true;
        this.needRedraw();
        return this;
    }

    _hideCanvas()
    {
        if (this._canvas)
        {
            this._canvas.style.visibility = 'hidden';
            this._canvas.style.display = 'none';
        }
        this.options.visible = false;
        return this;
    }

    _enableIdentify()
    {
        if (!this._map) return;

        this._map.on('click', this._onClick, this);
        this._map.on('mousemove', this._onMouseMove, this);

        if (this.options.onClick) this.on('click', this.options.onClick, this);
        if (this.options.onMouseMove) this.on('mousemove', this.options.onMouseMove, this);
    }

    _disableIdentify()
    {
        if (!this._map) return;

        this._map.off('click', this._onClick, this);
        this._map.off('mousemove', this._onMouseMove, this);

        if (this.options.onClick) this.off('click', this.options.onClick, this);
        if (this.options.onMouseMove) this.off('mousemove', this.options.onMouseMove, this);
    }

    _updateOpacity()
    {
        if (!this._canvas) return;

        const op = this.options.opacity ?? 1;
        L.DomUtil.setOpacity(this._canvas, op);
    }

    _applyBlendMode()
    {
        if (this._canvas)
        {
            this._canvas.style.mixBlendMode = this.options.mixBlendMode || '';
        }
    }

    _applyPointerEvents()
    {
        if (this._canvas)
        {
            const mode = this.options.pointerEvents;
            this._canvas.style.pointerEvents = mode === 'none' ? 'none' : 'auto';
        }
    }

    /** @param {L.LeafletMouseEvent} e */
    _onClick(e)
    {
        const v = this._queryValue(e);
        this.fire('click', v);
    }

    /** @param {L.LeafletMouseEvent} e */
    _onMouseMove(e)
    {
        const v = this._queryValue(e);
        this._changeCursorOn(v);
        this.fire('mousemove', v);
    }

    /**
     * @param {{latlng:L.LatLng,value:any}} v
     */
    _changeCursorOn(v)
    {
        const mmc = this.options.mouseMoveCursor;
        if (!mmc) return;

        const { value = 'pointer', noValue = 'default' } = mmc;
        const style = this._map.getContainer().style;
        style.cursor = (v.value !== null && v.value !== undefined) ? value : noValue;
    }

    /** Sample at mouse position (nearest or interpolated based on Field impl) */
    _queryValue(e)
    {
        const v = this._field
            ? this._field.valueAt(e.latlng.lng, e.latlng.lat)
            : null;

        return { latlng: e.latlng, value: v };
    }

    /** Clear and return 2D context */
    _getDrawingContext()
    {
        const g = this._canvas.getContext('2d');
        g.clearRect(0, 0, this._canvas.width, this._canvas.height);
        return g;
    }

    /**
     * Update the mouse-move cursor behavior at runtime.
     * @param {{ value?: string, noValue?: string }} cursorCfg
     * @returns {this}
     */
    setMouseMoveCursor(cursorCfg = {})
    {
        this.options.mouseMoveCursor = {
            ...(this.options.mouseMoveCursor || {}),
            ...cursorCfg
        };
        return this;
    }

    /**
     * Replace identify handlers (click / mousemove) at runtime.
     * Existing handlers are removed and new ones are registered immediately
     * if the layer is on the map.
     * @param {(e:any)=>void} [onClick]
     * @param {(e:any)=>void} [onMouseMove]
     * @returns {this}
     */
    setIdentifyHandlers(onClick, onMouseMove)
    {
        // Detach previous external listeners if present
        if (this.options.onClick) this.off('click', this.options.onClick, this);
        if (this.options.onMouseMove) this.off('mousemove', this.options.onMouseMove, this);

        // Store new handlers
        this.options.onClick = onClick || null;
        this.options.onMouseMove = onMouseMove || null;

        // If mounted, re-wire immediately
        if (this._map)
        {
            if (onClick) this.on('click', onClick, this);
            if (onMouseMove) this.on('mousemove', onMouseMove, this);
        }
        return this;
    }

    /**
     * Move the layer to a different pane.
     * Mirrors Leaflet layer pane switching but ensures canvas alignment.
     * @param {string} paneName
     * @returns {this}
     */
    setPane(paneName)
    {
        if (paneName && paneName !== this.options.pane)
        {
            this.options.pane = paneName;
            if (this._map)
            {
                // Force re-mount on new pane
                const map = this._map;
                map.removeLayer(this);
                map.addLayer(this);
                
                this._ensureCanvasAlignment();
                this._updateOpacity();
                this._applyPointerEvents();
                this._applyBlendMode();
            }
        }
        return this;
    }

    /**
     * Ensure z-index updates affect the underlying canvas element.
     * @param {number} z
     * @returns {this}
     */
    setZIndex(z)
    {
        this.options.zIndex = z;
        if (this._canvas)
        {
            this._canvas.style.zIndex = (z != null) ? String(z) : '';
        }
        // Also call Leaflet's built-in if available
        if (typeof super.setZIndex === 'function')
        {
            super.setZIndex(z);
        }
        return this;
    }

    /**
     * Get the current opacity of the canvas layer.
     * @returns {number}
     */
    getOpacity()
    {
        return this.options.opacity;
    }

    /**
     * Force a complete redraw of the canvas, clearing and re-rendering.
     * Useful if external state changed but data reference remained the same.
     * @returns {this}
     */
    refresh()
    {
        if (this._canvas)
        {
            const g = this._canvas.getContext('2d');
            g.clearRect(0, 0, this._canvas.width, this._canvas.height);
        }
        this.needRedraw();
        return this;
    }

    /**
     * Remove all data bound to this field layer.
     * Clears the field reference and redraws blank.
     * @returns {this}
     */
    clearData()
    {
        this._field = null;
        if (this._canvas)
        {
            const g = this._canvas.getContext('2d');
            g.clearRect(0, 0, this._canvas.width, this._canvas.height);
        }
        this.fire('cleardata');
        this.needRedraw();
        return this;
    }

    /**
     * Export the current field’s data and configuration.
     * @returns {{options:Object, field:Object|null}}
     */
    exportConfig()
    {
        return {
            options: { ...this.options },
            field: this._field ? this._field.params || {} : null
        };
    }

    /**
     * Apply or clear a spatial mask (GeoJSON Polygon/MultiPolygon) used by Field.contains().
     * Triggers a redraw.
     * @param {object|null} geojsonMask
     * @returns {this}
     */
    setSpatialMask(geojsonMask)
    {
        if (this._field && typeof this._field.setSpatialMask === 'function')
        {
            this._field.setSpatialMask(geojsonMask || null);
            this.needRedraw();
        }
        return this;
    }

    /**
     * Fit the map view to the current field bounds.
     * @param {L.FitBoundsOptions} [opts]
     * @returns {this}
     */
    fitToField(opts = {})
    {
        if (this._map && this._field)
        {
            const b = this.getBounds();
            if (b && b.isValid && b.isValid())
            {
                this._map.fitBounds(b, opts);
            }
        }
        return this;
    }

    /**
     * Enable/disable pointer interactivity (click/mousemove identify).
     * @param {boolean} enabled
     * @returns {this}
     */
    setInteractivity(enabled)
    {
        this._interactive = !!enabled;
        if (!this._map) return this;

        if (this._interactive)
        {
            this._enableIdentify();
            this._showCanvas();
        }
        else
        {
            this._disableIdentify();
            // keep canvas visible state as-is; caller can hide() if desired
        }
        return this;
    }

    /**
     * Set a CSS blend mode for the canvas element (e.g. 'multiply', 'screen').
     * Uses the CSS `mix-blend-mode` so it plays nicely with Leaflet panes.
     * @param {string|null} mode
     * @returns {this}
     */
    setCanvasBlendMode(mode)
    {
        this.options.mixBlendMode = mode || null;
        this._applyBlendMode();
        return this;
    }

    /**
     * Get the underlying Field instance bound to this layer.
     * @returns {import('./Field').default|null}
     */
    getField()
    {
        return this._field || null;
    }

    /**
     * Sample the field at a given LatLng.
     * @param {L.LatLng|{lat:number,lng:number}} latlng
     * @param {{ interpolate?: boolean }} [opts]
     * @returns {{ latlng: L.LatLng, value: any }}
     */
    sampleAtLatLng(latlng, opts = {})
    {
        const p = L.latLng(latlng);
        const useInterp = !!opts.interpolate;
        const v = this._field
            ? (useInterp
                ? this._field.interpolatedValueAt(p.lng, p.lat)
                : this._field.valueAt(p.lng, p.lat))
            : null;
        return { latlng: p, value: v };
    }

    /**
     * Export the current canvas as a data URL (PNG by default).
     * @param {('image/png'|'image/jpeg'|'image/webp')} [type='image/png']
     * @param {number} [quality]  // only for image/jpeg or image/webp
     * @returns {string|null}
     */
    captureSnapshot(type = 'image/png', quality)
    {
        if (!this._canvas) return null;
        try
        {
            return this._canvas.toDataURL(type, quality);
        }
        catch (e)
        {
            return null;
        }
    }

    /**
     * Control CSS pointer-events on the canvas element.
     * Useful to temporarily suppress hit-testing while dragging other UI.
     * @param {('auto'|'none')} mode
     * @returns {this}
     */
    setPointerEvents(mode)
    {
        this.options.pointerEvents = mode;
        this._applyPointerEvents();
        return this;
    }
}