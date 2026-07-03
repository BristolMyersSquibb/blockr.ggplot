// @ts-check
/**
 * gg-blocks.js — JS-first settings UI for blockr.ggplot blocks.
 *
 * Pattern follows blockr.viz/inst/js/chart.js (the settings-band pilot): a
 * Shiny.InputBinding binds the block's expr-UI container, builds the gear
 * header + in-flow settings band, and hands rendering of the band to the
 * shared Blockr.DrilldownConfig engine. Unlike blockr.viz (client-side
 * ECharts), the plot itself renders server-side via Shiny's plotOutput — the
 * JS side owns ONLY the configuration UI and echoes config changes back to R
 * through a single `<id>_action` input.
 *
 * The GG_ROLES / GG_TYPE_ROLES specs mirror the `chart_aesthetics` list in
 * R/ggplot-block.R (the authority for expression generation) — keep in sync.
 */
(() => {
  'use strict';

  // -- ggplot block spec ------------------------------------------------------

  // Role catalog: every config key the ggplot block exposes. Aesthetic
  // mappings are column pickers open to any column type (parity with the old
  // Shiny selects, which offered every column; the R expression wraps
  // shape/linetype/discrete-fill in as.factor() where needed).
  const GG_ROLES = {
    x:        { label: 'X-axis',       kind: 'column', colType: 'any' },
    y:        { label: 'Y-axis',       kind: 'column', colType: 'any' },
    color:    { label: 'Color by',     kind: 'column', colType: 'any' },
    fill:     { label: 'Fill by',      kind: 'column', colType: 'any' },
    size:     { label: 'Size by',      kind: 'column', colType: 'any' },
    shape:    { label: 'Shape by',     kind: 'column', colType: 'any' },
    linetype: { label: 'Line type by', kind: 'column', colType: 'any' },
    group:    { label: 'Group by',     kind: 'column', colType: 'any' },
    alpha:    { label: 'Alpha by',     kind: 'column', colType: 'any' },
    position: {
      label: 'Position', kind: 'select',
      // Per-type option lists — context() is the chart type.
      optionsBy: {
        bar: ['stack', 'dodge', 'fill'],
        histogram: ['stack', 'identity', 'dodge']
      }
    },
    // Bounds match the old numericInput(min = 1, max = 100).
    bins:          { label: 'Bins',    kind: 'slider', min: 1, max: 100, step: 1, unit: '' },
    density_alpha: { label: 'Opacity', kind: 'slider', min: 0, max: 1, step: 0.05, unit: '' },
    donut: {
      label: 'Donut', kind: 'segmented',
      options: [
        { value: 'on', label: 'Donut chart style' },
        { value: 'off', label: 'Off' }
      ]
    }
  };

  // Per-chart-type sections — a direct translation of `chart_aesthetics`
  // (R/ggplot-block.R), with the old show/hide special cases baked in:
  // density has no variable alpha/group (fixed-opacity slider; group is
  // derived from fill), bar/histogram expose position, histogram bins,
  // pie the donut toggle.
  /** @type {Record<string, { requiredMap: string[], optionalMap: string[], mapping: any[], presentation: any[] }>} */
  const GG_TYPE_ROLES = {
    point:     { requiredMap: ['x', 'y'], optionalMap: ['color', 'shape', 'size', 'alpha', 'fill'], mapping: [], presentation: [] },
    bar:       { requiredMap: ['x'],      optionalMap: ['y', 'fill', 'color', 'alpha'],             mapping: [], presentation: ['position'] },
    line:      { requiredMap: ['x', 'y'], optionalMap: ['color', 'linetype', 'alpha', 'group'],     mapping: [], presentation: [] },
    boxplot:   { requiredMap: ['x', 'y'], optionalMap: ['fill', 'color', 'alpha'],                  mapping: [], presentation: [] },
    violin:    { requiredMap: ['x', 'y'], optionalMap: ['fill', 'color', 'alpha'],                  mapping: [], presentation: [] },
    density:   { requiredMap: ['x'],      optionalMap: ['fill'],                                    mapping: [], presentation: ['density_alpha'] },
    area:      { requiredMap: ['x', 'y'], optionalMap: ['fill', 'alpha'],                           mapping: [], presentation: [] },
    histogram: { requiredMap: ['x'],      optionalMap: ['fill', 'color', 'alpha'],                  mapping: [], presentation: ['bins', 'position'] },
    pie:       { requiredMap: ['x'],      optionalMap: ['y', 'fill', 'alpha'],                      mapping: [], presentation: ['donut'] }
  };

  // Same order as the old icon strip.
  const GG_TYPE_ORDER = [
    'point', 'bar', 'line', 'boxplot', 'pie',
    'histogram', 'density', 'violin', 'area'
  ];

  /** @param {string} t */
  const ggSections = (t) => GG_TYPE_ROLES[t] || GG_TYPE_ROLES.point;

  /** @type {Record<string, any>} */
  const SPECS = {
    ggplot: {
      roles: GG_ROLES,
      typeKey: 'type',
      typeGroups: [{ label: 'Type', types: GG_TYPE_ORDER }],
      sectionsFor: ggSections,
      title: 'Chart settings',
      // Keys echoed back to R on every change (full-config echo, like the
      // blockr.viz chart). Must match the config list the R server pushes.
      configKeys: [
        'type', 'x', 'y', 'color', 'fill', 'size', 'shape', 'linetype',
        'group', 'alpha', 'density_alpha', 'position', 'bins', 'donut'
      ]
    }
  };

  // No paired-tail roles in any ggplot spec (viz pairs metric+agg etc.).
  const GG_SECONDARY = new Set();

  // -- host -------------------------------------------------------------------

  class GgBlock {
    /** @param {HTMLElement} el */
    constructor(el) {
      this.el = el;
      const kind = el.getAttribute('data-gg-block') || 'ggplot';
      this.spec = SPECS[kind] || SPECS.ggplot;
      /** @type {any[]} */
      this.columns = [];
      /** @type {Record<string, any>} */
      this.config = {};
      this._open = false;
      /** @type {HTMLButtonElement} */
      this.gearBtn;
      /** @type {HTMLDivElement} */
      this.bandEl;
      this._buildDOM();
      this._cfg = this._makeConfig();
    }

    // Append-only DOM build: the container may hold R-rendered children (the
    // grid/facet blocks keep their SVG layout preview inside it), so the gear
    // header + band are inserted BEFORE any existing content, never replacing
    // it.
    _buildDOM() {
      const gearHeader = document.createElement('div');
      gearHeader.className = 'blockr-gear-header';
      this.gearBtn = document.createElement('button');
      this.gearBtn.type = 'button';
      this.gearBtn.className = 'blockr-gear-btn';
      this.gearBtn.innerHTML = (typeof Blockr !== 'undefined' && Blockr.icons)
        ? Blockr.icons.gear : '⚙';
      this.gearBtn.title = this.spec.title;
      this.gearBtn.setAttribute('aria-label', this.spec.title);
      this.gearBtn.setAttribute('aria-haspopup', 'dialog');
      this.gearBtn.setAttribute('aria-expanded', 'false');
      this.gearBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        this._toggleBand();
      });
      gearHeader.appendChild(this.gearBtn);

      // In-flow settings band (design-system: gear-panel proposal B). No
      // <body> portal, no fixed positioning, no outside-click dismissal —
      // opening pushes the plot below down so the result stays visible.
      this.bandEl = document.createElement('div');
      this.bandEl.className = 'blockr-settings dd-popover gg-settings';

      this.el.insertBefore(this.bandEl, this.el.firstChild);
      this.el.insertBefore(gearHeader, this.bandEl);
    }

    _makeConfig() {
      const DCfg = /** @type {typeof VizDrilldownConfig} */ (
        (typeof Blockr !== 'undefined' && Blockr.DrilldownConfig) ||
        window.DrilldownConfig);
      return new DCfg({
        popoverEl: () => this.bandEl,
        roles: this.spec.roles,
        config: () => this.config,
        columns: () => this.columns,
        // context keys the per-type extras (position optionsBy).
        context: () => this.config[this.spec.typeKey],
        currentType: () => this.config[this.spec.typeKey] || null,
        sections: () => this.spec.sectionsFor(this.config[this.spec.typeKey]),
        sectionsForFamily: (/** @type {string} */ fam) => this.spec.sectionsFor(fam),
        secondary: GG_SECONDARY,
        typeKey: this.spec.typeKey,
        typeGroups: this.spec.typeGroups,
        // Each chart type is its own "family": a type switch runs the
        // engine's carry logic (keep fitting mappings, stash the rest in
        // sticky role memory).
        familyFor: (/** @type {string} */ t) => t,
        title: this.spec.title,
        // The plot renders server-side: any change just echoes the config to
        // R; the state reactives re-run the plot expression.
        onChange: () => this._sendConfig(),
        onMults: () => this._sendConfig(),
        onClearFilter: () => {},
        // No default-column auto-picking: the block starts with a blank plot
        // until the user maps x (behavior parity with the old Shiny UI).
        ensureDefaults: () => {},
        isOpen: () => this._open,
        reopen: () => this._openBand()
      });
    }

    /**
     * @param {{ columns?: any[], config?: Record<string, any>,
     *           choices?: Record<string, any[]> }} msg
     */
    setData(msg) {
      this.columns = msg.columns || [];
      this.config = Object.assign({}, msg.config);
      // Runtime-dependent select options (e.g. the theme block's base_theme
      // list, gated on installed packages) travel with the push message.
      if (msg.choices) {
        for (const key of Object.keys(msg.choices)) {
          if (this.spec.roles[key]) this.spec.roles[key].options = msg.choices[key];
        }
      }
      this._cfg.render();
    }

    _sendConfig() {
      if (!this.el.id) return;
      /** @type {Record<string, any>} */
      const out = { action: 'config' };
      for (const k of this.spec.configKeys) {
        const v = this.config[k];
        out[k] = (v === null || v === undefined) ? '' : v;
      }
      Shiny.setInputValue(this.el.id + '_action', out, { priority: 'event' });
    }

    // -- band toggle (class flips only; the band is in flow) ------------------
    _toggleBand() {
      this._open ? this._closeBand() : this._openBand();
    }
    _openBand() {
      this.bandEl.classList.add('blockr-settings--open');
      this.el.classList.add('gg-settings-open');
      this._open = true;
      this.gearBtn.classList.add('blockr-gear-active');
      this.gearBtn.setAttribute('aria-expanded', 'true');
    }
    _closeBand() {
      this.bandEl.classList.remove('blockr-settings--open');
      this.el.classList.remove('gg-settings-open');
      this._open = false;
      this.gearBtn.classList.remove('blockr-gear-active');
      this.gearBtn.setAttribute('aria-expanded', 'false');
    }
  }

  // -- Shiny binding ----------------------------------------------------------

  const binding = new Shiny.InputBinding();
  Object.assign(binding, {
    find: (/** @type {any} */ scope) => $(scope).find('.gg-block-container'),
    getId: (/** @type {any} */ el) => el.id || null,
    getValue: () => null,
    subscribe: () => {},
    unsubscribe: () => {},
    initialize: (/** @type {any} */ el) => {
      el._block = new GgBlock(el);
      if (el._pendingData) {
        el._block.setData(el._pendingData);
        delete el._pendingData;
      }
    }
  });
  Shiny.inputBindings.register(binding, 'blockr.ggplot');

  // Data/config push from R. The element may not exist or not be bound yet
  // (dock panels, lazily revealed views) — buffer on the element or poll
  // briefly, exactly like blockr.viz's drilldown-data handler.
  Shiny.addCustomMessageHandler('gg-block-data', (/** @type {any} */ msg) => {
    const el = /** @type {any} */ (document.getElementById(msg.id));
    if (el?._block) {
      el._block.setData(msg);
    } else if (el) {
      el._pendingData = msg;
    } else {
      let n = 0;
      const t = setInterval(() => {
        n++;
        const el2 = /** @type {any} */ (document.getElementById(msg.id));
        if (el2?._block) { el2._block.setData(msg); clearInterval(t); }
        else if (el2) { el2._pendingData = msg; clearInterval(t); }
        if (n > 50) clearInterval(t);
      }, 100);
    }
  });
})();
