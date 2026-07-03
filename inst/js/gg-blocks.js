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

  // -- theme block spec ---------------------------------------------------
  // Mirrors the constructor args / expr logic in R/theme-block.R. base_theme
  // options are runtime-dependent (gated on installed theme packages), so
  // the R server ships them in the push message (`choices`), not here.

  /** @type {Record<string, any>} */
  const THEME_ROLES = {
    base_theme:      { label: 'Base theme',      kind: 'select', options: [] },
    legend_position: {
      label: 'Legend position', kind: 'select',
      options: [
        { value: 'auto', label: 'Auto (theme default)' },
        { value: 'right', label: 'Right' },
        { value: 'left', label: 'Left' },
        { value: 'top', label: 'Top' },
        { value: 'bottom', label: 'Bottom' },
        { value: 'none', label: 'None' }
      ]
    },
    palette_fill:    { label: 'Fill palette',    kind: 'select', options: [] },
    palette_colour:  { label: 'Colour palette',  kind: 'select', options: [] },
    panel_bg:        { label: 'Panel background', kind: 'color' },
    plot_bg:         { label: 'Plot background',  kind: 'color' },
    grid_color:      { label: 'Grid color',       kind: 'color' },
    base_size: {
      label: 'Base font size', kind: 'select',
      options: [
        { value: 'auto', label: 'Auto (theme default)' },
        '8', '9', '10', '11', '12', '13', '14', '16', '18', '20'
      ]
    },
    base_family: {
      label: 'Font family', kind: 'select',
      options: [
        { value: 'auto', label: 'Auto (theme default)' },
        { value: 'sans', label: 'Sans serif' },
        { value: 'serif', label: 'Serif' },
        { value: 'mono', label: 'Monospace' }
      ]
    },
    show_major_grid: {
      label: 'Major grid', kind: 'segmented',
      options: [
        { value: 'auto', label: 'Auto' },
        { value: 'show', label: 'Show' },
        { value: 'hide', label: 'Hide' }
      ]
    },
    show_minor_grid: {
      label: 'Minor grid', kind: 'segmented',
      options: [
        { value: 'auto', label: 'Auto' },
        { value: 'show', label: 'Show' },
        { value: 'hide', label: 'Hide' }
      ]
    },
    show_panel_border: {
      label: 'Panel border', kind: 'segmented',
      options: [
        { value: 'auto', label: 'Auto' },
        { value: 'show', label: 'Show' },
        { value: 'hide', label: 'Hide' }
      ]
    }
  };

  // Both viridis palettes share the same option list.
  const PALETTE_OPTIONS = [
    { value: 'auto', label: 'Auto (keep upstream)' },
    { value: 'viridis_d', label: 'Viridis (categorical)' },
    { value: 'viridis_c', label: 'Viridis (continuous)' },
    { value: 'magma_d', label: 'Magma (categorical)' },
    { value: 'magma_c', label: 'Magma (continuous)' },
    { value: 'plasma_d', label: 'Plasma (categorical)' },
    { value: 'plasma_c', label: 'Plasma (continuous)' },
    { value: 'ggplot2', label: 'ggplot2 default' }
  ];
  THEME_ROLES.palette_fill.options = PALETTE_OPTIONS;
  THEME_ROLES.palette_colour.options = PALETTE_OPTIONS.slice();

  // Ordered by causality: theme -> legend/palettes -> colors -> typography
  // -> grid & border.
  const THEME_SECTIONS = {
    requiredMap: [],
    optionalMap: [],
    mapping: [],
    presentation: [
      'base_theme', 'legend_position', 'palette_fill', 'palette_colour',
      'panel_bg', 'plot_bg', 'base_size', 'base_family',
      'show_major_grid', 'show_minor_grid', 'grid_color', 'show_panel_border'
    ]
  };

  // -- facet block spec -----------------------------------------------------
  // Mirrors R/facet-block.R: facet_wrap ("wrap") vs facet_grid ("grid"),
  // toggled by the engine's type picker instead of the old radio buttons +
  // shinyjs show/hide. The multi-column pickers are `columns` roles
  // (Blockr.Select.multi; empty selection = no faceting).

  /** @type {Record<string, any>} */
  const FACET_ROLES = {
    facets: { label: 'Facet by', kind: 'columns', placeholder: 'None' },
    rows:   { label: 'Rows',     kind: 'columns', placeholder: 'None' },
    cols:   { label: 'Columns',  kind: 'columns', placeholder: 'None' },
    ncol: {
      label: 'Columns', kind: 'select', ph: 'Auto',
      options: [{ value: '', label: 'Auto' }, '1', '2', '3', '4', '5']
    },
    nrow: {
      label: 'Rows', kind: 'select', ph: 'Auto',
      options: [{ value: '', label: 'Auto' }, '1', '2', '3', '4', '5']
    },
    scales: {
      label: 'Scales', kind: 'select',
      options: [
        { value: 'fixed', label: 'Fixed' },
        { value: 'free', label: 'Free' },
        { value: 'free_x', label: 'Free X' },
        { value: 'free_y', label: 'Free Y' }
      ]
    },
    labeller: {
      label: 'Labels', kind: 'select',
      options: [
        { value: 'label_value', label: 'Value only' },
        { value: 'label_both', label: 'Variable and value' },
        { value: 'label_parsed', label: 'Parsed expressions' }
      ]
    },
    dir: {
      // Two distinct values, not on/off -> stays a cycling pill.
      label: 'Direction', kind: 'segmented',
      options: [
        { value: 'h', label: 'Horizontal' },
        { value: 'v', label: 'Vertical' }
      ]
    },
    space: {
      label: 'Space', kind: 'select',
      options: [
        { value: 'fixed', label: 'Fixed' },
        { value: 'free_x', label: 'Free X' },
        { value: 'free_y', label: 'Free Y' }
      ]
    }
  };

  /** @type {Record<string, { requiredMap: string[], optionalMap: string[], mapping: any[], presentation: any[] }>} */
  const FACET_TYPE_ROLES = {
    wrap: {
      requiredMap: [], optionalMap: [],
      mapping: ['facets'],
      presentation: ['ncol', 'nrow', 'scales', 'labeller', 'dir']
    },
    grid: {
      requiredMap: [], optionalMap: [],
      mapping: ['rows', 'cols'],
      presentation: ['scales', 'labeller', 'space']
    }
  };

  // -- grid block spec ------------------------------------------------------
  // Mirrors R/grid-block.R (patchwork panel layout): no columns, no type
  // picker — layout selects, annotation text inputs, auto-tag style.

  /** @type {Record<string, any>} */
  const GRID_ROLES = {
    ncol: {
      label: 'Columns', kind: 'select', ph: 'Auto',
      options: [{ value: '', label: 'Auto' }, '1', '2', '3', '4', '5']
    },
    nrow: {
      label: 'Rows', kind: 'select', ph: 'Auto',
      options: [{ value: '', label: 'Auto' }, '1', '2', '3', '4', '5']
    },
    guides: {
      label: 'Legends', kind: 'select',
      options: [
        { value: 'auto', label: 'Auto' },
        { value: 'collect', label: 'Collect' },
        { value: 'keep', label: 'Keep separate' }
      ]
    },
    title:    { label: 'Title',    kind: 'text' },
    subtitle: { label: 'Subtitle', kind: 'text' },
    caption:  { label: 'Caption',  kind: 'text' },
    tag_levels: {
      label: 'Auto-tag plots', kind: 'select', ph: 'None',
      options: [
        { value: '', label: 'None' },
        { value: 'A', label: 'A, B, C…' },
        { value: 'a', label: 'a, b, c…' },
        { value: '1', label: '1, 2, 3…' },
        { value: 'I', label: 'I, II, III…' },
        { value: 'i', label: 'i, ii, iii…' }
      ]
    }
  };

  const GRID_SECTIONS = {
    requiredMap: [],
    optionalMap: [],
    mapping: [],
    presentation: [
      'ncol', 'nrow', 'guides', 'title', 'subtitle', 'caption', 'tag_levels'
    ]
  };

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
    },
    theme: {
      roles: THEME_ROLES,
      typeKey: null,
      typeGroups: null,
      sectionsFor: () => THEME_SECTIONS,
      title: 'Theme settings',
      configKeys: [
        'base_theme', 'legend_position', 'palette_fill', 'palette_colour',
        'panel_bg', 'plot_bg', 'base_size', 'base_family',
        'show_major_grid', 'show_minor_grid', 'grid_color',
        'show_panel_border'
      ]
    },
    facet: {
      roles: FACET_ROLES,
      typeKey: 'facet_type',
      typeGroups: [{ label: 'Layout', types: ['wrap', 'grid'] }],
      sectionsFor: (/** @type {string} */ t) =>
        FACET_TYPE_ROLES[t] || FACET_TYPE_ROLES.wrap,
      title: 'Facet settings',
      configKeys: [
        'facet_type', 'facets', 'rows', 'cols', 'ncol', 'nrow',
        'scales', 'labeller', 'dir', 'space'
      ]
    },
    grid: {
      roles: GRID_ROLES,
      typeKey: null,
      typeGroups: null,
      sectionsFor: () => GRID_SECTIONS,
      title: 'Grid settings',
      configKeys: [
        'ncol', 'nrow', 'guides', 'title', 'subtitle', 'caption',
        'tag_levels'
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
        // context keys per-type extras (position optionsBy). Blocks without
        // a type picker use their block kind as a constant context.
        context: () => this.spec.typeKey
          ? this.config[this.spec.typeKey]
          : (this.el.getAttribute('data-gg-block') || 'ggplot'),
        currentType: () => this.spec.typeKey
          ? (this.config[this.spec.typeKey] || null) : null,
        sections: () => this.spec.sectionsFor(
          this.spec.typeKey ? this.config[this.spec.typeKey] : null),
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
