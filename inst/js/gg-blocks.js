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
    // x/y are required, so empty is not a configuration to name: the
    // placeholder says what to supply. The optional roles below are never
    // blank — they carry a `(none)` option which displays when unset.
    x:        { label: 'X-axis',       kind: 'column', colType: 'any',
                ph: 'Select column…' },
    y:        { label: 'Y-axis',       kind: 'column', colType: 'any',
                ph: 'Select column…' },
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

  // Main-vs-advanced split: ALL optional aesthetics live in the main area's
  // "+ Add mapping" dropdown (they only show once added, so they don't
  // clutter it — no need for a separate advanced tier like the pre-band
  // UI had). The advanced band holds only the chart-specific presentation
  // extras (position, bins, donut, density opacity); for types without
  // any, the gear is hidden entirely.
  /** @param {string} t */
  const ggMainFor = (t) => {
    const s = ggSections(t);
    return {
      requiredMap: s.requiredMap,
      optionalMap: s.optionalMap,
      mapping: [],
      presentation: []
    };
  };

  /** @param {string} t */
  const ggAdvFor = (t) => {
    const s = ggSections(t);
    return {
      requiredMap: [],
      optionalMap: [],
      mapping: [],
      presentation: s.presentation
    };
  };

  // Subtle inline chart-type icons (successors of the old FontAwesome strip;
  // hand-drawn so no icon-font dependency). 14px, currentColor, dimmed via
  // CSS so the text label stays primary.
  /** @type {Record<string, string>} */
  const GG_TYPE_ICONS = {
    point:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor">' +
      '<circle cx="4" cy="11" r="1.6"/><circle cx="8" cy="6" r="1.6"/>' +
      '<circle cx="12" cy="9" r="1.6"/><circle cx="13" cy="3" r="1.6"/></svg>',
    bar:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor">' +
      '<rect x="2" y="8" width="3" height="6"/><rect x="6.5" y="4" width="3" height="10"/>' +
      '<rect x="11" y="10" width="3" height="4"/></svg>',
    line:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="1.6" stroke-linecap="round">' +
      '<path d="M2 12 L6 7 L10 9 L14 3"/></svg>',
    boxplot:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="1.4">' +
      '<rect x="4" y="5" width="8" height="6"/>' +
      '<path d="M4 8 h8 M8 2 v3 M8 11 v3 M6 2 h4 M6 14 h4"/></svg>',
    pie:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="1.4">' +
      '<circle cx="8" cy="8" r="6"/><path d="M8 8 V2 M8 8 L13 11"/></svg>',
    histogram:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor">' +
      '<rect x="2" y="9" width="3" height="5"/><rect x="5" y="5" width="3" height="9"/>' +
      '<rect x="8" y="7" width="3" height="7"/><rect x="11" y="11" width="3" height="3"/></svg>',
    density:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="1.6" stroke-linecap="round">' +
      '<path d="M2 13 C5 13 5 4 8 4 C11 4 11 13 14 13"/></svg>',
    violin:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="1.4">' +
      '<path d="M8 2 C10 5 12 6 12 9 C12 12 10 14 8 14 C6 14 4 12 4 9 C4 6 6 5 8 2 Z"/></svg>',
    area:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor">' +
      '<path d="M2 14 L2 11 L6 6 L10 8 L14 3 L14 14 Z" opacity="0.85"/></svg>'
  };

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

  // Main-vs-advanced split, mirroring the pre-band theme UI: base theme,
  // legend and palettes were always visible; colors & backgrounds,
  // typography and grid & borders sat behind "Show advanced options".
  const THEME_MAIN = {
    requiredMap: [],
    optionalMap: [],
    mapping: [],
    presentation: [
      'base_theme', 'legend_position', 'palette_fill', 'palette_colour'
    ]
  };
  const THEME_ADV = {
    requiredMap: [],
    optionalMap: [],
    mapping: [],
    presentation: [
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
    // No `hint`: the label names the field and the picker lists the columns,
    // so a help line here would only restate them (design-system ux-principles,
    // "Help text earns its place").
    facets: { label: 'Facet by', kind: 'columns', placeholder: 'None' },
    rows:   { label: 'Rows', kind: 'columns', placeholder: 'None' },
    cols:   { label: 'Columns', kind: 'columns', placeholder: 'None' },
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

  // Main-vs-advanced split, mirroring the pre-band facet UI: variables,
  // layout (ncol/nrow) and scales were always visible; labels, direction
  // and space sat behind "Show advanced options".
  /** @type {Record<string, { requiredMap: string[], optionalMap: string[], mapping: any[], presentation: any[] }>} */
  const FACET_TYPE_MAIN = {
    wrap: {
      // facets is required-empty: facet_wrap with no variables is a
      // pass-through, so the field gets the soft amber cue (the R-side
      // preview keeps a muted one-line hint, no warning banner).
      requiredMap: ['facets'], optionalMap: [],
      mapping: [],
      presentation: ['ncol', 'nrow', 'scales']
    },
    grid: {
      requiredMap: [], optionalMap: [],
      mapping: ['rows', 'cols'],
      presentation: ['scales']
    }
  };
  /** @type {Record<string, { requiredMap: string[], optionalMap: string[], mapping: any[], presentation: any[] }>} */
  const FACET_TYPE_ADV = {
    wrap: {
      requiredMap: [], optionalMap: [], mapping: [],
      presentation: ['labeller', 'dir']
    },
    grid: {
      requiredMap: [], optionalMap: [], mapping: [],
      presentation: ['labeller', 'space']
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

  // Each block spec provides TWO section functions: `mainFor` renders into
  // the always-visible main area (the pre-band "main UI"), `advFor` into the
  // gear-toggled advanced band (null = block has no advanced settings, so no
  // gear at all). `fullFor` is the union spec the engine's type-switch carry
  // logic runs against (so a type change preserves advanced mappings too).
  /** @type {Record<string, any>} */
  const SPECS = {
    ggplot: {
      roles: GG_ROLES,
      typeKey: 'type',
      typeGroups: [{ label: 'Chart type', types: GG_TYPE_ORDER }],
      typeIcons: GG_TYPE_ICONS,
      // Icon tile grid (design-system type-picker proposal B).
      typeTiles: true,
      mainFor: ggMainFor,
      advFor: ggAdvFor,
      fullFor: ggSections,
      title: 'Chart settings',
      advTitle: 'Advanced options',
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
      mainFor: () => THEME_MAIN,
      advFor: () => THEME_ADV,
      fullFor: () => THEME_MAIN,
      title: 'Theme settings',
      advTitle: 'Advanced options',
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
      mainFor: (/** @type {string} */ t) =>
        FACET_TYPE_MAIN[t] || FACET_TYPE_MAIN.wrap,
      advFor: (/** @type {string} */ t) =>
        FACET_TYPE_ADV[t] || FACET_TYPE_ADV.wrap,
      fullFor: (/** @type {string} */ t) =>
        FACET_TYPE_MAIN[t] || FACET_TYPE_MAIN.wrap,
      title: 'Facet settings',
      advTitle: 'Advanced options',
      configKeys: [
        'facet_type', 'facets', 'rows', 'cols', 'ncol', 'nrow',
        'scales', 'labeller', 'dir', 'space'
      ]
    },
    grid: {
      roles: GRID_ROLES,
      typeKey: null,
      typeGroups: null,
      // The pre-band grid UI had no advanced toggle — everything visible.
      mainFor: () => GRID_SECTIONS,
      advFor: null,
      fullFor: () => GRID_SECTIONS,
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
      /** @type {HTMLButtonElement | null} */
      this.gearBtn = null;
      /** @type {HTMLDivElement | null} */
      this.gearHeaderEl = null;
      /** @type {HTMLDivElement} */
      this.mainEl;
      /** @type {HTMLDivElement | null} */
      this.advEl = null;
      /** @type {any} */
      this._cfgAdv = null;
      this._buildDOM();
      // Advanced engine first: the main engine's afterTypeChange re-renders
      // it, so it must exist before the first main render.
      if (this.advEl) this._cfgAdv = this._makeEngine('adv');
      this._cfgMain = this._makeEngine('main');
    }

    // Append-only DOM build: the container may hold R-rendered children (the
    // grid/facet blocks keep their SVG layout preview inside it), so the
    // settings areas are inserted BEFORE any existing content, never
    // replacing it. Two areas: the always-visible main band (the pre-band
    // "main UI") and, when the spec has advanced settings, a gear-toggled
    // advanced band below it.
    _buildDOM() {
      const first = this.el.firstChild;

      if (this.spec.advFor) {
        const gearHeader = document.createElement('div');
        this.gearHeaderEl = gearHeader;
        gearHeader.className = 'blockr-gear-header';
        const gear = document.createElement('button');
        this.gearBtn = gear;
        gear.type = 'button';
        gear.className = 'blockr-gear-btn';
        gear.innerHTML = (typeof Blockr !== 'undefined' && Blockr.icons)
          ? Blockr.icons.gear : '⚙';
        gear.title = this.spec.advTitle;
        gear.setAttribute('aria-label', this.spec.advTitle);
        gear.setAttribute('aria-haspopup', 'dialog');
        gear.setAttribute('aria-expanded', 'false');
        gear.addEventListener('click', (e) => {
          e.stopPropagation();
          this._toggleBand();
        });
        gearHeader.appendChild(gear);
        this.el.insertBefore(gearHeader, first);
      }

      if (this.spec.advFor) {
        // In-flow advanced band, opening directly under the gear header
        // (placement P1 + connector T1 of the type-picker proposals: a beak
        // on the band points at the gear, which stays active while open).
        // No <body> portal, no fixed positioning; opening pushes the main
        // settings and plot down so everything stays visible.
        this.advEl = document.createElement('div');
        this.advEl.className =
          'blockr-settings blockr-settings--beak dd-popover gg-settings gg-settings-adv';
        this.el.insertBefore(this.advEl, first);
      }

      // Always-open main area: same band layout, lighter chrome (no box).
      this.mainEl = document.createElement('div');
      this.mainEl.className =
        'blockr-settings blockr-settings--open dd-popover gg-settings gg-settings-main';
      this.el.insertBefore(this.mainEl, first);
    }

    /** @param {'main' | 'adv'} which */
    _makeEngine(which) {
      const DCfg = /** @type {typeof VizDrilldownConfig} */ (
        (typeof Blockr !== 'undefined' && Blockr.DrilldownConfig) ||
        window.DrilldownConfig);
      const main = which === 'main';
      const cur = () => this.spec.typeKey
        ? this.config[this.spec.typeKey] : null;
      return new DCfg({
        popoverEl: () => (main ? this.mainEl : /** @type {HTMLDivElement} */ (this.advEl)),
        roles: this.spec.roles,
        config: () => this.config,
        columns: () => this.columns,
        // context keys per-type extras (position optionsBy). Blocks without
        // a type picker use their block kind as a constant context.
        context: () => this.spec.typeKey
          ? this.config[this.spec.typeKey]
          : (this.el.getAttribute('data-gg-block') || 'ggplot'),
        currentType: () => cur() || null,
        sections: () => (main ? this.spec.mainFor : this.spec.advFor)(cur()),
        // The type-switch carry runs against the FULL per-type spec so
        // advanced mappings survive a switch too.
        sectionsForFamily: (/** @type {string} */ fam) => this.spec.fullFor(fam),
        secondary: GG_SECONDARY,
        // Type picker (and its carry logic) lives on the main band only.
        typeKey: main ? this.spec.typeKey : null,
        typeGroups: main ? this.spec.typeGroups : null,
        typeIcon: (/** @type {string} */ t) =>
          (this.spec.typeIcons && this.spec.typeIcons[t]) || '',
        typeTiles: main ? !!this.spec.typeTiles : false,
        // Each chart type is its own "family": a type switch runs the
        // engine's carry logic (keep fitting mappings, stash the rest in
        // sticky role memory).
        familyFor: (/** @type {string} */ t) => t,
        title: main ? this.spec.title : this.spec.advTitle,
        // The main band re-renders on type switches; keep the advanced band
        // in sync (it shows the type-specific extras) and hide the gear
        // entirely when the current type has no advanced settings.
        afterTypeChange: main
          ? () => {
            if (this._cfgAdv) this._cfgAdv.render();
            this._syncAdvVisibility();
          }
          : undefined,
        // The plot renders server-side: any change just echoes the config to
        // R; the state reactives re-run the plot expression.
        onChange: () => this._sendConfig(),
        onMults: () => this._sendConfig(),
        onClearFilter: () => {},
        // No default-column auto-picking: the block starts with a blank plot
        // until the user maps x (behavior parity with the old Shiny UI).
        ensureDefaults: () => {},
        isOpen: () => (main ? true : this._open),
        reopen: () => { if (!main) this._openBand(); }
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
      // Main render re-renders the advanced band via afterTypeChange.
      this._cfgMain.render();
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

    // Hide the gear (and close the band) when the current type has no
    // advanced settings — e.g. point/line/boxplot have no chart-specific
    // extras, so there is nothing behind the gear.
    _syncAdvVisibility() {
      if (!this.advEl || !this.gearHeaderEl) return;
      const cur = this.spec.typeKey ? this.config[this.spec.typeKey] : null;
      const s = this.spec.advFor(cur);
      const empty = !s || (
        !s.requiredMap.length && !s.optionalMap.length &&
        !s.mapping.length && !s.presentation.length
      );
      if (empty && this._open) this._closeBand();
      this.gearHeaderEl.style.display = empty ? 'none' : '';
    }

    // -- advanced-band toggle (class flips only; the band is in flow) ---------
    _toggleBand() {
      this._open ? this._closeBand() : this._openBand();
    }
    _openBand() {
      if (!this.advEl || !this.gearBtn) return;
      this.advEl.classList.add('blockr-settings--open');
      this.el.classList.add('gg-settings-open');
      this._open = true;
      this.gearBtn.classList.add('blockr-gear-active');
      this.gearBtn.setAttribute('aria-expanded', 'true');
    }
    _closeBand() {
      if (!this.advEl || !this.gearBtn) return;
      this.advEl.classList.remove('blockr-settings--open');
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
