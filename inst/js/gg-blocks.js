// @ts-check
/**
 * gg-blocks.js: the face and the gear tray of the blockr.ggplot blocks
 * (ggplot, facet, grid, theme), built from blockr.ui's controls
 * (Blockr.Select, Blockr.segmented, Blockr.checkbox, Blockr.textCommit,
 * Blockr.gearTray, Blockr.menu, Blockr.tooltip).
 *
 * A Shiny input binding binds each block's container. R pushes the columns
 * and the config ('gg-block-data'); every change echoes the full config back
 * through one '<id>_action' input. The plot renders server-side in the
 * block's plotOutput, below the container.
 *
 * GG_TYPES mirrors `chart_aesthetics` in R/ggplot-block.R, which decides the
 * expression. Keep both in sync.
 */
(() => {
  'use strict';

  // -- ggplot block --------------------------------------------------------

  /** @type {Record<string, string>} */
  const COLUMN_ROLES = {
    x: 'X-axis',
    y: 'Y-axis',
    color: 'Color by',
    fill: 'Fill by',
    size: 'Size by',
    shape: 'Shape by',
    linetype: 'Line type by',
    group: 'Group by',
    alpha: 'Alpha by'
  };

  /** @type {Record<string, { label: string, required: string[], optional: string[] }>} */
  const GG_TYPES = {
    point:     { label: 'Point',     required: ['x', 'y'], optional: ['color', 'shape', 'size', 'alpha', 'fill'] },
    bar:       { label: 'Bar',       required: ['x'],      optional: ['y', 'fill', 'color', 'alpha'] },
    line:      { label: 'Line',      required: ['x', 'y'], optional: ['color', 'linetype', 'alpha', 'group'] },
    boxplot:   { label: 'Boxplot',   required: ['x', 'y'], optional: ['fill', 'color', 'alpha'] },
    pie:       { label: 'Pie',       required: ['x'],      optional: ['y', 'fill', 'alpha'] },
    histogram: { label: 'Histogram', required: ['x'],      optional: ['fill', 'color', 'alpha'] },
    // Density has no variable alpha or group: the opacity is a number in
    // the gear, and the group follows fill.
    density:   { label: 'Density',   required: ['x'],      optional: ['fill'] },
    violin:    { label: 'Violin',    required: ['x', 'y'], optional: ['fill', 'color', 'alpha'] },
    area:      { label: 'Area',      required: ['x', 'y'], optional: ['fill', 'alpha'] }
  };
  const GG_TYPE_ORDER = Object.keys(GG_TYPES);

  /** @type {Record<string, string>} */
  const GG_TYPE_ICONS = {
    point:
      '<svg viewBox="0 0 16 16" fill="currentColor" aria-hidden="true">' +
      '<circle cx="4" cy="11" r="1.6"/><circle cx="8" cy="6" r="1.6"/>' +
      '<circle cx="12" cy="9" r="1.6"/><circle cx="13" cy="3" r="1.6"/></svg>',
    bar:
      '<svg viewBox="0 0 16 16" fill="currentColor" aria-hidden="true">' +
      '<rect x="2" y="8" width="3" height="6"/><rect x="6.5" y="4" width="3" height="10"/>' +
      '<rect x="11" y="10" width="3" height="4"/></svg>',
    line:
      '<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.6" ' +
      'stroke-linecap="round" aria-hidden="true"><path d="M2 12 L6 7 L10 9 L14 3"/></svg>',
    boxplot:
      '<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.4" aria-hidden="true">' +
      '<rect x="4" y="5" width="8" height="6"/>' +
      '<path d="M4 8 h8 M8 2 v3 M8 11 v3 M6 2 h4 M6 14 h4"/></svg>',
    pie:
      '<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.4" aria-hidden="true">' +
      '<circle cx="8" cy="8" r="6"/><path d="M8 8 V2 M8 8 L13 11"/></svg>',
    histogram:
      '<svg viewBox="0 0 16 16" fill="currentColor" aria-hidden="true">' +
      '<rect x="2" y="9" width="3" height="5"/><rect x="5" y="5" width="3" height="9"/>' +
      '<rect x="8" y="7" width="3" height="7"/><rect x="11" y="11" width="3" height="3"/></svg>',
    density:
      '<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.6" ' +
      'stroke-linecap="round" aria-hidden="true"><path d="M2 13 C5 13 5 4 8 4 C11 4 11 13 14 13"/></svg>',
    violin:
      '<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.4" aria-hidden="true">' +
      '<path d="M8 2 C10 5 12 6 12 9 C12 12 10 14 8 14 C6 14 4 12 4 9 C4 6 6 5 8 2 Z"/></svg>',
    area:
      '<svg viewBox="0 0 16 16" fill="currentColor" aria-hidden="true">' +
      '<path d="M2 14 L2 11 L6 6 L10 8 L14 3 L14 14 Z" opacity="0.85"/></svg>'
  };

  /**
   * One field in the grid.
   * @typedef {{
   *   key: string,
   *   kind: 'column' | 'columns' | 'select' | 'segmented' | 'check' |
   *         'text' | 'number' | 'colour' | 'preview',
   *   label?: string,
   *   size?: 'small' | 'large' | 'full',
   *   options?: string[][],
   *   ph?: string,
   *   required?: boolean,
   *   removable?: boolean,
   *   min?: number, max?: number, step?: number,
   *   empty?: string
   * }} Field
   */
  /** @typedef {{ title?: string, fields: Field[] }} Section */
  /** @typedef {{ tiles?: boolean, fields?: Field[], add?: string[] }} FacePart */

  /** @param {any} v */
  const hasVal = (v) => Array.isArray(v) ? v.length > 0 : (v !== null && v !== undefined && v !== '');

  /**
   * The ggplot block's gear: presentation for the current chart type. One
   * section, so no title.
   * @param {GgBlock} b
   * @returns {Section[]}
   */
  const ggTray = (b) => {
    const t = b.config.type;
    /** @type {Field[]} */
    const f = [];
    if (t === 'bar') {
      f.push({ key: 'position', kind: 'segmented', label: 'Position',
               options: [['stack', 'Stack'], ['dodge', 'Dodge'], ['fill', 'Fill']] });
    }
    if (t === 'histogram') {
      f.push({ key: 'bins', kind: 'number', label: 'Bins', size: 'small',
               min: 1, max: 100, step: 1 });
      f.push({ key: 'position', kind: 'segmented', label: 'Position',
               options: [['stack', 'Stack'], ['identity', 'Overlap'], ['dodge', 'Dodge']] });
    }
    if (t === 'density') {
      f.push({ key: 'density_alpha', kind: 'number', label: 'Opacity', size: 'small',
               min: 0, max: 1, step: 0.05 });
    }
    if (t === 'pie') {
      f.push({ key: 'donut', kind: 'check', label: 'Donut' });
    }
    if (t === 'point' || t === 'line') {
      // The values are ggplot2's `method` argument, so the emitted
      // geom_smooth() reads as if typed.
      f.push({ key: 'smoother', kind: 'select', label: 'Trend line',
               options: [['none', 'None'], ['lm', 'Linear'], ['loess', 'Smooth']] });
      if (b.config.smoother && b.config.smoother !== 'none') {
        f.push({ key: 'smoother_se', kind: 'check', label: 'Confidence band' });
      }
    }
    if (t !== 'pie') {
      f.push({ key: 'y_trans', kind: 'select', label: 'Y scale',
               options: [['identity', 'Linear'], ['log10', 'Log 10'], ['sqrt', 'Square root']] });
      f.push({ key: 'y_zero', kind: 'check', label: 'Include zero' });
    }
    f.push({ key: 'title', kind: 'text', label: 'Title', ph: 'None' });
    f.push({ key: 'subtitle', kind: 'text', label: 'Subtitle', ph: 'None' });
    f.push({ key: 'caption', kind: 'text', label: 'Caption', ph: 'None' });
    if (t !== 'pie') {
      f.push({ key: 'xlab', kind: 'text', label: 'X label', ph: 'Column name' });
      f.push({ key: 'ylab', kind: 'text', label: 'Y label', ph: 'Column name' });
    }
    return [{ fields: f }];
  };

  /** @param {GgBlock} b */
  const ggShownOptional = (b) => {
    const spec = GG_TYPES[b.config.type] || GG_TYPES.point;
    return spec.optional.filter((k) => hasVal(b.config[k]) || b.added.has(k));
  };

  /**
   * The ggplot block's face: chart type tiles, the mapping, "Add mapping".
   * @param {GgBlock} b
   * @returns {FacePart[]}
   */
  const ggFace = (b) => {
    const spec = GG_TYPES[b.config.type] || GG_TYPES.point;
    const shown = ggShownOptional(b);
    /** @type {Field[]} */
    const fields = [
      ...spec.required.map((k) => /** @type {Field} */ (
        { key: k, kind: 'column', label: COLUMN_ROLES[k], required: true })),
      ...shown.map((k) => /** @type {Field} */ (
        { key: k, kind: 'column', label: COLUMN_ROLES[k], removable: true }))
    ];
    return [
      { tiles: true },
      { fields },
      { add: spec.optional.filter((k) => !shown.includes(k)) }
    ];
  };

  // -- facet block ---------------------------------------------------------

  const AUTO_1_5 = [['', 'Auto'], ['1', '1'], ['2', '2'], ['3', '3'], ['4', '4'], ['5', '5']];
  const LABELLERS = [
    ['label_value', 'Value'],
    ['label_both', 'Name: value'],
    ['label_parsed', 'Parsed expression']
  ];

  /** @param {GgBlock} b @returns {FacePart[]} */
  const facetFace = (b) => {
    /** @type {Field[]} */
    const f = [{ key: 'facet_type', kind: 'segmented', label: 'Layout',
                 options: [['wrap', 'Wrap'], ['grid', 'Grid']] }];
    if (b.config.facet_type === 'grid') {
      // Rows and columns are either-or, so neither is required.
      f.push({ key: 'rows', kind: 'columns', label: 'Rows', size: 'full', ph: 'None' });
      f.push({ key: 'cols', kind: 'columns', label: 'Columns', size: 'full', ph: 'None' });
    } else {
      // facet_wrap() with no variable passes the plot through: required.
      f.push({ key: 'facets', kind: 'columns', label: 'Facet by', size: 'full',
               ph: 'Select columns…', required: true });
    }
    return [{ fields: f }];
  };

  /** @param {GgBlock} b @returns {Section[]} */
  const facetTray = (b) => {
    /** @type {Field[]} */
    const f = [];
    const grid = b.config.facet_type === 'grid';
    if (!grid) {
      f.push({ key: 'ncol', kind: 'select', label: 'Columns', options: AUTO_1_5 });
      f.push({ key: 'nrow', kind: 'select', label: 'Rows', options: AUTO_1_5 });
    }
    f.push({ key: 'scales', kind: 'select', label: 'Scales', options: [
      ['fixed', 'Fixed'], ['free', 'Free'], ['free_x', 'Free x'], ['free_y', 'Free y']] });
    f.push({ key: 'labeller', kind: 'select', label: 'Labels', options: LABELLERS });
    if (grid) {
      f.push({ key: 'space', kind: 'select', label: 'Space', options: [
        ['fixed', 'Fixed'], ['free_x', 'Free x'], ['free_y', 'Free y']] });
    } else {
      f.push({ key: 'dir', kind: 'segmented', label: 'Direction',
               options: [['h', 'Across'], ['v', 'Down']] });
    }
    f.push({ key: 'preview', kind: 'preview', size: 'full' });
    return [{ fields: f }];
  };

  // -- grid block ----------------------------------------------------------

  /** @returns {Section[]} */
  const gridTray = () => [
    {
      title: 'Layout',
      fields: [
        { key: 'ncol', kind: 'select', label: 'Columns', options: AUTO_1_5 },
        { key: 'nrow', kind: 'select', label: 'Rows', options: AUTO_1_5 },
        { key: 'guides', kind: 'select', label: 'Legends', options: [
          ['auto', 'Auto'], ['collect', 'Collect'], ['keep', 'Keep separate']] },
        { key: 'tag_levels', kind: 'select', label: 'Auto-tag plots', options: [
          ['', 'None'], ['A', 'A, B, C'], ['a', 'a, b, c'], ['1', '1, 2, 3'],
          ['I', 'I, II, III'], ['i', 'i, ii, iii']] },
        { key: 'preview', kind: 'preview', size: 'full' }
      ]
    },
    {
      title: 'Titles',
      fields: [
        { key: 'title', kind: 'text', label: 'Title', ph: 'None' },
        { key: 'subtitle', kind: 'text', label: 'Subtitle', ph: 'None' },
        { key: 'caption', kind: 'text', label: 'Caption', ph: 'None' }
      ]
    }
  ];

  // -- theme block ---------------------------------------------------------

  const PALETTES = [
    ['auto', 'Auto'],
    ['viridis_d', 'Viridis, categorical'],
    ['viridis_c', 'Viridis, continuous'],
    ['magma_d', 'Magma, categorical'],
    ['magma_c', 'Magma, continuous'],
    ['plasma_d', 'Plasma, categorical'],
    ['plasma_c', 'Plasma, continuous'],
    ['ggplot2', 'ggplot2 default']
  ];
  const AUTO_SHOW_HIDE = [['auto', 'Auto'], ['show', 'Show'], ['hide', 'Hide']];

  /** @param {GgBlock} b @returns {FacePart[]} */
  const themeFace = (b) => [{
    fields: [
      // The base theme list depends on the installed theme packages, so R
      // sends it with the config.
      { key: 'base_theme', kind: 'select', label: 'Base theme',
        options: b.choices.base_theme || [['auto', 'Auto']] },
      { key: 'legend_position', kind: 'select', label: 'Legend', options: [
        ['auto', 'Auto'], ['right', 'Right'], ['left', 'Left'], ['top', 'Top'],
        ['bottom', 'Bottom'], ['none', 'None']] },
      { key: 'palette_fill', kind: 'select', label: 'Fill palette', options: PALETTES },
      { key: 'palette_colour', kind: 'select', label: 'Colour palette', options: PALETTES }
    ]
  }];

  /** @returns {Section[]} */
  const themeTray = () => [
    {
      title: 'Colours',
      fields: [
        { key: 'plot_bg', kind: 'colour', label: 'Plot background' },
        { key: 'panel_bg', kind: 'colour', label: 'Panel background' },
        { key: 'grid_color', kind: 'colour', label: 'Grid lines' }
      ]
    },
    {
      title: 'Text and lines',
      fields: [
        { key: 'base_size', kind: 'number', label: 'Font size', size: 'small',
          min: 1, max: 72, step: 1, ph: 'Auto', empty: 'auto' },
        { key: 'base_family', kind: 'select', label: 'Font', options: [
          ['auto', 'Auto'], ['sans', 'Sans serif'], ['serif', 'Serif'], ['mono', 'Monospace']] },
        { key: 'show_major_grid', kind: 'segmented', label: 'Major grid', options: AUTO_SHOW_HIDE },
        { key: 'show_minor_grid', kind: 'segmented', label: 'Minor grid', options: AUTO_SHOW_HIDE },
        { key: 'show_panel_border', kind: 'segmented', label: 'Border', options: AUTO_SHOW_HIDE }
      ]
    }
  ];

  // -- specs ---------------------------------------------------------------

  /**
   * `face` and `tray` list what to draw; `shape` names what decides the
   * set of controls, so a push that only changes values updates them in
   * place (an open menu stays open, a focused field keeps its text).
   * `configKeys` is the full config echoed to R.
   * @type {Record<string, {
   *   face: (b: GgBlock) => FacePart[],
   *   tray: (b: GgBlock) => Section[],
   *   shape: (b: GgBlock) => string,
   *   configKeys: string[]
   * }>}
   */
  const SPECS = {
    ggplot: {
      face: ggFace,
      tray: ggTray,
      shape: (b) => [
        b.config.type, ggShownOptional(b).join(','),
        b.config.smoother === 'none', b.columnsKey()
      ].join('|'),
      configKeys: [
        'type', 'x', 'y', 'color', 'fill', 'size', 'shape', 'linetype',
        'group', 'alpha', 'density_alpha', 'position', 'bins', 'donut',
        'smoother', 'smoother_se', 'y_trans', 'y_zero',
        'title', 'subtitle', 'caption', 'xlab', 'ylab'
      ]
    },
    facet: {
      face: facetFace,
      tray: facetTray,
      shape: (b) => [b.config.facet_type, b.columnsKey()].join('|'),
      configKeys: [
        'facet_type', 'facets', 'rows', 'cols', 'ncol', 'nrow',
        'scales', 'labeller', 'dir', 'space'
      ]
    },
    grid: {
      face: () => [],
      tray: gridTray,
      shape: () => 'grid',
      configKeys: [
        'ncol', 'nrow', 'guides', 'title', 'subtitle', 'caption', 'tag_levels'
      ]
    },
    theme: {
      face: themeFace,
      tray: themeTray,
      shape: (b) => JSON.stringify(b.choices.base_theme || []),
      configKeys: [
        'base_theme', 'legend_position', 'palette_fill', 'palette_colour',
        'panel_bg', 'plot_bg', 'base_size', 'base_family',
        'show_major_grid', 'show_minor_grid', 'grid_color',
        'show_panel_border'
      ]
    }
  };

  // -- DOM helpers ---------------------------------------------------------

  /**
   * @param {string} tag
   * @param {string} [cls]
   * @param {string} [text]
   * @returns {HTMLElement}
   */
  const make = (tag, cls, text) => {
    const n = document.createElement(tag);
    if (cls) n.className = cls;
    if (text !== undefined) n.textContent = text;
    return n;
  };

  /** A hex colour as the field shows it, or null when it is not one. @param {string} v */
  const normHex = (v) => {
    const m = /^#?([0-9a-f]{3}|[0-9a-f]{6})$/i.exec(v.trim());
    return m ? '#' + m[1].toUpperCase() : null;
  };

  /** The 6-digit form the native picker needs. @param {string} hex */
  const longHex = (hex) => hex.length === 4
    ? '#' + hex.slice(1).split('').map((c) => c + c).join('') : hex;

  // -- the block -----------------------------------------------------------

  class GgBlock {
    /** @param {HTMLElement} el */
    constructor(el) {
      this.el = el;
      this.kind = el.getAttribute('data-gg-block') || 'ggplot';
      this.spec = SPECS[this.kind] || SPECS.ggplot;
      /** @type {{ name: string, label?: string }[]} */
      this.columns = [];
      /** @type {Record<string, any>} */
      this.config = {};
      /** @type {Record<string, string[][]>} */
      this.choices = {};
      /** @type {any} */
      this.preview = null;
      /** Optional mappings added this session, shown before they hold a value. */
      this.added = new Set();
      /** @type {Record<string, string>} column roles cleared by a type switch */
      this.memory = {};
      /** @type {Record<string, (v: any) => void>} */
      this.setters = {};
      /** @type {{ key: string, el: HTMLElement }[]} */
      this.required = [];
      /** @type {{ destroy: () => void }[]} */
      this.selects = [];
      /** @type {HTMLElement | null} */
      this.previewEl = null;
      /** @type {string | null} */
      this.shape = null;

      const head = make('div', 'blockr-gear-header');
      const gear = /** @type {HTMLButtonElement} */ (make('button', 'blockr-gear-btn'));
      gear.type = 'button';
      gear.innerHTML = Blockr.icons.gear;
      head.appendChild(gear);
      this.trayEl = make('div', 'blockr-settings blockr-settings--beak gg-tray');
      this.faceEl = make('div', 'gg-face');
      const first = el.firstChild;
      el.insertBefore(head, first);
      el.insertBefore(this.trayEl, first);
      el.insertBefore(this.faceEl, first);
      Blockr.gearTray(this.trayEl, gear, { label: 'Settings' });
    }

    columnsKey() {
      return this.columns.map((c) => c.name + ':' + (c.label || '')).join(',');
    }

    /** @param {{ columns?: any[], config?: Record<string, any>, choices?: Record<string, any[]>, preview?: any }} msg */
    setData(msg) {
      this.columns = msg.columns || [];
      this.config = Object.assign({}, msg.config);
      if (msg.choices) {
        for (const k of Object.keys(msg.choices)) {
          this.choices[k] = msg.choices[k].map((o) => [o.value, o.label]);
        }
      }
      // R sends NULL as an empty object.
      this.preview = (msg.preview && msg.preview.cols) ? msg.preview : null;
      this.render();
    }

    // Draw the controls again when the set of controls changes; otherwise
    // only move their values.
    render() {
      const shape = this.spec.shape(this);
      if (shape !== this.shape) {
        this.shape = shape;
        this.build();
      } else {
        for (const k of Object.keys(this.setters)) this.setters[k](this.config[k]);
      }
      for (const r of this.required) {
        Blockr.setRequiredEmpty(r.el, !hasVal(this.config[r.key]));
      }
      this.drawPreview();
    }

    build() {
      for (const s of this.selects) s.destroy();
      this.selects = [];
      this.setters = {};
      this.required = [];
      this.previewEl = null;
      this.trayEl.textContent = '';
      this.faceEl.textContent = '';

      const sections = this.spec.tray(this);
      for (const sec of sections) {
        // A tray with one section has no title.
        if (sec.title && sections.length > 1) {
          this.trayEl.appendChild(make('div', 'blockr-settings__title', sec.title));
        }
        const grid = make('div', 'blockr-settings__grid');
        for (const f of sec.fields) this.field(grid, f);
        this.trayEl.appendChild(grid);
      }

      for (const part of this.spec.face(this)) {
        if (part.tiles) this.tiles();
        if (part.fields) {
          const grid = make('div', 'blockr-settings__grid gg-face__grid');
          for (const f of part.fields) this.field(grid, f);
          this.faceEl.appendChild(grid);
        }
        if (part.add && part.add.length) this.addButton(part.add);
      }
    }

    /** @param {string} key @param {any} value */
    set(key, value) {
      this.config[key] = value;
      this.send();
      this.render();
    }

    send() {
      if (!this.el.id) return;
      /** @type {Record<string, any>} */
      const out = { action: 'config' };
      for (const k of this.spec.configKeys) {
        const v = this.config[k];
        out[k] = (v === null || v === undefined) ? '' : v;
      }
      Shiny.setInputValue(this.el.id + '_action', out, { priority: 'event' });
    }

    // -- ggplot: type tiles, adding and removing mappings --------------------

    tiles() {
      const wrap = make('div', 'gg-types');
      const lab = make('div', 'blockr-label', 'Chart type');
      lab.id = Blockr.uid('gg-types');
      const row = make('div', 'gg-tiles');
      row.setAttribute('role', 'group');
      row.setAttribute('aria-labelledby', lab.id);
      for (const t of GG_TYPE_ORDER) {
        const b = /** @type {HTMLButtonElement} */ (make('button', 'gg-tile'));
        b.type = 'button';
        b.setAttribute('aria-pressed', String(this.config.type === t));
        b.innerHTML = GG_TYPE_ICONS[t];
        b.appendChild(make('span', 'gg-tile__name', GG_TYPES[t].label));
        b.addEventListener('click', () => {
          if (this.config.type !== t) this.setType(t);
        });
        row.appendChild(b);
      }
      wrap.appendChild(lab);
      wrap.appendChild(row);
      this.faceEl.appendChild(wrap);
    }

    // A column role the new type does not take is cleared and remembered; a
    // required role left empty takes back what it held before.
    /** @param {string} t */
    setType(t) {
      const spec = GG_TYPES[t];
      const keep = new Set([...spec.required, ...spec.optional]);
      for (const k of Object.keys(COLUMN_ROLES)) {
        if (!keep.has(k) && hasVal(this.config[k])) {
          this.memory[k] = this.config[k];
          this.config[k] = '';
        }
      }
      for (const k of spec.required) {
        const mem = this.memory[k];
        if (!hasVal(this.config[k]) && mem && this.columns.some((c) => c.name === mem)) {
          this.config[k] = mem;
        }
      }
      this.set('type', t);
    }

    /** @param {string[]} remaining */
    addButton(remaining) {
      const btn = /** @type {HTMLButtonElement} */ (
        make('button', 'blockr-btn blockr-btn--quiet blockr-btn--s gg-add'));
      btn.type = 'button';
      btn.innerHTML = Blockr.icons.plus;
      btn.appendChild(document.createTextNode('Add mapping'));
      Blockr.menu.bind(btn, () => ({
        items: remaining.map((k) => ({
          label: COLUMN_ROLES[k],
          onSelect: () => this.addRole(k)
        }))
      }));
      this.faceEl.appendChild(btn);
    }

    /** @param {string} key */
    addRole(key) {
      this.added.add(key);
      const mem = this.memory[key];
      if (!hasVal(this.config[key]) && mem && this.columns.some((c) => c.name === mem)) {
        this.set(key, mem);
      } else {
        this.render();
      }
      const input = /** @type {HTMLElement | null} */ (
        this.faceEl.querySelector(`[data-key="${key}"] input`));
      if (input) input.focus();
    }

    /** @param {string} key */
    removeRole(key) {
      this.added.delete(key);
      delete this.memory[key];
      this.set(key, '');
    }

    // -- fields --------------------------------------------------------------

    /** @param {HTMLElement} grid @param {Field} f */
    field(grid, f) {
      // A checkbox takes two columns here: in one, the field after it starts
      // mid-row and the columns stop lining up.
      const size = f.size || 'large';
      const wrap = make('div', 'blockr-settings__field gg-field' +
        (size === 'small' ? ' blockr-settings__field--small' : '') +
        (size === 'full' ? ' blockr-settings__field--full' : ''));
      wrap.dataset.key = f.key;
      grid.appendChild(wrap);
      // A checkbox is its own label; the preview has none.
      if (f.label && f.kind !== 'check') {
        wrap.appendChild(make('label', 'blockr-label', f.label));
      }
      if (f.removable) this.removeButton(wrap, f);
      if (f.required) this.required.push({ key: f.key, el: wrap });

      const v = this.config[f.key];
      switch (f.kind) {
        case 'column': return this.columnField(wrap, f, v);
        case 'columns': return this.columnsField(wrap, f, v);
        case 'select': return this.selectField(wrap, f, v);
        case 'segmented': return this.segmentedField(wrap, f, v);
        case 'check': return this.checkField(wrap, f, v);
        case 'text': return this.textField(wrap, f, v);
        case 'number': return this.numberField(wrap, f, v);
        case 'colour': return this.colourField(wrap, f, v);
        case 'preview':
          this.previewEl = make('div', 'gg-preview');
          wrap.appendChild(this.previewEl);
          return undefined;
      }
      return undefined;
    }

    /** @param {HTMLElement} wrap @param {Field} f */
    removeButton(wrap, f) {
      const b = /** @type {HTMLButtonElement} */ (make('button', 'gg-remove'));
      b.type = 'button';
      b.innerHTML = Blockr.icons.x;
      b.setAttribute('aria-label', 'Remove ' + f.label);
      Blockr.tooltip.set(b, 'Remove');
      b.addEventListener('click', () => this.removeRole(f.key));
      wrap.appendChild(b);
    }

    columnOptions() {
      return this.columns.map((c) => ({
        value: c.name,
        label: c.label && c.label !== c.name ? c.label : ''
      }));
    }

    /** @param {HTMLElement} wrap @param {Field} f @param {any} v */
    columnField(wrap, f, v) {
      const host = make('div');
      wrap.appendChild(host);
      const h = /** @type {NonNullable<typeof Blockr.Select>} */ (Blockr.Select).single(host, {
        bordered: true,
        allowEmpty: true,
        placeholder: 'Select column…',
        options: this.columnOptions(),
        selected: hasVal(v) ? v : '',
        onChange: (x) => this.set(f.key, x)
      });
      this.selects.push(h);
      this.setters[f.key] = (x) => {
        if (h.getValue() !== (x || '')) h.setValue(x || '');
      };
    }

    /** @param {HTMLElement} wrap @param {Field} f @param {any} v */
    columnsField(wrap, f, v) {
      const host = make('div');
      wrap.appendChild(host);
      const h = /** @type {NonNullable<typeof Blockr.Select>} */ (Blockr.Select).multi(host, {
        bordered: true,
        placeholder: f.ph || '',
        options: this.columnOptions(),
        selected: Array.isArray(v) ? v : [],
        onChange: (x) => this.set(f.key, x)
      });
      this.selects.push(h);
      this.setters[f.key] = (x) => {
        const next = Array.isArray(x) ? x : [];
        if (JSON.stringify(h.getValue()) !== JSON.stringify(next)) h.setValue(next);
      };
    }

    // A fixed set shows its labels only: the widget lists the labels, and
    // the value is looked up on the way out.
    /** @param {HTMLElement} wrap @param {Field} f @param {any} v */
    selectField(wrap, f, v) {
      const opts = f.options || [];
      /** @param {any} x */
      const labelOf = (x) => {
        const o = opts.find((p) => p[0] === (x ?? ''));
        return o ? o[1] : String(x ?? '');
      };
      /** @param {string} l */
      const valueOf = (l) => {
        const o = opts.find((p) => p[1] === l);
        return o ? o[0] : l;
      };
      const host = make('div');
      wrap.appendChild(host);
      const h = /** @type {NonNullable<typeof Blockr.Select>} */ (Blockr.Select).single(host, {
        bordered: true,
        options: opts.map((o) => o[1]),
        selected: labelOf(v),
        onChange: (l) => this.set(f.key, valueOf(l))
      });
      this.selects.push(h);
      this.setters[f.key] = (x) => {
        if (h.getValue() !== labelOf(x)) h.setValue(labelOf(x));
      };
    }

    /** @param {HTMLElement} wrap @param {Field} f @param {any} v */
    segmentedField(wrap, f, v) {
      const host = make('div', 'blockr-ui-segmented');
      const h = Blockr.segmented(
        (f.options || []).map((o) => ({ value: o[0], label: o[1] })),
        v, (x) => this.set(f.key, x), { label: f.label }
      );
      host.appendChild(h.el);
      wrap.appendChild(host);
      this.setters[f.key] = (x) => { if (h.get() !== x) h.set(x); };
    }

    // On/off travels as "on"/"off" (the ggplot block's protocol).
    /** @param {HTMLElement} wrap @param {Field} f @param {any} v */
    checkField(wrap, f, v) {
      const h = Blockr.checkbox(f.label || '', v === 'on',
        (on) => this.set(f.key, on ? 'on' : 'off'));
      wrap.appendChild(h.el);
      this.setters[f.key] = (x) => h.set(x === 'on');
    }

    /**
     * A text input that commits on Enter or blur, inside the commit field.
     * @param {HTMLElement} wrap @param {Field} f @param {string} shown
     * @param {(value: string, tc: BlockrTextCommitHandle) => void} onCommit
     * @param {string} [type]
     */
    commitInput(wrap, f, shown, onCommit, type) {
      const box = make('div', 'blockr-commit-field');
      const input = /** @type {HTMLInputElement} */ (make('input', 'blockr-text-input'));
      input.type = type || 'text';
      input.placeholder = f.ph || '';
      input.value = shown;
      box.appendChild(input);
      wrap.appendChild(box);
      /** @type {BlockrTextCommitHandle} */
      const tc = Blockr.textCommit(input, { onCommit: (x) => onCommit(x, tc) });
      return { input, tc };
    }

    /** @param {HTMLElement} wrap @param {Field} f @param {any} v */
    textField(wrap, f, v) {
      const { input, tc } = this.commitInput(wrap, f, v || '',
        (x) => this.set(f.key, x));
      this.setters[f.key] = (x) => {
        if (document.activeElement !== input && input.value !== (x || '')) tc.sync(x || '');
      };
    }

    /** @param {HTMLElement} wrap @param {Field} f @param {any} v */
    numberField(wrap, f, v) {
      /** @param {any} x */
      const shown = (x) => (x === null || x === undefined || x === '' || x === f.empty)
        ? '' : String(x);
      const { input, tc } = this.commitInput(wrap, f, shown(v), (raw) => {
        const s = raw.trim();
        if (s === '' && f.empty !== undefined) { this.set(f.key, f.empty); return; }
        const n = Number(s);
        const ok = s !== '' && Number.isFinite(n) &&
          (f.min === undefined || n >= f.min) && (f.max === undefined || n <= f.max);
        if (!ok) { tc.sync(shown(this.config[f.key])); return; }
        this.set(f.key, f.empty !== undefined ? String(n) : n);
      }, 'number');
      input.classList.add('blockr-ui-number');
      if (f.min !== undefined) input.min = String(f.min);
      if (f.max !== undefined) input.max = String(f.max);
      if (f.step !== undefined) input.step = String(f.step);
      this.setters[f.key] = (x) => {
        if (document.activeElement !== input && input.value !== shown(x)) tc.sync(shown(x));
      };
    }

    /**
     * The colour field (design system, "Colour field"): the text field's
     * shell with a swatch and the hex value in capitals. The swatch opens the
     * browser's picker, whose pick commits when it closes; a typed hex value
     * commits on Enter or blur. Empty means the theme's own colour.
     *
     * It lives here until a second package needs it, then moves to blockr.ui.
     * @param {HTMLElement} wrap @param {Field} f @param {any} v
     */
    colourField(wrap, f, v) {
      const box = make('div', 'blockr-commit-field gg-colour');
      const swatch = /** @type {HTMLButtonElement} */ (make('button', 'gg-colour__swatch'));
      swatch.type = 'button';
      swatch.setAttribute('aria-label', 'Pick ' + (f.label || 'a colour').toLowerCase());
      Blockr.tooltip.set(swatch, 'Pick a colour');
      const input = /** @type {HTMLInputElement} */ (make('input', 'gg-colour__input'));
      input.type = 'text';
      input.placeholder = 'Theme default';
      input.spellcheck = false;
      const picker = /** @type {HTMLInputElement} */ (make('input', 'gg-colour__picker'));
      picker.type = 'color';
      picker.tabIndex = -1;
      picker.setAttribute('aria-hidden', 'true');
      box.appendChild(swatch);
      box.appendChild(input);
      box.appendChild(picker);
      wrap.appendChild(box);

      /** @param {string} hex */
      const paint = (hex) => {
        swatch.style.backgroundColor = hex || '';
        swatch.classList.toggle('gg-colour__swatch--empty', !hex);
      };
      /** @param {any} x */
      const shown = (x) => (x ? (normHex(String(x)) || String(x)) : '');

      input.value = shown(v);
      paint(input.value);
      const tc = Blockr.textCommit(input, {
        onCommit: (raw) => {
          if (raw.trim() === '') { paint(''); this.set(f.key, ''); return; }
          const hex = normHex(raw);
          if (!hex) { tc.sync(shown(this.config[f.key])); return; }
          tc.sync(hex);
          paint(hex);
          this.set(f.key, hex);
        }
      });

      swatch.addEventListener('click', () => {
        const cur = normHex(input.value);
        picker.value = cur ? longHex(cur).toLowerCase() : '#ffffff';
        if (typeof picker.showPicker === 'function') {
          try { picker.showPicker(); return; } catch (_e) { /* fall through */ }
        }
        picker.click();
      });
      // The swatch follows the picker live; the value commits on close.
      picker.addEventListener('input', () => paint(picker.value));
      picker.addEventListener('change', () => {
        const hex = /** @type {string} */ (normHex(picker.value));
        tc.sync(hex);
        paint(hex);
        this.set(f.key, hex);
      });

      this.setters[f.key] = (x) => {
        if (document.activeElement !== input && input.value !== shown(x)) {
          tc.sync(shown(x));
          paint(shown(x));
        }
      };
    }

    // -- layout preview (grid and facet) --------------------------------------

    // R sends the layout: rows, columns, a label per cell (row-major, ''
    // for an empty slot), a state ('fit', 'gaps' or 'invalid') and the
    // status line. No layout, no preview.
    drawPreview() {
      const host = this.previewEl;
      if (!host) return;
      const field = /** @type {HTMLElement} */ (host.parentElement);
      const p = this.preview;
      field.style.display = p ? '' : 'none';
      host.textContent = '';
      if (!p) return;
      const invalid = p.state === 'invalid';
      const grid = make('div', 'gg-preview__grid' + (invalid ? ' gg-preview__grid--invalid' : ''));
      // Cells shrink as the layout grows, so a tall layout stays short.
      const cell = Math.max(24, Math.min(64, Math.floor(200 / Math.max(p.rows, p.cols))));
      grid.style.setProperty('--blockr-ggplot-cell-w', cell + 'px');
      grid.style.gridTemplateColumns =
        `repeat(${p.cols}, minmax(0, var(--blockr-ggplot-cell-w)))`;
      grid.setAttribute('aria-hidden', 'true');
      for (const c of (p.cells || [])) {
        grid.appendChild(make('div',
          'gg-preview__cell ' + (c ? 'gg-preview__cell--filled' : 'gg-preview__cell--empty'),
          c || ''));
      }
      host.appendChild(grid);
      host.appendChild(make('div',
        'gg-preview__status' + (invalid ? ' gg-preview__status--invalid' : ''),
        p.status || ''));
    }
  }

  // -- Shiny binding -------------------------------------------------------

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
      } else if (window.Shiny && Shiny.setInputValue) {
        // Nothing waiting for us. Shiny drops a custom message with no
        // registered handler, so a block on a view nobody had opened yet
        // missed its startup payload. Announce, and let R send its last
        // payload again (the same handshake as blockr.viz's chart block).
        Shiny.setInputValue(el.id + '_ready', Date.now(), { priority: 'event' });
      }
    }
  });
  Shiny.inputBindings.register(binding, 'blockr.ggplot');

  // Push from R. The element may not exist or not be bound yet (dock
  // panels, views revealed later): buffer on the element or poll briefly.
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
