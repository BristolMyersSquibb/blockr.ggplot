/**
 * CANONICAL SOURCE: blockr.viz/inst/js/drilldown-config.js — keep in sync.
 * Copied into blockr.ggplot per the design-system convention (shared assets
 * are copied until they graduate to blockr.ui). Local extension vs canonical:
 * _buildSlider reads role.min/max/step/unit (upstream candidate).
 *
 * Second local extension vs canonical: kind 'color' — a native color swatch
 * paired with a "Theme default" checkbox ('' = default); used by the theme
 * block in place of colourpicker (upstream candidate).
 *
 * DrilldownConfig — the shared gear-popover config engine for blockr drilldown
 * blocks (chart, table, …). Host-agnostic: it renders a grouped, role-spec
 * driven popover (Mapping / Presentation + a Drill-down section) and calls
 * back into a host for everything block-specific.
 *
 * A host provides (see DrilldownChart for the chart implementation):
 *   popoverEl()      -> the <body>-portaled popover element
 *   roles            -> the ROLES dict (key -> {label,kind,colType,...})
 *   config()         -> the mutable config object (mutated in place)
 *   columns()        -> column metadata array [{name,type,n_unique,label?}]
 *   context()        -> string key for colTypeBy/optionsBy/hintBy (chart=family)
 *   currentType()    -> the chart-type value (for type-conditional rows) or null
 *   sections()       -> {requiredMap,optionalMap,mapping,presentation} (current)
 *                       (`mapping` = always-on controls shown under the Mapping
 *                        header after the role-picker rows, e.g. metric + agg)
 *   sectionsForFamily(fam) -> the same for a specific family (carry-over)
 *   secondary        -> Set of paired-tail role keys (skipped in section loops)
 *   typeKey          -> the config key the type picker writes (e.g. 'chart_type')
 *   typeGroups       -> [{label, types:[...]}] for the type picker, or null
 *   familyFor(type)  -> family string for a type, or null (no families)
 *   entryRequired(role) -> mark a section role required (chart: metric in aggregated)
 *   drillAutoLabel() -> label for the drill "Auto" option, or null (no drill section)
 *   title            -> popover title string
 *   onChange()       -> a config value changed: re-render output + send to R
 *   onMults()        -> a slider changed (separate transport)
 *   onClearFilter()  -> clear the emitted filter
 *   ensureDefaults() -> fill block defaults after a type change
 *   afterTypeChange()-> e.g. update family CSS classes (optional)
 *   isOpen()         -> is the popover open
 *   reopen()         -> reopen the popover (keep it open across a re-render)
 *
 * Exposed as Blockr.DrilldownConfig (and window.DrilldownConfig).
 */
// @ts-check
(() => {
  'use strict';

  class DrilldownConfig {
    /** @param {VizDrilldownHost} host */
    constructor(host) {
      this.h = host;
      /** @type {Record<string, any>} */
      this._selects = {};
      /** @type {Set<string>} */
      this._added = new Set();      // optional roles the user added this session
      /** @type {Record<string, string>} */
      this._roleMemory = {};        // role key -> last chosen column (sticky)
    }

    // -- small helpers --------------------------------------------------------
    get _SECONDARY() { return this.h.secondary; }
    /** @param {*} v */
    _hasVal(v) { return v !== null && v !== undefined && v !== '' && v !== '(none)'; }
    _cols() { return this.h.columns() || []; }
    _cfg() { return this.h.config(); }
    /** @param {string} key */
    _role(key) { return this.h.roles[key]; }
    /** @param {string} name */
    _colExists(name) { return this._cols().some(c => c.name === name); }

    // colType / allowCount may be declared as a function of the current config
    // (e.g. the chart metric widens to any column under agg "count_distinct").
    /** @param {any} role */
    _roleColType(role) {
      const ct = role.colTypeBy ? role.colTypeBy[this.h.context()] : role.colType;
      return (typeof ct === 'function') ? ct(this._cfg()) : ct;
    }
    /** @param {any} role */
    _roleAllowCount(role) {
      return (typeof role.allowCount === 'function')
        ? role.allowCount(this._cfg()) : !!role.allowCount;
    }

    /** @param {string} key @param {string} name */
    _colFits(key, name) {
      const role = this._role(key);
      const c = this._cols().find(x => x.name === name);
      if (!c) return false;
      const ct = this._roleColType(role);
      if (ct === 'none') return false;
      if (ct === 'num') return c.type === 'numeric';
      if (ct === 'cat') return c.type === 'categorical' || (c.n_unique != null && c.n_unique <= 50);
      return true;
    }

    // A "segmented" role whose two values are literally on/off is a plain
    // boolean data option — per the design-system rule (values -> pill, data
    // options -> checkbox) it renders as a .blockr-checkbox, not a pill.
    // Non-boolean segmented roles (Good when up/down, Layout cards/table)
    // stay cycling pills: their label IS the value.
    /** @param {any} role */
    _isBoolSegmented(role) {
      return !!role && role.kind === 'segmented' && Array.isArray(role.options) &&
        role.options.length === 2 &&
        role.options.every((/** @type {any} */ o) => o.value === 'on' || o.value === 'off');
    }

    /** @param {string} key @param {*} val */
    _rememberRole(key, val) {
      const role = this._role(key);
      if (role && role.kind === 'column' && this._hasVal(val)) {
        this._roleMemory[key] = val;
      }
    }

    /** @param {string} key @param {{ required?: boolean }} param1 */
    _colOptionsFor(key, { required }) {
      const role = this._role(key);
      const ct = this._roleColType(role);
      let cols = this._cols();
      if (ct === 'none') cols = [];
      else if (ct === 'num') cols = cols.filter(c => c.type === 'numeric');
      else if (ct === 'cat') cols = cols.filter(c => c.type === 'categorical' || (c.n_unique != null && c.n_unique <= 50));
      if (role.maxUnique) cols = cols.filter(c => c.n_unique != null && c.n_unique <= role.maxUnique);
      const opts = cols.map(c => c.label ? { value: c.name, label: c.label } : c.name);
      if (this._roleAllowCount(role)) opts.unshift('.count');
      else if (!required) opts.unshift('(none)');
      return opts;
    }

    /** @param {string} key */
    _selectOptionsFor(key) {
      const role = this._role(key);
      const raw = role.optionsBy ? (role.optionsBy[this.h.context()] || []) : (role.options || []);
      /** @type {Array<string | { value: string, label?: string }>} */
      const out = [];
      for (const o of raw) {
        if (o === '#num') {
          for (const c of this._cols().filter(c => c.type === 'numeric')) {
            out.push(c.label ? { value: c.name, label: c.label } : c.name);
          }
        } else { out.push(o); }
      }
      return out;
    }

    /** @param {string} key */
    _entryApplicable(key) {
      const spec = this.h.sections();
      const all = [...(spec.mapping || []), ...spec.presentation];
      const e = all.find(x => (typeof x === 'string' ? x : x.role) === key);
      if (!e) return false;
      return typeof e === 'string' || !e.types || e.types.includes(this.h.currentType());
    }

    /** @param {string} key @param {*} value */
    _fieldHelp(key, value) {
      // Filled fields get no help line: the select's value display already
      // shows `name  label` (labels-everywhere, blockr-select.js
      // fillOptContent), so echoing `name (label)` here would duplicate it.
      // The help line only surfaces the role hint while the field is empty.
      if (this._hasVal(value)) return '';
      const role = this._role(key);
      if (!role) return '';
      return (role.hintBy && role.hintBy[this.h.context()]) || role.ph || '';
    }

    // -- render ---------------------------------------------------------------
    render() {
      const cfg = this._cfg();
      const spec = this.h.sections();
      const pop = this.h.popoverEl();

      for (const s of Object.values(this._selects)) {
        if (s && typeof s.destroy === 'function') s.destroy();
      }
      this._selects = {};

      pop.innerHTML = '';

      // a11y: the gear popover is a configuration dialog. Label it so screen
      // readers announce it; the title element is its accessible name.
      pop.setAttribute('role', 'dialog');
      pop.setAttribute('aria-label', (this.h.title || 'Settings'));

      const title = document.createElement('div');
      title.className = 'blockr-popover-label dd-popover-title';
      title.id = (pop.id || 'dd-pop') + '-title';
      title.textContent = this.h.title || 'Settings';
      pop.setAttribute('aria-labelledby', title.id);
      pop.appendChild(title);

      // Type picker (optional — chart only)
      if (this.h.typeGroups && this.h.typeGroups.length) {
        const typesRow = document.createElement('div');
        typesRow.className = 'blockr-popover-row dd-popover-types';
        for (const g of this.h.typeGroups) {
          const group = document.createElement('div');
          group.className = 'dd-type-group';
          const glabel = document.createElement('span');
          glabel.className = 'dd-type-group-label';
          glabel.textContent = g.label ?? '';
          group.appendChild(glabel);
          const btns = document.createElement('div');
          btns.className = 'dd-cfg-types';
          for (const t of g.types) {
            const btn = document.createElement('button');
            btn.className = 'dd-type-btn' + (t === cfg[this.h.typeKey] ? ' dd-type-active' : '');
            btn.textContent = t;
            btn.addEventListener('click', () => this._onType(t));
            btns.appendChild(btn);
          }
          group.appendChild(btns);
          typesRow.appendChild(group);
        }
        pop.appendChild(typesRow);
      }

      // Mapping: required rows, then any always-on mapping controls (the
      // chart's metric + aggregation), shown-optional rows, add menu. Skipped
      // whole if the block has no mapping roles at all (e.g. the table).
      const shownOpt = spec.optionalMap.filter((/** @type {string} */ k) => this._hasVal(cfg[k]) || this._added.has(k));
      const remaining = spec.optionalMap.filter((/** @type {string} */ k) => !shownOpt.includes(k));
      const mapExtra = this._filterEntries(spec.mapping || []);
      if (spec.requiredMap.length || mapExtra.length || shownOpt.length || remaining.length) {
        const mapSec = this._sectionEl('Mapping');
        for (const key of spec.requiredMap) this._renderRole(mapSec, key, { required: true });
        this._renderEntries(mapSec, mapExtra);
        for (const key of shownOpt) this._renderRole(mapSec, key, { removable: true });
        if (remaining.length) this._addMappingMenu(mapSec, remaining);
      }

      // Presentation — formatting, sorting, layout: everything past the data
      // mapping. (The former "Encoding" section was folded away: chart metric +
      // aggregation moved up into Mapping; tile value-formatting moved here.)
      this._renderSection('Presentation', spec.presentation);

      // Drill-down (optional — host opts out by returning null from drillAutoLabel)
      if (this.h.drillAutoLabel) this._renderDrillSection();

      if (this.h.afterTypeChange) this.h.afterTypeChange();
    }

    /** @param {string} titleText */
    _sectionEl(titleText) {
      const sec = document.createElement('div');
      sec.className = 'dd-section';
      const h = document.createElement('div');
      h.className = 'dd-section-title';
      h.textContent = titleText;
      sec.appendChild(h);
      this.h.popoverEl().appendChild(sec);
      return sec;
    }

    // Normalise a section's entry list and drop the ones not applicable to the
    // current type / handled as a paired tail. Shared by the Mapping extras and
    // the Presentation section so both filter identically.
    /** @param {any[]} entries */
    _filterEntries(entries) {
      const ct = this.h.currentType();
      return entries
        .map(e => (typeof e === 'string' ? { role: e } : e))
        .filter(e => !e.types || e.types.includes(ct))
        .filter(e => !this._SECONDARY.has(e.role));
    }

    /** @param {HTMLElement} container @param {any[]} list */
    _renderEntries(container, list) {
      for (const e of list) {
        const required = this.h.entryRequired ? this.h.entryRequired(e.role) : false;
        this._renderRole(container, e.role, { required });
      }
    }

    /** @param {string} titleText @param {any[]} entries */
    _renderSection(titleText, entries) {
      const list = this._filterEntries(entries);
      if (!list.length) return;
      this._renderEntries(this._sectionEl(titleText), list);
    }

    /** @param {HTMLElement} container @param {string} key @param {{ required?: boolean, removable?: boolean }} [opts] */
    _renderRole(container, key, opts = {}) {
      const role = this._role(key);
      const paired = !!(role.pairedWith && this._entryApplicable(role.pairedWith));
      const row = document.createElement('div');
      row.className = 'blockr-popover-row dd-form-row dd-role-' + key +
        (paired ? ' dd-role-paired' : '');

      // Boolean option -> a single self-describing checkbox row: the checkbox
      // label carries the affirmative meaning (the on-option label), so the
      // usual head label would just repeat it.
      if (!opts.removable && this._isBoolSegmented(role) &&
          typeof Blockr !== 'undefined' && typeof Blockr.checkbox === 'function') {
        const controls = document.createElement('div');
        controls.className = 'dd-row-controls';
        this._buildControl(controls, key, { onChange: () => {} });
        row.appendChild(controls);
        container.appendChild(row);
        return;
      }

      const head = document.createElement('div');
      head.className = 'dd-row-head';
      const lbl = document.createElement('span');
      lbl.className = 'blockr-popover-label';
      lbl.textContent = role.label + (opts.required ? ' *' : '');
      head.appendChild(lbl);
      if (opts.removable) {
        const rm = document.createElement('button');
        rm.type = 'button';
        rm.className = 'dd-role-remove';
        rm.title = 'Remove ' + role.label;
        rm.innerHTML = '✕';
        rm.addEventListener('click', (e) => { e.stopPropagation(); this._removeRole(key); });
        head.appendChild(rm);
      }
      row.appendChild(head);

      const controls = document.createElement('div');
      controls.className = 'dd-row-controls';
      const helpEl = document.createElement('span');
      helpEl.className = 'dd-form-help';

      const markRequired = () => {
        row.classList.toggle('dd-role-required-empty',
          !!opts.required && !this._hasVal(this._cfg()[key]));
      };
      const setHelp = () => {
        if (role.kind !== 'column') { helpEl.style.display = 'none'; return; }
        const txt = this._fieldHelp(key, this._cfg()[key]);
        helpEl.textContent = txt;
        helpEl.style.display = txt ? '' : 'none';
      };

      this._buildControl(controls, key, { required: opts.required, onChange: () => { setHelp(); markRequired(); } });
      if (paired) this._buildControl(controls, role.pairedWith, { onChange: () => {} });
      row.appendChild(controls);
      row.appendChild(helpEl);
      setHelp();
      markRequired();
      container.appendChild(row);
    }

    _renderDrillSection() {
      const cfg = this._cfg();
      const d = cfg.drill;
      const on = this._hasVal(d);
      const sec = this._sectionEl('Drill-down');

      const tRow = document.createElement('div');
      tRow.className = 'blockr-popover-row dd-form-row';
      const onDrillToggle = (/** @type {boolean} */ enabled) => {
        if (!enabled) cfg.drill = '';
        else if (!this._hasVal(cfg.drill)) cfg.drill = 'auto';
        const wasOpen = this.h.isOpen();
        this.render();
        if (wasOpen) setTimeout(() => this.h.reopen(), 0);
        this.h.onChange('drill'); this.h.onClearFilter();
      };
      if (typeof Blockr !== 'undefined' && typeof Blockr.checkbox === 'function') {
        // Boolean data option -> checkbox (design-system rule); the label
        // states the affirmative meaning, checked = filtering is on.
        const box = Blockr.checkbox('Filter downstream on selection', on,
          (/** @type {boolean} */ checked) => onDrillToggle(checked));
        tRow.appendChild(box.el);
      } else {
        const tHead = document.createElement('div');
        tHead.className = 'dd-row-head';
        const tLbl = document.createElement('span');
        tLbl.className = 'blockr-popover-label';
        tLbl.textContent = 'Filter on selection';
        tHead.appendChild(tLbl);
        tRow.appendChild(tHead);
        this._buildPill(
          tRow,
          [{ value: 'off', label: 'No filter' }, { value: 'on', label: 'Filters downstream' }],
          on ? 'on' : 'off',
          (val) => onDrillToggle(val === 'on')
        );
      }
      sec.appendChild(tRow);

      if (on) {
        const autoLabel = this.h.drillAutoLabel();
        const row = document.createElement('div');
        row.className = 'blockr-popover-row dd-form-row';
        const head = document.createElement('div');
        head.className = 'dd-row-head';
        const lbl = document.createElement('span');
        lbl.className = 'blockr-popover-label';
        lbl.textContent = 'Filter on';
        head.appendChild(lbl);
        row.appendChild(head);
        const controls = document.createElement('div');
        controls.className = 'dd-row-controls';
        const wrap = document.createElement('div');
        wrap.className = 'blockr-popover-select-wrap dd-picker-wrap';
        const colOpt = (/** @type {VizColumn} */ c) => c.label ? { value: c.name, label: c.label } : c.name;
        const opts = [{ value: 'auto', label: autoLabel }, ...this._cols().map(colOpt)];
        const sel = (this._hasVal(cfg.drill) && cfg.drill !== 'auto') ? cfg.drill : 'auto';
        const onSel = (/** @type {string} */ val) => { cfg.drill = val; this.h.onChange('drill'); this.h.onClearFilter(); };
        if (typeof Blockr !== 'undefined' && Blockr.Select) {
          this._selects['drill'] = Blockr.Select.single(wrap, { options: opts, selected: sel, onChange: onSel });
        } else {
          const s = document.createElement('select');
          s.className = 'dd-cfg-select';
          for (const o of opts) {
            const val = (typeof o === 'object' && o) ? o.value : o;
            const txt = (typeof o === 'object' && o && o.label) ? o.label : val;
            const op = document.createElement('option');
            op.value = val; op.textContent = txt;
            if (val === sel) op.selected = true;
            s.appendChild(op);
          }
          s.addEventListener('change', () => onSel(s.value));
          wrap.appendChild(s);
        }
        controls.appendChild(wrap);
        row.appendChild(controls);
        sec.appendChild(row);
      }
    }

    /** @param {HTMLElement} parent @param {string} key @param {{ required?: boolean, onChange?: () => void }} [param2] */
    _buildControl(parent, key, { required, onChange } = {}) {
      const role = this._role(key);
      const cb = onChange || (() => {});
      const cfg = this._cfg();
      if (role.kind === 'column') {
        const opts = this._colOptionsFor(key, { required });
        const wrap = document.createElement('div');
        wrap.className = 'blockr-popover-select-wrap dd-picker-wrap';
        const sel = (cfg[key] && cfg[key] !== '(none)') ? cfg[key] : (required ? '' : '(none)');
        const onSel = (/** @type {string} */ val) => {
          cfg[key] = (val === '(none)') ? '' : val;
          this._rememberRole(key, cfg[key]);
          cb();
          this.h.onChange(key);
          this.h.onClearFilter();
        };
        this._mkSelect(wrap, opts, sel, onSel, key, true);
        parent.appendChild(wrap);
      } else if (role.kind === 'select') {
        const opts = this._selectOptionsFor(key);
        const wrap = document.createElement('div');
        wrap.className = 'blockr-popover-select-wrap dd-picker-wrap';
        const cur = cfg[key];
        const selv = this._hasVal(cur) ? cur : ((typeof opts[0] === 'object' && opts[0]) ? opts[0].value : opts[0]);
        const onSel = (/** @type {string} */ val) => {
          cfg[key] = val; cb(); this.h.onChange(key);
          // Some selects gate which other rows are shown (e.g. the table's
          // Coloring mode reveals the column-scope picker). Re-render so the
          // section list updates live, preserving open state.
          if (role.rerender) {
            const wasOpen = this.h.isOpen();
            this.render();
            if (wasOpen) setTimeout(() => this.h.reopen(), 0);
          }
        };
        this._mkSelect(wrap, opts, selv, onSel, key, false);
        parent.appendChild(wrap);
      } else if (role.kind === 'columns') {
        // Multi-column scope picker. Wires the existing Blockr.Select.multi
        // primitive; empty selection is meaningful (= "all", per the host's
        // placeholder), so there is no '(none)' option. Value is an array.
        const opts = this._colOptionsFor(key, { required: true });
        const sel = Array.isArray(cfg[key]) ? cfg[key].slice() : [];
        const wrap = document.createElement('div');
        wrap.className = 'blockr-popover-select-wrap dd-picker-wrap';
        const onSel = (/** @type {string[]} */ vals) => { cfg[key] = vals; cb(); this.h.onChange(key); };
        if (typeof Blockr !== 'undefined' && Blockr.Select && Blockr.Select.multi) {
          this._selects[key] = Blockr.Select.multi(wrap, {
            options: opts, selected: sel, reorderable: false,
            placeholder: role.placeholder || 'All', onChange: onSel
          });
        } else {
          const s = document.createElement('select');
          s.className = 'dd-cfg-select'; s.multiple = true;
          for (const o of opts) {
            const val = (typeof o === 'object' && o) ? o.value : o;
            const txt = (typeof o === 'object' && o && o.label) ? o.label : val;
            const op = document.createElement('option');
            op.value = val; op.textContent = txt;
            if (sel.indexOf(val) >= 0) op.selected = true;
            s.appendChild(op);
          }
          s.addEventListener('change', () => onSel(
            Array.prototype.slice.call(s.selectedOptions).map(o => o.value)));
          wrap.appendChild(s);
        }
        parent.appendChild(wrap);
      } else if (role.kind === 'segmented') {
        const cur = this._hasVal(cfg[key]) ? cfg[key] : role.options[0].value;
        if (this._isBoolSegmented(role) &&
            typeof Blockr !== 'undefined' && typeof Blockr.checkbox === 'function') {
          const onOpt = role.options.find((/** @type {any} */ o) => o.value === 'on');
          const box = Blockr.checkbox(onOpt.label, cur === 'on',
            (/** @type {boolean} */ checked) => {
              cfg[key] = checked ? 'on' : 'off';
              cb();
              this.h.onChange(key);
            });
          const wrap = document.createElement('div');
          wrap.className = 'dd-pill-wrap';
          wrap.appendChild(box.el);
          parent.appendChild(wrap);
        } else {
          this._buildPill(parent, role.options, cur, (/** @type {string} */ val) => {
            cfg[key] = val; cb(); this.h.onChange(key);
          });
        }
      } else if (role.kind === 'text') {
        // Canonical popover text input (blockr.dplyr blockr-blocks.css). Note:
        // no .dd-picker-wrap here — that wrapper carries its own border for the
        // borderless Blockr.Select; a bordered input inside it double-borders.
        const inp = document.createElement('input');
        inp.type = 'text';
        inp.className = 'blockr-popover-input';
        inp.value = (cfg[key] == null) ? '' : String(cfg[key]);
        if (role.ph) inp.placeholder = role.ph;
        /** @type {ReturnType<typeof setTimeout> | undefined} */
        let deb;
        inp.addEventListener('input', () => {
          cfg[key] = inp.value;
          clearTimeout(deb);
          deb = setTimeout(() => { cb(); this.h.onChange(key); }, 300);
        });
        parent.appendChild(inp);
      } else if (role.kind === 'slider') {
        this._buildSlider(parent, key);
      } else if (role.kind === 'color') {
        this._buildColor(parent, key, cb);
      }
    }

    // blockr.ggplot extension: color control = native swatch + "Theme
    // default" checkbox. '' (empty) means "keep the theme default"; a hex
    // value overrides it. Picking a color unchecks the default box; checking
    // it clears the value (the swatch keeps its last hex for re-enabling).
    /** @param {HTMLElement} parent @param {string} key @param {() => void} cb */
    _buildColor(parent, key, cb) {
      const cfg = this._cfg();
      const role = this._role(key);
      const cur = cfg[key];
      const hasColor = this._hasVal(cur);
      const wrap = document.createElement('div');
      wrap.className = 'dd-color-wrap' + (hasColor ? '' : ' dd-color-default');
      const input = document.createElement('input');
      input.type = 'color';
      input.className = 'dd-color-swatch';
      input.value = hasColor ? cur : (role.fallback || '#ffffff');
      wrap.appendChild(input);
      /** @type {{ set: (v: boolean) => void } | null} */
      let box = null;
      input.addEventListener('input', () => {
        cfg[key] = input.value;
        wrap.classList.remove('dd-color-default');
        if (box) box.set(false);
        cb();
        this.h.onChange(key);
      });
      if (typeof Blockr !== 'undefined' && typeof Blockr.checkbox === 'function') {
        box = Blockr.checkbox('Theme default', !hasColor, (checked) => {
          cfg[key] = checked ? '' : input.value;
          wrap.classList.toggle('dd-color-default', checked);
          cb();
          this.h.onChange(key);
        });
        wrap.appendChild(/** @type {any} */ (box).el);
      }
      parent.appendChild(wrap);
    }

    // Build a Blockr.Select (or native fallback). `decorate` shows
    // `name (label)` option text (column pickers); else just the label.
    /**
     * @param {HTMLElement} wrap @param {any[]} opts @param {string} selected
     * @param {(val: string) => void} onSel @param {string} key @param {boolean} decorate
     */
    _mkSelect(wrap, opts, selected, onSel, key, decorate) {
      if (typeof Blockr !== 'undefined' && Blockr.Select) {
        // blockr.ggplot extension: role.ph doubles as the select placeholder
        // so ''-valued options ("Auto", "None") don't render an empty control.
        const ph = (this._role(key) || {}).ph;
        this._selects[key] = Blockr.Select.single(wrap, { options: opts, selected, placeholder: ph, onChange: onSel });
      } else {
        const s = document.createElement('select');
        s.className = 'dd-cfg-select';
        for (const o of opts) {
          const val = (typeof o === 'object' && o) ? o.value : o;
          const txt = (typeof o === 'object' && o && o.label)
            ? (decorate ? `${o.value} (${o.label})` : o.label) : val;
          const op = document.createElement('option');
          op.value = val; op.textContent = txt;
          if (val === selected) op.selected = true;
          s.appendChild(op);
        }
        s.addEventListener('change', () => onSel(s.value));
        wrap.appendChild(s);
      }
    }

    /** @param {HTMLElement} parent @param {string} key */
    _buildSlider(parent, key) {
      const cfg = this._cfg();
      // Bounds/unit come from the role spec when given; defaults match the
      // canonical blockr.viz slider (font/size multipliers).
      const role = this.h.roles[key] || {};
      const min = (typeof role.min === 'number') ? role.min : 0.5;
      const max = (typeof role.max === 'number') ? role.max : 3.0;
      const step = (typeof role.step === 'number') ? role.step : 0.1;
      const unit = (typeof role.unit === 'string') ? role.unit : '×';
      const dec = Math.max(0, Math.ceil(-Math.log10(step)));
      /** @param {number} v */
      const fmt = (v) => v.toFixed(dec) + unit;
      const init = cfg[key];
      const v0 = (typeof init === 'number' && isFinite(init)) ? init : 1.0;
      const wrap = document.createElement('div');
      wrap.className = 'dd-slider-wrap';
      const input = document.createElement('input');
      input.type = 'range';
      input.min = String(min); input.max = String(max); input.step = String(step);
      input.value = String(v0);
      input.className = 'dd-slider';
      wrap.appendChild(input);
      const value = document.createElement('span');
      value.className = 'dd-slider-value';
      value.textContent = fmt(v0);
      wrap.appendChild(value);
      /** @type {ReturnType<typeof setTimeout> | undefined} */
      let debounce;
      input.addEventListener('input', () => {
        const v = parseFloat(input.value);
        value.textContent = fmt(v);
        cfg[key] = v;
        clearTimeout(debounce);
        debounce = setTimeout(() => this.h.onMults(), 150);
      });
      parent.appendChild(wrap);
    }

    // Click-through pill (blockr.dplyr idiom: arrange dir-btn, filter op-toggle,
    // pivot drop-na). One self-labeling .blockr-pill that cycles through
    // `options` ([{value,label}]) on click; highlighted (blockr-popover-toggle-
    // active) whenever the value is off its first/default option. Replaces the
    // old two-button .dd-segmented control.
    /**
     * @param {HTMLElement} parent @param {Array<{ value: string, label: string }>} options
     * @param {string} current @param {(val: string) => void} onPick
     */
    _buildPill(parent, options, current, onPick) {
      const wrap = document.createElement('div');
      wrap.className = 'dd-pill-wrap';
      const btn = document.createElement('button');
      btn.type = 'button';
      btn.className = 'blockr-pill blockr-popover-toggle';
      let idx = options.findIndex((o) => o.value === current);
      if (idx < 0) idx = 0;
      const paint = () => {
        btn.textContent = options[idx].label;
        btn.classList.toggle('blockr-popover-toggle-active', idx !== 0);
      };
      paint();
      btn.addEventListener('click', () => {
        idx = (idx + 1) % options.length;
        paint();
        onPick(options[idx].value);
      });
      wrap.appendChild(btn);
      parent.appendChild(wrap);
      return btn;
    }

    /** @param {HTMLElement} container @param {string[]} remaining */
    _addMappingMenu(container, remaining) {
      const wrap = document.createElement('div');
      wrap.className = 'dd-add-wrap';
      const bar = document.createElement('div');
      bar.className = 'blockr-add-row';
      const btn = document.createElement('button');
      btn.type = 'button';
      btn.className = 'blockr-add-link dd-add-trigger';
      const plus = (typeof Blockr !== 'undefined' && Blockr.icons) ? Blockr.icons.plus : '+';
      btn.innerHTML = `<span class="blockr-add-icon">${plus}</span> Add mapping`;
      const menu = document.createElement('div');
      menu.className = 'dd-add-menu';
      menu.style.display = 'none';
      for (const key of remaining) {
        const item = document.createElement('button');
        item.type = 'button';
        item.className = 'dd-add-item';
        item.textContent = this._role(key).label;
        item.addEventListener('click', (e) => {
          e.stopPropagation();
          menu.style.display = 'none';
          this._addRole(key);
        });
        menu.appendChild(item);
      }
      btn.addEventListener('click', (e) => {
        e.stopPropagation();
        menu.style.display = (menu.style.display === 'none') ? '' : 'none';
      });
      bar.appendChild(btn);
      wrap.appendChild(bar);
      wrap.appendChild(menu);
      container.appendChild(wrap);
    }

    /** @param {string} key */
    _addRole(key) {
      const cfg = this._cfg();
      this._added.add(key);
      if (!this._hasVal(cfg[key]) && this._roleMemory[key] &&
          this._colExists(this._roleMemory[key]) && this._colFits(key, this._roleMemory[key])) {
        cfg[key] = this._roleMemory[key];
        this.h.onChange(key);
      }
      const wasOpen = this.h.isOpen();
      this.render();
      if (wasOpen) setTimeout(() => this.h.reopen(), 0);
    }

    /** @param {string} key */
    _removeRole(key) {
      const cfg = this._cfg();
      cfg[key] = '';
      this._added.delete(key);
      delete this._roleMemory[key];
      const wasOpen = this.h.isOpen();
      this.render();
      if (wasOpen) setTimeout(() => this.h.reopen(), 0);
      this.h.onChange(key);
      this.h.onClearFilter();
    }

    // Type/family switch (chart). No-op-ish when there are no families.
    /** @param {string} t */
    _onType(t) {
      const cfg = this._cfg();
      const oldFam = this.h.familyFor ? this.h.familyFor(cfg[this.h.typeKey]) : null;
      cfg[this.h.typeKey] = t;
      const newFam = this.h.familyFor ? this.h.familyFor(t) : null;
      if (this.h.familyFor && oldFam !== newFam) {
        this._carryRoles(newFam);
        this.h.onClearFilter();
      }
      if (this.h.ensureDefaults) this.h.ensureDefaults();
      const wasOpen = this.h.isOpen();
      this.render();
      if (wasOpen) setTimeout(() => this.h.reopen(), 0);
      this.h.onChange(this.h.typeKey);
    }

    // Identity-carry + sticky memory across a family switch.
    /** @param {string | null} newFam */
    _carryRoles(newFam) {
      const spec = this.h.sectionsForFamily(newFam);
      const cfg = this._cfg();
      const mapKeys = (spec.mapping || []).map((/** @type {any} */ e) => (typeof e === 'string' ? e : e.role));
      const keep = new Set([...spec.requiredMap, ...spec.optionalMap, ...mapKeys]);
      for (const key of Object.keys(this.h.roles)) {
        if (this.h.roles[key].kind !== 'column') continue;
        if (this.h.carryKeep && this.h.carryKeep.includes(key)) continue;
        const v = cfg[key];
        if (!this._hasVal(v)) continue;
        if (!(keep.has(key) && this._colFits(key, v))) {
          this._roleMemory[key] = v;
          cfg[key] = '';
        }
      }
      for (const key of spec.requiredMap) {
        const mem = this._roleMemory[key];
        if (!this._hasVal(cfg[key]) && mem && this._colExists(mem) && this._colFits(key, mem)) {
          cfg[key] = mem;
        }
      }
    }
  }

  const ns = /** @type {BlockrNamespace} */ (
    (typeof Blockr !== 'undefined') ? Blockr
      : (window.Blockr = window.Blockr || /** @type {BlockrNamespace} */ ({})));
  ns.DrilldownConfig = DrilldownConfig;
  window.DrilldownConfig = DrilldownConfig;
})();
