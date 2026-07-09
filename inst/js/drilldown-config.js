/**
 * CANONICAL SOURCE: blockr.viz/inst/js/drilldown-config.js
 * Vendored verbatim into blockr.ggplot/inst/js/. Edit here, then copy across.
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
 *                        header after the role-picker rows, e.g. value + agg)
 *   sectionsForFamily(fam) -> the same for a specific family (carry-over)
 *   secondary        -> Set of paired-tail role keys (skipped in section loops)
 *   typeKey          -> the config key the type picker writes (e.g. 'chart_type')
 *   typeGroups       -> [{label, types:[...]}] for the type picker, or null
 *   typeTiles        -> render the type picker as an icon-over-label tile grid
 *                       (design-system type-picker proposal B) instead of the
 *                       segmented strip; group labels become a field label
 *                       above the grid, or per-group headings with 2+ groups
 *   typeIcon(t)      -> inline SVG for a type button/tile, or '' (optional)
 *   familyFor(type)  -> family string for a type, or null (no families)
 *   entryRequired(role) -> mark a section role required (chart: value in aggregated)
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
      /** @type {Record<string, boolean> | null} */
      this._openSec = null;         // capability-section open state (lazy)
      /** @type {MutationObserver | null} */
      this._closeWatch = null;      // armed deferred re-render (multi picks)
    }

    // Rebuild the popover from the host's CURRENT config/columns, dropping
    // the per-session section-open memory so the capability checkboxes
    // (Aggregation, Drill-down, …) re-seed from the config. A host calls this
    // when the gear OPENS on state that may have changed server-side since
    // the popover was built (state restore, AI / external_ctrl edits) — see
    // table.js's refreshState(); plain interactions keep using render() /
    // _rerender(), which preserve the open-section memory.
    refresh() {
      this._openSec = null;
      this.render();
    }

    // -- small helpers --------------------------------------------------------
    get _SECONDARY() { return this.h.secondary; }
    /** @param {*} v */
    // An empty multi-column selection ([]) counts as "no value": otherwise a
    // required `columns` role never gets the amber required-empty cue.
    _hasVal(v) {
      if (Array.isArray(v)) return v.length > 0;
      return v !== null && v !== undefined && v !== '' && v !== '(none)';
    }
    _cols() { return this.h.columns() || []; }
    _cfg() { return this.h.config(); }
    /** @param {string} key */
    _role(key) { return this.h.roles[key]; }
    /** @param {string} name */
    _colExists(name) { return this._cols().some(c => c.name === name); }

    // colType / allowCount may be declared as a function of the current config
    // (e.g. the chart value widens to any column under agg "count_distinct").
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
          for (const c of this._cols().filter(cc => cc.type === 'numeric')) {
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

    /**
     * A help line speaks about the field's VALUE, so it renders whatever the
     * value is, empty or not. It must never restate the placeholder, which
     * speaks about the empty slot and disappears on fill, nor the label, which
     * names the field. See blockr.docs design-system/ux-principles.md.
     *
     * There is deliberately no `role.ph` fallback: echoing the placeholder
     * beneath its own control printed the same string twice.
     *
     * @param {string} key
     */
    _fieldHelp(key) {
      const role = this._role(key);
      if (!role) return '';
      // `hint` is a context-independent help line; `hintBy` keys it by context.
      return (role.hintBy && role.hintBy[this.h.context()]) || role.hint || '';
    }

    // -- render ---------------------------------------------------------------
    render() {
      const cfg = this._cfg();
      const spec = this.h.sections();
      const pop = this.h.popoverEl();

      // A full rebuild reflects the current config, so an armed deferred
      // re-render (multi-select dropdown still open) is moot — and its
      // observed select is about to be destroyed anyway.
      this._dropCloseWatch();
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
      if (this.h.typeGroups && this.h.typeGroups.length && this.h.typeTiles) {
        // Tile type picker (design-system type-picker proposal B). One group ->
        // its label is a normal field label above the grid; several groups
        // -> per-group micro-headings inside one grid.
        const typesRow = document.createElement('div');
        typesRow.className = 'blockr-popover-row dd-popover-types dd-popover-types-tiles';
        const single = this.h.typeGroups.length === 1;
        for (const g of this.h.typeGroups) {
          if (g.label) {
            const glabel = document.createElement('div');
            glabel.className = single
              ? 'blockr-popover-label dd-type-grid-label'
              : 'dd-type-group-head';
            glabel.textContent = g.label;
            typesRow.appendChild(glabel);
          }
          const grid = document.createElement('div');
          grid.className = 'dd-type-grid';
          for (const t of g.types) {
            const btn = document.createElement('button');
            btn.type = 'button';
            btn.className = 'dd-type-tile' +
              (t === cfg[this.h.typeKey] ? ' dd-type-active' : '');
            btn.title = t;
            const ic = this.h.typeIcon ? this.h.typeIcon(t) : '';
            btn.innerHTML = (ic ? '<span class="dd-type-tile-icon">' + ic + '</span>' : '') +
              '<span class="dd-type-tile-label">' + t + '</span>';
            btn.addEventListener('click', () => this._onType(t));
            grid.appendChild(btn);
          }
          typesRow.appendChild(grid);
        }
        pop.appendChild(typesRow);
      } else if (this.h.typeGroups && this.h.typeGroups.length) {
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
            // Optional subtle icon before the label (host.typeIcon).
            const ic = this.h.typeIcon ? this.h.typeIcon(t) : '';
            if (ic) {
              btn.innerHTML = ic;
              const lbl = document.createElement('span');
              lbl.textContent = t;
              btn.appendChild(lbl);
            } else {
              btn.textContent = t;
            }
            btn.addEventListener('click', () => this._onType(t));
            btns.appendChild(btn);
          }
          group.appendChild(btns);
          typesRow.appendChild(group);
        }
        pop.appendChild(typesRow);
      }

      // Optional input-type badge (a small chip above Mapping). A host sets
      // spec.badge to name the input when the mapping controls are absent —
      // e.g. an already-summarized ("annotated data frame") table has no
      // pickable columns to group or aggregate. spec.badgeTitle is the hover
      // tooltip with the fuller explanation.
      if (spec.badge) {
        const chip = document.createElement('span');
        chip.className = 'dd-input-badge';
        chip.textContent = spec.badge;
        if (spec.badgeTitle) chip.title = spec.badgeTitle;
        pop.appendChild(chip);
      }

      // Mapping: required rows, then any always-on mapping controls (the
      // chart's value + aggregation), shown-optional rows, add menu. Skipped
      // whole if the block has no mapping roles at all (e.g. the table).
      // optionalMap entries may be plain role keys or { role, types } — the
      // latter offers the role only for those chart types (e.g. the chart's
      // color is inert on pie/treemap, so it is not offered there).
      const optKeys = this._filterEntries(spec.optionalMap || []).map(e => e.role);
      const shownOpt = optKeys.filter((/** @type {string} */ k) => this._hasVal(cfg[k]) || this._added.has(k));
      const remaining = optKeys.filter((/** @type {string} */ k) => !shownOpt.includes(k));
      const mapExtra = this._filterEntries(spec.mapping || []);

      // Mapping section: required + optional (add-as-needed) display roles. A
      // pure-aggregation host (the table) has none of these, so it is skipped
      // and only the Aggregation section below shows; the chart renders its
      // aesthetics here; the tile renders value / measure / secondary / … here
      // AND its group + summaries in the Aggregation section below. The mapping
      // extras (chart: value + func) sit here unless the host splits them
      // into a trailing aggTitle section, or owns them in the Aggregation
      // checkbox section (aggregatable hosts — the group lives there).
      const mapNeeded = spec.requiredMap.length || shownOpt.length ||
        remaining.length || (!spec.aggregatable && mapExtra.length);
      if (mapNeeded) {
        const mapSec = this._sectionEl(this._mappingTitle('Mapping'));
        for (const key of spec.requiredMap) this._renderRole(mapSec, key, { required: true });
        if (!spec.aggTitle && !spec.aggregatable) this._renderEntries(mapSec, mapExtra);
        // Repeatable aggregation list under Mapping only for non-aggregatable
        // value hosts; aggregatable hosts render it in the Aggregation section.
        if (!spec.aggregatable && spec.summaries && this.h.metricsList) this._renderMetrics(mapSec);
        for (const key of shownOpt) this._renderRole(mapSec, key, { removable: true });
        if (remaining.length) this._addMappingMenu(mapSec, remaining);

        // ggplot-style split: the aggregation stat gets its own section after
        // the aesthetic mapping (host opts in via spec.aggTitle). With
        // spec.aggMetrics the section renders the REPEATABLE summaries list
        // instead of the single value+func pair (the chart's bar type —
        // one series per aggregation x column); other types keep the pair.
        if (spec.aggTitle && (mapExtra.length || spec.aggMetrics)) {
          const aggSec = this._sectionEl(spec.aggTitle);
          if (spec.aggMetrics && this.h.metricsList) this._renderMetrics(aggSec);
          else this._renderEntries(aggSec, mapExtra);
        }
      }

      // Presentation — formatting, sorting, layout: everything past the data
      // mapping. Renders BEFORE the capability checkboxes: the always-on
      // sections (Mapping / the chart's required stat / Presentation) come
      // first, and the opt-in capabilities cluster at the bottom in a fixed
      // order (Aggregation → Drill-down → Coloring → Row color), so every
      // block's gear reads the same top-to-bottom: what it shows, how it
      // looks, what it can do.
      this._renderSection('Presentation', spec.presentation);

      // Aggregation as a checkbox capability (Variant A). Activation is
      // DECOUPLED from the group: checking seeds a default value (a count) so
      // the box reads "on" before any group is picked, and unchecking clears
      // both the summaries and the group. With the box on and NO group the
      // summaries reduce the whole frame to a single totals row (grand totals);
      // with a group, one row per group level. The group picker (mapExtra) and
      // the repeatable summaries list both render inside this section.
      if (spec.aggregatable) {
        const hasMetrics = !!(cfg.summaries && cfg.summaries.length);
        // spec.aggSeedsFromGroup === false (the tile): the group is a MAPPING
        // (clustering of precomputed cards) that lives outside this section —
        // aggregation is active iff summaries are set, and unchecking must NOT
        // clear the clustering group. Default (the table): the group lives in
        // this section, seeds the open state and clears on uncheck.
        const groupIsAgg = spec.aggSeedsFromGroup !== false;
        const hasGrp = groupIsAgg && !!(cfg.group && cfg.group.length);
        const open = this._secOpen('agg', () => hasMetrics || hasGrp);
        const clearGroup = () => {
          if (!groupIsAgg) return;
          cfg.group = Array.isArray(cfg.group) ? [] : '';
          this.h.onChange('group');
        };
        const sec = this._sectionEl(this._mappingTitle('Aggregation'), {
          toggle: { checked: open, onToggle: (on) => this._toggleSection('agg', on,
            () => { clearGroup(); if (this.h.onMetricsChange) this.h.onMetricsChange([]); },
            () => { if (this.h.onMetricsChange && !hasMetrics)
                      this.h.onMetricsChange([{ func: 'count', cols: [] }]); }) }
        });
        if (open) {
          this._renderEntries(sec, mapExtra);
          if (spec.summaries && this.h.metricsList) this._renderMetrics(sec);
        }
      }

      // Drill-down as a checkbox capability (Variant A). spec.drillToggle names
      // the config key the picker writes (the table's 'drill'). Checked reveals
      // the filter-column picker; unchecking clears it.
      if (spec.drillToggle) {
        this._renderToggleColumnSection('Drill-down', 'drill', spec.drillToggle,
          spec.drillDefault);
      }

      // Chart / tile drill-down. The chart opts in via drillAutoLabel (the
      // Auto + column picker); the tile via drillHint (picker-less — its
      // target is structurally determined). Same slot as drillToggle so all
      // drill styles land in the same position of the capability cluster.
      if (this.h.drillAutoLabel || this.h.drillHint) this._renderDrillSection();

      // COLOR — a PLAIN section (deliberately NOT a checkbox capability:
      // unlike Aggregation/Drill, checking would seed nothing real — color's
      // activation lives in the picks; "(none)" / no shading rows IS off).
      // spec.colorSection = { colorKey, shadings }:
      //   colorKey — the categorical IDENTITY color ("Color by"): the chart's
      //     color aesthetic applied to rows/cards via the board scale map.
      //   shadings — true renders the repeatable VALUE-encoding rules
      //     ("Shade cells [mode] on [cols]") via shadingsList()/
      //     onShadingsChange() (table only; a tile has no cell matrix).
      if (spec.colorSection) {
        const sec = this._sectionEl('Color');
        this._renderRole(sec, spec.colorSection.colorKey);
        if (spec.colorSection.shadings && this.h.shadingsList) {
          this._renderShadings(sec);
        }
      }

      if (this.h.afterTypeChange) this.h.afterTypeChange();
    }

    /**
     * @param {string} titleText
     * @param {{ toggle?: { checked: boolean, onToggle: (on: boolean) => void } }} [opts]
     *   When `toggle` is given the header carries a checkbox (Variant A): the
     *   section is a capability that is off by default and reveals its body only
     *   when checked.
     */
    _sectionEl(titleText, opts = {}) {
      const sec = document.createElement('div');
      sec.className = 'dd-section';
      const h = document.createElement('div');
      h.className = 'dd-section-title';
      if (opts.toggle) {
        const toggle = opts.toggle;
        h.classList.add('dd-section-title--toggle');
        const box = document.createElement('span');
        box.className = 'dd-section-checkbox' + (toggle.checked ? ' dd-on' : '');
        box.setAttribute('role', 'checkbox');
        box.setAttribute('tabindex', '0');
        box.setAttribute('aria-checked', toggle.checked ? 'true' : 'false');
        const label = document.createElement('span');
        label.textContent = titleText;
        h.appendChild(box);
        h.appendChild(label);
        const flip = (/** @type {Event} */ e) => {
          e.stopPropagation();
          toggle.onToggle(!toggle.checked);
        };
        h.addEventListener('click', flip);
        // The checkbox is focusable (role=checkbox, tabindex=0) but the click
        // listener sits on the header row — give Space/Enter the same toggle.
        box.addEventListener('keydown', (e) => {
          if (e.key !== ' ' && e.key !== 'Enter') return;
          e.preventDefault();   // Space must toggle, not scroll the popover
          flip(e);
        });
      } else {
        h.textContent = titleText;
      }
      sec.appendChild(h);
      this.h.popoverEl().appendChild(sec);
      return sec;
    }

    // Per-section open state for the Variant A toggle sections (aggregation,
    // drill-down). Persists across re-renders (it is an instance field), seeded
    // from the config the first time a section is seen.
    /** @param {string} key @param {() => boolean} initial */
    _secOpen(key, initial) {
      if (!this._openSec) this._openSec = /** @type {Record<string, boolean>} */ ({});
      if (!(key in this._openSec)) this._openSec[key] = !!initial();
      return this._openSec[key];
    }
    /** @param {string} key @param {boolean} on */
    _setSecOpen(key, on) {
      if (!this._openSec) this._openSec = /** @type {Record<string, boolean>} */ ({});
      this._openSec[key] = on;
    }
    // Flip a toggle section; when turning OFF run offFn to clear the capability
    // (raw table / no drill / no coloring), when turning ON run the optional
    // onFn to seed a default (e.g. the coloring mode), then re-render preserving
    // the open popover.
    /** @param {string} key @param {boolean} on @param {() => void} offFn @param {() => void} [onFn] */
    _toggleSection(key, on, offFn, onFn) {
      this._setSecOpen(key, on);
      if (on) { if (typeof onFn === 'function') onFn(); }
      else if (typeof offFn === 'function') offFn();
      this._rerender();
    }

    // A checkbox capability whose body is a single required column picker
    // (Drill-down, Row color). Off = header only; on reveals the picker (no
    // '(none)' option — the checkbox IS the none state); unchecking clears the
    // config key. Seeds/persists its open state via _secOpen under `secKey`.
    /** @param {string} title @param {string} secKey @param {string} cfgKey */
    /**
     * @param {string} title @param {string} secKey @param {string} cfgKey
     * @param {string} [seed] column pre-filled when the user CHECKS the box
     *   (e.g. the table's rowname/stub for drill), so the capability works in
     *   one click; the picker stays for re-aiming. Empty/absent = the picker
     *   opens required-empty as before.
     */
    _renderToggleColumnSection(title, secKey, cfgKey, seed) {
      const cfg = this._cfg();
      const open = this._secOpen(secKey,
        () => this._hasVal(cfg[cfgKey]) && cfg[cfgKey] !== '(none)');
      const sec = this._sectionEl(title, {
        toggle: { checked: open, onToggle: (on) =>
          this._toggleSection(secKey, on,
            // Unchecking the capability also clears the active emitted
            // filter (single source of truth: the engine's off-branch, same
            // as _renderDrillSection) — otherwise downstream stays filtered
            // on the last click with clicks now inert.
            () => { cfg[cfgKey] = ''; this.h.onChange(cfgKey);
                    this.h.onClearFilter(); },
            () => {
              if (!this._hasVal(cfg[cfgKey]) && seed && this._colExists(seed)) {
                cfg[cfgKey] = seed;
                this.h.onChange(cfgKey);
              }
            }) }
      });
      if (open) this._renderRole(sec, cfgKey, { required: true });
    }

    // Resolve the mapping-section header title. A host may supply a plain string
    // (the table's "Aggregation") or a function of the current state (the chart,
    // which reads "Aggregation" only for its aggregated family, "Mapping" else).
    /** @param {string} fallback */
    _mappingTitle(fallback) {
      const mt = this.h.mappingTitle;
      const v = (typeof mt === 'function') ? mt() : mt;
      return v || fallback;
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
      // Verb-object pair (e.g. value+func): render "[agg] of [value]" with
      // the aggregation leading; the value only shows for aggregations that
      // consume it (a bare row count ignores it, reading just "Count").
      const reversed = paired && !!role.pairReversed;
      const usesMetric = () => {
        const a = this._cfg()[role.pairedWith];
        return !!a && a !== 'count';
      };
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
      // In a reversed pair the required marker tracks the value, which is only
      // needed for aggregations that consume it (not a bare count).
      const reqMark = opts.required && (!reversed || usesMetric());
      // A role.label may be a function of the current config (e.g. the chart's
      // value reads "Value" on a boxplot, "Aggregate" when it feeds an
      // aggregation), so resolve it before use.
      const roleLabel = (typeof role.label === 'function') ? role.label(this._cfg()) : role.label;
      lbl.textContent = roleLabel + (reqMark ? ' *' : '');
      head.appendChild(lbl);
      if (opts.removable) {
        const rm = document.createElement('button');
        rm.type = 'button';
        rm.className = 'dd-role-remove';
        rm.title = 'Remove ' + roleLabel;
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
          !!opts.required && (!reversed || usesMetric()) &&
          !this._hasVal(this._cfg()[key]));
      };
      const setHelp = () => {
        const picker = role.kind === 'column' || role.kind === 'columns';
        if (!picker || (reversed && !usesMetric())) {
          helpEl.style.display = 'none'; return;
        }
        const txt = this._fieldHelp(key);
        helpEl.textContent = txt;
        helpEl.style.display = txt ? '' : 'none';
      };

      if (reversed) {
        // "[agg ▾] of [value ▾]" — aggregation leads; value only when used.
        this._buildControl(controls, role.pairedWith, { onChange: () => {} });
        if (usesMetric()) {
          const of = document.createElement('span');
          of.className = 'dd-pair-connector';
          of.textContent = 'of';
          controls.appendChild(of);
          this._buildControl(controls, key, { required: opts.required, onChange: () => { setHelp(); markRequired(); } });
        }
      } else {
        this._buildControl(controls, key, { required: opts.required, onChange: () => { setHelp(); markRequired(); } });
        if (paired) this._buildControl(controls, role.pairedWith, { onChange: () => {} });
      }
      row.appendChild(controls);
      row.appendChild(helpEl);
      setHelp();
      markRequired();
      container.appendChild(row);
    }

    // Re-render the popover, preserving the open state (used by the repeatable
    // summaries group when a row is added / removed / its function changes).
    _rerender() {
      const wasOpen = this.h.isOpen();
      this.render();
      if (wasOpen) setTimeout(() => this.h.reopen(), 0);
    }

    // Deferred gated re-render for a MULTI-select role with rerender
    // semantics (the table's Group picker): Blockr.Select keeps its dropdown
    // open across picks, so rebuilding the popover on every onChange would
    // destroy the dropdown after each pick — selecting three columns meant
    // reopening it three times. Instead, watch the select's open state
    // (blockr-select.js mirrors it on the control root via aria-expanded)
    // and re-render ONCE when the dropdown closes; the config itself is
    // committed per pick (onChange already ran), only the popover rebuild
    // waits. Single-select roles keep their immediate re-render: their
    // dropdown closes on pick anyway.
    /** @param {string} key */
    _rerenderOnDropdownClose(key) {
      const inst = this._selects[key];
      const el = inst && inst.el;
      // No live select (native <select multiple> fallback) or dropdown
      // already closed (tag-remove click) -> re-render now, as before.
      if (!el || el.getAttribute('aria-expanded') !== 'true') {
        this._rerender();
        return;
      }
      if (this._closeWatch) return;   // already armed for this open dropdown
      const mo = new MutationObserver(() => {
        if (el.getAttribute('aria-expanded') === 'true') return;
        this._dropCloseWatch();
        this._rerender();
      });
      mo.observe(el, { attributes: true, attributeFilter: ['aria-expanded'] });
      this._closeWatch = mo;
    }

    _dropCloseWatch() {
      if (!this._closeWatch) return;
      this._closeWatch.disconnect();
      this._closeWatch = null;
    }

    // Column options filtered by a colType string ('num' / 'cat' / 'any' /
    // 'none'), for a control that is not a role (the per-value column picker).
    /** @param {string} ct */
    _colOptsByType(ct) {
      if (ct === 'none') return [];
      let cols = this._cols();
      if (ct === 'num') cols = cols.filter(c => c.type === 'numeric');
      else if (ct === 'cat') cols = cols.filter(c => c.type === 'categorical' || (c.n_unique != null && c.n_unique <= 50));
      return cols.map(c => c.label ? { value: c.name, label: c.label } : c.name);
    }

    // Repeatable aggregation list: renders each value as a verb-object row
    // "[agg] of [columns]" (the aggregation leads; the columns show only for
    // functions that consume them, so a bare count reads just "Count"), with a
    // remove control per row and an "Add aggregation" button. The host owns the
    // list via metricsList() / onMetricsChange(). This is the table/tile form;
    // the chart uses the single value role (_renderRole reversed pair).
    /** @param {HTMLElement} container */
    _renderMetrics(container) {
      const summaries = (this.h.metricsList && this.h.metricsList()) || [];
      const aggOpts = (this._role('func') || {}).options || [];
      const usesCols = (/** @type {string} */ fn) => !!fn && fn !== 'count';
      const colType = (/** @type {string} */ fn) =>
        fn === 'count_distinct' ? 'any' : (usesCols(fn) ? 'num' : 'none');
      const commit = () => { if (this.h.onMetricsChange) this.h.onMetricsChange(summaries); };
      const S = (typeof Blockr !== 'undefined' && Blockr.Select) || null;

      // Full-width block (breaks out of the section's narrow grid cells) so each
      // "[agg] of [columns]" row has room. One "Aggregate" label for the list.
      const wrap = document.createElement('div');
      wrap.className = 'dd-summaries';
      const lbl = document.createElement('span');
      lbl.className = 'blockr-popover-label';
      lbl.textContent = 'Aggregate';
      wrap.appendChild(lbl);

      summaries.forEach((/** @type {any} */ m, /** @type {number} */ i) => {
        const row = document.createElement('div');
        row.className = 'dd-value-row';

        const aggWrap = document.createElement('div');
        aggWrap.className = 'blockr-popover-select-wrap dd-picker-wrap dd-value-agg';
        if (S && S.single) {
          S.single(aggWrap, {
            options: aggOpts, selected: m.func || 'count',
            onChange: (/** @type {string} */ val) => {
              m.func = val;
              // Keep the columns consistent with the new function: count drops
              // them; a numeric aggregation keeps only numeric columns.
              const ct = colType(val);
              if (ct === 'none') m.cols = [];
              else if (ct === 'num') {
                m.cols = (m.cols || []).filter((/** @type {string} */ c) => {
                  const col = this._cols().find(x => x.name === c);
                  return col && col.type === 'numeric';
                });
              }
              commit(); this._rerender();
            }
          });
        }
        row.appendChild(aggWrap);

        if (usesCols(m.func)) {
          const of = document.createElement('span');
          of.className = 'dd-pair-connector';
          of.textContent = 'of';
          row.appendChild(of);
          const colsWrap = document.createElement('div');
          colsWrap.className = 'blockr-popover-select-wrap dd-picker-wrap dd-value-cols';
          const opts = this._colOptsByType(colType(m.func));
          // Empty selection on a NUMERIC aggregation means "all numeric
          // columns not claimed by another row" (default-function rule,
          // override semantics — mirrors the R dd_metric_plan expansion and
          // the colour scope's empty = all convention). The placeholder says
          // so — but ONLY for hosts with that expansion (table/tile, via
          // spec.metricsDefaultAll); the chart stays explicit (20 series
          // from one empty picker would be unreadable), so its placeholder
          // must not promise the rule. count_distinct is explicit-only
          // everywhere.
          const defAll = !!this.h.sections().metricsDefaultAll;
          const ph = (m.func !== 'count_distinct' && defAll)
            ? 'All numeric columns' : 'column(s)…';
          if (S && S.multi) {
            S.multi(colsWrap, {
              options: opts, selected: (m.cols || []).slice(), reorderable: false,
              placeholder: ph,
              onChange: (/** @type {string[]} */ vals) => { m.cols = vals; commit(); }
            });
          }
          row.appendChild(colsWrap);
        }

        // A single value is the floor (a grouped table always shows something),
        // so the remove control appears only when there is more than one.
        if (summaries.length > 1) {
          const rm = document.createElement('button');
          rm.type = 'button';
          rm.className = 'dd-role-remove dd-value-remove';
          rm.title = 'Remove aggregation';
          rm.innerHTML = '✕';
          rm.addEventListener('click', (e) => {
            e.stopPropagation();
            summaries.splice(i, 1); commit(); this._rerender();
          });
          row.appendChild(rm);
        }
        wrap.appendChild(row);
      });

      const add = document.createElement('button');
      add.type = 'button';
      add.className = 'blockr-add-link dd-add-trigger dd-add-value';
      const plus = (typeof Blockr !== 'undefined' && Blockr.icons) ? Blockr.icons.plus : '+';
      add.innerHTML = `<span class="blockr-add-icon">${plus}</span> Add aggregation`;
      add.addEventListener('click', (e) => {
        e.stopPropagation();
        summaries.push({ func: 'mean', cols: [] }); commit(); this._rerender();
      });
      wrap.appendChild(add);
      container.appendChild(wrap);
    }

    // Repeatable cell value-encoding rules ("Shade cells"): one
    // "[mode] on [columns]" row per rule — the same interaction as the
    // summaries list, same shape family ({func, cols} / {mode, cols}).
    // ZERO rows is a valid state (no shading — there is no floor row, unlike
    // summaries); "Add shading" seeds `[diverging] on []`, and an EMPTY
    // column pick means "all numeric columns not claimed by another rule"
    // (override semantics; the R dd_shading_visuals expansion) — which makes
    // the correlation matrix the one-click default. The host owns the list
    // via shadingsList() / onShadingsChange(). Table only.
    /** @param {HTMLElement} container */
    _renderShadings(container) {
      const shadings = (this.h.shadingsList && this.h.shadingsList()) || [];
      const modeRole = this._role('shade_mode') || {};
      const modeOpts = modeRole.options ||
        ['diverging', 'sequential', 'bar'];
      const commit = () => {
        if (this.h.onShadingsChange) this.h.onShadingsChange(shadings);
      };
      const S = (typeof Blockr !== 'undefined' && Blockr.Select) || null;

      const wrap = document.createElement('div');
      wrap.className = 'dd-summaries dd-shadings';
      const lbl = document.createElement('span');
      lbl.className = 'blockr-popover-label';
      lbl.textContent = 'Shade cells';
      wrap.appendChild(lbl);

      shadings.forEach((/** @type {any} */ s, /** @type {number} */ i) => {
        const row = document.createElement('div');
        row.className = 'dd-value-row dd-shading-row';

        const modeWrap = document.createElement('div');
        modeWrap.className = 'blockr-popover-select-wrap dd-picker-wrap dd-value-agg';
        if (S && S.single) {
          S.single(modeWrap, {
            options: modeOpts, selected: s.mode || 'diverging',
            onChange: (/** @type {string} */ val) => { s.mode = val; commit(); }
          });
        }
        row.appendChild(modeWrap);

        const on = document.createElement('span');
        on.className = 'dd-pair-connector';
        on.textContent = 'on';
        row.appendChild(on);

        const colsWrap = document.createElement('div');
        colsWrap.className = 'blockr-popover-select-wrap dd-picker-wrap dd-value-cols';
        if (S && S.multi) {
          S.multi(colsWrap, {
            options: this._colOptsByType('num'),
            selected: (s.cols || []).slice(), reorderable: false,
            placeholder: 'All numeric columns',
            onChange: (/** @type {string[]} */ vals) => { s.cols = vals; commit(); }
          });
        }
        row.appendChild(colsWrap);

        const rm = document.createElement('button');
        rm.type = 'button';
        rm.className = 'dd-role-remove dd-value-remove';
        rm.title = 'Remove shading';
        rm.innerHTML = '✕';
        rm.addEventListener('click', (e) => {
          e.stopPropagation();
          shadings.splice(i, 1); commit(); this._rerender();
        });
        row.appendChild(rm);
        wrap.appendChild(row);
      });

      const add = document.createElement('button');
      add.type = 'button';
      add.className = 'blockr-add-link dd-add-trigger dd-add-shading';
      const plus = (typeof Blockr !== 'undefined' && Blockr.icons) ? Blockr.icons.plus : '+';
      add.innerHTML = `<span class="blockr-add-icon">${plus}</span> Add shading`;
      add.addEventListener('click', (e) => {
        e.stopPropagation();
        shadings.push({ mode: 'diverging', cols: [] }); commit(); this._rerender();
      });
      wrap.appendChild(add);
      container.appendChild(wrap);
    }

    _renderDrillSection() {
      const cfg = this._cfg();
      // Picker-less variant (tile): the host supplies drillHint() -> a string
      // ("Click a card to filter downstream on <col>") when a drill target is
      // structurally determined, or null when the block has none (bare KPI,
      // grand totals) -- then the whole section is hidden: nothing to enable.
      const hint = this.h.drillHint ? this.h.drillHint() : undefined;
      if (this.h.drillHint && hint == null) return;
      // Variant A: the enable checkbox lives in the section header (matches the
      // table's Drill-down), replacing the old separate "Filter downstream on
      // selection" checkbox row that read as a different control style. `on` here
      // is the section-open state; unchecking clears drill, checking defaults it
      // to 'auto'.
      const on = this._secOpen('chartdrill', () => this._hasVal(cfg.drill));
      const sec = this._sectionEl('Drill-down', {
        toggle: { checked: on, onToggle: (/** @type {boolean} */ enabled) => {
          this._setSecOpen('chartdrill', enabled);
          if (!enabled) cfg.drill = '';
          else if (!this._hasVal(cfg.drill)) cfg.drill = 'auto';
          this._rerender();
          this.h.onChange('drill'); this.h.onClearFilter();
        } }
      });

      // Hint-only body: the drill target is implied by the block's structure
      // (the tile filters on its group / Name column), so there is no column
      // picker -- a muted line says what a click will do.
      if (on && this.h.drillHint) {
        const p = document.createElement('div');
        p.className = 'dd-form-help dd-drill-hint';
        p.textContent = hint;
        sec.appendChild(p);
        return;
      }

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
        const onSel = (/** @type {string[]} */ vals) => {
          cfg[key] = vals; cb(); this.h.onChange(key);
          // A multi-picker that gates other rows (e.g. the table's group
          // reveals the summaries list once set) re-renders the section list
          // — deferred until its dropdown closes, so a multi-pick keeps the
          // dropdown (and focus) alive across picks.
          if (role.rerender) this._rerenderOnDropdownClose(key);
        };
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
        // Commit model (design-system §5.5): typing never mutates cfg — the
        // value commits on Enter, blur or the "Enter ↵" chip, which then fades
        // to ✓; Escape reverts to the last committed value.
        const inp = document.createElement('input');
        inp.type = 'text';
        inp.className = 'blockr-popover-input';
        inp.value = (cfg[key] == null) ? '' : String(cfg[key]);
        if (role.ph) inp.placeholder = role.ph;
        const wrap = document.createElement('div');
        wrap.className = 'dd-text-wrap';
        const chip = document.createElement('button');
        chip.type = 'button';
        chip.className = 'blockr-expr-confirm dd-text-commit';
        chip.title = 'Apply (Enter)';
        chip.setAttribute('aria-label', 'Apply (Enter)');
        chip.style.display = 'none';
        let committed = inp.value;
        let everCommitted = false;
        const confirmIcon = () =>
          (typeof Blockr !== 'undefined' && Blockr.icons && Blockr.icons.confirm) ?
            Blockr.icons.confirm : '✓';
        const syncChip = () => {
          if (inp.value !== committed) {
            chip.style.display = '';
            chip.classList.remove('confirmed');
            chip.innerHTML = 'Enter <span class="blockr-kbd">↵</span>';
          } else if (everCommitted) {
            chip.style.display = '';
            chip.classList.add('confirmed');
            chip.innerHTML = confirmIcon();
          } else {
            chip.style.display = 'none';
          }
        };
        const commit = () => {
          if (inp.value === committed) return;
          committed = inp.value;
          everCommitted = true;
          cfg[key] = inp.value;
          cb();
          this.h.onChange(key);
          syncChip();
        };
        inp.addEventListener('input', syncChip);
        inp.addEventListener('keydown', (e) => {
          if (e.key === 'Enter') { e.preventDefault(); commit(); }
          else if (e.key === 'Escape') { inp.value = committed; syncChip(); }
        });
        inp.addEventListener('blur', commit);
        // Keep focus on the input so the chip click doesn't race blur-commit.
        chip.addEventListener('mousedown', (e) => e.preventDefault());
        chip.addEventListener('click', commit);
        wrap.appendChild(inp);
        wrap.appendChild(chip);
        parent.appendChild(wrap);
      } else if (role.kind === 'slider') {
        this._buildSlider(parent, key);
      } else if (role.kind === 'color') {
        this._buildColor(parent, key, cb);
      }
    }

    // Color control (kind 'color') = native swatch + "Theme
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
        // The placeholder speaks for the empty slot: on an optional field it
        // names what empty does ("Auto", "None"), on a required one it says
        // what to supply. `phBy` keys it by context, like colTypeBy.
        const role = this._role(key) || {};
        const ph = (role.phBy && role.phBy[this.h.context()]) || role.ph;
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
      // The menu is a dropdown (styled in chart.css), so it dismisses on
      // outside click like any menu.
      if (typeof Blockr !== 'undefined' && typeof Blockr.onDocClick === 'function') {
        Blockr.onDocClick(wrap, (/** @type {MouseEvent} */ e) => {
          if (!wrap.contains(/** @type {Node} */ (e.target))) {
            menu.style.display = 'none';
          }
        });
      }
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
      const asKey = (/** @type {any} */ e) => (typeof e === 'string' ? e : e.role);
      const mapKeys = (spec.mapping || []).map(asKey);
      // optionalMap entries may be { role, types } (type-conditional roles);
      // for the carry keep-set the KEY suffices — type applicability is
      // re-checked at render time.
      const optKeys = (spec.optionalMap || []).map(asKey);
      const keep = new Set([...spec.requiredMap, ...optKeys, ...mapKeys]);
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
