/**
 * Protocol / ambient types for blockr.ggplot's hand-written JS.
 *
 * CANONICAL SOURCE: blockr.viz/inst/js/types.d.ts — this is the subset
 * blockr.ggplot consumes (no ECharts; the plot renders server-side via
 * Shiny's plotOutput). Dev-tooling only: type-checked via tsconfig.json /
 * `npx -p typescript tsc`, never referenced by an htmlDependency, never run
 * in the browser.
 *
 * blockr.ggplot reuses blockr.dplyr's shared JS (blockr-core.js +
 * blockr-select.js) at runtime, plus the DrilldownConfig engine and
 * settings-band checkbox copied from blockr.viz.
 */

/* --- Column metadata (R pushes these; JS host `columns()` returns them) --- */

interface VizColumn {
  name: string;
  /** A coarse type tag; conventions vary per host ('numeric'/'categorical'/'any'/...). */
  type: string;
  /** attr(col, "label") or "" */
  label?: string;
  /** distinct count, used to decide a column "fits" a categorical role */
  n_unique?: number;
  /** factor-level ordering; absent for non-factor columns */
  levels?: string[];
}

/* --- Drilldown gear-popover engine (drilldown-config.js) --- */

interface VizDrilldownRole {
  kind?: 'column' | 'select' | string;
  allowCount?: boolean | ((cfg: Record<string, any>) => boolean);
  ph?: string;
  hintBy?: Record<string, string>;
  // Roles carry many host-specific, dynamically-shaped fields (colTypeBy,
  // optionsBy, options, pairedWith, maxUnique, placeholder, label, and the
  // blockr.ggplot slider extension min/max/step/unit, ...).
  [field: string]: any;
}

interface VizDrilldownHost {
  /** Current column metadata. */
  columns(): VizColumn[] | null | undefined;
  /** Current persisted config (role -> value); values are dynamic JSON. */
  config(): Record<string, any>;
  /** Role specs, keyed by R config param. */
  roles: Record<string, VizDrilldownRole>;
  /** Host context tag (e.g. chart type) selecting type-conditional sections. */
  context(): string;
  /** Optional type-picker groups (null when the host has none). */
  typeGroups?: Array<{ label?: string; types: string[] }> | null;
  /** Fires when a config entry commits, with the role key that changed. */
  onChange: (key: string) => void;
  // The host exposes many block-specific callbacks (popoverEl, context,
  // currentType, sections, isOpen, reopen, onClearFilter, ...) — all dynamic.
  [field: string]: any;
}

declare class VizDrilldownConfig {
  constructor(host: VizDrilldownHost);
  /** Rebuilds the band/popover DOM in place. */
  render(): void;
  [member: string]: any;
}

/* --- Blockr namespace: the subset blockr.ggplot consumes --- */

/** Option entry: a bare value string, or {value, label} for a muted label. */
type BlockrSelectOption = string | { value: string; label?: string };

interface BlockrSelectHandleBase {
  el: HTMLDivElement;
  setOptions(
    opts: BlockrSelectOption[] | BlockrSelectOption | null | undefined,
    sel?: string | string[] | null
  ): void;
  updateOptions(opts: BlockrSelectOption[] | BlockrSelectOption | null | undefined): void;
  setLoading(flag: boolean): void;
  destroy(): void;
}

interface BlockrSelectSingleHandle extends BlockrSelectHandleBase {
  getValue(): string;
}

interface BlockrSelectMultiHandle extends BlockrSelectHandleBase {
  getValue(): string[];
}

interface BlockrSelectSingleConfig {
  options?: BlockrSelectOption[];
  selected?: string | null;
  placeholder?: string;
  onChange?: (value: string) => void;
  [opt: string]: unknown;
}

interface BlockrSelectMultiConfig {
  options?: BlockrSelectOption[];
  selected?: string[];
  placeholder?: string;
  reorderable?: boolean;
  max?: number;
  onChange?: (value: string[]) => void;
  [opt: string]: unknown;
}

interface BlockrSelectStatic {
  single(container: HTMLElement, config: BlockrSelectSingleConfig): BlockrSelectSingleHandle;
  multi(container: HTMLElement, config: BlockrSelectMultiConfig): BlockrSelectMultiHandle;
}

interface BlockrNamespace {
  /** Shared select component (blockr-select.js). */
  Select?: BlockrSelectStatic;
  /** SVG icon strings (gear, plus, ...). */
  icons: Record<string, string>;
  /** Document-level click delegate that drops listeners for removed nodes. */
  onDocClick(el: Element, cb: (e: MouseEvent) => void): void;
  uid(prefix?: string): string;
  escapeHtml(s: string): string;
  removeNode(node: Node | null | undefined): void;
  contentWidth(el: Element): number;
  /** The shared drilldown popover engine (copied from blockr.viz). */
  DrilldownConfig: typeof VizDrilldownConfig;
  /** Design-system checkbox factory (settings-band.js). */
  checkbox(
    label: string,
    checked: boolean,
    onChange: (checked: boolean) => void
  ): {
    el: HTMLLabelElement;
    input: HTMLInputElement;
    set(v: boolean): void;
    get(): boolean;
  };
  [member: string]: unknown;
}

declare var Blockr: BlockrNamespace;

/* --- Ambient third-party globals (no @types dependency) --- */

declare const Shiny: {
  InputBinding: new () => any;
  inputBindings: { register(binding: object, name: string): void };
  addCustomMessageHandler(name: string, handler: (msg: any) => void): void;
  setInputValue(
    name: string,
    value: unknown,
    opts?: { priority?: 'event' | 'immediate' | 'deferred' }
  ): void;
  bindAll?(scope?: unknown): void;
  unbindAll?(scope?: unknown): void;
};

declare function jQuery(selector: unknown): any;
declare const $: typeof jQuery;

interface Window {
  Blockr?: BlockrNamespace;
  DrilldownConfig?: typeof VizDrilldownConfig;
  Shiny?: typeof Shiny;
  jQuery?: typeof jQuery;
}
