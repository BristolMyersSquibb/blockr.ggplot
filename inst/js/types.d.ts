/**
 * Ambient types for gg-blocks.js. Dev tooling only: type-checked via
 * tsconfig.json (`npx -p typescript tsc`), never served to the browser.
 *
 * The Blockr namespace is blockr.ui's (inst/assets/js/types.d.ts there);
 * this is the subset blockr.ggplot uses.
 */

type BlockrSelectOption = string | { value: string; label?: string };

interface BlockrSelectHandleBase {
  el: HTMLDivElement;
  setOptions(
    opts: BlockrSelectOption[] | BlockrSelectOption | null | undefined,
    sel?: string | string[] | null
  ): void;
  setValue(value: string | string[] | null): void;
  destroy(): void;
}

interface BlockrSelectSingleHandle extends BlockrSelectHandleBase {
  getValue(): string;
}

interface BlockrSelectMultiHandle extends BlockrSelectHandleBase {
  getValue(): string[];
}

interface BlockrSelectConfigBase {
  options?: BlockrSelectOption[];
  placeholder?: string;
  bordered?: boolean;
  search?: boolean;
}

interface BlockrSelectStatic {
  single(
    container: HTMLElement,
    config: BlockrSelectConfigBase & {
      selected?: string | null;
      allowEmpty?: boolean;
      onChange?: (value: string) => void;
    }
  ): BlockrSelectSingleHandle;
  multi(
    container: HTMLElement,
    config: BlockrSelectConfigBase & {
      selected?: string[];
      onChange?: (value: string[]) => void;
    }
  ): BlockrSelectMultiHandle;
}

interface BlockrTextCommitHandle {
  chip: HTMLButtonElement;
  commit(): void;
  sync(value: string): void;
}

interface BlockrMenuItem {
  label: string;
  onSelect?: () => void;
}

interface BlockrMenuConfig {
  items: BlockrMenuItem[];
}

interface BlockrNamespace {
  Select?: BlockrSelectStatic;
  icons: Record<string, string>;
  uid(prefix?: string): string;
  tooltip: { set(el: Element, content: string): void };
  menu: {
    bind(trigger: HTMLElement, config: BlockrMenuConfig | (() => BlockrMenuConfig)): void;
  };
  checkbox(
    label: string,
    checked: boolean,
    onChange: (checked: boolean) => void
  ): { el: HTMLLabelElement; set(v: boolean): void; get(): boolean };
  segmented(
    options: { value: string; label: string }[],
    selected: string,
    onChange: (value: string) => void,
    opts?: { size?: 'xs'; label?: string }
  ): { el: HTMLDivElement; set(value: string): void; get(): string };
  gearTray(
    band: HTMLElement,
    gear: HTMLButtonElement,
    opts?: { label?: string; open?: boolean }
  ): { set(open: boolean): void; toggle(): void; isOpen(): boolean };
  setRequiredEmpty(el: Element, empty: boolean): void;
  textCommit(
    input: HTMLInputElement,
    opts: { onCommit: (value: string) => void }
  ): BlockrTextCommitHandle;
}

declare var Blockr: BlockrNamespace;

declare const Shiny: {
  InputBinding: new () => any;
  inputBindings: { register(binding: object, name: string): void };
  addCustomMessageHandler(name: string, handler: (msg: any) => void): void;
  setInputValue(
    name: string,
    value: unknown,
    opts?: { priority?: 'event' | 'immediate' | 'deferred' }
  ): void;
};

declare function jQuery(selector: unknown): any;
declare const $: typeof jQuery;

interface Window {
  Blockr?: BlockrNamespace;
  Shiny?: typeof Shiny;
}
