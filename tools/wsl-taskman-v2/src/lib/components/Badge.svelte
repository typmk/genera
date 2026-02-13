<script lang="ts">
  import type { Snippet } from 'svelte';

  interface Props {
    variant?: 'default' | 'success' | 'warning' | 'danger' | 'info';
    size?: 'sm' | 'md';
    children: Snippet;
  }

  let { variant = 'default', size = 'sm', children }: Props = $props();
</script>

<span class="badge {variant} {size}">
  {@render children()}
</span>

<style>
  .badge {
    display: inline-flex;
    align-items: center;
    font-weight: 500;
    border-radius: var(--radius-sm);
    white-space: nowrap;
  }

  .sm {
    padding: 2px 6px;
    font-size: var(--text-xs);
  }

  .md {
    padding: 4px 8px;
    font-size: var(--text-sm);
  }

  .default {
    background: var(--surface-active);
    color: var(--text-secondary);
  }

  .success {
    background: oklch(0.65 0.15 145 / 0.2);
    color: var(--color-success);
  }

  .warning {
    background: oklch(0.70 0.15 85 / 0.2);
    color: var(--color-warning);
  }

  .danger {
    background: oklch(0.65 0.18 25 / 0.2);
    color: var(--color-danger);
  }

  .info {
    background: oklch(0.65 0.12 220 / 0.2);
    color: var(--color-info);
  }
</style>
