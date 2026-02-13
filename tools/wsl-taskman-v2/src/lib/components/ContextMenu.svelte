<script lang="ts">
  import { onMount } from 'svelte';
  import { fade, scale } from 'svelte/transition';
  import type { MenuItem } from '$lib/types';

  interface Props {
    items: MenuItem[];
    x: number;
    y: number;
    onClose: () => void;
  }

  let { items, x, y, onClose }: Props = $props();
  let menuEl: HTMLDivElement;

  // Adjust position to keep menu in viewport
  let adjustedX = $state(x);
  let adjustedY = $state(y);

  onMount(() => {
    const rect = menuEl.getBoundingClientRect();
    const padding = 8;

    if (x + rect.width > window.innerWidth - padding) {
      adjustedX = window.innerWidth - rect.width - padding;
    }
    if (y + rect.height > window.innerHeight - padding) {
      adjustedY = window.innerHeight - rect.height - padding;
    }

    // Close on click outside
    const handleClick = (e: MouseEvent) => {
      if (!menuEl.contains(e.target as Node)) {
        onClose();
      }
    };

    // Close on escape
    const handleKeydown = (e: KeyboardEvent) => {
      if (e.key === 'Escape') onClose();
    };

    window.addEventListener('click', handleClick);
    window.addEventListener('keydown', handleKeydown);

    return () => {
      window.removeEventListener('click', handleClick);
      window.removeEventListener('keydown', handleKeydown);
    };
  });

  function handleItemClick(item: MenuItem) {
    if (item.disabled) return;
    item.action?.();
    onClose();
  }
</script>

<div
  bind:this={menuEl}
  class="context-menu"
  style:left="{adjustedX}px"
  style:top="{adjustedY}px"
  transition:scale={{ duration: 100, start: 0.95 }}
>
  {#each items as item}
    {#if item.type === 'divider'}
      <div class="divider"></div>
    {:else}
      <button
        class="menu-item"
        class:danger={item.danger}
        class:disabled={item.disabled}
        disabled={item.disabled}
        onclick={() => handleItemClick(item)}
      >
        {#if item.icon}
          <span class="icon">{item.icon}</span>
        {/if}
        <span class="label">{item.label}</span>
        {#if item.children}
          <span class="arrow">›</span>
        {/if}
      </button>
    {/if}
  {/each}
</div>

<style>
  .context-menu {
    position: fixed;
    z-index: 1000;
    min-width: 180px;
    padding: var(--space-1);
    background: var(--surface-overlay);
    border: 1px solid var(--border-default);
    border-radius: var(--radius-lg);
    box-shadow: var(--shadow-lg);
    backdrop-filter: blur(20px);
  }

  .menu-item {
    display: flex;
    align-items: center;
    gap: var(--space-2);
    width: 100%;
    padding: var(--space-2) var(--space-3);
    border: none;
    background: transparent;
    color: var(--text-primary);
    font-size: var(--text-sm);
    text-align: left;
    cursor: pointer;
    border-radius: var(--radius-sm);
    transition: background var(--transition-fast);
  }

  .menu-item:hover:not(.disabled) {
    background: var(--surface-hover);
  }

  .menu-item.danger {
    color: var(--color-danger);
  }

  .menu-item.disabled {
    color: var(--text-muted);
    cursor: not-allowed;
  }

  .icon {
    width: 16px;
    text-align: center;
  }

  .label {
    flex: 1;
  }

  .arrow {
    color: var(--text-muted);
  }

  .divider {
    height: 1px;
    margin: var(--space-1) var(--space-2);
    background: var(--border-subtle);
  }
</style>
