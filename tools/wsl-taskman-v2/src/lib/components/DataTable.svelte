<script lang="ts">
  import type { Column, SortState } from '$lib/types';

  interface Props {
    data: any[];
    columns: Column[];
    selected?: Set<number>;
    sort?: SortState;
    rowKey?: string;
    rowHeight?: number;
    onSelect?: (row: any) => void;
    onContext?: (row: any, x: number, y: number) => void;
    onSort?: (key: string) => void;
  }

  let {
    data,
    columns,
    selected = new Set(),
    sort,
    rowKey = 'pid',
    rowHeight = 36,
    onSelect,
    onContext,
    onSort
  }: Props = $props();

  // Virtual scrolling state
  let container: HTMLDivElement;
  let scrollTop = $state(0);
  let containerHeight = $state(400);

  // Calculate visible range with buffer
  const bufferRows = 5;
  let startIndex = $derived(Math.max(0, Math.floor(scrollTop / rowHeight) - bufferRows));
  let endIndex = $derived(Math.min(data.length, Math.ceil((scrollTop + containerHeight) / rowHeight) + bufferRows));
  let visibleData = $derived(data.slice(startIndex, endIndex));
  let topPadding = $derived(startIndex * rowHeight);
  let bottomPadding = $derived((data.length - endIndex) * rowHeight);

  function handleScroll(e: Event) {
    const target = e.target as HTMLDivElement;
    scrollTop = target.scrollTop;
  }

  function handleRowClick(row: any, event: MouseEvent) {
    onSelect?.(row);
  }

  function handleContextMenu(row: any, event: MouseEvent) {
    event.preventDefault();
    onSelect?.(row);
    onContext?.(row, event.clientX, event.clientY);
  }

  function handleHeaderClick(col: Column) {
    if (col.sortable !== false) {
      onSort?.(col.key as string);
    }
  }

  $effect(() => {
    if (container) {
      containerHeight = container.clientHeight;
      const observer = new ResizeObserver(entries => {
        containerHeight = entries[0].contentRect.height;
      });
      observer.observe(container);
      return () => observer.disconnect();
    }
  });
</script>

<div class="table-wrapper" bind:this={container} onscroll={handleScroll}>
  <table class="data-table">
    <thead>
      <tr>
        {#each columns as col}
          <th
            class:sortable={col.sortable !== false}
            class:sorted={sort?.key === col.key}
            class:asc={sort?.key === col.key && sort.direction === 'asc'}
            style:width={col.width}
            style:text-align={col.align || 'left'}
            onclick={() => handleHeaderClick(col)}
          >
            <span class="th-content">
              {col.label}
              {#if sort?.key === col.key}
                <span class="sort-icon">{sort.direction === 'asc' ? '↑' : '↓'}</span>
              {/if}
            </span>
          </th>
        {/each}
      </tr>
    </thead>
    <tbody>
      <!-- Top spacer for virtual scrolling -->
      {#if topPadding > 0}
        <tr style:height="{topPadding}px" class="spacer"><td colspan={columns.length}></td></tr>
      {/if}

      <!-- Visible rows only -->
      {#each visibleData as row (row[rowKey])}
        <tr
          class:selected={selected.has(row[rowKey])}
          style:height="{rowHeight}px"
          onclick={(e) => handleRowClick(row, e)}
          oncontextmenu={(e) => handleContextMenu(row, e)}
        >
          {#each columns as col}
            <td style:text-align={col.align || 'left'}>
              {#if col.render}
                {@html col.render(row[col.key], row)}
              {:else}
                {row[col.key] ?? '—'}
              {/if}
            </td>
          {/each}
        </tr>
      {/each}

      <!-- Bottom spacer for virtual scrolling -->
      {#if bottomPadding > 0}
        <tr style:height="{bottomPadding}px" class="spacer"><td colspan={columns.length}></td></tr>
      {/if}
    </tbody>
  </table>
</div>

<style>
  .table-wrapper {
    flex: 1;
    overflow: auto;
    contain: strict;
  }

  .data-table {
    width: 100%;
    border-collapse: collapse;
    table-layout: fixed;
  }

  thead {
    position: sticky;
    top: 0;
    z-index: 10;
    background: var(--surface-raised);
  }

  th {
    padding: var(--space-2) var(--space-3);
    font-size: var(--text-sm);
    font-weight: 600;
    color: var(--text-secondary);
    text-transform: uppercase;
    letter-spacing: 0.5px;
    border-bottom: 1px solid var(--border-subtle);
    white-space: nowrap;
  }

  th.sortable {
    cursor: pointer;
    user-select: none;
  }

  th.sortable:hover {
    color: var(--text-primary);
    background: var(--surface-hover);
  }

  .th-content {
    display: flex;
    align-items: center;
    gap: var(--space-1);
  }

  .sort-icon {
    font-size: var(--text-xs);
    opacity: 0.7;
  }

  th.sorted {
    color: var(--accent);
  }

  td {
    padding: var(--space-2) var(--space-3);
    font-size: var(--text-sm);
    color: var(--text-primary);
    border-bottom: 1px solid var(--border-subtle);
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  tr:not(.spacer) {
    transition: background 50ms;
  }

  tbody tr:not(.spacer):hover {
    background: var(--surface-hover);
  }

  tr.selected {
    background: var(--accent-subtle);
  }

  tr.selected:hover {
    background: oklch(0.72 0.12 220 / 0.25);
  }

  tr.spacer {
    visibility: hidden;
  }

  tr.spacer td {
    padding: 0;
    border: none;
  }
</style>
