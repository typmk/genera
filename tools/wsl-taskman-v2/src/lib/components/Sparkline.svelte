<script lang="ts">
  interface Props {
    values: number[];
    color?: string;
    width?: number;
    height?: number;
    maxValue?: number;
    filled?: boolean;
  }

  let {
    values,
    color = 'var(--accent)',
    width = 100,
    height = 40,
    maxValue,
    filled = false
  }: Props = $props();

  // Compute SVG path
  let path = $derived.by(() => {
    if (values.length === 0) return '';

    const max = maxValue ?? Math.max(...values, 1);
    const points = values.map((v, i) => {
      const x = (i / Math.max(values.length - 1, 1)) * width;
      const y = height - (v / max) * (height - 4) - 2;
      return `${x},${y}`;
    });

    if (filled) {
      return `M0,${height} L${points.join(' L')} L${width},${height} Z`;
    }
    return `M${points.join(' L')}`;
  });
</script>

<svg
  viewBox="0 0 {width} {height}"
  preserveAspectRatio="none"
  class="sparkline"
>
  {#if filled}
    <path d={path} class="fill" style:fill={color} />
  {:else}
    <polyline points={path.replace(/[ML]/g, '').trim()} class="line" style:stroke={color} />
  {/if}
</svg>

<style>
  .sparkline {
    width: 100%;
    height: 100%;
    display: block;
  }

  .line {
    fill: none;
    stroke-width: 1.5;
    stroke-linecap: round;
    stroke-linejoin: round;
  }

  .fill {
    opacity: 0.3;
  }
</style>
