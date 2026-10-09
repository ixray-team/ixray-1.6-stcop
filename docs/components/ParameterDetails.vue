<script setup lang="ts">
import { computed } from 'vue'
import { useData } from 'vitepress'

defineProps<{
  title: string
  parameter: string
  normal?: string
  zoom?: string
  zoomParameter?: string
  format?: string
  formatNote?: string
  normalLabel?: string
  zoomLabel?: string
  formatLabel?: string
  open?: boolean
}>()

const { theme } = useData<{
  parameterDetails: {
    defaultValue: string
    normalMode: string
    zoomMode: string
    format: string
  }
}>()

const labels = computed(() => theme.value.parameterDetails)
</script>

<template>
  <details class="parameter-details" :open="open">
    <summary>
      <span class="parameter-heading">
        <span class="parameter-title">{{ title }}</span>
        <span v-if="normal !== undefined || zoom !== undefined || format" class="parameter-defaults">
          <template v-if="format">
            <span>{{ formatLabel ?? labels.format }} <code>{{ format }}</code><template v-if="formatNote"> {{ formatNote }}</template></span>
          </template>
          <template v-else>
            <span v-if="normal !== undefined">{{ normalLabel ?? (zoom !== undefined ? labels.normalMode : labels.defaultValue) }} <code>{{ normal }}</code></span>
            <span v-if="zoom !== undefined">{{ zoomLabel ?? labels.zoomMode }} <code>{{ zoom }}</code></span>
          </template>
        </span>
      </span>
    </summary>
    <div class="parameter-body">
      <slot />
      <div class="parameter-fields">
        <code>{{ parameter }}</code>
        <code v-if="zoomParameter">{{ zoomParameter }}</code>
      </div>
    </div>
  </details>
</template>

<style scoped>
.parameter-details {
  margin: 0;
  padding: 18px 0;
  border-top: 1px solid var(--vp-c-divider);
  color: var(--vp-c-text-1);
}

.parameter-details summary {
  cursor: pointer;
}

.parameter-details summary::marker {
  color: var(--vp-c-text-2);
}

.parameter-details summary:focus-visible {
  outline: 2px solid var(--vp-c-brand-1);
  outline-offset: 6px;
  border-radius: 4px;
}

.parameter-heading {
  display: inline-flex;
  flex-direction: column;
  max-width: calc(100% - 24px);
  vertical-align: top;
}

.parameter-title {
  font-weight: 600;
}

.parameter-defaults {
  display: flex;
  flex-wrap: wrap;
  gap: 8px 18px;
  margin-top: 6px;
  color: var(--vp-c-text-2);
  font-size: 14px;
  font-weight: 400;
}

.parameter-defaults code {
  font-variant-numeric: tabular-nums;
}

.parameter-body {
  padding-top: 16px;
}

.parameter-body :deep(p) {
  margin: 0;
}

.parameter-body :deep(code) {
  overflow-wrap: anywhere;
  white-space: normal;
}

.parameter-fields {
  display: flex;
  flex-direction: column;
  align-items: flex-start;
  gap: 8px;
  margin-top: 16px;
}

.parameter-fields code {
  max-width: 100%;
}
</style>

