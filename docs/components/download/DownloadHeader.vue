<script setup lang="ts">
const props = defineProps<{
  badgeLabel: string
  branchLabel: string
  loading: boolean
  error?: string | null
}>()
</script>

<template>
  <div class="download-hero__head">
    <div class="download-hero__badge">
      <span class="dot"></span>
      <span>{{ badgeLabel }}</span>
    </div>
    <div class="download-hero__branch" :aria-busy="loading">
      <template v-if="!error">{{ branchLabel }}</template>
      <template v-else>{{ error }}</template>
    </div>
  </div>
</template>
