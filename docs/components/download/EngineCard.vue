<script setup lang="ts">
import { computed } from 'vue'

const props = defineProps<{
  card: {
    title: string
    description: string
    tag: string
    button: string
    dots: string[]
  }
  link?: string | null
  releaseUrl: string
  loading: boolean
  metaLabel?: string
  missingLabel: string
}>()

const targetLink = computed(() => props.link || props.releaseUrl)
</script>

<template>
  <div class="download-card download-card--accent">
    <div class="download-card__head">
      <span class="pill pill--primary">{{ card.tag }}</span>
      <span class="tag" v-if="metaLabel">{{ metaLabel }}</span>
    </div>
    <h2>{{ card.title }}</h2>
    <p class="download-card__desc">{{ card.description }}</p>
    <ul class="download-card__meta">
      <li v-for="dot in card.dots" :key="dot">{{ dot }}</li>
    </ul>
    <div class="download-card__footer">
      <a
        class="btn-primary"
        :class="{ 'is-loading': loading }"
        :href="targetLink"
        target="_blank"
        rel="noopener noreferrer"
      >
        {{ card.button }}
      </a>
      <span class="hash" v-if="!link && !loading">{{ missingLabel }}</span>
    </div>
  </div>
</template>
