<script setup lang="ts">
import { computed } from 'vue'

const props = defineProps<{
  card: {
    title: string
    description: string
    tag: string
    button: string
    secondaryText: string
  }
  link?: string | null
  secondaryLink?: string | null
  releaseUrl: string
  loading: boolean
  missingLabel: string
}>()

const targetLink = computed(() => props.link || props.releaseUrl)
const secondaryTarget = computed(() => props.secondaryLink || props.releaseUrl)
</script>

<template>
  <div class="download-card download-card--secondary">
    <div class="download-card__head">
      <span class="pill">{{ card.tag }}</span>
    </div>
    <h3>{{ card.title }}</h3>
    <p class="download-card__desc">{{ card.description }}</p>
    <div class="download-card__buttons">
      <a
        class="btn-ghost"
        :class="{ 'is-loading': loading }"
        :href="targetLink"
        target="_blank"
        rel="noopener noreferrer"
      >
        {{ card.button }}
      </a>
      <a
        class="link-minor"
        :href="secondaryTarget"
        target="_blank"
        rel="noopener noreferrer"
      >
        {{ card.secondaryText }}
      </a>
      <span class="hash" v-if="!link && !secondaryLink && !loading">{{ missingLabel }}</span>
    </div>
  </div>
</template>
