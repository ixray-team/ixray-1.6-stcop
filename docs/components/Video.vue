<script setup lang="ts">
import { computed } from 'vue'

const props = defineProps<{
  url: string
  title?: string
  aspect?: string // e.g. "16:9" or "4:3"
}>()

const isFileSource = computed(() => /\.(mp4|webm|ogg|mov|m4v)$/i.test(props.url))

const aspectRatio = computed(() => {
  const [w, h] = (props.aspect ?? '16:9').split(':').map(Number)
  return !Number.isNaN(w) && !Number.isNaN(h) && h !== 0 ? (h / w) * 100 : 56.25
})

// Normalize popular providers to embed URLs so users can drop regular links
const embedUrl = computed(() => {
  const url = props.url.trim()
  const yt = url.match(/(?:youtube\.com\/watch\?v=|youtu\.be\/)([\w-]{11})/)
  if (yt?.[1]) return `https://www.youtube.com/embed/${yt[1]}`

  const vimeo = url.match(/vimeo\.com\/(\d+)/)
  if (vimeo?.[1]) return `https://player.vimeo.com/video/${vimeo[1]}`

  return url
})
</script>

<template>
  <div class="video-shell" :style="{ paddingTop: `${aspectRatio}%` }">
    <video
      v-if="isFileSource"
      :src="embedUrl"
      :title="title"
      controls
      playsinline
    />
    <iframe
      v-else
      :src="embedUrl"
      :title="title"
      allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share"
      allowfullscreen
      loading="lazy"
      referrerpolicy="strict-origin-when-cross-origin"
    />
  </div>
</template>

<style scoped>
.video-shell {
  position: relative;
  width: 100%;
  border-radius: 16px;
  overflow: hidden;
  background: #0b1120;
  box-shadow: 0 16px 40px rgba(0, 0, 0, 0.25);
}

.video-shell iframe,
.video-shell video {
  position: absolute;
  inset: 0;
  width: 100%;
  height: 100%;
  border: 0;
  background: #000;
}
</style>
