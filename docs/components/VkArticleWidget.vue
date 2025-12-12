<script setup lang="ts">
import { onMounted, ref } from 'vue'

const widgetId = 'vk-article-widget'
const articleUrl = '@ixray_platform-ix-ray-platform-13-obzor'
const isLoading = ref(true)
const errorMessage = ref<string | null>(null)

const loadScript = () =>
  new Promise<void>((resolve, reject) => {
    if (typeof window === 'undefined') {
      resolve()
      return
    }

    if ((window as Record<string, unknown>).VK) {
      resolve()
      return
    }

    const existingScript = document.querySelector<HTMLScriptElement>(
      'script[src="https://vk.com/js/api/openapi.js?169"]'
    )

    if (existingScript) {
      existingScript.addEventListener('load', () => resolve(), { once: true })
      existingScript.addEventListener(
        'error',
        () => reject(new Error('VK OpenAPI failed to load')),
        { once: true }
      )
      return
    }

    const script = document.createElement('script')
    script.src = 'https://vk.com/js/api/openapi.js?169'
    script.async = true
    script.onload = () => resolve()
    script.onerror = () => reject(new Error('VK OpenAPI failed to load'))
    document.head.appendChild(script)
  })

const initWidget = () => {
  const vkApi = (window as Record<string, any>).VK

  if (vkApi?.Widgets?.Article) {
    vkApi.Widgets.Article(widgetId, articleUrl)
  } else {
    throw new Error('VK Article widget is unavailable')
  }
}

onMounted(async () => {
  try {
    await loadScript()
    if (typeof window !== 'undefined') {
      initWidget()
    }
  } catch (error) {
    errorMessage.value = 'Не удалось загрузить виджет'
    console.error(error)
  } finally {
    isLoading.value = false
  }
})
</script>

<template>
  <div class="vk-article-widget">
    <div :id="widgetId" class="vk-article-widget__container" />
    <p v-if="isLoading" class="vk-article-widget__status">Загрузка...</p>
    <p v-else-if="errorMessage" class="vk-article-widget__status vk-article-widget__status--error">
      {{ errorMessage }}
    </p>
  </div>
</template>

<style scoped>
.vk-article-widget {
  margin-top: 16px;
  padding: 12px 16px;
  border: 1px solid var(--vp-c-divider);
  border-radius: 12px;
  background: var(--vp-c-bg-alt);
}

.vk-article-widget__container {
  min-height: 120px;
}

.vk-article-widget__status {
  margin: 8px 0 0;
  font-size: 14px;
  color: var(--vp-c-text-2);
}

.vk-article-widget__status--error {
  color: var(--vp-c-danger-1);
}
</style>
