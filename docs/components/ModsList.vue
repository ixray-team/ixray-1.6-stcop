<script setup lang="ts">
import { computed, onMounted, ref } from 'vue'
import { useData } from 'vitepress'

type ModsCopy = {
  eyebrow: string
  title: string
  subtitle: string
  labels: {
    view: string
    soon: string
  }
  mods: ModCard[]
}

interface ModCard {
  id: string
  title: string
  description: string
  author?: string
  link?: string
  image?: string
}

const props = defineProps<{
  copy?: ModsCopy
}>()

const { page, lang } = useData()

// Собираем все JSON из каталога docs, чтобы подобрать по имени страницы.
const jsonModules = import.meta.glob('../docs/**/*.json')

const fallbackCopy: ModsCopy = {
  eyebrow: '',
  title: '',
  subtitle: '',
  labels: { view: '', soon: '' },
  mods: [],
}

const runtimeCopy = ref<ModsCopy | null>(null)

onMounted(async () => {
  if (props.copy || runtimeCopy.value) return

  const relPath = page.value?.relativePath?.replace(/\\/g, '/')
  const baseName = relPath?.split('/')?.pop()?.replace(/\.md$/, '.json')
  const primaryKey = relPath ? `../docs/${relPath.replace(/\.md$/, '.json')}` : null
  const currentLang = lang.value === 'en' ? 'en' : 'ru'

  const langKeys = baseName
    ? Object.keys(jsonModules).filter(
        (key) => key.includes(`/${currentLang}/`) && key.endsWith(`/${baseName}`)
      )
    : []

  const fallbackKeys = baseName
    ? Object.keys(jsonModules).filter((key) => key.endsWith(`/${baseName}`) && !langKeys.includes(key))
    : []

  const candidateKeys = [primaryKey, ...langKeys, ...fallbackKeys].filter(Boolean) as string[]

  for (const key of candidateKeys) {
    const loader = jsonModules[key] as () => Promise<{ default: ModsCopy }>
    if (!loader) continue
    try {
      const mod = await loader()
      runtimeCopy.value = mod.default
      return
    } catch (err) {
      console.warn('ModsList: failed to load copy', key, err)
    }
  }
})

const copy = computed(() => props.copy ?? runtimeCopy.value ?? fallbackCopy)
const labels = computed(() => copy.value.labels)
const mods = computed(() => copy.value.mods as ModCard[])
</script>

<template>
  <section class="mods">
    <header class="mods__header">
      <p class="mods__eyebrow">{{ labels.eyebrow }}</p>
      <h1 class="mods__title">{{ labels.title }}</h1>
      <p class="mods__subtitle">{{ labels.subtitle }}</p>
    </header>

    <div class="mods__grid">
      <article v-for="mod in mods" :key="mod.id" class="mod-card">
        <div v-if="mod.image" class="mod-card__media">
          <img :src="mod.image" :alt="mod.title" loading="lazy" />
        </div>
        <div class="mod-card__head">
          <div>
            <p class="mod-card__eyebrow">
              <span v-if="mod.updated">{{ mod.updated }}</span>
              <span v-if="mod.version"> В· v{{ mod.version }}</span>
            </p>
            <h3 class="mod-card__title">{{ mod.title }}</h3>
          </div>
          <span v-if="mod.tag" class="pill">{{ mod.tag }}</span>
        </div>

        <p class="mod-card__desc">{{ mod.description }}</p>

        <p class="mod-card__meta" v-if="mod.author">
          {{ mod.author }}
        </p>

        <div class="mod-card__footer">
          <a
            v-if="mod.link"
            class="btn"
            :href="mod.link"
            target="_blank"
            rel="noreferrer"
          >
            {{ labels.view }}
          </a>
          <span v-else class="muted">{{ labels.soon }}</span>
        </div>
      </article>
    </div>
  </section>
</template>

<style scoped>
.mods {
  display: flex;
  flex-direction: column;
  gap: 18px;
  padding: 8px 0 18px;
}

.mods__header {
  display: grid;
  gap: 6px;
}

.mods__eyebrow {
  margin: 0;
  font-size: 12px;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: var(--vp-c-text-2);
}

.mods__title {
  margin: 0;
  font-size: 26px;
  line-height: 1.2;
  color: var(--vp-c-text-1);
}

.mods__subtitle {
  margin: 0;
  font-size: 14px;
  color: var(--vp-c-text-2);
  max-width: 720px;
}

.mods__grid {
  display: grid;
  gap: 12px;
  grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
}

.mod-card {
  border: 1px solid var(--vp-c-divider);
  border-radius: 14px;
  padding: 14px;
  background: var(--vp-c-bg-alt);
  box-shadow: 0 8px 24px rgba(0, 0, 0, 0.04);
  display: grid;
  gap: 10px;
}

.mod-card__media {
  border-radius: 10px;
  overflow: hidden;
  border: 1px solid var(--vp-c-divider);
}

.mod-card__media img {
  display: block;
  width: 100%;
  height: auto;
  object-fit: cover;
}

.mod-card__head {
  display: flex;
  justify-content: space-between;
  gap: 8px;
  align-items: flex-start;
  margin-bottom: 6px;
}

.mod-card__eyebrow {
  margin: 0 0 4px;
  font-size: 12px;
  color: var(--vp-c-text-2);
}

.mod-card__title {
  margin: 0;
  font-size: 17px;
  color: var(--vp-c-text-1);
}

.mod-card__desc {
  margin: 0 0 10px;
  font-size: 13px;
  color: var(--vp-c-text-2);
}

.mod-card__meta {
  margin: 0 0 10px;
  font-size: 12px;
  color: var(--vp-c-text-2);
}

.mod-card__footer {
  display: flex;
  align-items: center;
  justify-content: space-between;
}

.pill {
  display: inline-flex;
  align-items: center;
  padding: 4px 8px;
  border-radius: 999px;
  font-size: 11px;
  background: var(--vp-c-brand-1);
  color: #fff;
  border: 1px solid var(--vp-c-brand-2);
}

.btn {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  padding: 8px 12px;
  border-radius: 10px;
  background: var(--vp-c-brand-1);
  color: var(--vp-c-white);
  text-decoration: none;
  font-size: 13px;
  border: 1px solid var(--vp-c-brand-2);
  transition: transform 0.15s ease, box-shadow 0.15s ease;
}

.btn:hover {
  transform: translateY(-1px);
  box-shadow: 0 8px 18px rgba(0, 0, 0, 0.16);
}

.muted {
  font-size: 12px;
  color: var(--vp-c-text-3);
}
</style>
