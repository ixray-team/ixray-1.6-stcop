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
  score?: string
  rank?: string
}

const props = defineProps<{
  copy?: ModsCopy
}>()

const { page, lang } = useData()

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
      <p v-if="copy.eyebrow" class="mods__eyebrow">{{ copy.eyebrow }}</p>
      <h1 v-if="copy.title" class="mods__title">{{ copy.title }}</h1>
      <p v-if="copy.subtitle" class="mods__subtitle">{{ copy.subtitle }}</p>
    </header>

    <div class="mods__grid">
      <article
        v-for="mod in mods"
        :key="mod.id"
        class="mod-card"
        :class="{ 'mod-card--disabled': !mod.link }"
      >
        <a
          class="mod-card__link"
          :href="mod.link || undefined"
          :aria-disabled="!mod.link || undefined"
          :tabindex="!mod.link ? -1 : undefined"
          target="_blank"
          rel="noreferrer noopener"
        >
          <div class="mod-card__media">
            <img
              v-if="mod.image"
              :src="mod.image"
              :alt="mod.title"
              class="mod-card__img"
              loading="lazy"
            />
            <div v-else class="mod-card__img-placeholder" aria-hidden="true" />

            <div class="mod-card__badges">
              <span v-if="mod.score" class="mod-card__badge mod-card__badge--score">
                <svg class="mod-card__badge-icon" viewBox="0 0 12 12" fill="none" xmlns="http://www.w3.org/2000/svg" aria-hidden="true">
                  <path d="M6 1L7.39 4.26L11 4.64L8.45 6.97L9.18 10.5L6 8.77L2.82 10.5L3.55 6.97L1 4.64L4.61 4.26L6 1Z" fill="currentColor"/>
                </svg>
                {{ mod.score }}
              </span>
              <span v-if="mod.rank" class="mod-card__badge mod-card__badge--rank">
                <svg class="mod-card__badge-icon" viewBox="0 0 12 12" fill="none" xmlns="http://www.w3.org/2000/svg" aria-hidden="true">
                  <path d="M2 10V7M5 10V4M8 10V6M11 10V2" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
                </svg>
                {{ mod.rank }}
              </span>
            </div>
          </div>

          <div class="mod-card__body">
            <div class="mod-card__body-top">
              <h3 class="mod-card__title">{{ mod.title }}</h3>
              <p v-if="mod.author" class="mod-card__author">{{ mod.author }}</p>
            </div>

            <p class="mod-card__desc">{{ mod.description }}</p>

            <div v-if="mod.link" class="mod-card__footer">
              <span class="mod-card__btn">{{ labels.view }}</span>
            </div>
            <div v-else class="mod-card__footer">
              <span class="mod-card__btn mod-card__btn--soon">{{ labels.soon }}</span>
            </div>
          </div>
        </a>
      </article>
    </div>
  </section>
</template>

<style scoped>
/* Design tokens */
.mods {
  --card-radius: 14px;
  --card-gap: 16px;
  --card-pad: 1rem;
  --card-img-ratio: 262 / 241;

  /* Light theme */
  --card-bg: #ffffff;
  --card-bg-hover: #fafbff;
  --card-border: rgba(0, 0, 0, 0.08);
  --card-border-hover: rgba(99, 102, 241, 0.35);
  --card-shadow: 0 1px 3px rgba(0, 0, 0, 0.07), 0 4px 12px rgba(0, 0, 0, 0.06);
  --card-shadow-hover: 0 4px 8px rgba(0, 0, 0, 0.06), 0 12px 28px rgba(99, 102, 241, 0.14);

  --text-primary: #111827;
  --text-secondary: #4b5563;
  --text-muted: #9ca3af;

  --accent: #4f46e5;
  --accent-subtle: rgba(79, 70, 229, 0.08);

  --badge-score-bg: #bbf7d0;
  --badge-score-border: #4ade80;
  --badge-score-text: #14532d;

  --badge-rank-bg: #ddd6fe;
  --badge-rank-border: #7c3aed;
  --badge-rank-text: #3b0764;

  --btn-bg: #4f46e5;
  --btn-bg-hover: #4338ca;
  --btn-text: #ffffff;

  --placeholder-bg: #f3f4f6;

  display: flex;
  flex-direction: column;
  gap: 32px;
  padding: 8px 0 24px;
}

/* Dark mode overrides */
:root.dark .mods {
  --card-bg: #0f1117;
  --card-bg-hover: #13161f;
  --card-border: rgba(255, 255, 255, 0.07);
  --card-border-hover: rgba(114, 214, 255, 0.3);
  --card-shadow: 0 1px 3px rgba(0, 0, 0, 0.4), 0 4px 16px rgba(0, 0, 0, 0.5);
  --card-shadow-hover: 0 4px 8px rgba(0, 0, 0, 0.5), 0 16px 40px rgba(0, 0, 0, 0.6);

  --text-primary: #f0f2ff;
  --text-secondary: #9ca3c8;
  --text-muted: #6b7280;

  --accent: #72d6ff;
  --accent-subtle: rgba(114, 214, 255, 0.08);

  --badge-score-bg: rgba(16, 185, 129, 0.18);
  --badge-score-border: rgba(16, 185, 129, 0.5);
  --badge-score-text: #6ee7b7;

  --badge-rank-bg: rgba(99, 102, 241, 0.2);
  --badge-rank-border: rgba(99, 102, 241, 0.5);
  --badge-rank-text: #a5b4fc;

  --btn-bg: rgba(114, 214, 255, 0.12);
  --btn-bg-hover: rgba(114, 214, 255, 0.2);
  --btn-text: #72d6ff;

  --placeholder-bg: #1a1d27;
}

/* Section header */
.mods__header {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.mods__eyebrow {
  margin: 0;
  font-size: 11px;
  font-weight: 600;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--text-muted);
}

.mods__title {
  margin: 0;
  font-size: clamp(22px, 4vw, 30px);
  line-height: 1.2;
  font-weight: 700;
  color: var(--text-primary);
  letter-spacing: -0.02em;
}

.mods__subtitle {
  margin: 0;
  font-size: 15px;
  line-height: 1.6;
  color: var(--text-secondary);
  max-width: 640px;
}

/* Grid */
.mods__grid {
  display: grid;
  gap: var(--card-gap);
  grid-template-columns: repeat(auto-fill, minmax(240px, 1fr));
}

/* Card shell*/
.mod-card {
  position: relative;
  border-radius: var(--card-radius);
  background: var(--card-bg);
  border: 1px solid var(--card-border);
  box-shadow: var(--card-shadow);
  overflow: hidden;
  transition:
    transform 0.2s ease,
    box-shadow 0.2s ease,
    border-color 0.2s ease,
    background-color 0.2s ease;
}

.mod-card:not(.mod-card--disabled):hover {
  transform: translateY(-2px);
  background: var(--card-bg-hover);
  border-color: var(--card-border-hover);
  box-shadow: var(--card-shadow-hover);
}

.mod-card:not(.mod-card--disabled):focus-within {
  outline: 2px solid var(--accent);
  outline-offset: 2px;
}

.mod-card--disabled {
  opacity: 0.6;
}

/* Link wrapper */
.mod-card__link {
  display: flex;
  flex-direction: column;
  height: 100%;
  text-decoration: none;
  color: inherit;
  outline: none; /* focus handled at card level */
}

.mod-card__link[aria-disabled] {
  pointer-events: none;
  cursor: default;
}

/* Media area*/
.mod-card__media {
  position: relative;
  aspect-ratio: var(--card-img-ratio);
  background: var(--placeholder-bg);
  overflow: hidden;
  flex-shrink: 0;
}

.mod-card__img {
  width: 100%;
  height: 100%;
  object-fit: cover;
  display: block;
  margin: 0;
  transition: transform 0.35s ease;
}

.mod-card:not(.mod-card--disabled):hover .mod-card__img {
  transform: scale(1.04);
}

.mod-card__img-placeholder {
  width: 100%;
  height: 100%;
  background: linear-gradient(135deg, var(--placeholder-bg) 0%, transparent 100%);
}

/* Badges */
.mod-card__badges {
  position: absolute;
  top: 10px;
  right: 10px;
  display: flex;
  flex-direction: column;
  gap: 5px;
  align-items: flex-end;
}

.mod-card__badge {
  display: inline-flex;
  align-items: center;
  padding: 3px 8px;
  border-radius: 6px;
  font-size: 10px;
  font-weight: 700;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  line-height: 1;
  backdrop-filter: blur(6px);
  -webkit-backdrop-filter: blur(6px);
}

.mod-card__badge-icon {
  width: 10px;
  height: 10px;
  flex-shrink: 0;
  margin-right: 4px;
}

.mod-card__badge--score {
  background: var(--badge-score-bg);
  border: 1px solid var(--badge-score-border);
  color: var(--badge-score-text);
}

.mod-card__badge--rank {
  background: var(--badge-rank-bg);
  border: 1px solid var(--badge-rank-border);
  color: var(--badge-rank-text);
}

/* Body */
.mod-card__body {
  flex: 1;
  display: flex;
  flex-direction: column;
  gap: 8px;
  padding: var(--card-pad);
}

.mod-card__body-top {
  display: flex;
  flex-direction: column;
  gap: 3px;
}

.mod-card__title {
  margin: 0;
  font-size: 14px;
  font-weight: 700;
  line-height: 1.3;
  color: var(--text-primary);
  letter-spacing: -0.01em;
}

.mod-card__author {
  margin: 0;
  font-size: 12px;
  font-weight: 500;
  color: var(--text-muted);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.mod-card__desc {
  flex: 1;
  margin: 0;
  font-size: 13px;
  line-height: 1.55;
  color: var(--text-secondary);
  /* Clamp to 5 lines for consistent card heights */
  display: -webkit-box;
  -webkit-line-clamp: 5;
  -webkit-box-orient: vertical;
  overflow: hidden;
}

/* Footer / CTA */
.mod-card__footer {
  margin-top: auto;
  padding-top: 4px;
}

.mod-card__btn {
  display: block;
  width: 100%;
  padding: 7px 14px;
  border-radius: 8px;
  background: var(--btn-bg);
  color: var(--btn-text);
  font-size: 12px;
  font-weight: 600;
  letter-spacing: 0.04em;
  text-align: center;
  text-transform: uppercase;
  transition:
    background-color 0.15s ease,
    opacity 0.15s ease;
}

.mod-card:not(.mod-card--disabled):hover .mod-card__btn {
  background: var(--btn-bg-hover);
}

.mod-card__btn--soon {
  background: transparent;
  color: var(--text-muted);
  border: 1px solid var(--card-border);
  cursor: default;
}

/* Responsive*/
@media (max-width: 480px) {
  .mods__grid {
    grid-template-columns: 1fr;
  }

  .mods {
    gap: 24px;
  }
}

@media (min-width: 900px) {
  .mods__grid {
    grid-template-columns: repeat(auto-fill, minmax(260px, 1fr));
  }
}
</style>