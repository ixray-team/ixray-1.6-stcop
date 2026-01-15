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
      <article v-for="mod in mods" :key="mod.id" class="mod-card mod-card--vertical">
		<a class="mod-card__link" 
		:href="mod.link || '#'" 
		:aria-disabled="!mod.link"
		target="_blank" 
		rel="noreferrer">
			<div class="mod-card__media">
				<img :src="mod.image" :alt="mod.title" loading="lazy">
				<span v-if="mod.tag" class="mod-card__badge">{{mod.tag}}</span>
			</div>
			
			<div class="mod-card__body">
				<h3 class="mod-card__title">{{ mod.title }}</h3>
				<div class="mod-card__meta" v-if="mod.author">
					<span class="mod-card__author">{{ mod.author }}</span>
					<!--<span class="mod-card__dot"></span>
					<span class="mod-card__info">(ещё какая-то инфа)</span>-->
				</div>
				
				<p class="mod-card__desc">
					{{mod.description}}
				</p>
				
				
				
				<button class="mod-card__btn" type="button" v-if="mod.link">
					{{ labels.view }}
				</button>
			</div>
		</a>
      </article>
    </div>
  </section>
</template>

<style scoped>

:first-child {
  --mod-card-radius: 16px;
  --mod-card-bg: #05070a;
  --mod-card-border: rgba(255, 255, 255, 0.06);
  --mod-card-accent: #72d6ff;
  --mod-card-accent-soft: rgba(114, 214, 255, 0.18);
  --mod-card-text-muted: #a3adc2;
  --mod-card-text-soft: #7b85a0;
}

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
  //min-height: 510px;
  height: 100%;
  position: relative;
  border-radius: var(--mod-card-radius);
  background: radial-gradient(circle at 0 0, rgba(37, 99, 235, 0.08), transparent 55%),
              radial-gradient(circle at 100% 100%, rgba(100, 214, 255, 0.12), transparent 55%),
              white;
    border: 1px solid var(--mod-card-border);

  max-width: 350px;
  
  overflow: hidden;
  box-shadow:
    0 10px 26px rgba(0, 0, 0, 0.17),
    0 0 0 1px rgba(255, 255, 255, 0.02);
  transition:
    transform 0.18s ease,
    box-shadow 0.18s ease,
    border-color 0.18s ease,
    background 0.18s ease;
}



/* Вертикальная: тянется по ширине колонки, высота из aspect-ratio */
.mod-card--vertical {
  width: 100%;         /* управляешь шириной через grid/col */
  aspect-ratio: 3 / 5; /* ~0.6 */
  display: flex;
}

.mod-card__link {
  display: flex;
  flex-direction: column;
  text-decoration: none;
  color: inherit;
  width: 100%;
}

/* Верхняя область с изображением */
.mod-card__media {
  position: relative;
  flex: 0 0 55%;
  overflow: hidden;
}

.mod-card__media::before {
  content: "";
  position: absolute;
  inset: -25%;
  background:
    radial-gradient(circle at 20% 0, rgba(114, 214, 255, 0.3), transparent 60%),
    radial-gradient(circle at 80% 100%, rgba(255, 255, 255, 0.08), transparent 60%);
  mix-blend-mode: screen;
  opacity: 0.5;
  pointer-events: none;
}

.mod-card__media img {
  width: 100%;
  height: 100%;
  object-fit: cover;
  filter: saturate(1.1) contrast(1.05);
  transform-origin: center;
  transition: transform 0.3s ease, filter 0.3s ease;
}

/* Бейдж поверх */
.mod-card__badge {
  position: absolute;
  left: 0.75rem;
  top: 0.75rem;
  padding: 0.2rem 0.55rem;
  border-radius: 999px;
  background: rgba(7, 235, 196, 0.16);
  border: 1px solid rgba(7, 235, 196, 0.5);
  color: #8fffe1;
  font-size: 0.6rem;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  font-weight: 600;
}

/* Текстовая часть */
.mod-card__body {
  flex: 1;
  min-height: 0; 
  padding: 0.65rem 0.7rem 0.75rem;
  display: flex;
  flex-direction: column;
  gap: 0.4rem;
}

.mod-card__title {
  margin: 0;
  font-size: 0.95rem;
  line-height: 1.2;
  font-weight: 700;
  color: #374151;
}

.mod-card__desc {
  flex: 1; 
  overflow: hidden;
  margin: 0;
  font-size: 0.78rem;
  line-height: 1.4;
  color: #4b5563;
}

/* Метаданные */
.mod-card__meta {
  display: flex;
  flex-wrap: wrap;
  gap: 0.35rem;
  align-items: center;
  font-size: 0.7rem;
  color: var(--mod-card-text-soft);
}

.mod-card__author {
  font-weight: 500;
  color:  #636e85;;
}

.mod-card__dot {
  width: 0.2rem;
  height: 0.2rem;
  border-radius: 50%;
  background: rgba(163, 173, 194, 0.7);
}

.mod-card__info {
  opacity: 0.9;
}

/* Кнопка действия */
.mod-card__btn {
  margin-top: auto;
  align-self: stretch;
  padding: 0.45rem 0.7rem;
  border-radius: 999px;
  border: none;
  background:
    radial-gradient(circle at 0 0, rgba(255, 255, 255, 0.4), transparent 55%),
    linear-gradient(135deg, #363bd3, #8f7bff);
  color: white;
  font-size: 0.7rem;
  font-weight: 600;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  cursor: pointer;
  box-shadow:
    0 0 0 1px rgba(0, 0, 0, 0.05),
    0 6px 18px rgba(76, 210, 255, 0.35);
  transition:
    transform 0.18s ease,
    box-shadow 0.18s ease,
    filter 0.2s ease;
}

/* Hover */
.mod-card:hover {
  transform: translateY(-3px);
  box-shadow:
    0 18px 40px rgba(0, 0, 0, 0.20),
    0 0 22px var(--mod-card-accent-soft);
  border-color: rgba(114, 214, 255, 0.5);
}

.mod-card:hover .mod-card__media img {
  transform: scale(1.05);
  filter: saturate(1.25) contrast(1.08);
}

.mod-card__btn:hover {
  transform: translateY(-1px);
  box-shadow:
    0 10px 24px rgba(76, 210, 255, 0.55);
  filter: brightness(1.05);
}

.btn:hover {
  transform: translateY(-1px);
  box-shadow: 0 8px 18px rgba(0, 0, 0, 0.16);
}

.muted {
  font-size: 12px;
  color: var(--vp-c-text-3);
}
.mod-card__link[aria-disabled="true"] {
  pointer-events: none;
  cursor: default;
}

/*       D.A.R.K.        */

:root.dark .mod-card {
  background: radial-gradient(circle at 0 0, rgba(114, 214, 255, 0.12), transparent 55%),
              radial-gradient(circle at 100% 100%, rgba(120, 100, 255, 0.18), transparent 55%),
              var(--mod-card-bg);
  border: 1px solid var(--mod-card-border);
  box-shadow:
    0 10px 26px rgba(0, 0, 0, 0.7),
    0 0 0 1px rgba(255, 255, 255, 0.02);		  
}
:root.dark .mod-card:hover {
  transform: translateY(-3px);
  box-shadow:
    0 18px 40px rgba(0, 0, 0, 0.85),
    0 0 22px var(--mod-card-accent-soft);
  border-color: rgba(114, 214, 255, 0.5);

}

:root.dark .mod-card__btn {
  background:
    radial-gradient(circle at 0 0, rgba(255, 255, 255, 0.4), transparent 55%),
    linear-gradient(135deg, #4fe6ff, #8f7bff);
  color: #02040a;
  box-shadow:
    0 0 0 1px rgba(0, 0, 0, 0.8),
    0 6px 18px rgba(76, 210, 255, 0.35);
}

:root.dark .mod-card__title {
  color: #f5f7ff;
}

:root.dark .mod-card__desc {
  color: var(--mod-card-text-muted);
}
:root.dark .mod-card__author {
  color: var(--mod-card-text-muted);
}
</style>
