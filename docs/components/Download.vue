<script setup lang="ts">
import { computed, onMounted, ref } from 'vue'
import { useData } from 'vitepress'
import DownloadHeader from './download/DownloadHeader.vue'
import DeveloperCard from './download/DeveloperCard.vue'
import EngineCard from './download/EngineCard.vue'
import ServerCard from './download/ServerCard.vue'
import UtilitiesCard from './download/UtilitiesCard.vue'
import enCopy from '../docs/main/en/download.json'
import ruCopy from '../docs/main/ru/download.json'

type LocaleKey = 'ru' | 'en'
type LocaleCopy = typeof ruCopy

interface GithubAsset {
  name: string
  browser_download_url: string
}

interface GithubRelease {
  tag_name?: string
  name?: string
  published_at?: string
  html_url?: string
  assets?: GithubAsset[]
}

const repoSlug = 'ixray-team/ixray-1.6-stcop'
const apiUrl = `https://api.github.com/repos/${repoSlug}/releases/latest`

const { lang } = useData()

const copyMap: Record<LocaleKey, LocaleCopy> = {
  en: enCopy,
  ru: ruCopy,
}

const copy = computed(() => copyMap[(lang.value as LocaleKey) || 'ru'] ?? ruCopy)

const release = ref<GithubRelease | null>(null)
const loading = ref<boolean>(true)
const error = ref<string | null>(null)

const fetchRelease = async () => {
  loading.value = true
  error.value = null
  try {
    const response = await fetch(apiUrl, {
      headers: {
        Accept: 'application/vnd.github+json',
      },
    })

    if (!response.ok) {
      throw new Error(`GitHub API responded with ${response.status}`)
    }

    const data: GithubRelease = await response.json()
    release.value = data
  } catch (err) {
    error.value = (err as Error).message
  } finally {
    loading.value = false
  }
}

onMounted(fetchRelease)

const releaseVersion = computed(() => release.value?.tag_name || copy.value.fallback.version)

const releaseDate = computed(() => {
  if (!release.value?.published_at) {
    return copy.value.fallback.date
  }

  const options: Intl.DateTimeFormatOptions = { year: 'numeric', month: 'long', day: 'numeric' }
  const locale = lang.value === 'ru' ? 'ru-RU' : 'en-US'
  return new Date(release.value.published_at).toLocaleDateString(locale, options)
})

const releaseUrl = computed(() => release.value?.html_url || copy.value.fallback.releasePage)

const findAssetLink = (keywords: string[]) => {
  const assets = release.value?.assets || []
  const lowerKeywords = keywords.map((keyword) => keyword.toLowerCase())

  const asset = assets.find((candidate) => {
    const normalizedName = candidate.name.toLowerCase()
    return lowerKeywords.every((keyword) => normalizedName.includes(keyword))
  })

  return asset?.browser_download_url || null
}

const preferReleaseLink = (keywords: string[]) => {
  return findAssetLink([...keywords, 'release']) || findAssetLink(keywords)
}

const assetLinks = computed(() => ({
  game: findAssetLink(['engine', 'game']),
  developer: findAssetLink(['engine', 'develop']),
  utilitiesBin: preferReleaseLink(['utilities', 'bin']),
  utilitiesLib: preferReleaseLink(['utilities', 'lib']),
  serverBin: preferReleaseLink(['server', 'bin']),
  serverLib: preferReleaseLink(['server', 'lib']),
}))

const heroTitle = computed(() => copy.value.hero.title.replace('{version}', releaseVersion.value))
const heroDescription = computed(() =>
  copy.value.hero.description
    .replace('{date}', releaseDate.value)
    .replace('{version}', releaseVersion.value)
)

const branchLabel = computed(() => copy.value.branchLabel.replace('{version}', releaseVersion.value))

const mainCard = computed(() => ({ ...copy.value.cards.main }))
const developerCard = computed(() => ({ ...copy.value.cards.developer }))
const utilitiesCard = computed(() => ({ ...copy.value.cards.utilities }))
const serverCard = computed(() => ({ ...copy.value.cards.server }))
</script>

<template>
<section class="download-hero">
  <DownloadHeader
    :badge-label="copy.badgeLabel"
    :branch-label="branchLabel"
    :loading="loading"
    :error="error"
  />

  <div class="download-hero__title">
    <h1 v-html="heroTitle"></h1>
    <p v-html="heroDescription"></p>
  </div>

  <div class="download-hero__grid">
    <EngineCard
      :card="mainCard"
      :link="assetLinks.game"
      :release-url="releaseUrl"
      :loading="loading"
      :meta-label="copy.labels.production"
      :missing-label="copy.labels.missing"
    />

    <div class="download-column">
      <DeveloperCard
        :card="developerCard"
        :link="assetLinks.developer"
        :release-url="releaseUrl"
        :loading="loading"
        :missing-label="copy.labels.missing"
      />

      <UtilitiesCard
        :card="utilitiesCard"
        :link="assetLinks.utilitiesBin"
        :secondary-link="assetLinks.utilitiesLib"
        :release-url="releaseUrl"
        :loading="loading"
        :missing-label="copy.labels.missing"
      />

      <ServerCard
        :card="serverCard"
        :link="assetLinks.serverBin"
        :secondary-link="assetLinks.serverLib"
        :release-url="releaseUrl"
        :loading="loading"
        :missing-label="copy.labels.missing"
      />
    </div>
  </div>
</section>
</template>

<style>
/* -------------------------
   LIGHT THEME
------------------------- */
.download-hero {
  position: relative;
  padding: 24px 22px 26px;
  border-radius: 24px;
  background: #ffffff;
  border: 1px solid rgba(0, 0, 0, 0.12);
  box-shadow: 0 22px 60px rgba(0, 0, 0, 0.1);
  color: #111827;
}

.download-hero__head {
  display: flex;
  justify-content: space-between;
  gap: 12px;
  flex-wrap: wrap;
  margin-bottom: 16px;
}

.download-hero__badge {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  padding: 5px 12px;
  border-radius: 999px;
  border: 1px solid rgba(0,0,0,0.12);
  font-size: 11px;
  color: #374151;
  background: rgba(255,255,255,0.7);
  backdrop-filter: blur(14px);
}

.download-hero__badge .dot {
  width: 9px;
  height: 9px;
  border-radius: 999px;
  background: radial-gradient(circle, #22c55e 0, #16a34a 60%, transparent 65%);
}

.download-hero__branch {
  font-size: 11px;
  padding: 5px 10px;
  border-radius: 999px;
  border: 1px dashed rgba(0,0,0,0.25);
  color: #374151;
  background: rgba(255,255,255,0.7);
}

.download-hero__branch[aria-busy="true"] {
  opacity: 0.75;
}

.download-hero__title h1 {
  margin: 0 0 6px;
  font-size: 26px;
  line-height: 1.1;
  letter-spacing: 0.02em;
  color: #111827;
}

.download-hero__title p {
  margin: 0 0 18px;
  font-size: 13px;
  color: #4b5563;
  max-width: 460px;
}

.download-hero__grid {
  display: grid;
  grid-template-columns: minmax(0, 3fr) minmax(0, 2.4fr);
  gap: 18px;
}

@media (max-width: 900px) {
  .download-hero__grid {
    grid-template-columns: minmax(0, 1fr);
  }
}

.download-card {
  position: relative;
  padding: 16px 16px 15px;
  border-radius: 18px;
  background: #ffffff;
  color: #111827;
  border: 1px solid rgba(0, 0, 0, 0.08);
}

.download-card--accent {
  border: 1px solid rgba(76, 29, 149, 0.4);
}

.download-card--secondary {
  border: 1px solid rgba(0,0,0,0.12);
  background: #f9f9f9;
}

.download-card__head {
  display: flex;
  justify-content: space-between;
  align-items: center;
  gap: 8px;
  margin-bottom: 10px;
}

.download-card__head .tag {
  font-size: 11px;
  color: #4b5563;
}

.pill {
  display: inline-flex;
  align-items: center;
  padding: 3px 9px;
  border-radius: 999px;
  font-size: 10px;
  color: #111827;
  border: 1px solid rgba(0,0,0,0.2);
  background: rgba(255,255,255,0.8);
}

.pill--primary {
  background: linear-gradient(to right, #4f46e5, #22c55e);
  color: #ffffff;
  border: none;
}

.tag {
  display: inline-flex;
  align-items: center;
  padding: 3px 9px;
  border-radius: 999px;
  font-size: 11px;
  color: #4b5563;
  border: 1px dashed rgba(0, 0, 0, 0.25);
  background: rgba(255,255,255,0.7);
}

.download-card h2,
.download-card h3 {
  margin: 0 0 6px;
  font-size: 16px;
  color: #636e85;
}

.download-card__desc {
  margin: 0 0 12px;
  font-size: 12px;
  color: #4b5563;
}

.download-card__meta {
  margin: 0 0 12px;
  padding-left: 16px;
  font-size: 12px;
  color: #111827;
}

.download-card__meta li + li {
  margin-top: 3px;
}

.download-card__footer {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 10px;
  margin-top: 4px;
}

.download-card__buttons {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
  margin-top: 6px;
}

.btn-primary {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  padding: 7px 14px;
  border-radius: 999px;
  font-size: 12px;
  font-weight: 500;
  color: var(--vp-button-brand-text) !important;
  background-color: var(--vp-button-brand-bg);
  border: 1px solid #1E40AF;
  cursor: pointer;
  text-decoration: none !important;
  transition: all 0.2s ease;
}

.btn-primary:hover {
  transform: translateY(-1px);
  background-color: #1D4ED8;
  border-color: #1E3A8A;
  box-shadow: 0 4px 12px rgba(30, 64, 175, 0.3);
}

.btn-ghost {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  padding: 6px 11px;
  border-radius: 999px;
  font-size: 12px;
  color: #111827;
  background: rgba(255,255,255,0.9);
  border: 1px solid rgba(0,0,0,0.2);
  text-decoration: none;
  transition: all 0.2s ease;
}

.btn-ghost:hover {
  border-color: rgba(0,0,0,0.35);
  background: rgba(240,240,240,0.9);
}

.btn-primary.is-loading,
.btn-ghost.is-loading {
  opacity: 0.7;
  pointer-events: none;
}

.link-minor {
  display: inline-flex;
  font-size: 12px;
  color: #4f46e5;
  text-decoration: none;
  transition: all 0.2s ease;
  align-items: center;
  padding: 6px 11px;
  border-radius: 999px;
  border: 1px solid transparent;
}

.link-minor:hover {
  color: #4338ca;
  text-decoration: underline;
  border-color: rgba(165, 180, 252, 0.3);
}

.hash {
  font-size: 11px;
  color: #4b5563;
}

.download-column {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

/* -------------------------
   DARK THEME (исправленный)
------------------------- */
:root.dark .download-hero {
  background: #202127;
  border: 1px solid rgba(148, 163, 184, 0.45);
  box-shadow: 0 22px 60px rgba(15, 23, 42, 0.85);
  color: #98989f;
}

:root.dark .download-hero__badge {
  border-color: rgba(148, 163, 184, 0.5);
  color: #9ca3af;
  background: rgba(30, 41, 59, 0.6);
}

:root.dark .download-hero__branch {
  border-color: rgba(148, 163, 184, 0.6);
  color: #9ca3af;
  background: rgba(15, 23, 42, 0.85);
}

:root.dark .download-hero__title h1 {
  color: #ffffff;
}

:root.dark .download-hero__title p {
  color: #cbd5f5;
}

:root.dark .download-card {
  background: #202127;
  border: none;
  color: #e5e7eb;
}

:root.dark .download-card--accent {
  border: 1px solid rgba(129, 140, 248, 0.8);
  background: #202127;
  box-shadow: 0 2px 8px rgba(0,0,0,0.4);
}

/* ИСПРАВЛЕНИЕ: добавлена граница и улучшен фон для ВСЕХ вторичных карточек */
:root.dark .download-card--secondary {
  background: #1a1a23;
  border: 1px solid rgba(148, 163, 184, 0.3);
}

/* Улучшение контрастности текста для всех вторичных карточек */
:root.dark .download-card--secondary h3,
:root.dark .download-card--secondary .download-card__desc {
  color: #e5e7eb;
}

:root.dark .pill {
  color: #e5e7eb;
  border: 1px solid rgba(148, 163, 184, 0.65);
  background: rgba(15, 23, 42, 0.8);
}

:root.dark .tag {
  color: #e5e7eb;
  border-color: rgba(148, 163, 184, 0.5);
  background: rgba(30, 41, 59, 0.7);
}

/* Улучшение контрастности меток вторичных карточек */
:root.dark .download-card--secondary .pill {
  color: #e5e7eb;
  border-color: rgba(148, 163, 184, 0.5);
  background: rgba(30, 41, 59, 0.7);
}

:root.dark .download-card__desc {
  color: #cbd5f5;
}

:root.dark .download-card__meta {
  color: #e5e7eb;
}

:root.dark .btn-primary
{
  border-color: #60A5FA;
  color: var(--vp-button-brand-text);
}

:root.dark .btn-primary:hover {
  background-color: #2563EB;
  border-color: #3B82F6;
  box-shadow: 0 4px 12px rgba(59, 130, 246, 0.4);
}

:root.dark .btn-ghost {
  color: #e5e7eb;
  background: rgba(15, 23, 42, 0.9);
  border: 1px solid rgba(148, 163, 184, 0.5);
}

:root.dark .btn-ghost:hover {
  background: rgba(30, 41, 59, 0.9);
  border-color: rgba(148, 163, 184, 0.8);
}

:root.dark .link-minor {
  color: #a5b4fc;
}

:root.dark .link-minor:hover {
  color: #818cf8;
}

:root.dark .hash {
  color: #9ca3af;
}
</style>
