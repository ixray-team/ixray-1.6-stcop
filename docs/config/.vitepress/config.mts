import { defineConfig } from 'vitepress'
import lightbox from 'vitepress-plugin-lightbox'
import { enLocale } from './locales/en.mts'
import { ruLocale } from './locales/ru.mts'

// https://vitepress.dev/reference/site-config
export default defineConfig({
  title: "IX-Ray Platform", //Название в шапке
  description: "Официальная страница проекта IX-Ray Platform\nhttps://github.com/ixray-team/ixray-1.6-stcop",

  base: '/ixray-1.6-stcop/',
  srcDir: "../docs",
  outDir: '../public',
  lastUpdated: true,
  ignoreDeadLinks: true,
  rewrites: {
    'main/ru/:rest*': ':rest*',
    'main/en/:rest*': 'en/:rest*',
    'mods/ru/:rest*': 'mods/:rest*',
    'mods/en/:rest*': 'en/mods/:rest*',
  },

  head: [
    ['link', { rel: 'icon', href: '/ixray-1.6-stcop/favicon.ico' }] 
  ],

  locales: {
    root: ruLocale,
    en: enLocale,
  },

  markdown: {
    config: (md) => {
      md.use(lightbox, {})
    },
  },

  themeConfig: {
	 logo: '/logo.svg',
    search: {
      provider: 'local',
      options: {}
    },
    socialLinks: [
      { icon: 'github', link: 'https://github.com/ixray-team/ixray-1.6-stcop' },
      { icon: 'discord', link: 'https://discord.gg/hWTbHxaYWz' },
      { icon: 'telegram', link: 'https://t.me/ixray_platform' },
    ]
  }
})
