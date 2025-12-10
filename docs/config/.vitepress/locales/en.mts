import type { DefaultTheme } from 'vitepress'
import { mainSidebar } from './en/main_sidebar.mts'
import { modsSidebar } from './en/mods_sidebar.mts'

const nav: DefaultTheme.NavItem[] = [
  { text: 'Home', link: '/en/' },
  { text: 'Download', link: '/en/download' },
  { text: 'Documentation', link: '/en/main/getting-started' },
]

const navWithDevExamples: DefaultTheme.NavItem[] = [
  ...nav,
  { text: 'Modifications', link: '/en/mods/released/mods' },
]

export const enLocale = {
  label: 'English',
  lang: 'en',
  link: '/en/',
  dir: 'ltr',
  themeConfig: {
    outline: { level: [2, 3], label: 'Page contents' },
    docFooter: {
      prev: 'Previous page',
      next: 'Next page'
    },
    notFound: {
      title: 'PAGE NOT FOUND',
      quote:
          'But if you do not change direction and keep looking, you may end up where you are headed.',
      linkLabel: 'Go to the home page',
      linkText: 'Take me home'
    },
    editLink: {
      pattern: 'https://github.com/ixray-team/ixray-1.6-stcop/edit/default/docs/:path',
      text: 'Edit page'
    },
    lastUpdated: {
      text: 'Updated'
    },
    footer: {
      message: 'Published under the MIT license.',
      copyright: '© 2025 ixray-team / IX-Ray Platform'
    },
    darkModeSwitchLabel: 'Appearance',
    lightModeSwitchTitle: 'Switch to light theme',
    darkModeSwitchTitle: 'Switch to dark theme',
    sidebarMenuLabel: 'Menu',
    returnToTopLabel: 'Back to top',
    langMenuLabel: 'Change language',
    skipToContentLabel: 'Skip to content',
    nav: navWithDevExamples,
    sidebar: {
      '/en/mods/': modsSidebar,
      '/en/': mainSidebar,
    },
  },
}



