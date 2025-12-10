import type { DefaultTheme } from 'vitepress'

export const modsSidebar: DefaultTheme.Sidebar = [
  {
    text: 'Старт',
    collapsed: false,
    items: [
      { text: 'Тест', link: '/mods/released/mods' },
    ]
  },
  {
    text: 'Аддоны',
    collapsed: false,
    items: [
      { text: 'Звуки', link: '/mods/addons/sound' }
    ]
  }
]
