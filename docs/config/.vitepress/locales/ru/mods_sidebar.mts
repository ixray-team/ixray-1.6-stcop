import type { DefaultTheme } from 'vitepress'

export const modsSidebar: DefaultTheme.Sidebar = [
  {
    text: 'Модификации',
    collapsed: false,
    items: [
      { text: 'Вышедшие', link: '/mods/released/mods' },
      { text: 'В разработке', link: '/mods/released/mods_dev' },
    ]
  },
  {
    text: 'Аддоны',
    collapsed: false,
    items: [
      { text: 'Погода', link: '/mods/addons/weather' },
      { text: 'UI/UX', link: '/mods/addons/ui-ux' }
    ]
  }
]
