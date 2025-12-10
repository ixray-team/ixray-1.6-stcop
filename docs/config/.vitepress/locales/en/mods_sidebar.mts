import type { DefaultTheme } from 'vitepress'

export const modsSidebar: DefaultTheme.Sidebar = [
  {
    text: 'Start',
    collapsed: false,
    items: [
      { text: 'Test', link: '/en/mods/released/mods' },
    ]
  },
  {
    text: 'Addons',
    collapsed: false,
    items: [
      { text: 'Sounds', link: '/en/mods/addons/sound' },
    ]
  }
]
