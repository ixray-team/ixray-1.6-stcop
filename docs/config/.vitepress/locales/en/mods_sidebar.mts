import type { DefaultTheme } from 'vitepress'

export const modsSidebar: DefaultTheme.Sidebar = [
  {
    text: 'Start',
    collapsed: false,
    items: [
      { text: 'Released', link: '/en/mods/released/mods' },
      { text: 'WIP', link: '/en/mods/released/mods_dev' },
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
