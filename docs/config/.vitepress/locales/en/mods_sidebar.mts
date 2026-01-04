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
      { text: 'Weather', link: '/en/mods/addons/weather' },
      { text: 'UI/UX', link: '/en/mods/addons/ui-ux' },
      { text: 'Weapons pack', link: '/mods/addons/weapons-pack' }
    ]
  }
]
