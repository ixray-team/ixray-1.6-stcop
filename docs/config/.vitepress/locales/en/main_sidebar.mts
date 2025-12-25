import type { DefaultTheme } from 'vitepress'

export const mainSidebar: DefaultTheme.Sidebar = [
  {
    text: 'General',
    collapsed: false,
    items: [
      { text: 'Installation', link: '/en/main/getting-started' },
      { text: 'Integrations', link: '/en/main/integrations' },
      { text: 'Launch keys', link: '/en/main/launch-keys' },
      { text: 'Console commands', link: '/en/main/console-commands' },
      { text: 'File ignoring system', link: '/en/main/file-ignoring-system' },
      { text: 'Engine extension configuration', link: '/en/main/engine-extension-configuration' },
      { text: 'How to download IX-Ray (stable and rolling versions)', link: '/en/main/download' },
    ]
  },
  {
    text: 'Configs',
    collapsed: false,
    items: [
      { text: 'DLTX', link: '/en/configs/dltx' },
      { text: 'XMLOverride', link: '/en/configs/xml-override' },
      { text: 'Game localization', link: '/en/configs/localization' },
      { text: 'File encoding', link: '/en/configs/file-encoding' },
      { text: 'Batch file connection using masks', link: '/en/configs/group-file-connection-using-masks' },
      { text: 'Preset user parameters', link: '/en/configs/preset-user-parameters' },
    ]
  },
  {
    text: 'Gameplay',
    collapsed: false,
    items: [
      {
        text: 'General',
        items: [
          { text: 'Dialogues', link: '/en/gameplay/general/dialogues' },
          { text: 'Mutants', link: '/en/gameplay/general/mutants' },
          { text: 'Transport', link: '/en/gameplay/general/transport' },
          { text: 'Characters', link: '/en/gameplay/general/characters' },
          { text: 'Third-person view', link: '/en/gameplay/general/3rd-person-view' },
          { text: 'First-person legs', link: '/en/gameplay/general/legs-from-the-first-person' },
          { text: 'Life simulation', link: '/en/gameplay/general/life-simulation' },
          { text: 'Items used', link: '/en/gameplay/general/items-used' },
          { text: 'Zones (bonfires, anomalies...)', link: '/en/gameplay/general/zones-bonfires-anomalies' },
        ]
      },
      {
        text: 'Equipment',
        items: [
          { text: 'General information', link: '/en/gameplay/equipment/general-information' },
          { text: 'Item upgrades', link: '/en/gameplay/equipment/item-upgrades' },
        ]
      }
    ]
  },
  {
    text: 'Weapon system',
    collapsed: false,
    items: [
      { text: 'General information', link: '/en/weapon-system/weapon-classes' },
      { text: 'Explosive classes', link: '/en/weapon-system/explosive-classes' }
    ]
  },
  {
    text: 'Animation system',
    collapsed: false,
    items: [
      { text: 'General information', link: '/en/animation-system/general-information' },
      { text: 'First-person animation system (Hud Animator)', link: '/en/animation-system/hud-animator' },
      { text: 'Animation frame callbacks (Anim Notify)', link: '/en/animation-system/anim-notify' },
    ]
  },
  {
    text: 'Dynamic models',
    collapsed: false,
    items: [
      { text: 'General information', link: '/en/dynamic-models/general-information' },
      { text: 'Adjusting model clipping', link: '/en/dynamic-models/adjusting-model-clipping' },
    ]
  },
  {
    text: 'Interface',
    collapsed: false,
    items: [
      { text: 'General information', link: '/en/interface/general-information' },
      { text: 'Item parameters', link: '/en/interface/item-parameters' },
      { text: 'Inventory slots', link: '/en/interface/inventory-slots' },
      { text: 'Custom icon atlases', link: '/en/interface/custom-icon-atlases' },
      { text: 'Fonts', link: '/en/interface/fonts' },
      { text: 'XML Expression', link: '/en/interface/xml-expression' },
    ]
  },
  {
    text: 'Scripting',
    collapsed: false,
    items: [
      { text: 'Base namespace', link: '/en/scripting/base-namespace' },
      { text: 'Exported enums', link: '/en/scripting/exported-enums' },
      { text: 'Lua: Callbacks', link: '/en/scripting/lua-callbacks' },
      { text: 'New functions', link: '/en/scripting/new-functions' },
      { text: 'Script callback system', link: '/en/scripting/script-callback-system' },
      { text: 'Saving script data (luamarshal)', link: '/en/scripting/luamarshal' },
      { text: 'Weather manager', link: '/en/scripting/weather-manager' },
    ]
  },
  {
    text: 'Graphics',
    collapsed: false,
    items: [
      { text: 'General information', link: '/en/graphics/general-information' },
      { text: 'Physically based rendering (PBR)', link: '/en/graphics/pbr' },
      { text: 'Dynamic wallmarks (Dynamic Wallmark)', link: '/en/graphics/dynamic-wallmark' },
      { text: 'Shader constants', link: '/en/graphics/shader-constants' },
      { text: 'Weather', link: '/en/graphics/weather/main' },
      {
        items: [
          { text: 'Snowing', link: '/en/graphics/weather/snowing' },
          { text: 'Getting wet', link: '/en/graphics/weather/getting-wet' },
        ]
      },
      { text: 'Shader options', link: '/en/graphics/shader-options' },
      { text: 'XML Blends', link: '/en/graphics/xml-blends' },
    ]
  },
  {
    text: 'Sounds',
    collapsed: false,
    items: [
      { text: 'General information', link: '/en/sounds/general-information' },
      { text: 'Sound layers (Sound Layers)', link: '/en/sounds/sound-layers' },
      { text: 'Sound zones (Sound Env)', link: '/en/sounds/sound-env' },
      { text: 'Equipment effects', link: '/en/sounds/equipment-effects' },
    ]
  },
  {
    text: 'Editors',
    collapsed: false,
    items: [
      { text: 'IXR SDK', link: '/en/editors/ixr-sdk' },
      {
        items: [
          { text: 'Plugins', link: '/en/editors/plugins' },
          { text: 'Dialog Editor', link: '/en/editors/dialog-editor' },
          { text: 'Particles', link: '/en/editors/particles' },
        ]
      },
    ]
  },
  {
    text: 'Utilities',
    collapsed: false,
    items: [
      { text: 'Compilers', link: '/en/utilities/compilers' },
      { text: 'Compressor', link: '/en/utilities/compressor' },
    ]
  },
  {
    text: 'Addon system',
    collapsed: false,
    items: [
      { text: 'General information', link: '/en/addon-system/general-information' },
    ]
  },
  {
    text: 'Platforms',
    collapsed: false,
    items: [
      {
        text: 'Clear Sky',
        items: [
          { text: 'Technical features', link: '/en/platforms/clear-sky/technical-features' },
        ]
      },
      {
        text: 'Multiplayer',
        items: [
          { text: 'General information', link: '/en/platforms/multiplayer/general-information' },
          { text: 'Dedicated Server', link: '/en/platforms/multiplayer/dedicated-server' },
        ]
      }
    ]
  },
  {
    text: 'Guides and contributions',
    collapsed: false,
    items: [
      { text: 'Documentation rules for GitHub Wiki', link: '/en/guidelines-contributions/documentation-rules-github-wiki' },
    ]
  },
]
