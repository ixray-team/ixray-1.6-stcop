import type { DefaultTheme } from 'vitepress'

export const mainSidebar: DefaultTheme.Sidebar = [
  {
    text: 'Общее',
    collapsed: false,
    items: [
      { text: 'Установка', link: '/main/getting-started' },
      { text: 'Интеграции', link: '/main/integrations' },
      { text: 'Ключи запуска', link: '/main/launch-keys' },
      { text: 'Консольные команды', link: '/main/console-commands' },
      { text: 'Система игнорирования файлов', link: '/main/file-ignoring-system' },
      { text: 'Настройка расширения движка', link: '/main/engine-extension-configuration' },
      { text: 'Как скачать IX-Ray (stable и rolling)', link: '/main/download' },
    ],
  },
  {
    text: 'Конфиги',
    collapsed: false,
    items: [
      { text: 'DLTX', link: '/configs/dltx' },
      { text: 'XMLOverride', link: '/configs/xml-override' },
      { text: 'Локализация игры', link: '/configs/localization' },
      { text: 'Кодировка файлов', link: '/configs/file-encoding' },
      { text: 'Групповое подключение файлов по маске', link: '/configs/group-file-connection-using-masks' },
      { text: 'Пользовательские пресеты параметров', link: '/configs/preset-user-parameters' },
    ],
  },
  {
    text: 'Геймплей',
    collapsed: false,
    items: [
      {
        text: 'Общее',
        items: [
          { text: 'Диалоги', link: '/gameplay/general/dialogues' },
          { text: 'Мутанты', link: '/gameplay/general/mutants' },
          { text: 'Транспорт', link: '/gameplay/general/transport' },
          { text: 'Персонажи', link: '/gameplay/general/characters' },
          { text: 'Вид от 3-го лица', link: '/gameplay/general/3rd-person-view' },
          { text: 'Ноги от 1-го лица', link: '/gameplay/general/legs-from-the-first-person' },
          { text: 'Симуляция жизни', link: '/gameplay/general/life-simulation' },
          { text: 'Используемые предметы', link: '/gameplay/general/items-used' },
          { text: 'Зоны, костры, аномалии', link: '/gameplay/general/zones-bonfires-anomalies' },
        ],
      },
      {
        text: 'Экипировка',
        items: [
          { text: 'Общая информация', link: '/gameplay/equipment/general-information' },
          { text: 'Апгрейды предметов', link: '/gameplay/equipment/item-upgrades' },
        ],
      },
    ],
  },
  {
    text: 'Система оружия',
    collapsed: false,
    items: [
      { text: 'Общие данные', link: '/weapon-system/weapon-classes' },
      { text: 'Взрывчатка', link: '/weapon-system/explosive-classes' }
    ],
  },
  {
    text: 'Анимационная система',
    collapsed: false,
    items: [
      { text: 'Общая информация', link: '/animation-system/general-information' },
      { text: 'Hud Animator', link: '/animation-system/hud-animator' },
      { text: 'Anim Notify', link: '/animation-system/anim-notify' },
    ],
  },
  {
    text: 'Динамические модели',
    collapsed: false,
    items: [
      { text: 'Общая информация', link: '/dynamic-models/general-information' },
      { text: 'Настройка отсечения модели', link: '/dynamic-models/adjusting-model-clipping' },
    ],
  },
  {
    text: 'Интерфейс',
    collapsed: false,
    items: [
      { text: 'Общая информация', link: '/interface/general-information' },
      { text: 'Параметры предметов', link: '/interface/item-parameters' },
      { text: 'Слоты инвентаря', link: '/interface/inventory-slots' },
      { text: 'Пользовательские атласы иконок', link: '/interface/custom-icon-atlases' },
      { text: 'Шрифты', link: '/interface/fonts' },
      { text: 'XML Expression', link: '/interface/xml-expression' },
    ],
  },
  {
    text: 'Скрипты',
    collapsed: false,
    items: [
      { text: 'Базовое пространство имён', link: '/scripting/base-namespace' },
      { text: 'Экспорт enum', link: '/scripting/exported-enums' },
      { text: 'Lua: Callbacks', link: '/scripting/lua-callbacks' },
      { text: 'Новые функции', link: '/scripting/new-functions' },
      { text: 'Система callback', link: '/scripting/script-callback-system' },
      { text: 'Luamarshal', link: '/scripting/luamarshal' },
      { text: 'Менеджер погоды', link: '/scripting/weather-manager' },
    ],
  },
  {
    text: 'Графика',
    collapsed: false,
    items: [
      { text: 'Общая информация', link: '/graphics/general-information' },
      { text: 'PBR', link: '/graphics/pbr' },
      { text: 'Dynamic Wallmark', link: '/graphics/dynamic-wallmark' },
      { text: 'Константы шейдеров', link: '/graphics/shader-constants' },
      { text: 'Погода', link: '/graphics/weather/main' },
      {
        items: [
          { text: 'Снег', link: '/graphics/weather/snowing' },
          { text: 'Намокание', link: '/graphics/weather/getting-wet' },
        ],
      },
      { text: 'Опции шейдеров', link: '/graphics/shader-options' },
      { text: 'XML Blends', link: '/graphics/xml-blends' },
    ],
  },
  {
    text: 'Звуки',
    collapsed: false,
    items: [
      { text: 'Общая информация', link: '/sounds/general-information' },
      { text: 'Слои звука', link: '/sounds/sound-layers' },
      { text: 'Звуковые зоны', link: '/sounds/sound-env' },
      { text: 'Эффекты снаряжения', link: '/sounds/equipment-effects' },
    ],
  },
  {
    text: 'Редакторы',
    collapsed: false,
    items: [
      { text: 'IXR SDK', link: '/editors/ixr-sdk' },
      {
        items: [
          { text: 'Плагины', link: '/editors/plugins' },
          { text: 'Dialog Editor', link: '/editors/dialog-editor' },
          { text: 'Particles', link: '/editors/particles' },
        ],
      },
    ],
  },
  {
    text: 'Утилиты',
    collapsed: false,
    items: [
      { text: 'Компиляторы', link: '/utilities/compilers' },
      { text: 'Компрессор', link: '/utilities/compressor' },
    ],
  },
  {
    text: 'Система аддонов',
    collapsed: false,
    items: [{ text: 'Общая информация', link: '/addon-system/general-information' }],
  },
  {
    text: 'Платформы',
    collapsed: false,
    items: [
      {
        text: 'Clear Sky',
        items: [{ text: 'Технические особенности', link: '/platforms/clear-sky/technical-features' }],
      },
      {
        text: 'Мультиплеер',
        items: [
          { text: 'Общая информация', link: '/platforms/multiplayer/general-information' },
          { text: 'Dedicated Server', link: '/platforms/multiplayer/dedicated-server' },
        ],
      },
    ],
  },
  {
    text: 'Гайды и вклад',
    collapsed: false,
    items: [
      {
        text: 'Правила документации для GitHub Wiki',
        link: '/guidelines-contributions/documentation-rules-github-wiki',
      },
    ],
  },
]
