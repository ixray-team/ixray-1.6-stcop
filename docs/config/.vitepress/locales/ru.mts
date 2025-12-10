import type { DefaultTheme } from 'vitepress'
import { mainSidebar } from './ru/main_sidebar.mts'
import { modsSidebar } from './ru/mods_sidebar.mts'

const nav: DefaultTheme.NavItem[] = [
  { text: 'Главная', link: '/' },
  { text: 'Загрузка', link: '/download' },
  { text: 'Документация', link: '/main/getting-started' },
]

const navWithMods: DefaultTheme.NavItem[] = [
  ...nav,
  { text: 'Модификации', link: '/mods/released/mods' },
]

const devModsSidebar = modsSidebar
const sidebar = mainSidebar

export const ruLocale = {
  label: 'Русский',
  lang: 'ru',
  dir: 'ltr',
  themeConfig: {
    outline: { level: [2, 3], label: 'Содержание' },
    docFooter: {
      prev: 'Предыдущая страница',
      next: 'Следующая страница',
    },
    notFound: {
      title: 'Страница не найдена',
      quote: 'Похоже, такой страницы нет. Попробуйте вернуться назад или воспользоваться поиском.',
      linkLabel: 'На главную',
      linkText: 'Вернуться домой',
    },
    editLink: {
      pattern: 'https://github.com/ixray-team/ixray-1.6-stcop/edit/default/docs/:path',
      text: 'Редактировать страницу',
    },
    lastUpdated: {
      text: 'Обновлено',
    },
    footer: {
      message: 'Опубликовано под лицензией MIT.',
      copyright: '© 2025 ixray-team / IX-Ray Platform',
    },
    darkModeSwitchLabel: 'Тема',
    lightModeSwitchTitle: 'Переключить на светлую тему',
    darkModeSwitchTitle: 'Переключить на тёмную тему',
    sidebarMenuLabel: 'Меню',
    returnToTopLabel: 'Наверх',
    langMenuLabel: 'Язык',
    skipToContentLabel: 'Перейти к содержимому',
    search: {
      options: {
        locales: {
          root: {
            translations: {
              button: {
                buttonText: 'Поиск',
                buttonAriaLabel: 'Поиск',
              },
              modal: {
                displayDetails: 'Показать детали',
                resetButtonTitle: 'Сбросить',
                backButtonTitle: 'Назад',
                noResultsText: 'Ничего не найдено',
                footer: {
                  selectText: 'Выбрать',
                  navigateText: 'Навигация',
                  closeText: 'Закрыть',
                },
              },
            },
          },
        },
      },
    },
    nav: navWithMods,
    sidebar: {
      '/mods/': devModsSidebar,
      '/': sidebar,
    },
  },
}
