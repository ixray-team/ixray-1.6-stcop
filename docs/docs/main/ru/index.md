---
# https://vitepress.dev/reference/default-theme-home-page
layout: home

hero:
  name: "IX-Ray Platform"
  #text: "IX-Ray — это форк X-Ray Engine 1.6, направленный на улучшение игрового и девелоперского опыта, исправление багов оригинала и расширение возможностей для модмейкеров."
  tagline: IX-Ray — это форк движка X-Ray 1.6, который направлен на улучшение игрового процесса и упрощение разработки модификаций. Общими целями проекта являются улучшение опыта разработки и игрового опыта, исправление множества ошибок оригинального движка и расширение поддержки новых функций.
  actions:
    - theme: brand
      text: Документация
      link: /main/getting-started
    - theme: alt
      text: GitHub
      link: https://github.com/ixray-team/ixray-1.6-stcop
    - theme: alt
      text: Discord
      link: https://discord.gg/hWTbHxaYWz
    - theme: alt
      text: Telegram
      link: https://t.me/ixray_platform

heroVideo:
  url: https://www.youtube.com/watch?v=XPe_rKGrO6Y
  title: Обзор IX-Ray Platform

features:
  - title: Code Quality
    details: Обновлённая и оптимизированная кодовая база расчитаная на современные IDE и системы сборки
  - title: SDK
    details: Полный набор инструментов, включая компиляторы, редакторы, различные утилиты и плагины
  - title: Platforms
    details: Расширенная поддержка мультиплеера, CoC, OMP и Clear Sky
---

<script setup>
import VkArticleWidget from '../../../components/VkArticleWidget.vue';
</script>

<VkArticleWidget />
