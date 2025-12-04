import type { Theme } from 'vitepress'
import DefaultTheme from 'vitepress/theme'
import Layout from '../../../components/Layout.vue'
import Video from '../../../components/Video.vue'
import './style.css'

export default {
  extends: DefaultTheme,
  Layout,
  enhanceApp(ctx) {
    DefaultTheme.enhanceApp?.(ctx)
    ctx.app.component('Video', Video)
  },
} satisfies Theme
