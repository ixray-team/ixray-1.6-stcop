import type { Theme } from 'vitepress'
import DefaultTheme from 'vitepress/theme'
import Layout from '../../../components/Layout.vue'
import Video from '../../../components/Video.vue'
import ParameterDetails from '../../../components/ParameterDetails.vue'
import './style.css'
import 'virtual:group-icons.css'

export default {
  extends: DefaultTheme,
  Layout,
  enhanceApp(ctx) {
    DefaultTheme.enhanceApp?.(ctx)
    ctx.app.component('Video', Video)
    ctx.app.component('ParameterDetails', ParameterDetails)
  },
} satisfies Theme
