export type SupportState = 'supported' | 'unsupported' | 'legacy'

export type SupportMeta = {
  label: string
  state: SupportState
}

/**
 * Значки поддержки боковой панели на странице.
 * Ключи должны соответствовать разрешенной ссылке на боковой панели (например, "/configs/dltx").
 */
export const supportStatusMap: Record<string, SupportMeta> = {
  '/platforms/multiplayer/dedicated-server': { label: '1.3', state: 'supported' },
  '/graphics/weather/snowing': { label: '1.3', state: 'supported' },
  '/graphics/pbr': { label: '1.3', state: 'supported' },
  '/interface/custom-icon-atlases': { label: '1.3', state: 'supported' },
  '/animation-system/anim-notify': { label: '1.2', state: 'supported' },
  '/sounds/general-information': { label: '2.0', state: 'supported' },
  '/platforms/multiplayer/general-information': { label: '2.0', state: 'supported' },
  '/platforms/clear-sky/technical-features': { label: '1.4', state: 'supported' },
  '/platforms/shadow-of-chernobyl/technical-features': { label: '2.0', state: 'supported' },
  '/graphics/dynamic-wallmark': { label: '2.0', state: 'supported' },
  '/scripting/weather-manager': { label: 'Unsupported', state: 'unsupported' },
  '/scripting/luamarshal': { label: 'Unsupported', state: 'unsupported' },
  '/editors/particles': { label: '1.4', state: 'supported' },
}

const normalizeLink = (link: string) => link.replace(/^\/(en|ru)(?=\/)/, '')

export const getSupportMeta = (link?: string): SupportMeta | null => {
  if (!link) return null
  return supportStatusMap[link] ?? supportStatusMap[normalizeLink(link)] ?? null
}
