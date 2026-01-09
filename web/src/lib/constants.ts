export const API_BASE_URL = import.meta.env.VITE_API_BASE_URL || '/api'
export const WS_BASE_URL =
  import.meta.env.VITE_WS_BASE_URL || 'ws://localhost:8080'

export const AUTH_TOKEN_KEY = 'zako_auth_token'
export const AUTH_USER_KEY = 'zako_auth_user'

export const THEME_STORAGE_KEY = 'zako-ui-theme'

export const DEFAULT_PAGE_SIZE = 20
export const PAGE_SIZE_OPTIONS = [10, 20, 50, 100] as const

export const TAP_ID_REGEX = /^[a-z0-9_.]+$/
export const TAP_ID_MIN_LENGTH = 3
export const TAP_ID_MAX_LENGTH = 32
export const TAP_NAME_MAX_LENGTH = 64
export const TAP_DESCRIPTION_MAX_LENGTH = 500

export const NOTIFICATION_LEVELS = [
  'info',
  'success',
  'warning',
  'error',
] as const

export const TAP_ROLES = ['music', 'tts'] as const

export const TAP_PERMISSIONS = [
  'owner_only',
  'public',
  'whitelisted',
  'blacklisted',
] as const

export const TAP_OCCUPATIONS = ['official', 'verified', 'base'] as const

export const ROUTES = {
  HOME: '/',
  LOGIN: '/login',
  AUTH_CALLBACK: '/auth/callback',
  DASHBOARD: '/dashboard',
  SETTINGS: '/settings',
  TAPS: '/taps',
  TAPS_CREATE: '/taps/create',
  TAPS_MINE: '/taps/mine',
  TAP_SETTINGS: (tapId: string) => `/taps/${tapId}/settings`,
  TAP_STATS: (tapId: string) => `/taps/${tapId}/stats`,
  ADMIN: '/admin',
  ADMIN_USERS: '/admin/users',
  ADMIN_USER: (userId: string) => `/admin/users/${userId}`,
  ADMIN_TAPS: '/admin/taps',
  ADMIN_TAP: (tapId: string) => `/admin/taps/${tapId}`,
  ADMIN_NOTIFICATIONS: '/admin/notifications',
} as const
