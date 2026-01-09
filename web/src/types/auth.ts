export interface AuthState {
  isAuthenticated: boolean
  user: AuthUser | null
  token: string | null
}

export interface AuthUser {
  id: string
  discordId: string
  username: string
  avatar: string
  email?: string
  isAdmin: boolean
}

export interface LoginResponse {
  redirectUrl: string
}

export interface AuthCallbackResponse {
  token: string
  user: AuthUser
}

export interface RefreshTokenResponse {
  token: string
}
