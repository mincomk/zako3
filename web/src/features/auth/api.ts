import { apiClient } from '@/lib/api-client'
import type {
  AuthUser,
  LoginResponse,
  AuthCallbackResponse,
  RefreshTokenResponse,
} from '@/types'

export const authApi = {
  getLoginUrl: async (): Promise<LoginResponse> => {
    const response = await apiClient.get<LoginResponse>('/auth/login')
    if (response.error) throw new Error(response.error.message)
    return response.data
  },

  handleCallback: async (code: string): Promise<AuthCallbackResponse> => {
    const response = await apiClient.get<AuthCallbackResponse>(
      `/auth/callback?code=${code}`
    )
    if (response.error) throw new Error(response.error.message)
    return response.data
  },

  logout: async (): Promise<void> => {
    await apiClient.post('/auth/logout')
  },

  refreshToken: async (): Promise<RefreshTokenResponse> => {
    const response = await apiClient.get<RefreshTokenResponse>('/auth/refresh')
    if (response.error) throw new Error(response.error.message)
    return response.data
  },

  getCurrentUser: async (): Promise<AuthUser> => {
    const response = await apiClient.get<AuthUser>('/users/me')
    if (response.error) throw new Error(response.error.message)
    return response.data
  },
}
