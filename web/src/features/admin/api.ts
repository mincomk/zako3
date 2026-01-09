import { apiClient, buildQueryString } from '@/lib/api-client'
import type {
  PaginatedResponse,
  PaginationParams,
  AdminActivity,
  Tap,
} from '@/types'

export const adminApi = {
  getActivity: async (
    params: Partial<PaginationParams> = {}
  ): Promise<PaginatedResponse<AdminActivity>> => {
    const query = buildQueryString({
      page: params.page,
      perPage: params.perPage,
    })
    const response = await apiClient.get<PaginatedResponse<AdminActivity>>(
      `/admin/activity${query}`
    )
    if (response.error) throw new Error(response.error.message)
    return response.data
  },

  getPendingVerifications: async (): Promise<Tap[]> => {
    const response = await apiClient.get<Tap[]>(
      '/admin/taps/pending-verification'
    )
    if (response.error) throw new Error(response.error.message)
    return response.data
  },
}
