import { apiClient, buildQueryString } from '@/lib/api-client'
import type {
  PaginatedResponse,
  PaginationParams,
  AdminActivity,
  Tap,
  VerificationRequestFull,
  VerificationStatus,
} from '@/types'

interface GetVerificationRequestsParams extends Partial<PaginationParams> {
  status?: VerificationStatus
}

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

  getVerificationRequests: async (
    params: GetVerificationRequestsParams = {}
  ): Promise<PaginatedResponse<VerificationRequestFull>> => {
    const query = buildQueryString({
      page: params.page,
      perPage: params.perPage,
      status: params.status,
    })
    const response = await apiClient.get<
      PaginatedResponse<VerificationRequestFull>
    >(`/admin/verifications${query}`)
    if (response.error) throw new Error(response.error.message)
    return response.data
  },

  approveVerification: async (requestId: string): Promise<void> => {
    const response = await apiClient.post(
      `/admin/verifications/${requestId}/approve`
    )
    if (response.error) throw new Error(response.error.message)
  },

  rejectVerification: async (
    requestId: string,
    reason: string
  ): Promise<void> => {
    const response = await apiClient.post(
      `/admin/verifications/${requestId}/reject`,
      {
        reason,
      }
    )
    if (response.error) throw new Error(response.error.message)
  },
}
