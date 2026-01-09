import { apiClient, buildQueryString } from '@/lib/api-client'
import { apiCall } from '@/lib/api-helpers'
import type {
  PaginatedResponse,
  PaginationParams,
  Tap,
  TapWithAccess,
  TapFilters,
  TapSort,
  TapStats,
  TapAuditLogEntry,
} from '@/types'
import type {
  CreateTapInput,
  UpdateTapInput,
  ReportTapInput,
  VerificationRequestInput,
} from './schemas'

interface GetTapsParams extends Partial<PaginationParams>, Partial<TapFilters> {
  sortField?: TapSort['field']
  sortDirection?: TapSort['direction']
}

export const tapsApi = {
  getTaps: async (
    params: GetTapsParams = {}
  ): Promise<PaginatedResponse<TapWithAccess>> => {
    const query = buildQueryString({
      page: params.page,
      perPage: params.perPage,
      search: params.search,
      roles: params.roles?.join(','),
      accessible: params.accessible,
      ownerId: params.ownerId,
      sortField: params.sortField,
      sortDirection: params.sortDirection,
    })
    return apiCall(
      apiClient.get<PaginatedResponse<TapWithAccess>>(`/taps${query}`)
    )
  },

  getTap: async (tapId: string): Promise<TapWithAccess> => {
    return apiCall(apiClient.get<TapWithAccess>(`/taps/${tapId}`))
  },

  createTap: async (data: CreateTapInput): Promise<Tap> => {
    return apiCall(apiClient.post<Tap>('/taps', data))
  },

  updateTap: async (tapId: string, data: UpdateTapInput): Promise<Tap> => {
    return apiCall(apiClient.patch<Tap>(`/taps/${tapId}`, data))
  },

  deleteTap: async (tapId: string): Promise<void> => {
    return apiCall(apiClient.delete(`/taps/${tapId}`))
  },

  getTapStats: async (tapId: string): Promise<TapStats> => {
    return apiCall(apiClient.get<TapStats>(`/taps/${tapId}/stats`))
  },

  getTapAuditLog: async (
    tapId: string,
    params: Partial<PaginationParams> = {}
  ): Promise<PaginatedResponse<TapAuditLogEntry>> => {
    const query = buildQueryString({
      page: params.page,
      perPage: params.perPage,
    })
    return apiCall(
      apiClient.get<PaginatedResponse<TapAuditLogEntry>>(
        `/taps/${tapId}/audit-log${query}`
      )
    )
  },

  reportTap: async (tapId: string, data: ReportTapInput): Promise<void> => {
    return apiCall(apiClient.post(`/taps/${tapId}/report`, data))
  },

  requestVerification: async (
    tapId: string,
    data: VerificationRequestInput
  ): Promise<void> => {
    return apiCall(apiClient.post(`/taps/${tapId}/verify`, data))
  },

  getMyTaps: async (
    params: Partial<PaginationParams> = {}
  ): Promise<PaginatedResponse<Tap>> => {
    const query = buildQueryString({
      page: params.page,
      perPage: params.perPage,
    })
    return apiCall(
      apiClient.get<PaginatedResponse<Tap>>(`/users/me/taps${query}`)
    )
  },
}
