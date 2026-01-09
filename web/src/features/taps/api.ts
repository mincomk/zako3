import { apiClient, buildQueryString } from '@/lib/api-client'
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
import type { CreateTapInput, UpdateTapInput, ReportTapInput, VerificationRequestInput } from './schemas'

interface GetTapsParams extends Partial<PaginationParams>, Partial<TapFilters> {
    sortField?: TapSort['field']
    sortDirection?: TapSort['direction']
}

export const tapsApi = {
    getTaps: async (params: GetTapsParams = {}): Promise<PaginatedResponse<TapWithAccess>> => {
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
        const response = await apiClient.get<PaginatedResponse<TapWithAccess>>(`/taps${query}`)
        if (response.error) throw new Error(response.error.message)
        return response.data
    },

    getTap: async (tapId: string): Promise<TapWithAccess> => {
        const response = await apiClient.get<TapWithAccess>(`/taps/${tapId}`)
        if (response.error) throw new Error(response.error.message)
        return response.data
    },

    createTap: async (data: CreateTapInput): Promise<Tap> => {
        const response = await apiClient.post<Tap>('/taps', data)
        if (response.error) throw new Error(response.error.message)
        return response.data
    },

    updateTap: async (tapId: string, data: UpdateTapInput): Promise<Tap> => {
        const response = await apiClient.patch<Tap>(`/taps/${tapId}`, data)
        if (response.error) throw new Error(response.error.message)
        return response.data
    },

    deleteTap: async (tapId: string): Promise<void> => {
        const response = await apiClient.delete(`/taps/${tapId}`)
        if (response.error) throw new Error(response.error.message)
    },

    getTapStats: async (tapId: string): Promise<TapStats> => {
        const response = await apiClient.get<TapStats>(`/taps/${tapId}/stats`)
        if (response.error) throw new Error(response.error.message)
        return response.data
    },

    getTapAuditLog: async (
        tapId: string,
        params: Partial<PaginationParams> = {}
    ): Promise<PaginatedResponse<TapAuditLogEntry>> => {
        const query = buildQueryString({
            page: params.page,
            perPage: params.perPage,
        })
        const response = await apiClient.get<PaginatedResponse<TapAuditLogEntry>>(
            `/taps/${tapId}/audit-log${query}`
        )
        if (response.error) throw new Error(response.error.message)
        return response.data
    },

    reportTap: async (tapId: string, data: ReportTapInput): Promise<void> => {
        const response = await apiClient.post(`/taps/${tapId}/report`, data)
        if (response.error) throw new Error(response.error.message)
    },

    requestVerification: async (tapId: string, data: VerificationRequestInput): Promise<void> => {
        const response = await apiClient.post(`/taps/${tapId}/verify`, data)
        if (response.error) throw new Error(response.error.message)
    },

    getMyTaps: async (params: Partial<PaginationParams> = {}): Promise<PaginatedResponse<Tap>> => {
        const query = buildQueryString({
            page: params.page,
            perPage: params.perPage,
        })
        const response = await apiClient.get<PaginatedResponse<Tap>>(`/users/me/taps${query}`)
        if (response.error) throw new Error(response.error.message)
        return response.data
    },
}
