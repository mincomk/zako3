import { apiClient, buildQueryString } from '@/lib/api-client'
import type {
  PaginatedResponse,
  PaginationParams,
  User,
  UserWithActivity,
  UserFilters,
  UserSort,
  BanUserInput,
  UpdateUserRoleInput,
} from '@/types'

interface GetUsersParams extends Partial<PaginationParams>, Partial<UserFilters> {
  sortField?: UserSort['field']
  sortDirection?: UserSort['direction']
}

export const usersApi = {
  getUsers: async (params: GetUsersParams = {}): Promise<PaginatedResponse<UserWithActivity>> => {
    const query = buildQueryString({
      page: params.page,
      perPage: params.perPage,
      search: params.search,
      isBanned: params.isBanned,
      isAdmin: params.isAdmin,
      sortField: params.sortField,
      sortDirection: params.sortDirection,
    })
    const response = await apiClient.get<PaginatedResponse<UserWithActivity>>(
      `/admin/users${query}`
    )
    if (response.error) throw new Error(response.error.message)
    return response.data
  },

  getUser: async (userId: string): Promise<UserWithActivity> => {
    const response = await apiClient.get<UserWithActivity>(`/admin/users/${userId}`)
    if (response.error) throw new Error(response.error.message)
    return response.data
  },

  getUserPublic: async (userId: string): Promise<User> => {
    const response = await apiClient.get<User>(`/users/${userId}`)
    if (response.error) throw new Error(response.error.message)
    return response.data
  },

  banUser: async (userId: string, data: Omit<BanUserInput, 'userId'>): Promise<UserWithActivity> => {
    const response = await apiClient.post<UserWithActivity>(`/admin/users/${userId}/ban`, data)
    if (response.error) throw new Error(response.error.message)
    return response.data
  },

  unbanUser: async (userId: string): Promise<UserWithActivity> => {
    const response = await apiClient.post<UserWithActivity>(`/admin/users/${userId}/unban`)
    if (response.error) throw new Error(response.error.message)
    return response.data
  },

  updateUserRole: async (
    userId: string,
    data: Omit<UpdateUserRoleInput, 'userId'>
  ): Promise<UserWithActivity> => {
    const response = await apiClient.patch<UserWithActivity>(`/admin/users/${userId}/role`, data)
    if (response.error) throw new Error(response.error.message)
    return response.data
  },
}
