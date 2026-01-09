import { apiClient, buildQueryString } from '@/lib/api-client'
import type {
  PaginatedResponse,
  PaginationParams,
  Notification,
  NotificationFilters,
  NotificationSort,
} from '@/types'

interface GetNotificationsParams
  extends Partial<PaginationParams>,
    Partial<NotificationFilters> {
  sortField?: NotificationSort['field']
  sortDirection?: NotificationSort['direction']
}

export const notificationsApi = {
  getNotifications: async (
    params: GetNotificationsParams = {}
  ): Promise<PaginatedResponse<Notification>> => {
    const query = buildQueryString({
      page: params.page,
      perPage: params.perPage,
      search: params.search,
      level: params.level,
      category: params.category,
      isRead: params.isRead,
      sortField: params.sortField,
      sortDirection: params.sortDirection,
    })
    const response = await apiClient.get<PaginatedResponse<Notification>>(
      `/notifications${query}`
    )
    if (response.error) throw new Error(response.error.message)
    return response.data
  },

  getUnreadCount: async (): Promise<{ count: number }> => {
    const response = await apiClient.get<{ count: number }>('/notifications/unread-count')
    if (response.error) throw new Error(response.error.message)
    return response.data
  },

  markAsRead: async (notificationId: string): Promise<Notification> => {
    const response = await apiClient.patch<Notification>(
      `/notifications/${notificationId}/read`
    )
    if (response.error) throw new Error(response.error.message)
    return response.data
  },

  markAllAsRead: async (): Promise<void> => {
    const response = await apiClient.patch('/notifications/read-all')
    if (response.error) throw new Error(response.error.message)
  },

  deleteNotification: async (notificationId: string): Promise<void> => {
    const response = await apiClient.delete(`/notifications/${notificationId}`)
    if (response.error) throw new Error(response.error.message)
  },

  getAdminNotifications: async (
    params: GetNotificationsParams = {}
  ): Promise<PaginatedResponse<Notification>> => {
    const query = buildQueryString({
      page: params.page,
      perPage: params.perPage,
      search: params.search,
      level: params.level,
      category: params.category,
      sortField: params.sortField,
      sortDirection: params.sortDirection,
    })
    const response = await apiClient.get<PaginatedResponse<Notification>>(
      `/admin/notifications${query}`
    )
    if (response.error) throw new Error(response.error.message)
    return response.data
  },
}
