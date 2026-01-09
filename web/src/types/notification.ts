export type NotificationLevel = 'info' | 'success' | 'warning' | 'error'

export type NotificationCategory =
  | 'tap_created'
  | 'tap_updated'
  | 'tap_deleted'
  | 'tap_reported'
  | 'tap_verified'
  | 'tap_verification_requested'
  | 'tap_verification_approved'
  | 'tap_verification_rejected'
  | 'user_banned'
  | 'user_unbanned'
  | 'user_role_changed'
  | 'system_alert'
  | 'custom'

export interface Notification {
  id: string
  userId: string
  category: NotificationCategory
  level: NotificationLevel
  title: string
  message: string
  metadata?: Record<string, unknown>
  isRead: boolean
  createdAt: string
}

export interface NotificationFilters {
  search?: string
  level?: NotificationLevel
  category?: NotificationCategory
  isRead?: boolean
}

export interface NotificationSort {
  field: 'createdAt' | 'level'
  direction: 'asc' | 'desc'
}

export interface AuditLogEntry {
  id: string
  tapId: string
  actorId: string
  action: string
  level: NotificationLevel
  details: Record<string, unknown>
  createdAt: string
}

export interface AuditLogFilters {
  search?: string
  level?: NotificationLevel
  action?: string
  actorId?: string
  startDate?: string
  endDate?: string
}
