import type { SortDirection } from './api'
import type { User } from './user'

export type TapOccupation = 'official' | 'verified' | 'base'

export type TapRole = 'music' | 'tts'

export type TapPermissionConfig =
  | { type: 'owner_only' }
  | { type: 'public' }
  | { type: 'whitelisted'; userIds: string[] }
  | { type: 'blacklisted'; userIds: string[] }

export interface TapBase {
  id: string
  name: string
  description: string
  createdAt: string
  updatedAt: string
  ownerId: string
  occupation: TapOccupation
  roles: TapRole[]
  totalUses: number
}

export interface Tap extends TapBase {
  permission: TapPermissionConfig
}

/**
 * Summary of user information (used in tap owner references)
 * Derived from the User type to maintain consistency
 */
export type UserSummary = Pick<User, 'id' | 'username' | 'avatar'>

export interface TapWithAccess extends Tap {
  hasAccess: boolean
  owner: UserSummary
}

export interface TapStats {
  tapId: string
  currentlyActive: number
  totalUses: number
  cacheHits: number
  uniqueUsers: number
  useRateHistory: TimeSeriesPoint[]
  cacheHitRateHistory: TimeSeriesPoint[]
}

export interface TimeSeriesPoint {
  timestamp: string
  value: number
}

export interface TapFilters {
  search?: string
  roles?: TapRole[]
  accessible?: boolean
  ownerId?: string
}

export interface TapSort {
  field: 'mostUsed' | 'recentlyCreated' | 'alphabetical'
  direction: SortDirection
}

export interface CreateTapInput {
  id: string
  name: string
  description: string
  roles: TapRole[]
  permission: TapPermissionConfig
}

export interface UpdateTapInput {
  id?: string
  name?: string
  description?: string
  roles?: TapRole[]
  permission?: TapPermissionConfig
}

export interface TapNotificationSettings {
  enabled: boolean
  channels: NotificationChannel[]
}

export interface NotificationChannel {
  type: 'email' | 'discord_dm' | 'webhook'
  enabled: boolean
  config?: Record<string, string>
}

export interface TapReport {
  tapId: string
  reason: string
  description: string
}

export interface TapVerificationRequest {
  tapId: string
  reason: string
  evidence?: string
}

export type VerificationStatus = 'pending' | 'approved' | 'rejected'

export interface VerificationRequestFull {
  id: string
  tapId: string
  tap: TapWithAccess
  reason: string
  evidence?: string
  status: VerificationStatus
  requestedAt: string
  reviewedAt?: string
  reviewedBy?: string
  rejectionReason?: string
}
