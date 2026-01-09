export interface User {
  id: string
  discordId: string
  username: string
  avatar: string
  email?: string
  isAdmin: boolean
  isBanned: boolean
  banReason?: string
  banExpiresAt?: string
  createdAt: string
  updatedAt: string
}

export interface UserWithActivity extends User {
  lastActiveAt: string
  tapCount: number
  totalTapUses: number
}

export interface UserFilters {
  search?: string
  isBanned?: boolean
  isAdmin?: boolean
}

export interface UserSort {
  field: 'username' | 'createdAt' | 'lastActiveAt' | 'tapCount'
  direction: 'asc' | 'desc'
}

export interface BanUserInput {
  userId: string
  reason: string
  expiresAt?: string
}

export interface UpdateUserRoleInput {
  userId: string
  isAdmin: boolean
}
