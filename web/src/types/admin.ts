export interface AdminActivity {
  id: string
  adminId: string
  adminUsername: string
  action: string
  targetType: 'user' | 'tap' | 'notification' | 'system'
  targetId: string
  targetName: string
  timestamp: string
  details?: string
}
