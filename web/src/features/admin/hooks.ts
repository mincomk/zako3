import { useQuery } from '@tanstack/react-query'
import { adminApi } from './api'
import type { PaginationParams } from '@/types'

export const adminKeys = {
  all: ['admin'] as const,
  activity: (params: Partial<PaginationParams>) =>
    [...adminKeys.all, 'activity', params] as const,
  pendingVerifications: () =>
    [...adminKeys.all, 'pending-verifications'] as const,
}

export const useAdminActivity = (params: Partial<PaginationParams> = {}) => {
  return useQuery({
    queryKey: adminKeys.activity(params),
    queryFn: () => adminApi.getActivity(params),
  })
}

export const usePendingVerifications = () => {
  return useQuery({
    queryKey: adminKeys.pendingVerifications(),
    queryFn: () => adminApi.getPendingVerifications(),
  })
}
