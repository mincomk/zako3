import {
  useQuery,
  useMutation,
  useQueryClient,
} from '@tanstack/react-query'
import { tapsApi } from './api'
import type { TapFilters, TapSort, PaginationParams } from '@/types'
import type { CreateTapInput, UpdateTapInput, ReportTapInput, VerificationRequestInput } from './schemas'

export const tapKeys = {
  all: ['taps'] as const,
  lists: () => [...tapKeys.all, 'list'] as const,
  list: (filters: UseTapsParams) => [...tapKeys.lists(), filters] as const,
  details: () => [...tapKeys.all, 'detail'] as const,
  detail: (id: string) => [...tapKeys.details(), id] as const,
  stats: (id: string) => [...tapKeys.detail(id), 'stats'] as const,
  auditLog: (id: string) => [...tapKeys.detail(id), 'audit-log'] as const,
  myTaps: () => [...tapKeys.all, 'my-taps'] as const,
}

interface UseTapsParams extends Partial<PaginationParams>, Partial<TapFilters> {
  sortField?: TapSort['field']
  sortDirection?: TapSort['direction']
}

export const useTaps = (params: UseTapsParams = {}) => {
  return useQuery({
    queryKey: tapKeys.list(params),
    queryFn: () => tapsApi.getTaps(params),
  })
}

export const useTap = (tapId: string | undefined) => {
  return useQuery({
    queryKey: tapKeys.detail(tapId!),
    queryFn: () => tapsApi.getTap(tapId!),
    enabled: !!tapId,
  })
}

export const useTapStats = (tapId: string | undefined) => {
  return useQuery({
    queryKey: tapKeys.stats(tapId!),
    queryFn: () => tapsApi.getTapStats(tapId!),
    enabled: !!tapId,
  })
}

export const useTapAuditLog = (tapId: string | undefined, params: Partial<PaginationParams> = {}) => {
  return useQuery({
    queryKey: [...tapKeys.auditLog(tapId!), params],
    queryFn: () => tapsApi.getTapAuditLog(tapId!, params),
    enabled: !!tapId,
  })
}

export const useMyTaps = (params: Partial<PaginationParams> = {}) => {
  return useQuery({
    queryKey: [...tapKeys.myTaps(), params],
    queryFn: () => tapsApi.getMyTaps(params),
  })
}

export const useCreateTap = () => {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: (data: CreateTapInput) => tapsApi.createTap(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: tapKeys.lists() })
      queryClient.invalidateQueries({ queryKey: tapKeys.myTaps() })
    },
  })
}

export const useUpdateTap = (tapId: string) => {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: (data: UpdateTapInput) => tapsApi.updateTap(tapId, data),
    onSuccess: (updatedTap) => {
      queryClient.setQueryData(tapKeys.detail(tapId), updatedTap)
      queryClient.invalidateQueries({ queryKey: tapKeys.lists() })
      queryClient.invalidateQueries({ queryKey: tapKeys.myTaps() })
    },
  })
}

export const useDeleteTap = () => {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: (tapId: string) => tapsApi.deleteTap(tapId),
    onSuccess: (_, tapId) => {
      queryClient.removeQueries({ queryKey: tapKeys.detail(tapId) })
      queryClient.invalidateQueries({ queryKey: tapKeys.lists() })
      queryClient.invalidateQueries({ queryKey: tapKeys.myTaps() })
    },
  })
}

export const useReportTap = () => {
  return useMutation({
    mutationFn: ({ tapId, data }: { tapId: string; data: ReportTapInput }) =>
      tapsApi.reportTap(tapId, data),
  })
}

export const useRequestVerification = () => {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: ({ tapId, data }: { tapId: string; data: VerificationRequestInput }) =>
      tapsApi.requestVerification(tapId, data),
    onSuccess: (_, { tapId }) => {
      queryClient.invalidateQueries({ queryKey: tapKeys.detail(tapId) })
    },
  })
}
