export interface PaginationParams {
  page: number
  perPage: number
}

export interface PaginationMeta {
  total: number
  page: number
  perPage: number
  totalPages: number
}

export interface PaginatedResponse<T> {
  data: T[]
  meta: PaginationMeta
}

export type SortDirection = 'asc' | 'desc'

export interface SortParams<T extends string = string> {
  field: T
  direction: SortDirection
}

export interface ApiError {
  code: string
  message: string
  details?: Record<string, unknown>
}

export interface ApiResponse<T> {
  data: T
  error?: ApiError
}
