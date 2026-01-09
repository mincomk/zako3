import { z } from 'zod'
import {
  TAP_ID_REGEX,
  TAP_ID_MIN_LENGTH,
  TAP_ID_MAX_LENGTH,
  TAP_NAME_MAX_LENGTH,
  TAP_DESCRIPTION_MAX_LENGTH,
  TAP_ROLES,
} from '@/lib/constants'

const permissionConfigSchema = z.discriminatedUnion('type', [
  z.object({ type: z.literal('owner_only') }),
  z.object({ type: z.literal('public') }),
  z.object({
    type: z.literal('whitelisted'),
    userIds: z.array(z.string()),
  }),
  z.object({
    type: z.literal('blacklisted'),
    userIds: z.array(z.string()),
  }),
])

export const tapIdSchema = z
  .string()
  .min(
    TAP_ID_MIN_LENGTH,
    `Tap ID must be at least ${TAP_ID_MIN_LENGTH} characters`
  )
  .max(
    TAP_ID_MAX_LENGTH,
    `Tap ID must be at most ${TAP_ID_MAX_LENGTH} characters`
  )
  .regex(
    TAP_ID_REGEX,
    'Tap ID can only contain lowercase letters, numbers, underscores, and periods'
  )

export const createTapSchema = z.object({
  id: tapIdSchema,
  name: z
    .string()
    .min(1, 'Tap name is required')
    .max(
      TAP_NAME_MAX_LENGTH,
      `Tap name must be at most ${TAP_NAME_MAX_LENGTH} characters`
    ),
  description: z
    .string()
    .max(
      TAP_DESCRIPTION_MAX_LENGTH,
      `Description must be at most ${TAP_DESCRIPTION_MAX_LENGTH} characters`
    )
    .default(''),
  roles: z.array(z.enum(TAP_ROLES)).min(1, 'At least one role is required'),
  permission: permissionConfigSchema.default({ type: 'owner_only' }),
})

export const updateTapSchema = z.object({
  id: tapIdSchema.optional(),
  name: z
    .string()
    .min(1, 'Tap name is required')
    .max(
      TAP_NAME_MAX_LENGTH,
      `Tap name must be at most ${TAP_NAME_MAX_LENGTH} characters`
    )
    .optional(),
  description: z
    .string()
    .max(
      TAP_DESCRIPTION_MAX_LENGTH,
      `Description must be at most ${TAP_DESCRIPTION_MAX_LENGTH} characters`
    )
    .optional(),
  roles: z
    .array(z.enum(TAP_ROLES))
    .min(1, 'At least one role is required')
    .optional(),
  permission: permissionConfigSchema.optional(),
})

export const reportTapSchema = z.object({
  reason: z.enum(['inappropriate', 'spam', 'copyright', 'other']),
  description: z.string().min(10, 'Description must be at least 10 characters'),
})

export const verificationRequestSchema = z.object({
  reason: z
    .string()
    .min(20, 'Please provide a detailed reason (at least 20 characters)'),
  evidence: z.string().optional(),
})

export const createTapApiTokenSchema = z.object({
  label: z
    .string()
    .min(1, 'Token label is required')
    .max(64, 'Token label must be at most 64 characters'),
  expiry: z.enum(['1_month', '3_months', '6_months', '1_year', 'never']),
})

export const updateTapApiTokenSchema = z.object({
  label: z
    .string()
    .min(1, 'Token label is required')
    .max(64, 'Token label must be at most 64 characters'),
})

export type CreateTapInput = z.infer<typeof createTapSchema>
export type UpdateTapInput = z.infer<typeof updateTapSchema>
export type ReportTapInput = z.infer<typeof reportTapSchema>
export type VerificationRequestInput = z.infer<typeof verificationRequestSchema>
export type CreateTapApiTokenInput = z.infer<typeof createTapApiTokenSchema>
export type UpdateTapApiTokenInput = z.infer<typeof updateTapApiTokenSchema>
