import { http, HttpResponse, delay } from 'msw'
import { faker } from '@faker-js/faker'
import type { AdminActivity, PaginatedResponse } from '@/types'

const API_BASE = '/api'

// Mock admin activity data
const generateMockActivity = (): AdminActivity => ({
  id: faker.string.uuid(),
  adminId: faker.string.uuid(),
  adminUsername: faker.internet.email(),
  action: faker.helpers.arrayElement([
    'ban_user',
    'unban_user',
    'delete_tap',
    'approve_tap',
    'reject_verification',
  ]),
  targetType: faker.helpers.arrayElement([
    'user',
    'tap',
    'notification',
    'system',
  ]),
  targetId: faker.string.uuid(),
  targetName: faker.helpers.arrayElement([
    'user_' + faker.person.firstName(),
    'tap_' + faker.lorem.word(),
    'system_config',
  ]),
  timestamp: faker.date.recent({ days: 7 }).toISOString(),
  details: faker.lorem.sentence(),
})

const mockActivity = Array.from({ length: 50 }, generateMockActivity).sort(
  (a, b) => new Date(b.timestamp).getTime() - new Date(a.timestamp).getTime()
)

export const adminHandlers = [
  // Get admin activity log
  http.get(`${API_BASE}/admin/activity`, async ({ request }) => {
    await delay(200)
    const url = new URL(request.url)
    const page = parseInt(url.searchParams.get('page') || '1')
    const perPage = parseInt(url.searchParams.get('perPage') || '20')

    const total = mockActivity.length
    const totalPages = Math.ceil(total / perPage)
    const start = (page - 1) * perPage
    const end = start + perPage
    const data = mockActivity.slice(start, end)

    const result: PaginatedResponse<AdminActivity> = {
      data,
      meta: {
        total,
        page,
        perPage,
        totalPages,
      },
    }

    return HttpResponse.json(result)
  }),

  // Get pending verification requests
  http.get(`${API_BASE}/admin/taps/pending-verification`, async () => {
    await delay(200)
    // Return empty array for now - can be populated with mock pending taps if needed
    return HttpResponse.json([])
  }),
]
