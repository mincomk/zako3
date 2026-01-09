# ZAKO Dashboard - Implementation Guide for Remaining Features

## Progress Summary

### ✅ Completed (Fully Implemented)

1. **Translations** - All missing translations added for EN and KO
2. **Routes** - Updated constants and fixed settings link
3. **Create Tap Page** - Full form implementation with validation
4. **User Settings Page** - Profile, appearance, account settings
5. **Tap Settings Page** - Edit form with delete functionality
6. **Tap Statistics Page** - Charts, stats cards, audit log table
7. **Chart Components** - TimeSeriesChart wrapper for recharts

### 🔨 Remaining Implementation Tasks

---

## 1. Admin User Detail Page

**File:** `src/pages/admin/user-detail.tsx`

**Route:** `/admin/users/:userId` (already defined in constants)

**Purpose:** Detailed view of a specific user for admin management

**Implementation Requirements:**

### Data Fetching

```typescript
const { userId } = useParams<{ userId: string }>()
const { data: user } = useUser(userId!) // Already exists in features/users/hooks.ts
const { data: tapsData } = useTaps({ ownerId: userId }) // Filter user's taps
```

### UI Components Needed

- Back button to `/admin/users`
- User profile card with:
  - Avatar, username, email, ID
  - Role badge (Admin/User)
  - Status badge (Active/Banned)
  - Last active timestamp
  - Account creation date
- Action buttons:
  - Ban/Unban user (with confirmation dialog)
  - Make Admin/Remove Admin (with confirmation)
- Owned taps section:
  - Grid of TapCard components
  - Show user's created taps
  - Link to tap detail pages
- Activity log section (optional):
  - Recent user actions
  - Login history

### Key Features

- All user management actions from the users list page
- Visual overview of user's contributions (taps)
- Easy navigation back to users list
- Real-time status updates after actions

### Translation Keys Needed

Already exist in `admin.users.*`

---

## 2. Admin Tap Detail Page

**File:** `src/pages/admin/tap-detail.tsx`

**Route:** `/admin/taps/:tapId` (already defined)

**Purpose:** Admin view and management of specific tap

### Data Fetching

```typescript
const { tapId } = useParams<{ tapId: string }>()
const { data: tap } = useTap(tapId!)
const { data: stats } = useTapStats(tapId!)
const { data: owner } = useUserPublic(tap?.ownerId) // Get owner info
```

### UI Components

- Back button to `/admin/taps`
- Tap information card:
  - Name, ID, description
  - Owner (with link to owner detail)
  - Occupation badge
  - Roles, permissions
  - Creation date, last updated
- Statistics overview:
  - Reuse stats cards from TapStatsPage
  - Total uses, active users, cache metrics
- Admin actions:
  - Edit tap settings (admin override)
  - Delete tap (with confirmation)
  - Approve/Reject verification requests
  - View reports about this tap
- Statistics charts:
  - Reuse TimeSeriesChart components
  - Use rate and cache hit rate

### Verification Approval

If tap has pending verification:

- Show verification request details
- Approve/Reject buttons
- Reason field for rejection

### Translation Keys Needed

Add to `admin.taps.*`:

```json
"approveVerification": "Approve Verification",
"rejectVerification": "Reject Verification",
"tapDetails": "Tap Details",
"verificationPending": "Pending Verification"
```

✅ Already added!

---

## 3. Admin Notifications Page

**File:** `src/pages/admin/notifications.tsx`

**Route:** `/admin/notifications` (already defined)

**Purpose:** System-wide notification management for admins

### Data Fetching

```typescript
const { data, isLoading } = useAdminNotifications({
  page: pagination.page,
  perPage: pagination.perPage,
  level: levelFilter, // 'info' | 'success' | 'warning' | 'error'
  category: categoryFilter,
  isRead: readFilter, // undefined | true | false
})
```

### UI Components

- Filter toolbar:
  - FilterDropdown for level (info, success, warning, error)
  - FilterDropdown for category
  - FilterDropdown for read status (all, read, unread)
  - SearchInput for text search
- Action bar:
  - "Mark All as Read" button
  - Bulk delete (optional)
- Notifications table:
  - Level badge (color-coded)
  - Category
  - Message
  - Timestamp
  - Read/Unread status indicator
  - Actions: Mark as read, Delete
- Pagination

### Level Colors

```typescript
const levelColors = {
  info: 'default',
  success: 'success',
  warning: 'warning',
  error: 'destructive',
}
```

### Features

- Sort by timestamp (newest first)
- Filter by level, category, read status
- Mark individual as read
- Mark all as read
- Delete individual notifications
- Auto-refresh every 30 seconds (optional)

### Translation Keys

✅ Already added in `admin.notifications.*`

---

## 4. Enhanced Admin Dashboard

**File:** `src/pages/admin/dashboard.tsx` (update existing)

**Current State:** Shows basic stats, has placeholder sections

**Updates Needed:**

### Quick Actions Card

Replace placeholder with:

```typescript
<Card>
  <CardHeader>
    <CardTitle>{t('admin.quickActions')}</CardTitle>
  </CardHeader>
  <CardContent className="space-y-2">
    <Button asChild variant="outline" className="w-full justify-start">
      <Link to={ROUTES.ADMIN_NOTIFICATIONS}>
        <Bell className="mr-2 h-4 w-4" />
        View Notifications
      </Link>
    </Button>
    <Button asChild variant="outline" className="w-full justify-start">
      <Link to={ROUTES.ADMIN_TAPS}>
        <AlertTriangle className="mr-2 h-4 w-4" />
        Pending Verifications
      </Link>
    </Button>
    <Button asChild variant="outline" className="w-full justify-start">
      <Link to={import.meta.env.VITE_GRAFANA_URL || '#'} target="_blank">
        <ExternalLink className="mr-2 h-4 w-4" />
        Open Grafana
      </Link>
    </Button>
  </CardContent>
</Card>
```

### Recent Activity Card

Show recent admin actions:

```typescript
// Mock data or create new API endpoint
const recentActivity = [
  { user: 'admin@zako.com', action: 'Banned user', target: 'user123', timestamp: '...' },
  { user: 'admin@zako.com', action: 'Approved tap', target: 'my_tap', timestamp: '...' },
]

<Card>
  <CardHeader>
    <CardTitle>{t('admin.recentActivity')}</CardTitle>
  </CardHeader>
  <CardContent>
    <div className="space-y-3">
      {recentActivity.map((activity, i) => (
        <div key={i} className="flex items-center justify-between text-sm">
          <div>
            <span className="font-medium">{activity.action}</span>
            <span className="text-muted-foreground"> on {activity.target}</span>
          </div>
          <span className="text-xs text-muted-foreground">
            {formatRelativeTime(activity.timestamp, i18n.language)}
          </span>
        </div>
      ))}
    </div>
  </CardContent>
</Card>
```

### Pending Approvals Section

Add new section for verification requests:

```typescript
const { data: pendingTaps } = useTaps({
  // Need to add verification status filter to API
  verificationStatus: 'pending'
})

<Card>
  <CardHeader>
    <CardTitle>Pending Verification Requests</CardTitle>
  </CardHeader>
  <CardContent>
    {pendingTaps?.data.length === 0 ? (
      <p className="text-muted-foreground">No pending requests</p>
    ) : (
      <div className="space-y-2">
        {pendingTaps?.data.slice(0, 5).map((tap) => (
          <div key={tap.id} className="flex items-center justify-between">
            <Link to={ROUTES.ADMIN_TAP(tap.id)} className="hover:underline">
              {tap.name}
            </Link>
            <Button size="sm" variant="outline">Review</Button>
          </div>
        ))}
      </div>
    )}
  </CardContent>
</Card>
```

### Environment Variable

Add to `.env.example`:

```
VITE_GRAFANA_URL=https://grafana.example.com
```

---

## 5. Notification Panel Component

**File:** `src/components/dashboard/notification-panel.tsx`

**Purpose:** Dropdown panel from notification bell in header

**Current State:** Bell exists, shows count, but no panel on click

### Implementation

Use Sheet or DropdownMenu from ShadCN:

```typescript
export const NotificationBell = () => {
  const { data: unreadCount } = useUnreadCount()
  const { data: notifications } = useNotifications({ perPage: 5 })
  const { mutate: markAsRead } = useMarkAsRead()
  const [open, setOpen] = useState(false)

  return (
    <Sheet open={open} onOpenChange={setOpen}>
      <SheetTrigger asChild>
        <Button variant="ghost" size="icon-sm" className="relative">
          <Bell className="h-4 w-4" />
          {unreadCount && unreadCount > 0 && (
            <Badge className="absolute -top-1 -right-1 h-5 w-5 p-0 flex items-center justify-center">
              {unreadCount > 9 ? '9+' : unreadCount}
            </Badge>
          )}
        </Button>
      </SheetTrigger>
      <SheetContent side="right" className="w-80">
        <SheetHeader>
          <SheetTitle>{t('notifications.title')}</SheetTitle>
        </SheetHeader>
        <div className="mt-4 space-y-2">
          {notifications?.data.length === 0 ? (
            <p className="text-center py-8 text-muted-foreground">
              {t('notifications.noNotifications')}
            </p>
          ) : (
            notifications?.data.map((notif) => (
              <div
                key={notif.id}
                className={cn(
                  'p-3 rounded-lg border cursor-pointer hover:bg-accent',
                  !notif.isRead && 'bg-primary/5'
                )}
                onClick={() => markAsRead(notif.id)}
              >
                <div className="flex items-start justify-between gap-2">
                  <div className="flex-1">
                    <p className="text-sm font-medium">{notif.title}</p>
                    <p className="text-xs text-muted-foreground">{notif.message}</p>
                  </div>
                  {!notif.isRead && (
                    <div className="h-2 w-2 rounded-full bg-primary shrink-0 mt-1" />
                  )}
                </div>
                <p className="text-xs text-muted-foreground mt-1">
                  {formatRelativeTime(notif.createdAt, i18n.language)}
                </p>
              </div>
            ))
          )}
        </div>
        <SheetFooter className="mt-4">
          <Button asChild variant="outline" className="w-full">
            <Link to="/notifications">View All</Link>
          </Button>
        </SheetFooter>
      </SheetContent>
    </Sheet>
  )
}
```

**Update:** Replace existing NotificationBell component in `src/components/dashboard/notification-bell.tsx`

---

## 6. MSW Mock Updates

**Files to Update:**

### `src/mocks/handlers/taps.ts`

Add audit log mock generator:

```typescript
export const generateAuditLogs = (tapId: string, count: number = 20) => {
  const events = ['created', 'updated', 'used', 'cached', 'reported', 'verification_requested']

  return Array.from({ length: count }, (_, i) => ({
    id: faker.string.uuid(),
    tapId,
    event: faker.helpers.arrayElement(events),
    userId: Math.random() > 0.5 ? faker.string.uuid() : null,
    timestamp: faker.date.recent({ days: 30 }).toISOString(),
    details: faker.lorem.sentence(),
  }))
}

// Add handler for audit log endpoint
http.get('/api/taps/:tapId/audit-log', ({ params, request }) => {
  const { tapId } = params
  const url = new URL(request.url)
  const page = Number(url.searchParams.get('page')) || 1
  const perPage = Number(url.searchParams.get('perPage')) || 20

  const allLogs = generateAuditLogs(tapId as string, 50)
  const start = (page - 1) * perPage
  const end = start + perPage
  const logs = allLogs.slice(start, end)

  return HttpResponse.json({
    data: logs,
    meta: {
      page,
      perPage,
      total: allLogs.length,
      totalPages: Math.ceil(allLogs.length / perPage),
    },
  })
}),
```

### `src/mocks/handlers/users.ts`

Add user detail mock:

```typescript
http.get('/api/admin/users/:userId', ({ params }) => {
  const { userId } = params
  const user = mockUsers.find((u) => u.id === userId)

  if (!user) {
    return new HttpResponse(null, { status: 404 })
  }

  return HttpResponse.json(user)
}),
```

### Environment Variables

Add to `src/mocks/handlers/index.ts` or create new file for admin activity:

```typescript
// Mock admin activity log
export const mockAdminActivity = [
  {
    id: '1',
    adminId: 'admin-123',
    adminUsername: 'admin@zako.com',
    action: 'ban_user',
    targetType: 'user',
    targetId: 'user-456',
    targetName: 'problematic_user',
    timestamp: new Date(Date.now() - 1000 * 60 * 5).toISOString(),
    details: 'Spam behavior',
  },
  // ... more activities
]

http.get('/api/admin/activity', ({ request }) => {
  const url = new URL(request.url)
  const page = Number(url.searchParams.get('page')) || 1
  const perPage = Number(url.searchParams.get('perPage')) || 20

  // Paginate mockAdminActivity
  // Return paginated response
}),
```

---

## 7. Router Updates

**File:** `src/app/router.tsx`

Add admin detail routes:

```typescript
{/* Admin routes */}
<Route
  element={
    <AdminGuard>
      <AdminLayout />
    </AdminGuard>
  }
>
  <Route path={ROUTES.ADMIN} element={<AdminDashboardPage />} />
  <Route path={ROUTES.ADMIN_USERS} element={<AdminUsersPage />} />
  <Route path="/admin/users/:userId" element={<AdminUserDetailPage />} />
  <Route path={ROUTES.ADMIN_TAPS} element={<AdminTapsPage />} />
  <Route path="/admin/taps/:tapId" element={<AdminTapDetailPage />} />
  <Route path={ROUTES.ADMIN_NOTIFICATIONS} element={<AdminNotificationsPage />} />
</Route>
```

**Page Exports:** `src/pages/admin/index.ts`

```typescript
export { AdminDashboardPage } from './dashboard'
export { AdminUsersPage } from './users'
export { AdminUserDetailPage } from './user-detail'
export { AdminTapsPage } from './taps'
export { AdminTapDetailPage } from './tap-detail'
export { AdminNotificationsPage } from './notifications'
```

---

## 8. Additional Reusable Components

### Permission Badge Component

**File:** `src/components/tap/permission-badge.tsx`

```typescript
import { Badge } from '@/components/ui/badge'
import { Lock, Globe, Users, Ban } from 'lucide-react'
import { useTranslation } from 'react-i18next'
import type { TapPermission } from '@/types'

interface PermissionBadgeProps {
  permission: TapPermission
  hasAccess?: boolean
}

export const PermissionBadge = ({ permission, hasAccess }: PermissionBadgeProps) => {
  const { t } = useTranslation()

  const config = {
    owner_only: { icon: Lock, variant: 'secondary' as const },
    public: { icon: Globe, variant: 'default' as const },
    whitelisted: { icon: Users, variant: 'outline' as const },
    blacklisted: { icon: Ban, variant: 'destructive' as const },
  }

  const { icon: Icon, variant } = config[permission]

  return (
    <Badge variant={variant} className="gap-1">
      <Icon className="h-3 w-3" />
      {t(`taps.permissions.${permission}`)}
      {hasAccess !== undefined && (
        <span className="ml-1">
          {hasAccess ? '✓' : '✗'}
        </span>
      )}
    </Badge>
  )
}
```

### Occupation Badge Component

**File:** `src/components/tap/occupation-badge.tsx`

```typescript
import { Badge } from '@/components/ui/badge'
import { Crown, CheckCircle } from 'lucide-react'
import { useTranslation } from 'react-i18next'
import type { TapOccupation } from '@/types'

interface OccupationBadgeProps {
  occupation: TapOccupation
}

export const OccupationBadge = ({ occupation }: OccupationBadgeProps) => {
  const { t } = useTranslation()

  if (occupation === 'base') return null

  return (
    <Badge
      variant={occupation === 'official' ? 'default' : 'secondary'}
      className="gap-1"
    >
      {occupation === 'official' ? (
        <Crown className="h-3 w-3" />
      ) : (
        <CheckCircle className="h-3 w-3" />
      )}
      {t(`taps.occupations.${occupation}`)}
    </Badge>
  )
}
```

Update `src/components/tap/index.ts`:

```typescript
export { TapCard } from './tap-card'
export { TapList } from './tap-list'
export { TapFiltersComponent as TapFilters } from './tap-filters'
export { ReportModal } from './report-modal'
export { PermissionBadge } from './permission-badge'
export { OccupationBadge } from './occupation-badge'
```

---

## 9. API Additions Needed

### User API

`src/features/users/api.ts` - Already has `getUserPublic`

### Taps API

Add to `src/features/taps/api.ts`:

```typescript
getTapAuditLog: (tapId: string, params?: Partial<PaginationParams>) =>
  apiClient.get<PaginatedResponse<AuditLogEntry>>(`/taps/${tapId}/audit-log`, {
    params,
  })
```

### Admin Activity API (new)

**File:** `src/features/admin/api.ts`

```typescript
export const adminApi = {
  getActivity: (params?: Partial<PaginationParams>) =>
    apiClient.get<PaginatedResponse<AdminActivity>>('/admin/activity', {
      params,
    }),

  getPendingVerifications: () =>
    apiClient.get<Tap[]>('/admin/taps/pending-verification'),
}
```

**Hooks:** `src/features/admin/hooks.ts`

```typescript
export const useAdminActivity = (params = {}) => {
  return useQuery({
    queryKey: ['admin', 'activity', params],
    queryFn: () => adminApi.getActivity(params),
  })
}

export const usePendingVerifications = () => {
  return useQuery({
    queryKey: ['admin', 'pending-verifications'],
    queryFn: () => adminApi.getPendingVerifications(),
  })
}
```

---

## 10. Type Definitions

Add to `src/types/admin.ts` (new file):

```typescript
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

export interface AuditLogEntry {
  id: string
  tapId: string
  event: string
  userId: string | null
  timestamp: string
  details?: string
}
```

Export from `src/types/index.ts`:

```typescript
export * from './admin'
```

---

## Implementation Priority

### High Priority (Core Functionality)

1. ✅ Create Tap Page - **DONE**
2. ✅ User Settings Page - **DONE**
3. ✅ Tap Settings Page - **DONE**
4. ✅ Tap Statistics Page - **DONE**
5. Admin Notifications Page - **NEEDED**
6. Notification Panel Component - **NEEDED**

### Medium Priority (Admin Tools)

7. Admin User Detail Page - **NEEDED**
8. Admin Tap Detail Page - **NEEDED**
9. Enhanced Admin Dashboard - **NEEDED**

### Low Priority (Polish)

10. Permission Badge Component - **OPTIONAL**
11. Occupation Badge Component - **OPTIONAL**
12. MSW Mock Enhancements - **OPTIONAL**

---

## Testing Checklist

After implementation, test:

### User Features

- [ ] Create tap form validation works
- [ ] Create tap success redirects to settings
- [ ] Tap settings updates work
- [ ] Tap settings delete works
- [ ] Tap stats shows charts correctly
- [ ] User settings displays correctly
- [ ] Theme and language toggles work

### Admin Features

- [ ] Admin user detail shows user info
- [ ] Admin can ban/unban users
- [ ] Admin can grant/revoke admin role
- [ ] Admin tap detail shows tap info
- [ ] Admin can delete taps
- [ ] Admin notifications filter works
- [ ] Admin can mark notifications as read
- [ ] Admin dashboard quick actions work
- [ ] Grafana link opens correctly

### Navigation

- [ ] All routes work correctly
- [ ] Settings link in sidebar works
- [ ] Back buttons navigate correctly
- [ ] Breadcrumbs show on deep pages

### Responsive Design

- [ ] All pages work on mobile
- [ ] Tables scroll on mobile
- [ ] Forms work on mobile
- [ ] Charts resize properly

---

## Quick Start Commands

```bash
# Start dev server
pnpm dev

# Run type check
pnpm tsc --noEmit

# Run linter
pnpm lint

# Run tests
pnpm test

# Build for production
pnpm build
```

---

## Notes

- All hooks and API functions already exist in the codebase
- All translations have been added
- Chart library (recharts) is already installed
- MSW is set up and working
- Focus on clean, modular code
- Keep UI components pure (no data fetching)
- Use existing patterns from implemented pages
- Test each feature after implementation

---

## Files Created So Far

✅ `/src/pages/taps/create.tsx`
✅ `/src/pages/settings/index.tsx`
✅ `/src/pages/taps/settings.tsx`
✅ `/src/pages/taps/stats.tsx`
✅ `/src/components/common/time-series-chart.tsx`
✅ Updated translations (en.json, ko.json)
✅ Updated routes and exports
✅ Fixed settings sidebar link

## Files Still Needed

❌ `/src/pages/admin/user-detail.tsx`
❌ `/src/pages/admin/tap-detail.tsx`
❌ `/src/pages/admin/notifications.tsx`
❌ `/src/components/dashboard/notification-panel.tsx` (update existing notification-bell.tsx)
❌ `/src/components/tap/permission-badge.tsx`
❌ `/src/components/tap/occupation-badge.tsx`
❌ `/src/features/admin/api.ts`
❌ `/src/features/admin/hooks.ts`
❌ `/src/types/admin.ts`
❌ MSW mock updates

---

End of Implementation Guide
