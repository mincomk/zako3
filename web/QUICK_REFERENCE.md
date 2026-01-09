# ZAKO Dashboard - Quick Implementation Reference

## ✅ What's Been Completed

### 1. Translations

- Added all occupation labels (official, verified, base)
- Added all permission labels (owner_only, public, whitelisted, blacklisted)
- Added tap form labels and validation messages
- Added tap settings/stats page translations
- Added user settings page translations
- Added admin notifications translations
- Both EN and KO languages complete

### 2. Core User Pages

- **Create Tap** (`/taps/create`) - Full form with React Hook Form + Zod validation
- **User Settings** (`/settings`) - Profile, appearance, notifications, account
- **Tap Settings** (`/taps/:tapId/settings`) - Edit form with delete, uses `useUpdateTap`
- **Tap Statistics** (`/taps/:tapId/stats`) - Charts, stats, audit log

### 3. Components

- `TimeSeriesChart` - Recharts wrapper for use rate and cache hit rate graphs
- All existing components working (TapCard, TapList, StatsCard, etc.)

### 4. Infrastructure

- Routes updated and working
- Settings link fixed in sidebar
- All page exports configured
- MSW mocks working for implemented features

## 🔨 What Needs Implementation

### Priority 1: Admin Pages (Required for Admin Workflow)

1. **Admin Notifications Page** - Table with filters, mark as read, delete
2. **Admin User Detail** - Full user profile view with ban/role management
3. **Admin Tap Detail** - Full tap view with admin override controls

### Priority 2: Enhancements

4. **Notification Panel** - Dropdown from bell icon showing recent notifications
5. **Enhanced Admin Dashboard** - Quick actions, recent activity, pending approvals

### Priority 3: Polish (Optional)

6. **Badge Components** - PermissionBadge, OccupationBadge for visual consistency
7. **MSW Mock Updates** - Audit logs, admin activity, verification status

## 📁 File Structure Reference

```
src/
├── pages/
│   ├── admin/
│   │   ├── dashboard.tsx ✅ (needs enhancement)
│   │   ├── users.tsx ✅
│   │   ├── taps.tsx ✅
│   │   ├── user-detail.tsx ❌ NEEDED
│   │   ├── tap-detail.tsx ❌ NEEDED
│   │   └── notifications.tsx ❌ NEEDED
│   ├── taps/
│   │   ├── explore.tsx ✅
│   │   ├── my-taps.tsx ✅
│   │   ├── create.tsx ✅ NEW
│   │   ├── settings.tsx ✅ NEW
│   │   └── stats.tsx ✅ NEW
│   ├── settings/
│   │   └── index.tsx ✅ NEW
│   └── dashboard/
│       └── index.tsx ✅
├── components/
│   ├── common/
│   │   └── time-series-chart.tsx ✅ NEW
│   ├── dashboard/
│   │   └── notification-bell.tsx ✅ (needs panel)
│   └── tap/
│       ├── tap-card.tsx ✅
│       ├── permission-badge.tsx ❌ OPTIONAL
│       └── occupation-badge.tsx ❌ OPTIONAL
├── features/
│   ├── auth/ ✅
│   ├── taps/ ✅
│   ├── users/ ✅
│   ├── notifications/ ✅
│   └── admin/ ❌ NEW NEEDED
├── i18n/
│   └── locales/
│       ├── en.json ✅ UPDATED
│       └── ko.json ✅ UPDATED
└── types/
    └── admin.ts ❌ NEW NEEDED
```

## 🔑 Key Patterns to Follow

### 1. Page Structure

```typescript
export const PageName = () => {
  const { t } = useTranslation()
  const { param } = useParams()

  // Data fetching
  const { data, isLoading } = useHook(param)

  // Loading state
  if (isLoading) return <Skeleton />

  // Error/empty state
  if (!data) return <EmptyState />

  // Main content
  return <div className="space-y-6">...</div>
}
```

### 2. Form Handling

```typescript
const form = useForm({
  resolver: zodResolver(schema),
  defaultValues: { ... },
})

const onSubmit = async (data) => {
  try {
    await mutateAsync(data)
    toast.success(t('success'))
    navigate(...)
  } catch (error) {
    toast.error(...)
  }
}
```

### 3. Data Fetching

```typescript
// Use existing hooks from features/*/hooks.ts
const { data } = useTap(tapId)
const { mutateAsync } = useUpdateTap(tapId)
const { data } = useTaps({ filters... })
```

## 🎨 UI Patterns

### Stats Cards

```typescript
<StatsCard
  title={t('...')}
  value={number.toLocaleString()}
  icon={<Icon className="h-4 w-4" />}
  description={t('...')} // optional
/>
```

### Charts

```typescript
<TimeSeriesChart
  title={t('...')}
  data={stats.history}
  valueFormatter={(v) => v.toLocaleString()}
/>
```

### Tables

```typescript
<Table>
  <TableHeader>
    <TableRow>
      <TableHead>Column</TableHead>
    </TableRow>
  </TableHeader>
  <TableBody>
    {items.map(item => (
      <TableRow key={item.id}>
        <TableCell>{item.value}</TableCell>
      </TableRow>
    ))}
  </TableBody>
</Table>
```

### Confirm Dialogs

```typescript
<ConfirmDialog
  open={open}
  onOpenChange={setOpen}
  title={t('...')}
  description={t('...')}
  confirmLabel={t('...')}
  onConfirm={handleConfirm}
  isLoading={isPending}
  variant="destructive" // for delete actions
/>
```

## 🚀 Quick Implementation Steps

### For Admin Notifications Page:

1. Create `/src/pages/admin/notifications.tsx`
2. Use `useAdminNotifications` hook (already exists)
3. Add filters (FilterDropdown from common)
4. Add table with Badge for level colors
5. Add mark as read and delete actions
6. Add pagination (DataPagination from common)
7. Export from `/src/pages/admin/index.ts`
8. Add route in router.tsx
9. Test with MSW mocks

### For Admin User/Tap Detail:

1. Create page file
2. Get :id from useParams
3. Fetch data with existing hooks
4. Show info in cards
5. Add action buttons
6. Add confirmation dialogs
7. Follow same export/route pattern

### For Notification Panel:

1. Update `/src/components/dashboard/notification-bell.tsx`
2. Use Sheet from ShadCN
3. Fetch notifications with `useNotifications({ perPage: 5 })`
4. Show list with mark as read on click
5. Add "View All" link to notifications page

## 📋 Checklist for Each Feature

- [ ] Create component/page file
- [ ] Add necessary imports
- [ ] Implement data fetching
- [ ] Handle loading state
- [ ] Handle error/empty state
- [ ] Implement main UI
- [ ] Add actions (mutations)
- [ ] Add confirmation dialogs
- [ ] Test with existing MSW mocks
- [ ] Export from index.ts
- [ ] Add to router if needed
- [ ] Verify translations exist
- [ ] Test responsive design

## 🔗 Useful Links

- ShadCN Components: Already installed in `src/components/ui/`
- Existing Hooks: Check `src/features/*/hooks.ts`
- Translation Keys: `src/i18n/locales/en.json`
- Routes: `src/lib/constants.ts`
- Types: `src/types/`

## 💡 Tips

1. **Reuse existing components** - TapCard, StatsCard, ConfirmDialog, etc.
2. **Follow existing patterns** - Check similar pages for reference
3. **Use TypeScript** - Leverage existing types, don't use `any`
4. **Mobile-first** - Use responsive grid classes (md:, lg:)
5. **Accessibility** - Use semantic HTML, proper labels
6. **Translations** - All text should use `t()` function
7. **Error handling** - Always use try/catch with toast
8. **Loading states** - Show skeletons while loading
9. **Empty states** - Handle no data gracefully
10. **Type safety** - Use zod schemas for forms

---

Ready to implement! Start with Admin Notifications page as it's the most straightforward.
