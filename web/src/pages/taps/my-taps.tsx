import { useState } from 'react'
import { useTranslation } from 'react-i18next'
import { useNavigate, Link } from 'react-router-dom'
import { Plus, Compass } from 'lucide-react'
import { toast } from 'sonner'
import { useMyTaps, useDeleteTap } from '@/features/taps'
import { ConfirmDialog, DataPagination } from '@/components/common'
import { Button } from '@/components/ui/button'
import { Card, CardContent, CardFooter, CardHeader } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { usePagination } from '@/hooks'
import { formatRelativeTime } from '@/lib/date'
import { ROUTES } from '@/lib/constants'
import type { Tap } from '@/types'

export const MyTapsPage = () => {
  const { t, i18n } = useTranslation()
  const navigate = useNavigate()
  const { pagination, setPage, setPerPage, getPaginationInfo } = usePagination()

  // Delete confirmation state
  const [deleteDialogOpen, setDeleteDialogOpen] = useState(false)
  const [tapToDelete, setTapToDelete] = useState<Tap | null>(null)

  const { data, isLoading } = useMyTaps({ page: pagination.page, perPage: pagination.perPage })
  const { mutateAsync: deleteTap, isPending: isDeleting } = useDeleteTap()

  const taps = data?.data ?? []
  const paginationInfo = getPaginationInfo(data?.meta)

  const handleTapClick = (tapId: string) => {
    navigate(ROUTES.TAP_SETTINGS(tapId))
  }

  

  const handleDelete = async () => {
    if (!tapToDelete) return
    await deleteTap(tapToDelete.id)
    toast.success(t('taps.deleteSuccess'))
    setDeleteDialogOpen(false)
    setTapToDelete(null)
  }

  if (isLoading) {
    return (
      <div className="space-y-6">
        <div className="flex items-center justify-between">
          <div>
            <h1 className="text-2xl font-semibold">{t('taps.myTaps')}</h1>
            <p className="text-muted-foreground">{t('taps.myTapsDescription')}</p>
          </div>
        </div>
        <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-3">
          {Array.from({ length: 6 }).map((_, i) => (
            <Card key={i} className="animate-pulse">
              <CardHeader className="space-y-2">
                <div className="h-5 w-32 rounded bg-muted" />
                <div className="h-4 w-24 rounded bg-muted" />
              </CardHeader>
              <CardContent>
                <div className="h-4 w-full rounded bg-muted" />
              </CardContent>
            </Card>
          ))}
        </div>
      </div>
    )
  }

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-2xl font-semibold">{t('taps.myTaps')}</h1>
          <p className="text-muted-foreground">{t('taps.myTapsDescription')}</p>
        </div>
        <Button asChild>
          <Link to={ROUTES.TAPS_CREATE}>
            <Plus className="mr-2 h-4 w-4" />
            {t('taps.create')}
          </Link>
        </Button>
      </div>

      {taps.length === 0 ? (
        <div className="flex flex-col items-center justify-center rounded-lg border border-dashed p-12 text-center">
          <Compass className="h-12 w-12 text-muted-foreground mb-4" />
          <h3 className="text-lg font-semibold">{t('taps.noTapsOwned')}</h3>
          <p className="text-muted-foreground mb-4">{t('taps.createFirstDescription')}</p>
          <Button asChild>
            <Link to={ROUTES.TAPS_CREATE}>
              <Plus className="mr-2 h-4 w-4" />
              {t('taps.createFirst')}
            </Link>
          </Button>
        </div>
      ) : (
        <>
          <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-3">
            {taps.map((tap) => (
              <Card
                key={tap.id}
                className="cursor-pointer transition-all hover:border-primary/50 hover:shadow-lg hover:-translate-y-1"
                onClick={() => handleTapClick(tap.id)}
              >
                <CardHeader className="pb-2">
                  <div className="flex items-center justify-between">
                    <h3 className="font-semibold truncate">{tap.name}</h3>
                    <Badge variant="secondary" className="shrink-0">
                      {t(`taps.occupations.${tap.occupation}`)}
                    </Badge>
                  </div>
                  <p className="text-xs text-muted-foreground font-mono">{tap.id}</p>
                </CardHeader>
                <CardContent className="pb-2">
                  <p className="text-sm text-muted-foreground line-clamp-2">
                    {tap.description || t('taps.noDescription')}
                  </p>
                  <div className="mt-2 flex flex-wrap gap-1">
                    {tap.roles.map((role) => (
                      <Badge key={role} variant="outline" className="text-xs">
                        {t(`taps.roleLabels.${role}`)}
                      </Badge>
                    ))}
                  </div>
                </CardContent>
                <CardFooter className="flex items-center justify-between border-t pt-3 text-xs text-muted-foreground">
                  <span>{tap.totalUses.toLocaleString()} uses</span>
                  <span>{formatRelativeTime(tap.createdAt, i18n.language)}</span>
                </CardFooter>
              </Card>
            ))}
          </div>

          {data?.meta && paginationInfo.totalPages > 1 && (
            <DataPagination
              meta={data.meta}
              onPageChange={setPage}
              onPerPageChange={setPerPage}
            />
          )}
        </>
      )}

      <ConfirmDialog
        open={deleteDialogOpen}
        onOpenChange={setDeleteDialogOpen}
        title={t('taps.deleteConfirmTitle')}
        description={t('taps.deleteConfirmDescription', { name: tapToDelete?.name })}
        confirmLabel={t('common.delete')}
        onConfirm={handleDelete}
        isLoading={isDeleting}
        variant="destructive"
      />
    </div>
  )
}
