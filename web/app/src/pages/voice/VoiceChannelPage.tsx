import { useParams } from 'react-router-dom'
import { useTranslation } from 'react-i18next'
import { SkipForward, Square, Pause, Play, RefreshCw } from 'lucide-react'
import { toast } from 'sonner'
import {
    usePlaybackState,
    useRefreshPlaybackState,
    useStopTrack,
    useSkipMusic,
    usePauseTrack,
    useResumeTrack,
    useEditQueue,
} from '@/features/playback'
import type { TrackDto, AudioMetadataDto } from '@/features/playback'
import { CopyableId } from '@/components/tap/copyable-id'
import { Avatar, AvatarFallback, AvatarImage } from '@/components/ui/avatar'
import { Button } from '@/components/ui/button'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Slider } from '@/components/ui/slider'
import { Skeleton } from '@/components/ui/skeleton'

function getNonUrlMetadata(metadata: AudioMetadataDto[]): AudioMetadataDto[] {
    return metadata.filter(
        (m) => !/url/i.test(m.type) && !m.value.startsWith('http')
    )
}

type QueueUser = { name: string; avatarUrl?: string | null }

interface TrackRowProps {
    track: TrackDto
    guildId: string
    channelId: string
}

const TrackRow = ({ track, guildId, channelId }: TrackRowProps) => {
    const { t } = useTranslation()
    const { mutate: editQueue } = useEditQueue()
    const { mutate: pauseTrack, isPending: isPausing } = usePauseTrack()
    const { mutate: resumeTrack, isPending: isResuming } = useResumeTrack()
    const { mutate: stopTrack, isPending: isStopping } = useStopTrack()

    const nonUrl = getNonUrlMetadata(track.metadata)
    const volumePct = Math.round(track.volume * 100)

    return (
        <div className="flex items-center gap-4 rounded-md border p-3">
            <div className="min-w-0 flex-1">
                <p className="truncate text-sm font-medium">
                    {nonUrl.length > 0
                        ? nonUrl.map((m) => m.value).join(' · ')
                        : track.audioRequestString}
                </p>
                <p className="text-muted-foreground flex items-center gap-1 text-xs">
                    {t('voice.tap')}: {track.tapId} · {t('voice.requestedBy')}:
                    <CopyableId id={track.requestedBy} />
                </p>
                <div className="mt-2 flex items-center gap-2">
                    <span className="text-muted-foreground text-xs w-10">
                        {t('voice.volume')}
                    </span>
                    <Slider
                        className="w-32"
                        min={0}
                        max={100}
                        defaultValue={[volumePct]}
                        onValueCommit={([vol]) =>
                            editQueue(
                                {
                                    guildId,
                                    channelId,
                                    operations: [
                                        {
                                            op: 'set_volume',
                                            trackId: track.trackId,
                                            volume: vol / 100,
                                        },
                                    ],
                                },
                                {
                                    onSuccess: () => toast.success('Volume updated'),
                                    onError: () => toast.error('Failed to update volume'),
                                }
                            )
                        }
                    />
                    <span className="text-muted-foreground text-xs w-8">
                        {volumePct}%
                    </span>
                </div>
            </div>
            <Button
                size="sm"
                variant="outline"
                disabled={isPausing || isResuming}
                onClick={() =>
                    track.paused
                        ? resumeTrack(
                            { guildId, channelId, trackId: track.trackId },
                            {
                                onSuccess: () => toast.success('Resumed'),
                                onError: () => toast.error('Failed to resume'),
                            }
                        )
                        : pauseTrack(
                            { guildId, channelId, trackId: track.trackId },
                            {
                                onSuccess: () => toast.success('Paused'),
                                onError: () => toast.error('Failed to pause'),
                            }
                        )
                }
            >
                {track.paused ? <Play className="h-4 w-4" /> : <Pause className="h-4 w-4" />}
            </Button>
            <Button
                size="sm"
                variant="outline"
                disabled={isStopping}
                onClick={() =>
                    stopTrack(
                        { guildId, channelId, trackId: track.trackId },
                        {
                            onSuccess: () => toast.success('Track stopped'),
                            onError: () => toast.error('Failed to stop track'),
                        }
                    )
                }
            >
                <Square className="h-4 w-4" />
            </Button>
        </div>
    )
}

interface QueueCardProps {
    queueName: string
    tracks: TrackDto[]
    queueUser?: QueueUser
    guildId: string
    channelId: string
}

const QueueCard = ({ queueName, tracks, queueUser, guildId, channelId }: QueueCardProps) => {
    const { t } = useTranslation()
    const { mutate: skipMusic, isPending: isSkipping } = useSkipMusic()

    return (
        <Card>
            <CardHeader className="flex flex-row items-center justify-between pb-3">
                <CardTitle className="text-base font-medium flex items-center gap-2">
                    {queueUser ? (
                        <>
                            <Avatar className="h-5 w-5">
                                <AvatarImage src={queueUser.avatarUrl ?? undefined} />
                                <AvatarFallback className="text-xs">{queueUser.name[0]}</AvatarFallback>
                            </Avatar>
                            {queueUser.name}
                        </>
                    ) : (
                        <span className="capitalize">{queueName}</span>
                    )}
                </CardTitle>
                {queueName === 'music' && (
                    <Button
                        size="sm"
                        variant="outline"
                        disabled={isSkipping}
                        onClick={() =>
                            skipMusic(
                                { guildId, channelId },
                                {
                                    onSuccess: () => toast.success('Skipped'),
                                    onError: () => toast.error('Failed to skip'),
                                }
                            )
                        }
                    >
                        <SkipForward className="mr-1.5 h-4 w-4" />
                        {t('voice.skip')}
                    </Button>
                )}
            </CardHeader>
            <CardContent className="space-y-3">
                {tracks.length === 0 ? (
                    <p className="text-muted-foreground text-sm">
                        {t('voice.noTracksInQueue')}
                    </p>
                ) : (
                    tracks.map((track) => (
                        <TrackRow
                            key={track.trackId}
                            track={track}
                            guildId={guildId}
                            channelId={channelId}
                        />
                    ))
                )}
            </CardContent>
        </Card>
    )
}

export const VoiceChannelPage = () => {
    const { t } = useTranslation()
    const { guildId, channelId } = useParams<{
        guildId: string
        channelId: string
    }>()

    const { data: states, isLoading: stateLoading, isRefetching } = usePlaybackState()
    const refreshState = useRefreshPlaybackState()

    const channelState = states?.find(
        (s) => s.guildId === guildId && s.channelId === channelId
    )

    if (!guildId || !channelId) return null

    const visibleQueues = channelState
        ? Object.entries(channelState.queues as Record<string, TrackDto[]>)
            .filter(([, tracks]) => tracks.length > 0)
            .sort(([nameA], [nameB]) => {
                if (nameA === 'music') return -1
                if (nameB === 'music') return 1
                return nameA.localeCompare(nameB)
            })
        : []

    return (
        <div className="space-y-6">
            <div>
                <h1 className="text-2xl font-semibold">
                    {channelState?.channelName || `...${channelId.slice(-6)}`}
                </h1>
                <p className="text-muted-foreground text-sm">
                    {channelState?.guildName || `Server ...${guildId.slice(-6)}`}
                </p>
            </div>

            {/* Queues */}
            <div className="space-y-4">
                <div className="flex items-center justify-between">
                    <h2 className="text-lg font-medium">{t('voice.queues')}</h2>
                    <Button
                        size="sm"
                        variant="ghost"
                        onClick={refreshState}
                        disabled={isRefetching}
                    >
                        <RefreshCw className={`h-4 w-4${isRefetching ? ' animate-spin' : ''}`} />
                    </Button>
                </div>
                {stateLoading ? (
                    <div className="space-y-3">
                        <Skeleton className="h-24 w-full" />
                        <Skeleton className="h-24 w-full" />
                    </div>
                ) : !channelState ? (
                    <div className="flex items-center justify-center py-12">
                        <p className="text-muted-foreground text-sm">
                            {t('voice.noActiveSession')}
                        </p>
                    </div>
                ) : visibleQueues.length === 0 ? (
                    <div className="flex items-center justify-center py-12">
                        <p className="text-muted-foreground text-sm">
                            {t('voice.allQueuesEmpty')}
                        </p>
                    </div>
                ) : (
                    visibleQueues.map(([queueName, tracks]) => (
                        <QueueCard
                            key={queueName}
                            queueName={queueName}
                            tracks={tracks}
                            queueUser={channelState.queueMeta[queueName]?.user}
                            guildId={guildId}
                            channelId={channelId}
                        />
                    ))
                )}
            </div>
        </div>
    )
}
