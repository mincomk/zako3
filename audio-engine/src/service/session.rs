use crate::{
    error::ZakoResult,
    types::{AudioRequestString, AudioStopFilter, ChannelId, TapName, TrackId, Volume},
};

pub trait SessionService: Send + Sync + 'static {
    async fn join(&self, channel_id: ChannelId) -> ZakoResult<()>;
    async fn leave(&self) -> ZakoResult<()>;

    async fn play(
        &self,
        tap_name: TapName,
        request: AudioRequestString,
        volume: Volume,
    ) -> ZakoResult<TrackId>;
    async fn set_volume(&self, track_id: TrackId, volume: Volume) -> ZakoResult<()>;

    async fn stop(&self, track_id: TrackId) -> ZakoResult<()>;
    async fn stop_many(&self, filter: AudioStopFilter) -> ZakoResult<()>;

    async fn next_music(&self) -> ZakoResult<()>;

    async fn set_paused(&self, paused: bool) -> ZakoResult<()>;
}
