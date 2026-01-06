use zako3_audio_engine_types::{
    AudioRequestString, GuildId, QueueName, TapName, TrackId, Volume, ZakoResult,
};

pub trait AudioEngineInterface: Send + Sync + 'static {
    async fn play(
        &self,
        guild_id: GuildId,
        queue_name: QueueName,
        tap_name: TapName,
        request: AudioRequestString,
        volume: Volume,
    ) -> ZakoResult<TrackId>;
}
