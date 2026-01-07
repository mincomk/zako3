use mockall::automock;

use crate::{
    error::ZakoResult,
    types::{AudioMetaResponse, AudioRequest, AudioResponse, CachedAudioRequest},
};

#[automock]
pub trait TapHubService: Send + Sync + 'static {
    async fn request_audio(&self, request: CachedAudioRequest) -> ZakoResult<AudioResponse>;
    async fn preload_audio(&self, request: CachedAudioRequest) -> ZakoResult<()>;
    async fn request_audio_meta(&self, request: AudioRequest) -> ZakoResult<AudioMetaResponse>;
}
