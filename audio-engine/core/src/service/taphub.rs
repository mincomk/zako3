use std::io::Cursor;
use std::sync::Arc;

use crate::{
    error::ZakoResult,
    types::{
        AudioMetaResponse, AudioRequest, AudioResponse, CachedAudioRequest, StreamCacheKey,
        TrackDescription,
    },
};
use async_trait::async_trait;
use mockall::automock;

pub type ArcTapHubService = Arc<dyn TapHubService>;

#[automock]
#[async_trait]
pub trait TapHubService: Send + Sync + 'static {
    async fn request_audio(&self, request: CachedAudioRequest) -> ZakoResult<AudioResponse>;
    async fn preload_audio(&self, request: CachedAudioRequest) -> ZakoResult<()>;
    async fn request_audio_meta(&self, request: AudioRequest) -> ZakoResult<AudioMetaResponse>;
}

pub struct StubTapHubService;

#[async_trait]
impl TapHubService for StubTapHubService {
    async fn request_audio(&self, request: CachedAudioRequest) -> ZakoResult<AudioResponse> {
        // Return dummy response
        let dummy_data = b"dummy audio data";
        let cursor = Cursor::new(dummy_data.to_vec());

        Ok(AudioResponse {
            cache_key: Some(request.cache_key),
            description: TrackDescription::from("Dummy Track".to_string()),
            stream: Box::new(cursor),
        })
    }

    async fn preload_audio(&self, _request: CachedAudioRequest) -> ZakoResult<()> {
        Ok(())
    }

    async fn request_audio_meta(&self, request: AudioRequest) -> ZakoResult<AudioMetaResponse> {
        Ok(AudioMetaResponse {
            description: TrackDescription::from(format!(
                "Dummy Title for {}",
                request.request.to_string()
            )),
            cache_key: StreamCacheKey::from("dummy_cache_key".to_string()),
        })
    }
}
