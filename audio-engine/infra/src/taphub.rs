use std::io::Cursor;

use async_trait::async_trait;
use zako3_audio_engine_core::{
    error::ZakoResult,
    service::taphub::TapHubService,
    types::{
        AudioMetaResponse, AudioRequest, AudioResponse, CachedAudioRequest, StreamCacheKey,
        TrackDescription,
    },
};

pub struct StubTapHubService;

static SINE_DATA: &[u8] = include_bytes!("../sine.mp3");
static SPEAKY_DATA: &[u8] = include_bytes!("../good.mp3");

#[async_trait]
impl TapHubService for StubTapHubService {
    async fn request_audio(&self, request: CachedAudioRequest) -> ZakoResult<AudioResponse> {
        let cursor = if "sine" == request.audio_request.to_string() {
            Cursor::new(SINE_DATA.to_vec())
        } else {
            Cursor::new(SPEAKY_DATA.to_vec())
        };

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
