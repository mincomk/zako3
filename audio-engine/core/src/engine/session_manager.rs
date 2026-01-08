use std::sync::Arc;

use dashmap::DashMap;
use zako3_audio_engine_audio::{create_async_pcm_pair, create_stream_input};

use crate::{
    ArcDiscordService, ArcStateService, ArcTapHubService,
    audio::{SymphoniaDecoder, create_thread_mixer},
    error::ZakoResult,
    session::{SessionControl, create_session_control},
    types::{ChannelId, GuildId, SessionState},
};

pub struct SessionManager {
    discord_service: ArcDiscordService,
    state_service: ArcStateService,
    taphub_service: ArcTapHubService,

    sessions: DashMap<GuildId, Arc<SessionControl>>,
}

impl SessionManager {
    pub fn new(
        discord_service: ArcDiscordService,
        state_service: ArcStateService,
        taphub_service: ArcTapHubService,
    ) -> Self {
        SessionManager {
            discord_service,
            state_service,
            taphub_service,
            sessions: DashMap::new(),
        }
    }

    async fn initiate_session(&self, guild_id: GuildId) -> ZakoResult<()> {
        let (prod, cons) = create_async_pcm_pair();

        let mixer = create_thread_mixer(prod);
        let decoder = SymphoniaDecoder;

        let control = create_session_control(
            guild_id,
            Arc::new(mixer),
            Arc::new(decoder),
            self.state_service.clone(),
            self.taphub_service.clone(),
        );

        self.discord_service
            .play_audio(guild_id, create_stream_input(cons).await?.create_input())
            .await?;

        self.sessions.insert(guild_id, control);

        Ok(())
    }

    pub async fn join(&self, guild_id: GuildId, channel_id: ChannelId) -> ZakoResult<()> {
        self.discord_service
            .join_voice_channel(guild_id, channel_id)
            .await?;

        let session = SessionState {
            guild_id,
            channel_id: channel_id,
            queues: Default::default(),
        };

        self.state_service.save_session(&session).await?;

        self.initiate_session(guild_id).await?;

        Ok(())
    }

    pub async fn leave(&self, guild_id: GuildId) -> ZakoResult<()> {
        self.discord_service.leave_voice_channel(guild_id).await?;
        self.state_service.delete_session(guild_id).await?;

        self.sessions.remove(&guild_id);

        Ok(())
    }

    pub fn get_session(&self, guild_id: GuildId) -> Option<Arc<SessionControl>> {
        self.sessions.get(&guild_id).map(|s| s.clone())
    }
}
