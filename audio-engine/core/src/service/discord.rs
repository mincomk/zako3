use std::num::NonZeroU64;
use std::sync::Arc;

use async_trait::async_trait;
use serenity::all::{ChannelId as SerenityChannelId, GuildId as SerenityGuildId};
use songbird::input::{AsyncAdapterStream, Input, RawAdapter};
use songbird::Songbird;

use crate::{
    error::ZakoResult,
    types::{ChannelId, GuildId},
};

use mockall::automock;

pub type ArcDiscordService = Arc<dyn DiscordService>;

#[automock]
#[async_trait]
pub trait DiscordService: Send + Sync + 'static {
    async fn join_voice_channel(&self, guild_id: GuildId, channel_id: ChannelId) -> ZakoResult<()>;
    async fn leave_voice_channel(&self, guild_id: GuildId) -> ZakoResult<()>;
    async fn play_audio(
        &self,
        guild_id: GuildId,
        stream: RawAdapter<AsyncAdapterStream>,
    ) -> ZakoResult<()>;
}

pub struct SongbirdDiscordService {
    manager: Arc<Songbird>,
}

impl SongbirdDiscordService {
    pub fn new(manager: Arc<Songbird>) -> Self {
        Self { manager }
    }

    fn to_serenity_guild_id(guild_id: GuildId) -> SerenityGuildId {
        SerenityGuildId::new(guild_id.into())
    }

    fn to_songbird_guild_id(guild_id: GuildId) -> songbird::id::GuildId {
        songbird::id::GuildId::from(NonZeroU64::new(guild_id.into()).unwrap())
    }

    fn to_serenity_channel_id(channel_id: ChannelId) -> SerenityChannelId {
        SerenityChannelId::new(channel_id.into())
    }
}

#[async_trait]
impl DiscordService for SongbirdDiscordService {
    async fn join_voice_channel(&self, guild_id: GuildId, channel_id: ChannelId) -> ZakoResult<()> {
        let g_id = Self::to_songbird_guild_id(guild_id);
        let c_id = Self::to_serenity_channel_id(channel_id);

        let _handler = self.manager.join(g_id, c_id).await?;
        Ok(())
    }

    async fn leave_voice_channel(&self, guild_id: GuildId) -> ZakoResult<()> {
        let g_id = Self::to_songbird_guild_id(guild_id);
        self.manager.remove(g_id).await?;
        Ok(())
    }

    async fn play_audio(
        &self,
        guild_id: GuildId,
        stream: RawAdapter<AsyncAdapterStream>,
    ) -> ZakoResult<()> {
        let g_id = Self::to_songbird_guild_id(guild_id);

        if let Some(call_lock) = self.manager.get(g_id) {
            let mut call = call_lock.lock().await;

            // Create a track from the raw adapter
            let input = Input::from(stream);
            let track = songbird::tracks::Track::new(input);

            call.play(track);
        }

        Ok(())
    }
}
