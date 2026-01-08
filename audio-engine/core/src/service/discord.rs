use std::sync::Arc;

use async_trait::async_trait;
use songbird::input::{AsyncAdapterStream, RawAdapter};

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
