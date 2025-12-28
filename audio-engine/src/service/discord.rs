use crate::{error::ZakoResult, types::ChannelId};

pub trait DiscordService: Send + Sync + 'static {
    async fn join_voice_channel(&self, channel_id: ChannelId) -> ZakoResult<()>;
}
