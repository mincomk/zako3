use crate::{
    error::ZakoResult,
    types::{GuildId, SessionState},
};
use async_trait::async_trait;
use mockall::automock;
use redis::AsyncCommands;
use std::sync::Arc;

pub type ArcStateService = Arc<dyn StateService>;

#[automock]
#[async_trait]
pub trait StateService: Send + Sync + 'static {
    async fn get_session(&self, guild_id: GuildId) -> ZakoResult<Option<SessionState>>;
    async fn save_session(&self, session: &SessionState) -> ZakoResult<()>;
    async fn delete_session(&self, guild_id: GuildId) -> ZakoResult<()>;
}

pub struct RedisStateService {
    client: redis::Client,
}

impl RedisStateService {
    pub fn new(redis_url: &str) -> ZakoResult<Self> {
        let client = redis::Client::open(redis_url)?;
        Ok(Self { client })
    }

    fn get_key(guild_id: GuildId) -> String {
        let id: u64 = guild_id.into();
        format!("session:{}", id)
    }
}

#[async_trait]
impl StateService for RedisStateService {
    async fn get_session(&self, guild_id: GuildId) -> ZakoResult<Option<SessionState>> {
        let mut conn = self.client.get_multiplexed_async_connection().await?;
        let key = Self::get_key(guild_id);
        let data: Option<String> = conn.get(key).await?;

        match data {
            Some(json) => {
                let session: SessionState = serde_json::from_str(&json)?;
                Ok(Some(session))
            }
            None => Ok(None),
        }
    }

    async fn save_session(&self, session: &SessionState) -> ZakoResult<()> {
        let mut conn = self.client.get_multiplexed_async_connection().await?;
        let key = Self::get_key(session.guild_id);
        let json = serde_json::to_string(session)?;
        let _: () = conn.set(key, json).await?;
        Ok(())
    }

    async fn delete_session(&self, guild_id: GuildId) -> ZakoResult<()> {
        let mut conn = self.client.get_multiplexed_async_connection().await?;
        let key = Self::get_key(guild_id);
        let _: () = conn.del(key).await?;
        Ok(())
    }
}

pub async fn modify_state_session<F>(
    state_service: &ArcStateService,
    guild_id: GuildId,
    f: F,
) -> ZakoResult<()>
where
    F: FnOnce(&mut SessionState) + Send + 'static,
{
    if let Some(mut session) = state_service.get_session(guild_id).await? {
        f(&mut session);
        state_service.save_session(&session).await?;
    }

    Ok(())
}
