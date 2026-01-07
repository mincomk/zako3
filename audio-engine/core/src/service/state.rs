use std::sync::Arc;

use async_trait::async_trait;
use mockall::automock;

use crate::{
    error::ZakoResult,
    types::{GuildId, SessionState},
};

pub type ArcStateService = Arc<dyn StateService>;

#[automock]
#[async_trait]
pub trait StateService: Send + Sync + 'static {
    async fn get_session(&self, guild_id: GuildId) -> ZakoResult<Option<SessionState>>;
    async fn save_session(&self, session: &SessionState) -> ZakoResult<()>;
    async fn delete_session(&self, guild_id: GuildId) -> ZakoResult<()>;
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
