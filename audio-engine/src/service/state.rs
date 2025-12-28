use crate::{
    error::ZakoResult,
    types::{GuildId, Session},
};

pub trait StateService: Send + Sync + 'static {
    async fn get_session(&self, guild_id: GuildId) -> ZakoResult<Option<Session>>;
    async fn save_session(&self, session: &Session) -> ZakoResult<()>;
    async fn delete_session(&self, guild_id: GuildId) -> ZakoResult<()>;

    async fn modify_session<F>(&self, guild_id: GuildId, f: F) -> ZakoResult<()>
    where
        F: FnOnce(&mut Session) + Send;
}
