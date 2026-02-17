use crate::CoreResult;
use dotenvy::dotenv;
use serde::{Deserialize, Serialize};
use std::env;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AppConfig {
    pub database_url: String,
    pub discord_client_id: String,
    pub discord_client_secret: String,
    pub discord_redirect_uri: String,
    pub jwt_secret: String,
}

impl AppConfig {
    pub fn load() -> CoreResult<Self> {
        dotenv().ok();

        Ok(Self {
            database_url: env::var("DATABASE_URL")?,
            discord_client_id: env::var("DISCORD_CLIENT_ID")?,
            discord_client_secret: env::var("DISCORD_CLIENT_SECRET")?,
            discord_redirect_uri: env::var("DISCORD_REDIRECT_URI")?,
            jwt_secret: env::var("JWT_SECRET")?,
        })
    }
}
