use derive_more::{From, Into};
use serde::{Deserialize, Serialize};

use crate::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash, From, Into, Serialize, Deserialize)]
pub struct UserId(String);

#[derive(Debug, Clone, PartialEq, Eq, Hash, From, Into, Serialize, Deserialize)]
pub struct DiscordUserId(String);

#[derive(Debug, Clone, PartialEq, Eq, Hash, From, Into, Serialize, Deserialize)]
pub struct Username(String);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct User {
    pub id: UserId,
    pub discord_user_id: DiscordUserId,
    pub username: Username,
    pub avatar_url: Option<String>,
    pub email: Option<String>,
    pub permissions: Vec<Permission>,
}
