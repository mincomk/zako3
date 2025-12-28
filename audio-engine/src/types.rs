use derive_more::{From, FromStr, Into};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Into, From, Serialize, Deserialize)]
pub struct GuildId(u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Into, From, Serialize, Deserialize)]
pub struct ChannelId(u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Into, From, Serialize, Deserialize)]
pub struct QueueId(u64);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Into, From, FromStr, Serialize, Deserialize)]
pub struct QueueName(String);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Into, From, Serialize, Deserialize)]
pub struct TrackId(u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Into, From, Serialize, Deserialize)]
pub struct UserId(u64);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Into, From, FromStr, Serialize, Deserialize)]
pub struct TapName(String);

#[derive(Debug, Clone, Copy, PartialEq, Into, From, Serialize, Deserialize)]
pub struct Volume(f32);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Into, From, FromStr, Serialize, Deserialize)]
pub struct AudioRequestString(String);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AudioStopFilter {
    All,
    Music,
    TTS(UserId),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AudioRequest {
    pub tap_name: TapName,
    pub request: AudioRequestString,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Session {
    pub guild_id: GuildId,
    pub channel_id: ChannelId,
    pub queues: Vec<Queue>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Queue {
    pub queue_id: QueueId,
    pub name: QueueName,
    pub tracks: Vec<Track>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Track {
    pub track_id: TrackId,
    pub request: AudioRequest,
    pub volume: Volume,
}
