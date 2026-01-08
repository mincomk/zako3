use clap::{Args, Subcommand};

#[derive(Args)]
pub struct AudioEngineCommands {
    #[command(subcommand)]
    pub command: AudioEngineSubcommands,
}

#[derive(Subcommand)]
pub enum AudioEngineSubcommands {
    /// Join a voice channel
    Join {
        #[arg(help = "The Guild ID")]
        guild_id: u64,
        #[arg(help = "The Channel ID")]
        channel_id: u64,
    },
    /// Leave a voice channel
    Leave {
        #[arg(help = "The Guild ID")]
        guild_id: u64,
    },
    /// Play audio in a voice channel
    Play {
        #[arg(help = "The Guild ID")]
        guild_id: u64,
        #[arg(long, help = "Queue name", default_value = "default")]
        queue: String,
        #[arg(long, help = "Tap name", default_value = "ytdl")]
        tap: String,
        #[arg(help = "Audio request string (e.g. URL)")]
        request: String,
        #[arg(long, help = "Volume (0.0 - 1.0)", default_value_t = 1.0)]
        volume: f32,
    },
    /// Set volume for a specific track
    SetVolume {
        #[arg(help = "The Guild ID")]
        guild_id: u64,
        #[arg(help = "The Track ID")]
        track_id: u64,
        #[arg(help = "Volume (0.0 - 1.0)")]
        volume: f32,
    },
    /// Stop playback of a specific track
    Stop {
        #[arg(help = "The Guild ID")]
        guild_id: u64,
        #[arg(help = "The Track ID")]
        track_id: String,
    },
    /// Stop multiple tracks based on filter
    StopMany {
        #[arg(help = "The Guild ID")]
        guild_id: u64,
        #[arg(long, help = "Filter type: all, music, tts")]
        filter: String,
        #[arg(long, help = "User ID for TTS filter", required_if_eq("filter", "tts"))]
        user_id: Option<u64>,
    },
    /// Skip to the next music track
    NextMusic {
        #[arg(help = "The Guild ID")]
        guild_id: u64,
    },
    /// Get the current session state
    GetSessionState {
        #[arg(help = "The Guild ID")]
        guild_id: u64,
    },
}
