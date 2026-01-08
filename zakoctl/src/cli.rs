use crate::services::audio_engine::cli::AudioEngineCommands;
use crate::services::config::cli::ConfigCommands;
use clap::{Parser, Subcommand};
use clap_complete::Shell;

#[derive(Parser)]
#[command(name = "zakoctl")]
#[command(about = "Development CLI client for Zako", long_about = None)]
#[command(version, propagate_version = true)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,

    /// Address of the Audio Engine gRPC server (overrides config)
    #[arg(global = true, long, env = "ZAKO_AE_ADDR")]
    pub ae_addr: Option<String>,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Audio Engine service commands (abbr: ae)
    #[command(visible_alias = "ae")]
    AudioEngine(AudioEngineCommands),

    /// Configuration commands
    Config(ConfigCommands),

    /// Generate shell completion scripts
    Completion {
        /// The shell to generate the completions for
        #[arg(value_enum)]
        shell: Shell,
    },
}
