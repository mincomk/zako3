use clap::{Args, Subcommand};

#[derive(Args)]
pub struct ConfigCommands {
    #[command(subcommand)]
    pub command: ConfigSubcommands,
}

#[derive(Subcommand)]
pub enum ConfigSubcommands {
    /// View current configuration
    View,
    /// Get current context
    CurrentContext,
    /// Set the current context
    UseContext {
        #[arg(help = "Name of the context to use")]
        name: String,
    },
    /// Set a context entry in kubeconfig
    SetContext {
        #[arg(help = "Name of the context")]
        name: String,
        #[arg(long, help = "Address of the Audio Engine gRPC server")]
        ae_addr: String,
    },
    /// Delete a context from the config
    DeleteContext {
        #[arg(help = "Name of the context to delete")]
        name: String,
    },
}
