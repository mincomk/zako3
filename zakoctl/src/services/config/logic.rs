use crate::config::Config;
use crate::services::config::cli::{ConfigCommands, ConfigSubcommands};
use anyhow::Result;
use colored::*;

pub fn handle_command(cmd: ConfigCommands) -> Result<()> {
    let mut config = Config::load()?;

    match cmd.command {
        ConfigSubcommands::View => {
            println!("{}", toml::to_string_pretty(&config)?);
        }
        ConfigSubcommands::CurrentContext => {
            println!("{}", config.current_context);
        }
        ConfigSubcommands::UseContext { name } => {
            if config.get_context(&name).is_some() {
                config.current_context = name.clone();
                config.save()?;
                println!("Switched to context \"{}\".", name.green());
            } else {
                println!("{} Context \"{}\" not found.", "Error:".red(), name);
            }
        }
        ConfigSubcommands::SetContext { name, ae_addr } => {
            config.set_context(name.clone(), ae_addr);
            config.save()?;
            println!("Context \"{}\" set.", name.green());
        }
        ConfigSubcommands::DeleteContext { name } => {
            if name == config.current_context {
                println!("{} Cannot delete the current context.", "Error:".red());
                return Ok(());
            }

            if let Some(index) = config.contexts.iter().position(|c| c.name == name) {
                config.contexts.remove(index);
                config.save()?;
                println!("Context \"{}\" deleted.", name.green());
            } else {
                println!("{} Context \"{}\" not found.", "Error:".red(), name);
            }
        }
    }

    Ok(())
}
