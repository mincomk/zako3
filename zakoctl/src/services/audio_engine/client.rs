use anyhow::{Context, Result};
use colored::*;
use tonic::transport::Channel;
use zako3_audio_engine_protos::audio_engine_client::AudioEngineClient;
use zako3_audio_engine_protos::{
    AudioStopFilter, GetSessionStateRequest, JoinRequest, LeaveRequest, NextMusicRequest,
    PlayRequest, SetVolumeRequest, StopManyRequest, StopRequest, audio_stop_filter,
};

use crate::services::audio_engine::cli::{AudioEngineCommands, AudioEngineSubcommands};

pub async fn handle_command(ae_addr: String, cmd: AudioEngineCommands) -> Result<()> {
    let mut client = connect(ae_addr).await?;

    match cmd.command {
        AudioEngineSubcommands::Join {
            guild_id,
            channel_id,
        } => {
            let request = tonic::Request::new(JoinRequest {
                guild_id,
                channel_id,
            });
            let response = client.join(request).await?;
            println!(
                "{} {:?}",
                "Join Response:".green().bold(),
                response.into_inner()
            );
        }
        AudioEngineSubcommands::Leave { guild_id } => {
            let request = tonic::Request::new(LeaveRequest { guild_id });
            let response = client.leave(request).await?;
            println!(
                "{} {:?}",
                "Leave Response:".green().bold(),
                response.into_inner()
            );
        }
        AudioEngineSubcommands::Play {
            guild_id,
            queue,
            tap,
            request,
            volume,
        } => {
            let request = tonic::Request::new(PlayRequest {
                guild_id,
                queue_name: queue,
                tap_name: tap,
                audio_request_string: request,
                volume,
            });
            let response = client.play(request).await?;
            println!(
                "{} {:?}",
                "Play Response:".green().bold(),
                response.into_inner()
            );
        }
        AudioEngineSubcommands::SetVolume {
            guild_id,
            track_id,
            volume,
        } => {
            let request = tonic::Request::new(SetVolumeRequest {
                guild_id,
                track_id,
                volume,
            });
            let response = client.set_volume(request).await?;
            println!(
                "{} {:?}",
                "SetVolume Response:".green().bold(),
                response.into_inner()
            );
        }
        AudioEngineSubcommands::Stop { guild_id, track_id } => {
            let request = tonic::Request::new(StopRequest { guild_id, track_id });
            let response = client.stop(request).await?;
            println!(
                "{} {:?}",
                "Stop Response:".green().bold(),
                response.into_inner()
            );
        }
        AudioEngineSubcommands::StopMany {
            guild_id,
            filter,
            user_id,
        } => {
            let filter_type = match filter.to_lowercase().as_str() {
                "all" => Some(audio_stop_filter::FilterType::All(
                    audio_stop_filter::All {},
                )),
                "music" => Some(audio_stop_filter::FilterType::Music(
                    audio_stop_filter::Music {},
                )),
                "tts" => {
                    let uid = user_id.context("user_id is required for tts filter")?;
                    Some(audio_stop_filter::FilterType::Tts(audio_stop_filter::Tts {
                        user_id: uid,
                    }))
                }
                _ => {
                    return Err(anyhow::anyhow!(
                        "Invalid filter type. Options: all, music, tts"
                    ));
                }
            };

            let request = tonic::Request::new(StopManyRequest {
                guild_id,
                filter: Some(AudioStopFilter { filter_type }),
            });
            let response = client.stop_many(request).await?;
            println!(
                "{} {:?}",
                "StopMany Response:".green().bold(),
                response.into_inner()
            );
        }
        AudioEngineSubcommands::NextMusic { guild_id } => {
            let request = tonic::Request::new(NextMusicRequest { guild_id });
            let response = client.next_music(request).await?;
            println!(
                "{} {:?}",
                "NextMusic Response:".green().bold(),
                response.into_inner()
            );
        }
        AudioEngineSubcommands::GetSessionState { guild_id } => {
            let request = tonic::Request::new(GetSessionStateRequest { guild_id });
            let response = client.get_session_state(request).await?;
            println!(
                "{} {:?}",
                "GetSessionState Response:".green().bold(),
                response.into_inner()
            );
        }
    }

    Ok(())
}

async fn connect(addr: String) -> Result<AudioEngineClient<Channel>> {
    let endpoint = if addr.starts_with("http") {
        addr
    } else {
        format!("http://{}", addr)
    };

    println!("Connecting to Audio Engine at {}...", endpoint);
    AudioEngineClient::connect(endpoint)
        .await
        .context("Failed to connect to Audio Engine service")
}
