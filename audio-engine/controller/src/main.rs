use std::sync::Arc;

use serenity::Client;
use serenity::all::GatewayIntents;
use songbird::SerenityInit;

use zako3_audio_engine_controller::{
    AudioEngineServer, config::AppConfig,
    proto::audio_engine_server::AudioEngineServer as GrpcServer,
};

use zako3_audio_engine_core::engine::session_manager::SessionManager;
use zako3_audio_engine_infra::{
    discord::SongbirdDiscordService, state::RedisStateService, taphub::StubTapHubService,
};

use tonic::transport::Server;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Load Configuration
    let config = AppConfig::load();
    let addr = config.addr();

    // 2. Initialize Serenity Client (to drive Songbird)
    let intents = GatewayIntents::GUILD_VOICE_STATES;
    let mut client = Client::builder(&config.discord_token, intents)
        .register_songbird()
        .await
        .expect("Err creating client");

    // Extract Songbird manager reference
    // Note: songbird::get requires &Context, but we only have &Client here.
    // However, Songbird is registered in the TypeMap of the client data.
    // We can access it directly from the client data.
    let songbird_manager = {
        let data = client.data.read().await;
        data.get::<songbird::SongbirdKey>()
            .expect("Songbird VoiceClient placed in at initialisation.")
            .clone()
    };

    // Spawn client in background to keep connection alive
    tokio::spawn(async move {
        let _ = client.start().await.map_err(|why| {
            println!("Client ended: {:?}", why);
        });
    });

    // 3. Initialize Services
    let discord_service = Arc::new(SongbirdDiscordService::new(songbird_manager));
    let state_service = Arc::new(RedisStateService::new(&config.redis_url)?);
    let taphub_service = Arc::new(StubTapHubService);

    // 4. Create Session Manager
    let session_manager = Arc::new(SessionManager::new(
        discord_service,
        state_service,
        taphub_service,
    ));

    // 5. Initialize gRPC Server
    println!("Audio Engine Server listening on {}", addr);

    let engine_server = AudioEngineServer::new(session_manager);

    Server::builder()
        .add_service(GrpcServer::new(engine_server))
        .serve(addr)
        .await?;

    Ok(())
}
