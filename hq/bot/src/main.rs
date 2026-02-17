use hq_core::{AppConfig, Service, get_pool};
use poise::serenity_prelude as serenity;
use std::env;
use std::sync::Arc;

pub mod commands;

pub struct Data {
    pub service: Service,
}

pub type Error = Box<dyn std::error::Error + Send + Sync>;
pub type Context<'a> = poise::Context<'a, Data, Error>;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();

    // Load .env
    dotenvy::dotenv().ok();

    let config = Arc::new(AppConfig::load()?);
    let pool = get_pool(&config.database_url).await?;
    let service = Service::new(pool, config.clone()).await?;

    let token = env::var("DISCORD_BOT_TOKEN").expect("DISCORD_BOT_TOKEN missing");

    let intents = serenity::GatewayIntents::non_privileged();

    let framework = poise::Framework::builder()
        .options(poise::FrameworkOptions {
            commands: vec![
                commands::ping(),
                commands::tap::tap(), // Parent command
                                      // commands::tap::create(), // Subcommands are registered via parent? No, poise usually handles this differently.
                                      // Wait, if I use `subcommands("create", "list")` in `tap` function, I just register `tap`.
            ],
            ..Default::default()
        })
        .setup(move |ctx, _ready, framework| {
            Box::pin(async move {
                poise::builtins::register_globally(ctx, &framework.options().commands).await?;
                Ok(Data {
                    service: service.clone(),
                })
            })
        })
        .build();

    let client = serenity::ClientBuilder::new(token, intents)
        .framework(framework)
        .await;

    client.unwrap().start().await.unwrap();
    Ok(())
}
