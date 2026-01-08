use serde::Deserialize;

#[derive(Deserialize, Debug, Clone)]
pub struct AppConfig {
    pub redis_url: String,
    pub discord_token: String,
    pub port: u16,
    pub host: String,
}

impl AppConfig {
    pub fn load() -> Self {
        // Load .env file if it exists
        dotenvy::dotenv().ok();

        // Enforce required variables via envy
        match envy::from_env::<AppConfig>() {
            Ok(config) => config,
            Err(e) => {
                tracing::error!("Failed to load configuration: {}", e);
                std::process::exit(1);
            }
        }
    }

    pub fn addr(&self) -> std::net::SocketAddr {
        format!("{}:{}", self.host, self.port)
            .parse()
            .expect("Invalid host/port configuration")
    }
}
