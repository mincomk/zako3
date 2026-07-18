use serde::Deserialize;

#[derive(Deserialize, Debug, Clone)]
pub struct AppConfig {
    #[serde(default = "default_ae_port")]
    pub ae_port: u16,
    /// Optional override for advertised address (e.g., "host:port" or "http://host:port").
    /// If set, the heuristic address resolver is skipped.
    pub ae_advertise_addr: Option<String>,
    #[serde(default = "default_tl_rpc_url")]
    pub tl_rpc_url: String,
    /// Redis used to persist active voice sessions so a restarted AE can rejoin them.
    #[serde(default = "default_redis_url")]
    pub redis_url: String,
    #[serde(default = "default_taphub_url")]
    pub taphub_url: String,
    #[serde(default = "default_taphub_sni")]
    pub taphub_sni: String,
    #[serde(default = "default_taphub_transport_cert_file")]
    pub taphub_transport_cert_file: String,
    #[serde(default = "default_taphub_request_timeout_ms")]
    pub taphub_request_timeout_ms: u64,

    // Telemetry configuration
    #[serde(default = "default_service_name")]
    pub service_name: String,
    pub otlp_endpoint: Option<String>,
    #[serde(default = "default_metrics_port")]
    pub metrics_port: u16,
}

fn default_ae_port() -> u16 {
    8090
}

fn default_tl_rpc_url() -> String {
    "http://127.0.0.1:7070".to_string()
}

fn default_redis_url() -> String {
    "redis://127.0.0.1:6379".to_string()
}

fn default_taphub_url() -> String {
    "127.0.0.1:4000".to_string()
}

fn default_taphub_sni() -> String {
    "localhost".to_string()
}

fn default_taphub_transport_cert_file() -> String {
    "cert.pem".to_string()
}

/// Per-attempt taphub request timeout. Chosen to satisfy the timeout hierarchy so a
/// command that makes back-to-back taphub calls (a normal `play`: `request_audio_meta`
/// then `request_audio`) still finishes inside the 25s command backstop:
///   N_seq(2) × MAX_ATTEMPTS(2) × per_attempt(6s) = 24s ≤ backstop(25s) < TL dispatch(30s).
/// If legitimately-slow calls need more than 6s, raise this AND the backstop
/// (`server.rs`) and TL's dispatch timeout together to keep the inequality holding.
fn default_taphub_request_timeout_ms() -> u64 {
    6_000
}

fn default_service_name() -> String {
    "audio-engine".to_string()
}

fn default_metrics_port() -> u16 {
    9090
}

impl AppConfig {
    pub fn load() -> Self {
        dotenvy::dotenv().ok();

        match envy::from_env::<AppConfig>() {
            Ok(config) => config,
            Err(e) => {
                eprintln!("Failed to load configuration: {}", e);
                std::process::exit(1);
            }
        }
    }
}
