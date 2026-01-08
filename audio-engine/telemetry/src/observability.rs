use opentelemetry::KeyValue;
use opentelemetry_otlp::WithExportConfig;
use opentelemetry_sdk::propagation::TraceContextPropagator;
use opentelemetry_sdk::{
    Resource, runtime,
    trace::{self, Sampler},
};
use tracing_subscriber::{EnvFilter, Registry, layer::SubscriberExt, util::SubscriberInitExt};

pub fn init_tracing(service_name: &str, otlp_endpoint: Option<String>) -> anyhow::Result<()> {
    // Set global propagator
    opentelemetry::global::set_text_map_propagator(TraceContextPropagator::new());

    // Stdout logging layer
    let fmt_layer = tracing_subscriber::fmt::layer()
        .with_target(true)
        .with_thread_ids(true)
        .with_level(true)
        .compact();

    // Env filter
    let env_filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("debug"));

    let registry = Registry::default().with(env_filter).with(fmt_layer);

    // Optional OTLP tracing layer
    if let Some(endpoint) = otlp_endpoint {
        let tracer = opentelemetry_otlp::new_pipeline()
            .tracing()
            .with_exporter(
                opentelemetry_otlp::new_exporter()
                    .tonic()
                    .with_endpoint(endpoint),
            )
            .with_trace_config(
                trace::config()
                    .with_sampler(Sampler::AlwaysOn)
                    .with_resource(Resource::new(vec![KeyValue::new(
                        "service.name",
                        service_name.to_string(),
                    )])),
            )
            .install_batch(runtime::Tokio)?;

        let otel_layer = tracing_opentelemetry::layer().with_tracer(tracer);
        registry.with(otel_layer).init();
    } else {
        registry.init();
    }

    Ok(())
}

pub fn init_metrics(service_name: &str) -> anyhow::Result<()> {
    // Configure Prometheus exporter
    let registry = prometheus::default_registry();
    let exporter = opentelemetry_prometheus::exporter()
        .with_registry(registry.clone())
        .build()?;

    let provider = opentelemetry_sdk::metrics::MeterProvider::builder()
        .with_reader(exporter)
        .with_resource(Resource::new(vec![KeyValue::new(
            "service.name",
            service_name.to_string(),
        )]))
        .build();

    opentelemetry::global::set_meter_provider(provider);

    Ok(())
}
