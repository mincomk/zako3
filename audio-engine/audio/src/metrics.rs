use lazy_static::lazy_static;
use std::sync::atomic::{AtomicU64, Ordering};

#[cfg(feature = "telemetry")]
use prometheus::{IntCounter, IntGauge, register_int_counter, register_int_gauge};

pub struct AudioMetrics {
    pub mixer_underruns: AtomicU64,
    pub mixer_loops: AtomicU64,
    pub decoder_stalls: AtomicU64,
    pub stream_underruns: AtomicU64,
}

impl AudioMetrics {
    fn new() -> Self {
        Self {
            mixer_underruns: AtomicU64::new(0),
            mixer_loops: AtomicU64::new(0),
            decoder_stalls: AtomicU64::new(0),
            stream_underruns: AtomicU64::new(0),
        }
    }
}

lazy_static! {
    pub static ref AUDIO_METRICS: AudioMetrics = AudioMetrics::new();
}

#[cfg(feature = "telemetry")]
lazy_static! {
    pub static ref METRIC_MIXER_UNDERRUNS: IntCounter = register_int_counter!(
        "audio_mixer_underruns_total",
        "Total number of mixer underruns (starvation)"
    )
    .unwrap();
    pub static ref METRIC_MIXER_BUFFER_DEPTH: IntGauge = register_int_gauge!(
        "audio_mixer_buffer_depth_samples",
        "Current available samples in the mixer buffer"
    )
    .unwrap();
    pub static ref METRIC_DECODER_STALLS: IntCounter = register_int_counter!(
        "audio_decoder_stalls_total",
        "Total number of decoder stalls (buffer full)"
    )
    .unwrap();
    pub static ref METRIC_STREAM_UNDERRUNS: IntCounter = register_int_counter!(
        "audio_stream_underruns_total",
        "Total number of output stream underruns"
    )
    .unwrap();
}

pub fn record_mixer_underrun() {
    AUDIO_METRICS
        .mixer_underruns
        .fetch_add(1, Ordering::Relaxed);
    #[cfg(feature = "telemetry")]
    METRIC_MIXER_UNDERRUNS.inc();
}

pub fn record_mixer_buffer_depth(depth: usize) {
    #[cfg(feature = "telemetry")]
    METRIC_MIXER_BUFFER_DEPTH.set(depth as i64);
}

pub fn record_decoder_stall() {
    AUDIO_METRICS.decoder_stalls.fetch_add(1, Ordering::Relaxed);
    #[cfg(feature = "telemetry")]
    METRIC_DECODER_STALLS.inc();
}

pub fn record_stream_underrun() {
    AUDIO_METRICS
        .stream_underruns
        .fetch_add(1, Ordering::Relaxed);
    #[cfg(feature = "telemetry")]
    METRIC_STREAM_UNDERRUNS.inc();
}
