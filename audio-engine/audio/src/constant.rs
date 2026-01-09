pub const BUFFER_SIZE: usize = 3840;
pub const SAMPLE_RATE: u32 = 48000;
pub const CHANNELS: u32 = 2;

pub fn frame_duration() -> std::time::Duration {
    let secs = BUFFER_SIZE as f64 / SAMPLE_RATE as f64;
    std::time::Duration::from_secs_f64(secs)
}
