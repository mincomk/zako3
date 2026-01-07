use std::sync::Arc;

use mockall::automock;
use symphonia::core::io::MediaSourceStream;
use symphonia::core::{audio::SampleBuffer, io::ReadOnlySource};
use tokio::io::AsyncRead;
use tokio_util::io::SyncIoBridge;
use zako3_audio_engine_types::ZakoError;

use crate::create_boxed_ringbuf_pair;
use crate::{
    BoxProducer,
    error::ZakoResult,
    types::{BoxConsumer, TrackId},
};

pub type ArcDecoder = Arc<dyn Decoder>;

#[automock]
pub trait Decoder: Send + Sync + 'static {
    fn start_decoding(
        &self,
        track_id: TrackId,
        stream: Box<dyn AsyncRead + Unpin + Send + Sync>,
    ) -> ZakoResult<BoxConsumer>;
}

pub struct SymphoniaDecoder;

impl Decoder for SymphoniaDecoder {
    fn start_decoding(
        &self,
        _track_id: TrackId,
        stream: Box<dyn AsyncRead + Unpin + Send + Sync>,
    ) -> ZakoResult<BoxConsumer> {
        let media_source = create_media_source(stream);

        let (boxed_producer, consumer) = create_boxed_ringbuf_pair();
        spawn_decode_task(media_source, boxed_producer)?;

        Ok(consumer)
    }
}

fn create_media_source(
    stream: Box<dyn AsyncRead + Unpin + Send + Sync>,
) -> Box<dyn symphonia::core::io::MediaSource> {
    let bridge = SyncIoBridge::new(stream);
    Box::new(ReadOnlySource::new(bridge))
}

fn spawn_decode_task(
    media_source: Box<dyn symphonia::core::io::MediaSource>,
    mut producer: BoxProducer,
) -> ZakoResult<()> {
    let mss = MediaSourceStream::new(media_source, Default::default());

    // ... (Standard Symphonia Probe & Decoder Setup) ...
    let mut probed = symphonia::default::get_probe().format(
        &Default::default(),
        mss,
        &Default::default(),
        &Default::default(),
    )?;

    let track = probed
        .format
        .default_track()
        .ok_or_else(|| ZakoError::Decoding("No default track found in media source".into()))?;

    let mut decoder =
        symphonia::default::get_codecs().make(&track.codec_params, &Default::default())?;

    let mut sample_buf = None;

    std::thread::spawn(move || {
        loop {
            let packet = match probed.format.next_packet() {
                Ok(p) => p,
                Err(_) => break,
            };

            let decoded = match decoder.decode(&packet) {
                Ok(d) => d,
                Err(e) => {
                    tracing::warn!("Decode error: {}", e);
                    continue;
                }
            };

            if sample_buf.is_none() {
                sample_buf = Some(SampleBuffer::<f32>::new(
                    decoded.capacity() as u64,
                    *decoded.spec(),
                ));
            }

            if let Some(buf) = sample_buf.as_mut() {
                buf.copy_interleaved_ref(decoded);
                let samples = buf.samples();

                // Push samples into the ring buffer
                // Note: .push_slice returns the number of elements actually written.
                // You may need to loop/yield if the ring buffer is full.
                let mut written = 0;
                while written < samples.len() {
                    let n = producer.push_slice(&samples[written..]);
                    if n == 0 {
                        // Buffer full: sleep briefly or yield
                        std::thread::sleep(std::time::Duration::from_millis(10));
                        continue;
                    }
                    written += n;
                }
            }
        }
    });

    Ok(())
}
