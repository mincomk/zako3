use std::collections::VecDeque;
use std::sync::Arc;

use mockall::automock;
use serenity::async_trait;
use symphonia::core::io::MediaSourceStream;
use symphonia::core::{audio::SampleBuffer, io::ReadOnlySource};
use tokio::io::AsyncRead;
use tokio::sync::oneshot::Sender;
use tracing::instrument;
use zako3_audio_engine_types::ZakoError;

use crate::{
    BoxProducer,
    error::ZakoResult,
    types::{BoxConsumer, TrackId},
};
use crate::{async_to_sync_read, create_boxed_ringbuf_pair};

pub type ArcDecoder = Arc<dyn Decoder>;

#[automock]
#[async_trait]
pub trait Decoder: Send + Sync + 'static {
    async fn start_decoding(
        &self,
        track_id: TrackId,
        stream: Box<dyn AsyncRead + Unpin + Send + Sync>,
    ) -> ZakoResult<BoxConsumer>;
}

pub struct SymphoniaDecoder;

#[async_trait]
impl Decoder for SymphoniaDecoder {
    #[instrument(skip(self, stream))]
    async fn start_decoding(
        &self,
        _track_id: TrackId,
        stream: Box<dyn AsyncRead + Unpin + Send + Sync>,
    ) -> ZakoResult<BoxConsumer> {
        let media_source = create_media_source(stream)?;

        let (boxed_producer, consumer) = create_boxed_ringbuf_pair();
        spawn_decode_task(media_source, boxed_producer).await?;

        Ok(consumer)
    }
}

fn create_media_source(
    stream: Box<dyn AsyncRead + Unpin + Send + Sync>,
) -> ZakoResult<Box<dyn symphonia::core::io::MediaSource>> {
    let reader = async_to_sync_read(stream)?;
    Ok(Box::new(ReadOnlySource::new(reader)))
}

async fn spawn_decode_task(
    media_source: Box<dyn symphonia::core::io::MediaSource>,
    producer: BoxProducer,
) -> ZakoResult<()> {
    let (sender, receiver) = tokio::sync::oneshot::channel();

    std::thread::spawn(move || {
        let result = spawn_decode_task_raw(media_source, producer, sender);
        if let Err(e) = result {
            tracing::error!("Decoding task failed to start: {}", e);
        }
    });

    // timeout
    match tokio::time::timeout(std::time::Duration::from_secs(5), receiver).await {
        Ok(Ok(res)) => res,
        Ok(Err(_)) => Err(ZakoError::Decoding("Decoding task sender dropped".into())),
        Err(_) => Err(ZakoError::Decoding(
            "Decoding task startup timed out".into(),
        )),
    }
}

fn spawn_decode_task_raw(
    media_source: Box<dyn symphonia::core::io::MediaSource>,
    producer: BoxProducer,
    sender: Sender<ZakoResult<()>>,
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

    sender
        .send(Ok(()))
        .map_err(|_| ZakoError::Decoding("Failed to send startup confirmation".into()))?;

    let mut mid_buffer = VecDeque::new();

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
            mid_buffer.extend(samples);

            if mid_buffer.len() >= crate::BUFFER_SIZE {
                let mut pcm_sample = [0.0f32; crate::BUFFER_SIZE];
                for i in 0..crate::BUFFER_SIZE {
                    pcm_sample[i] = mid_buffer.pop_front().unwrap();
                }
                if let Err(e) = producer.send(pcm_sample) {
                    tracing::warn!("Failed to send PCM sample: {}", e);
                    break;
                }
            }
        }
    }

    Ok(())
}
