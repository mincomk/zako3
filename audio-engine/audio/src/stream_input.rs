use std::{
    io::{self, ErrorKind, SeekFrom},
    pin::Pin,
    task::{Context, Poll},
};

use async_os_pipe::Stream;
use async_trait::async_trait;
use songbird::input::{AsyncAdapterStream, AsyncMediaSource, RawAdapter};
use tokio::io::{AsyncRead, AsyncSeek, AsyncWriteExt, ReadBuf};
use zako3_audio_engine_types::ZakoResult;

use crate::{BUFFER_SIZE, CHANNELS, PCMReceiver, SAMPLE_RATE};

type InputStream = Stream;

pub async fn create_stream_input(mut consumer: PCMReceiver) -> ZakoResult<StreamInput> {
    let (reader, mut writer) = async_os_pipe::pipe().await?;

    tokio::spawn(async move {
        while let Some(buf) = consumer.recv().await {
            let mut buffer = vec![0u8; BUFFER_SIZE * 4]; // f32 is 4 bytes

            // Convert f32 slice to u8 slice (little endian)
            for i in 0..BUFFER_SIZE {
                let bytes = buf[i].to_le_bytes();
                buffer[i * 4..(i + 1) * 4].copy_from_slice(&bytes);
            }

            if writer.write_all(&buffer).await.is_err() {
                tracing::debug!("StreamInput: Writer closed, stopping write task.");
                break; // Pipe closed
            }
        }
    });

    Ok(StreamInput::new(reader))
}

pub struct StreamInput {
    stream: InputStream,
}

impl StreamInput {
    fn new(stream: InputStream) -> Self {
        Self { stream }
    }

    pub fn create_input(self) -> RawAdapter<AsyncAdapterStream> {
        RawAdapter::new(
            AsyncAdapterStream::new(Box::new(self), BUFFER_SIZE),
            SAMPLE_RATE,
            CHANNELS,
        )
    }
}

impl AsyncRead for StreamInput {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        Pin::new(&mut self.get_mut().stream).poll_read(cx, buf)
        /*
        Box::pin(async {
            // little endina
            let mut temp_buf = [0f32; BUFFER_SIZE];
            self.get_mut().stream.pop_slice(&mut temp_buf);

            buf.put_slice(bytemuck::cast_slice(&temp_buf));

            Ok(())
        })
        .as_mut()
        .poll(cx)
        */
    }
}

impl AsyncSeek for StreamInput {
    fn start_seek(self: Pin<&mut Self>, _position: SeekFrom) -> io::Result<()> {
        Err(ErrorKind::Unsupported.into())
    }

    fn poll_complete(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<io::Result<u64>> {
        unreachable!()
    }
}

#[async_trait]
impl AsyncMediaSource for StreamInput {
    fn is_seekable(&self) -> bool {
        false
    }

    async fn byte_len(&self) -> Option<u64> {
        None
    }
}

unsafe impl Sync for StreamInput {}

#[cfg(test)]
mod tests {
    use tokio::io::AsyncReadExt;

    use crate::{BUFFER_SIZE, create_async_pcm_pair, create_stream_input};

    #[tokio::test]
    async fn test_stream_input_read() {
        let (producer, consumer) = create_async_pcm_pair();

        let test_data = [1f32; BUFFER_SIZE];
        producer.send(test_data).await.unwrap();

        let stream_input = create_stream_input(consumer).await.unwrap();

        let mut read_buf = vec![0u8; BUFFER_SIZE * 4]; // f32 is 4 bytes
        let mut async_reader = stream_input;

        async_reader.read_exact(&mut read_buf).await.unwrap();

        let read_floats: &[f32] = bytemuck::cast_slice(&read_buf);

        assert_eq!(read_floats, test_data.as_slice());
    }
}
