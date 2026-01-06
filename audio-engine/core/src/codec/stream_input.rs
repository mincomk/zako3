use std::{
    io::{self, ErrorKind, SeekFrom},
    pin::Pin,
    task::{Context, Poll},
};

use async_trait::async_trait;
use songbird::input::{AsyncAdapterStream, AsyncMediaSource, RawAdapter};
use tokio::io::{AsyncRead, AsyncSeek, ReadBuf};

use crate::{BUFFER_SIZE, BoxConsumer, CHANNELS, SAMPLE_RATE};

pub struct StreamInput {
    stream: BoxConsumer,
}

impl From<BoxConsumer> for StreamInput {
    fn from(stream: BoxConsumer) -> Self {
        Self::new(stream)
    }
}

impl StreamInput {
    pub fn new(stream: BoxConsumer) -> Self {
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
        Box::pin(async {
            // little endina
            let mut temp_buf = [0f32; BUFFER_SIZE];
            self.get_mut().stream.pop_slice(&mut temp_buf);

            buf.put_slice(bytemuck::cast_slice(&temp_buf));

            Ok(())
        })
        .as_mut()
        .poll(cx)
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
    use ringbuf::{HeapRb, traits::Split};
    use tokio::io::AsyncReadExt;

    use crate::{BUFFER_SIZE, types::BoxProducer};

    use super::StreamInput;

    #[tokio::test]
    async fn test_stream_input_read() {
        // Create a BoxProducer and fill it with some test data
        let (producer, consumer) = HeapRb::new(BUFFER_SIZE).split();
        let mut producer: BoxProducer = Box::new(producer);
        let consumer = Box::new(consumer);

        let test_data: Vec<f32> = (0..BUFFER_SIZE).map(|x| x as f32).collect();
        producer.push_slice(&test_data);

        let stream_input = StreamInput { stream: consumer };

        let mut read_buf = vec![0u8; BUFFER_SIZE * 4]; // f32 is 4 bytes
        let mut async_reader = stream_input;

        async_reader.read_exact(&mut read_buf).await.unwrap();

        let read_floats: &[f32] = bytemuck::cast_slice(&read_buf);

        assert_eq!(read_floats, test_data.as_slice());
    }
}
