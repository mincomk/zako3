use crate::{BoxConsumer, BoxProducer, PCMReceiver, PCMSender};

pub fn create_boxed_ringbuf_pair() -> (BoxProducer, BoxConsumer) {
    crossbeam::channel::unbounded()
}

pub fn create_async_pcm_pair() -> (PCMSender, PCMReceiver) {
    tokio::sync::mpsc::channel(16)
}

pub fn async_to_sync_read<T>(async_read: T) -> std::io::Result<impl std::io::Read>
where
    T: tokio::io::AsyncRead + Unpin + Send + 'static,
{
    let (reader, mut writer) = os_pipe::pipe()?;

    tokio::spawn(async move {
        let mut async_read = async_read;
        let mut buffer = [0u8; 8192];

        loop {
            match tokio::io::AsyncReadExt::read(&mut async_read, &mut buffer).await {
                Ok(0) => break, // EOF
                Ok(n) => {
                    match std::io::Write::write_all(&mut writer, &buffer[..n]).is_err() {
                        true => {
                            break; // Pipe closed
                        }
                        false => (),
                    }
                }
                Err(_) => break, // Read error
            }
        }
    });

    Ok(reader)
}
