use thiserror::Error;

#[derive(Debug, Error)]
pub enum ZakoError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Symphonia error: {0}")]
    Symphonia(#[from] symphonia::core::errors::Error),

    #[error("Decoding error: {0}")]
    Decoding(String),
}

pub type ZakoResult<T> = Result<T, ZakoError>;
