use thiserror::Error;

#[derive(Debug, Error)]
pub enum ZakoError {}

pub type ZakoResult<T> = Result<T, ZakoError>;
