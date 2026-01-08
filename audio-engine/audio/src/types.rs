pub type PCMSample = [f32; BUFFER_SIZE];

pub type BoxConsumer = Receiver<PCMSample>;
pub type BoxProducer = Sender<PCMSample>;

pub type PCMSender = tokio::sync::mpsc::Sender<PCMSample>;
pub type PCMReceiver = tokio::sync::mpsc::Receiver<PCMSample>;

use crossbeam::channel::{Receiver, Sender};
pub use zako3_audio_engine_types::*;

use crate::BUFFER_SIZE;
