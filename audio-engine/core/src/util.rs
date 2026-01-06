use ringbuf::{HeapRb, traits::Split};

use crate::{BUFFER_SIZE, BoxConsumer, BoxProducer};

pub mod id_gen;
pub mod queue_name_gen;

pub fn create_boxed_ringbuf_pair() -> (BoxProducer, BoxConsumer) {
    let (p, c) = HeapRb::new(BUFFER_SIZE).split();

    (Box::new(p), Box::new(c))
}
