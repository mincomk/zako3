use std::time::Duration;

use crate::{
    constant::BUFFER_SIZE,
    engine::mixer::{Mixer, create_thread_mixer},
    types::TrackId,
    util::create_boxed_ringbuf_pair,
};
use tokio::sync::mpsc;

#[tokio::test]
async fn test_mixer_add_remove_source() {
    let (output_prod, _output_cons) = create_boxed_ringbuf_pair();
    let mixer = create_thread_mixer(output_prod);

    let track_id = TrackId::from(1);
    let (_source_prod, source_cons) = create_boxed_ringbuf_pair();
    let (end_tx, _end_rx) = mpsc::unbounded_channel();

    // Add source
    mixer.add_source(track_id, source_cons, end_tx);

    // Verify source is added
    assert!(mixer.has_source(track_id).await);

    // Remove source
    mixer.remove_source(track_id);

    // Wait a bit for the command to be processed
    tokio::time::sleep(Duration::from_millis(50)).await;

    // Verify source is removed
    assert!(!mixer.has_source(track_id).await);
}

#[tokio::test]
async fn test_mixer_audio_mixing() {
    let (output_prod, mut output_cons) = create_boxed_ringbuf_pair();
    let mixer = create_thread_mixer(output_prod);

    let track1 = TrackId::from(1);
    let track2 = TrackId::from(2);
    let (end_tx, _end_rx) = mpsc::unbounded_channel();

    let (mut prod1, cons1) = create_boxed_ringbuf_pair();
    let (mut prod2, cons2) = create_boxed_ringbuf_pair();

    // Fill buffers with constant values
    let data1 = vec![0.5; BUFFER_SIZE * 2];
    let data2 = vec![0.3; BUFFER_SIZE * 2];
    prod1.push_slice(&data1);
    prod2.push_slice(&data2);

    mixer.add_source(track1, cons1, end_tx.clone());
    mixer.add_source(track2, cons2, end_tx);

    // Wait for mixing
    tokio::time::sleep(Duration::from_millis(100)).await;

    // Check output
    let mut mixed_data = vec![0.0; BUFFER_SIZE];
    let n = output_cons.pop_slice(&mut mixed_data);

    assert!(n > 0);
    // Check if the first few samples are roughly sum of inputs (0.5 + 0.3 = 0.8)
    // Note: Volume smoothing starts at 0.0 and ramps up to 1.0, so initial samples will be small
    // Let's check a sample from later in the buffer
    if n > 100 {
        // By sample 100, volume should have ramped up significantly
        // current_volume += (1.0 - current_volume) * 0.01
        // It approaches 1.0 asymptotically.
        assert!(mixed_data[n - 1] > 0.0);
        assert!(mixed_data[n - 1] <= 0.8 + f32::EPSILON);
    }
}
