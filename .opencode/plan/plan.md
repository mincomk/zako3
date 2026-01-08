# Plan to Fix Audio Lag in Zako3

## Problem Analysis
The `audio-engine` in `zako3` is experiencing laggy audio. Comparing it with `zako2` (which works well) and analyzing the code in `zako3`, I've identified a critical bottleneck in `audio-engine/audio/src/stream_input.rs`.

### Root Cause: Blocking Buffer Fill
In `create_stream_input`, the loop waits to completely fill `float_buffer` (size 3840 samples) before processing and sending it downstream.

```rust
// Current implementation in stream_input.rs
while readed < float_buffer.len() {
    let r_len = consumer.pop_slice(&mut float_buffer[readed..]);
    // ... waits here if buffer isn't full ...
    readed += r_len;
}
```

This introduces unnecessary latency (lag) because the system holds onto audio samples until a full chunk is ready, rather than streaming them as they become available. This behavior is different from typical low-latency audio streaming where you want to push available samples as soon as possible.

## Proposed Changes

### 1. Optimize `stream_input.rs`
Modify the loop in `audio-engine/audio/src/stream_input.rs` to process whatever data is currently available in the consumer ring buffer, without waiting for a full buffer fill.

**Plan:**
1.  Remove the `while readed < float_buffer.len()` loop.
2.  Use `consumer.pop_slice(&mut float_buffer)` to read currently available samples.
3.  If 0 samples are read, perform the short sleep (1ms) to prevent busy-waiting.
4.  Process and write whatever amount of data (`len`) was read.

### 2. (Optional) Review Mixer Efficiency
While `zako2` uses SIMD for mixing, `zako3` uses scalar operations.
- `zako3` does per-sample volume interpolation: `source.current_volume += (source.target_volume - source.current_volume) * 0.01;`.
- This is computationally more expensive but smoother.
- *Decision:* We will focus on the `stream_input.rs` fix first as it addresses the latency/lag directly. The mixing efficiency is a separate performance optimization that likely isn't the primary cause of the "lag" unless the CPU is saturated.

## Verification
1.  Apply the fix to `audio-engine/audio/src/stream_input.rs`.
2.  Compile the project to ensure no regressions.
