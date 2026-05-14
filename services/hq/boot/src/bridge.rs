use futures_util::StreamExt;
use hq_core::PlaybackEvent;
use tokio::sync::broadcast;
use zako3_states::RedisPubSub;

pub async fn run_history_bridge(
    redis_url: String,
    event_tx: broadcast::Sender<PlaybackEvent>,
    stats_tx: broadcast::Sender<()>,
) {
    match RedisPubSub::new(&redis_url).await {
        Ok(pubsub) => match pubsub.subscribe_history().await {
            Ok(stream) => {
                let mut stream = Box::pin(stream);
                while stream.next().await.is_some() {
                    let _ = event_tx.send(PlaybackEvent::PlaybackChanged);
                    let _ = stats_tx.send(());
                }
            }
            Err(e) => tracing::error!(%e, "Failed to subscribe Redis history channel"),
        },
        Err(e) => tracing::error!(%e, "Failed to connect Redis PubSub for history bridge"),
    }
}
