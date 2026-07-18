use rcgen::generate_simple_self_signed;
use rustls::pki_types::{CertificateDer, PrivateKeyDer};
use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc;

use std::collections::HashMap;

use zako3_taphub_transport_client::TransportClient;
use zako3_taphub_transport_server::{TapHubBridgeHandler, Timestamp, TransportServer};
use zako3_types::{
    AudioCachePolicy, AudioCacheType, AudioMetaResponse, AudioMetadata, AudioRequest,
    CachedAudioRequest, TapHubError,
};

struct MockHandler;

#[async_trait::async_trait]
impl TapHubBridgeHandler for MockHandler {
    async fn handle_request_audio(
        &self,
        req: CachedAudioRequest,
        _headers: HashMap<String, String>,
    ) -> Result<(AudioMetaResponse, mpsc::Receiver<(Timestamp, bytes::Bytes)>), TapHubError> {
        let (tx, rx) = mpsc::channel(10);

        let meta = AudioMetaResponse {
            cache_key: req.cache_key.clone(),
            metadatas: vec![
                AudioMetadata::Title("Test Title".to_string()),
                AudioMetadata::Artist("Test Artist".to_string()),
            ],
            base_volume: 1.0,
        };

        tokio::spawn(async move {
            let _ = tx;
        });

        Ok((meta, rx))
    }

    async fn handle_preload_audio(
        &self,
        req: CachedAudioRequest,
        _headers: HashMap<String, String>,
    ) -> Result<AudioMetaResponse, TapHubError> {
        Ok(AudioMetaResponse {
            cache_key: req.cache_key.clone(),
            metadatas: vec![
                AudioMetadata::Title("Preload Title".to_string()),
                AudioMetadata::Artist("Preload Artist".to_string()),
            ],
            base_volume: 1.0,
        })
    }

    async fn handle_request_audio_meta(
        &self,
        _req: AudioRequest,
        _headers: HashMap<String, String>,
    ) -> Result<AudioMetaResponse, TapHubError> {
        Ok(AudioMetaResponse {
            cache_key: AudioCachePolicy {
                cache_type: AudioCacheType::CacheKey("meta_key".to_string()),
                ttl_seconds: None,
            },
            metadatas: vec![
                AudioMetadata::Title("Meta Title".to_string()),
                AudioMetadata::Artist("Meta Artist".to_string()),
            ],
            base_volume: 1.0,
        })
    }

    async fn handle_invalidate_cache(
        &self,
        _req: CachedAudioRequest,
        _headers: HashMap<String, String>,
    ) -> Result<(), TapHubError> {
        Ok(())
    }
}

fn generate_certs() -> (Vec<CertificateDer<'static>>, PrivateKeyDer<'static>) {
    let subject_alt_names = vec!["localhost".to_string()];
    let cert = generate_simple_self_signed(subject_alt_names).unwrap();
    let cert_der = CertificateDer::from(cert.cert.der().to_vec());
    let key_der = PrivateKeyDer::try_from(cert.key_pair.serialize_der()).unwrap();
    (vec![cert_der], key_der)
}

#[tokio::test]
async fn test_transport_integration() {
    let _ = rustls::crypto::ring::default_provider().install_default();

    let (server_certs, server_key) = generate_certs();
    let client_certs = server_certs.clone();

    let server_addr: SocketAddr = "127.0.0.1:0".parse().unwrap();

    let mut server =
        TransportServer::new(server_addr, server_certs, server_key, Arc::new(MockHandler))
            .expect("Failed to create server");

    let bound_addr = server.local_addr().expect("Failed to get local address");

    tokio::spawn(async move {
        server.run().await;
    });

    tokio::time::sleep(Duration::from_millis(100)).await;

    let client_bind_addr: SocketAddr = "127.0.0.1:0".parse().unwrap();
    let client = TransportClient::connect(
        client_bind_addr,
        &bound_addr.to_string(),
        "localhost".to_string(),
        client_certs,
        Duration::from_secs(10),
    )
    .await
    .expect("Failed to connect client");

    let req = CachedAudioRequest {
        tap_id: zako3_types::hq::TapId("test_tap_id".to_string()),
        audio_request: "yt:preload".to_string().into(),
        cache_key: AudioCachePolicy {
            cache_type: AudioCacheType::CacheKey("preload_key".to_string()),
            ttl_seconds: None,
        },
        discord_user_id: "123".to_string().into(),
        headers: HashMap::new(),
    };

    let resp = client
        .preload_audio(req)
        .await
        .expect("preload_audio failed");
    if let AudioCacheType::CacheKey(k) = resp.cache_key.cache_type {
        assert_eq!(k, "preload_key");
    } else {
        panic!("Expected CacheKey");
    }

    // Check title in metadatas
    let mut found_title = false;
    for m in resp.metadatas {
        if let AudioMetadata::Title(t) = m {
            assert_eq!(t, "Preload Title");
            found_title = true;
        }
    }
    assert!(found_title);

    let meta_req = AudioRequest {
        tap_id: zako3_types::hq::TapId("test_tap_id".to_string()),
        request: "yt:meta".to_string().into(),
        discord_user_id: "123".to_string().into(),
        headers: HashMap::new(),
    };
    let meta_resp = client
        .request_audio_meta(meta_req)
        .await
        .expect("request_audio_meta failed");
    if let AudioCacheType::CacheKey(k) = meta_resp.cache_key.cache_type {
        assert_eq!(k, "meta_key");
    } else {
        panic!("Expected CacheKey");
    }
}

struct StallOnceHandler {
    calls: Arc<std::sync::atomic::AtomicUsize>,
}

#[async_trait::async_trait]
impl TapHubBridgeHandler for StallOnceHandler {
    async fn handle_request_audio(
        &self,
        _req: CachedAudioRequest,
        _headers: HashMap<String, String>,
    ) -> Result<(AudioMetaResponse, mpsc::Receiver<(Timestamp, bytes::Bytes)>), TapHubError> {
        Err(TapHubError::Internal("unused".to_string()))
    }

    async fn handle_preload_audio(
        &self,
        _req: CachedAudioRequest,
        _headers: HashMap<String, String>,
    ) -> Result<AudioMetaResponse, TapHubError> {
        Err(TapHubError::Internal("unused".to_string()))
    }

    async fn handle_request_audio_meta(
        &self,
        _req: AudioRequest,
        _headers: HashMap<String, String>,
    ) -> Result<AudioMetaResponse, TapHubError> {
        let call = self
            .calls
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        if call == 0 {
            tokio::time::sleep(Duration::from_secs(60)).await;
        }
        Ok(AudioMetaResponse {
            cache_key: AudioCachePolicy {
                cache_type: AudioCacheType::CacheKey("retry_key".to_string()),
                ttl_seconds: None,
            },
            metadatas: vec![AudioMetadata::Title("Retry Title".to_string())],
            base_volume: 1.0,
        })
    }

    async fn handle_invalidate_cache(
        &self,
        _req: CachedAudioRequest,
        _headers: HashMap<String, String>,
    ) -> Result<(), TapHubError> {
        Ok(())
    }
}

struct AlwaysErrHandler {
    calls: Arc<std::sync::atomic::AtomicUsize>,
}

#[async_trait::async_trait]
impl TapHubBridgeHandler for AlwaysErrHandler {
    async fn handle_request_audio(
        &self,
        _req: CachedAudioRequest,
        _headers: HashMap<String, String>,
    ) -> Result<(AudioMetaResponse, mpsc::Receiver<(Timestamp, bytes::Bytes)>), TapHubError> {
        Err(TapHubError::Internal("unused".to_string()))
    }

    async fn handle_preload_audio(
        &self,
        _req: CachedAudioRequest,
        _headers: HashMap<String, String>,
    ) -> Result<AudioMetaResponse, TapHubError> {
        Err(TapHubError::Internal("unused".to_string()))
    }

    async fn handle_request_audio_meta(
        &self,
        _req: AudioRequest,
        _headers: HashMap<String, String>,
    ) -> Result<AudioMetaResponse, TapHubError> {
        self.calls
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        Err(TapHubError::TapNotFound("nope".to_string()))
    }

    async fn handle_invalidate_cache(
        &self,
        _req: CachedAudioRequest,
        _headers: HashMap<String, String>,
    ) -> Result<(), TapHubError> {
        Ok(())
    }
}

async fn spawn_server(
    handler: Arc<dyn TapHubBridgeHandler>,
) -> (SocketAddr, Vec<CertificateDer<'static>>) {
    let (server_certs, server_key) = generate_certs();
    let client_certs = server_certs.clone();

    let server_addr: SocketAddr = "127.0.0.1:0".parse().unwrap();
    let mut server = TransportServer::new(server_addr, server_certs, server_key, handler)
        .expect("Failed to create server");
    let bound_addr = server.local_addr().expect("Failed to get local address");

    tokio::spawn(async move {
        server.run().await;
    });
    tokio::time::sleep(Duration::from_millis(100)).await;

    (bound_addr, client_certs)
}

fn meta_request() -> AudioRequest {
    AudioRequest {
        tap_id: zako3_types::hq::TapId("test_tap_id".to_string()),
        request: "yt:meta".to_string().into(),
        discord_user_id: "123".to_string().into(),
        headers: HashMap::new(),
    }
}

/// A per-attempt timeout retries the request on a fresh stream (the connection is left
/// intact — no global reset). The first server call stalls past the 500ms per-attempt
/// timeout, so the client retries and the second call succeeds.
#[tokio::test]
async fn test_request_timeout_retries_on_fresh_stream() {
    let _ = rustls::crypto::ring::default_provider().install_default();

    let calls = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let (bound_addr, client_certs) = spawn_server(Arc::new(StallOnceHandler {
        calls: calls.clone(),
    }))
    .await;

    let client = TransportClient::connect(
        "127.0.0.1:0".parse().unwrap(),
        &bound_addr.to_string(),
        "localhost".to_string(),
        client_certs,
        Duration::from_millis(500),
    )
    .await
    .expect("Failed to connect client");

    let start = std::time::Instant::now();
    let resp = client
        .request_audio_meta(meta_request())
        .await
        .expect("expected retry to succeed after first attempt stalls");

    assert!(start.elapsed() >= Duration::from_millis(500));
    assert_eq!(calls.load(std::sync::atomic::Ordering::SeqCst), 2);
    if let AudioCacheType::CacheKey(k) = resp.cache_key.cache_type {
        assert_eq!(k, "retry_key");
    } else {
        panic!("Expected CacheKey");
    }
}

/// A request that stalls (and is retried) must not tear down a concurrent request:
/// with the old shared-connection `reset()` the sibling's stream would be killed, but
/// per-request isolation keeps it alive. Both requests succeed.
#[tokio::test]
async fn test_stalled_request_does_not_disturb_concurrent_request() {
    let _ = rustls::crypto::ring::default_provider().install_default();

    let calls = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let (bound_addr, client_certs) = spawn_server(Arc::new(StallOnceHandler {
        calls: calls.clone(),
    }))
    .await;

    let client = TransportClient::connect(
        "127.0.0.1:0".parse().unwrap(),
        &bound_addr.to_string(),
        "localhost".to_string(),
        client_certs,
        Duration::from_millis(500),
    )
    .await
    .expect("Failed to connect client");

    // Fire two requests concurrently. Whichever the server sees first stalls once and is
    // retried; the other is served straight away. Neither should be torn down.
    let (a, b) = tokio::join!(
        client.request_audio_meta(meta_request()),
        client.request_audio_meta(meta_request()),
    );

    a.expect("first concurrent request should succeed");
    b.expect("second concurrent request should succeed");
}

#[tokio::test]
async fn test_app_error_is_not_retried() {
    let _ = rustls::crypto::ring::default_provider().install_default();

    let calls = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let (bound_addr, client_certs) = spawn_server(Arc::new(AlwaysErrHandler {
        calls: calls.clone(),
    }))
    .await;

    let client = TransportClient::connect(
        "127.0.0.1:0".parse().unwrap(),
        &bound_addr.to_string(),
        "localhost".to_string(),
        client_certs,
        Duration::from_millis(500),
    )
    .await
    .expect("Failed to connect client");

    let err = client
        .request_audio_meta(meta_request())
        .await
        .expect_err("expected app-level error");

    assert!(matches!(err, TapHubError::TapNotFound(_)));
    assert_eq!(calls.load(std::sync::atomic::Ordering::SeqCst), 1);
}
