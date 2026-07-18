use zako3_types::hq::UserId;

use crate::cache_repo::CacheRepositoryRef;

/// Freshness window for a user's cached accessible tap-name list. Autocomplete
/// tolerates brief staleness, and the value is a per-user accessible set (a single
/// tap change touches many users' keys), so precise invalidation is impractical —
/// the short TTL bounds staleness instead.
const TAP_NAMES_TTL_SECS: u64 = 30;

/// Redis-backed cache of each user's accessible tap-name list, shared across all
/// bot replicas. Mirrors the `UserSettingsStateService` pattern: a thin typed
/// wrapper over `CacheRepository` that owns key namespacing and serialization.
#[derive(Clone)]
pub struct TapNamesCacheService {
    cache_repository: CacheRepositoryRef,
}

impl TapNamesCacheService {
    pub fn new(cache_repository: CacheRepositoryRef) -> Self {
        Self { cache_repository }
    }

    fn key(user_id: &Option<UserId>) -> String {
        match user_id {
            Some(id) => format!("tap_names:{}", id.0),
            None => "tap_names:anon".to_string(),
        }
    }

    pub async fn get_cached(&self, user_id: &Option<UserId>) -> Option<Vec<String>> {
        let raw = self.cache_repository.get(&Self::key(user_id)).await?;
        serde_json::from_str(&raw).ok()
    }

    pub async fn set_cached(&self, user_id: &Option<UserId>, names: &[String]) {
        if let Ok(raw) = serde_json::to_string(names) {
            self.cache_repository
                .set_ex(&Self::key(user_id), &raw, TAP_NAMES_TTL_SECS)
                .await;
        }
    }
}
