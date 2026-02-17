use crate::CoreResult;
use async_trait::async_trait;
use hq_types::{Tap, TapId, TapName, UserId};
use sqlx::{PgPool, Row};
use uuid::Uuid;

#[async_trait]
pub trait TapRepository: Send + Sync {
    async fn create(&self, tap: &Tap) -> CoreResult<Tap>;
    async fn list_by_owner(&self, owner_id: Uuid) -> CoreResult<Vec<Tap>>;
}

pub struct PgTapRepository {
    pool: PgPool,
}

impl PgTapRepository {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }
}

#[async_trait]
impl TapRepository for PgTapRepository {
    async fn create(&self, tap: &Tap) -> CoreResult<Tap> {
        let id: Uuid = tap.id.0;
        let owner_id: Uuid = tap.owner_id.0;
        let name: String = tap.name.0.clone();
        let description = tap.description.clone();
        let occupation: String = serde_json::to_string(&tap.occupation)
            .unwrap_or_else(|_| "base".to_string())
            .trim_matches('"')
            .to_string(); // Simple serialization for TEXT
        let permission =
            serde_json::to_value(&tap.permission).unwrap_or(serde_json::json!("owner_only"));
        let role = tap.role.as_ref().map(|r| {
            serde_json::to_string(r)
                .unwrap_or_default()
                .trim_matches('"')
                .to_string()
        });

        sqlx::query(
            r#"
            INSERT INTO taps (id, owner_id, name, description, occupation, permission, role, created_at, updated_at)
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
            "#,
        )

        .bind(id)
        .bind(owner_id)
        .bind(name)
        .bind(description)
        .bind(occupation)
        .bind(permission)
        .bind(role)
        .bind(tap.timestamp.created_at)
        .bind(tap.timestamp.updated_at)
        .execute(&self.pool)
        .await?;

        Ok(tap.clone())
    }

    async fn list_by_owner(&self, owner_id: Uuid) -> CoreResult<Vec<Tap>> {
        let rows = sqlx::query(
            r#"
            SELECT id, owner_id, name, description, occupation, permission, role, created_at, updated_at
            FROM taps
            WHERE owner_id = $1
            "#,
        )
        .bind(owner_id)
        .fetch_all(&self.pool)
        .await?;

        let taps = rows
            .into_iter()
            .map(|row| {
                let id: Uuid = row.try_get("id").unwrap();
                let owner_id: Uuid = row.try_get("owner_id").unwrap();
                let name: String = row.try_get("name").unwrap();
                let description: Option<String> = row.try_get("description").unwrap_or(None);

                let occupation_str: String = row
                    .try_get("occupation")
                    .unwrap_or_else(|_| "base".to_string());
                let occupation = serde_json::from_str(&format!("\"{}\"", occupation_str))
                    .unwrap_or(hq_types::TapOccupation::Base);

                let permission_val: serde_json::Value = row
                    .try_get("permission")
                    .unwrap_or(serde_json::json!("owner_only"));
                let permission = serde_json::from_value(permission_val)
                    .unwrap_or(hq_types::TapPermission::OwnerOnly);

                let role_str: Option<String> = row.try_get("role").unwrap_or(None);
                let role = role_str.and_then(|r| serde_json::from_str(&format!("\"{}\"", r)).ok());

                let created_at: chrono::DateTime<chrono::Utc> = row.try_get("created_at").unwrap();
                let updated_at: chrono::DateTime<chrono::Utc> = row.try_get("updated_at").unwrap();

                Tap {
                    id: TapId(id),
                    name: TapName(name),
                    description,
                    owner_id: UserId(owner_id),
                    occupation,
                    permission,
                    role,
                    timestamp: hq_types::ResourceTimestamp {
                        created_at,
                        updated_at,
                    },
                }
            })
            .collect();

        Ok(taps)
    }
}
