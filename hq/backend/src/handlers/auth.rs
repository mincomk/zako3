use axum::{Json, extract::State};
use hq_core::Service;
use hq_types::{AuthCallbackDto, AuthResponseDto};
use std::sync::Arc;
use utoipa;

#[utoipa::path(
    post,
    path = "/api/v1/auth/login",
    request_body = AuthCallbackDto,
    responses(
        (status = 200, description = "Login successful", body = AuthResponseDto)
    )
)]
pub async fn login_handler(
    State(service): State<Arc<Service>>,
    Json(payload): Json<AuthCallbackDto>,
) -> Result<Json<AuthResponseDto>, (axum::http::StatusCode, String)> {
    let token = service
        .auth
        .authenticate(&payload.code)
        .await
        .map_err(|e| (axum::http::StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;

    Ok(Json(AuthResponseDto { token }))
}
