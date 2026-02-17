use axum::{Router, routing::post};
use hq_core::Service;
use std::sync::Arc;
use tower_http::cors::CorsLayer;
use tower_http::trace::TraceLayer;
use utoipa::OpenApi;
use utoipa_swagger_ui::SwaggerUi;

pub mod handlers;
pub mod middleware;

use handlers::auth;
use handlers::tap;

#[derive(OpenApi)]
#[openapi(
    paths(
        handlers::auth::login_handler,
        handlers::tap::create_tap,
        handlers::tap::list_taps,
    ),
    components(
        schemas(
            hq_types::CreateTapDto,
            hq_types::AuthCallbackDto,
            hq_types::AuthResponseDto,
            hq_types::Tap,
            hq_types::TapId,
            hq_types::TapName,
            hq_types::TapOccupation,
            hq_types::TapPermission,
            hq_types::TapRole,
            hq_types::UserId,
            hq_types::ResourceTimestamp,
        )
    ),
    tags(
        (name = "hq", description = "HQ API")
    ),
    security(
        ("bearer_auth" = [])
    )
)]
pub struct ApiDoc;

pub fn app(service: Service) -> Router {
    let state = Arc::new(service);

    Router::new()
        .merge(SwaggerUi::new("/swagger-ui").url("/api-docs/openapi.json", ApiDoc::openapi()))
        .route("/api/v1/auth/login", post(auth::login_handler))
        .route("/api/v1/taps", post(tap::create_tap).get(tap::list_taps))
        .layer(TraceLayer::new_for_http())
        .layer(CorsLayer::permissive())
        .with_state(state)
}
