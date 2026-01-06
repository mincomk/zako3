use std::result::Result;
use tonic::{Request, Response, Status};

use crate::proto::audio_engine_server::AudioEngine;

pub mod interface;
pub use interface::*;

mod proto {
    tonic::include_proto!("audio_engine");
}

#[derive(Debug, Clone)]
struct AudioEngineServer {}

#[tonic::async_trait]
impl AudioEngine for AudioEngineServer {
    async fn join(
        &self,
        request: Request<proto::JoinRequest>,
    ) -> Result<Response<proto::OkResponse>, Status> {
        todo!()
    }

    async fn leave(
        &self,
        request: Request<proto::LeaveRequest>,
    ) -> Result<Response<proto::OkResponse>, Status> {
        todo!()
    }

    async fn play(
        &self,
        request: Request<proto::PlayRequest>,
    ) -> Result<Response<proto::PlayResponse>, Status> {
        todo!()
    }

    async fn set_volume(
        &self,
        request: Request<proto::SetVolumeRequest>,
    ) -> Result<Response<proto::OkResponse>, Status> {
        todo!()
    }

    async fn stop(
        &self,
        request: Request<proto::StopRequest>,
    ) -> Result<Response<proto::OkResponse>, Status> {
        todo!()
    }

    async fn stop_many(
        &self,
        request: Request<proto::StopManyRequest>,
    ) -> Result<Response<proto::OkResponse>, Status> {
        todo!()
    }

    async fn next_music(
        &self,
        request: Request<proto::NextMusicRequest>,
    ) -> Result<Response<proto::OkResponse>, Status> {
        todo!()
    }

    async fn get_session_state(
        &self,
        request: Request<proto::GetSessionStateRequest>,
    ) -> Result<Response<proto::SessionStateResponse>, Status> {
        todo!()
    }
}
