use std::{collections::HashMap, sync::Arc};

use tokio::sync::mpsc::Sender;
use tracing::instrument;
use zako3_audio_engine_types::SessionState;

use crate::{
    ArcStateService, ArcTapHubService,
    audio::{ArcDecoder, ArcMixer},
    error::ZakoResult,
    modify_state_session,
    types::{
        AudioRequest, AudioRequestString, AudioStopFilter, CachedAudioRequest, GuildId, QueueName,
        TapName, Track, TrackId, Volume,
    },
    util::id_gen,
};

pub struct SessionControl {
    pub guild_id: GuildId,

    pub(crate) mixer: ArcMixer,
    pub(crate) decoder: ArcDecoder,

    pub(crate) end_tx: Sender<TrackId>,

    pub(crate) state_service: ArcStateService,
    pub(crate) taphub_service: ArcTapHubService,
}

impl SessionControl {
    fn new(
        guild_id: GuildId,
        end_tx: Sender<TrackId>,
        mixer: ArcMixer,
        decoder: ArcDecoder,
        state_service: ArcStateService,
        taphub_service: ArcTapHubService,
    ) -> Self {
        SessionControl {
            guild_id,
            mixer,
            decoder,
            end_tx,
            state_service,
            taphub_service,
        }
    }

    #[instrument(skip(self))]
    pub async fn play(
        &self,
        queue_name: QueueName,
        tap_name: TapName,
        request: AudioRequestString,
        volume: Volume,
    ) -> ZakoResult<TrackId> {
        tracing::debug!(
            "Playing audio in guild {:?}, queue {:?}, tap {:?}, request {:?}, volume {:?}",
            self.guild_id,
            queue_name,
            tap_name,
            request,
            volume
        );

        let track_id: TrackId = id_gen::generate_id();

        let ar = AudioRequest {
            tap_name: tap_name.clone(),
            request: request.clone(),
        };

        let meta = self.taphub_service.request_audio_meta(ar.clone()).await?;

        modify_state_session(&self.state_service, self.guild_id, move |session| {
            let track = Track {
                track_id,
                description: meta.description,
                request: CachedAudioRequest {
                    tap_name,
                    audio_request: request,
                    cache_key: meta.cache_key,
                },
                volume,
                queue_name: queue_name.clone(),
            };

            upsert_track(&mut session.queues, queue_name.clone(), track);
        })
        .await?;

        self.reconcile().await?;

        Ok(track_id)
    }

    #[instrument(skip(self))]
    pub async fn set_volume(&self, track_id: TrackId, volume: Volume) -> ZakoResult<()> {
        self.mixer.set_volume(track_id, volume.into());
        modify_state_session(&self.state_service, self.guild_id, move |session| {
            if let Some(track) = session.find_track_mut(track_id) {
                track.volume = volume;
            }
        })
        .await?;
        Ok(())
    }

    #[instrument(skip(self))]
    pub async fn stop(&self, track_id: TrackId) -> ZakoResult<()> {
        self.mixer.remove_source(track_id);
        modify_state_session(&self.state_service, self.guild_id, move |session| {
            session.remove_track(track_id);
        })
        .await?;

        self.reconcile().await?;

        Ok(())
    }

    #[instrument(skip(self))]
    pub async fn stop_many(&self, filter: AudioStopFilter) -> ZakoResult<()> {
        let mut session = self.state_service.get_session(self.guild_id).await?;
        let track_ids = session
            .as_ref()
            .map(|s| match filter {
                AudioStopFilter::All => s.get_all_track_ids(),
                AudioStopFilter::Music => s.get_all_track_ids_by_queue_name_prefix("music"),
                AudioStopFilter::TTS(user_id) => {
                    s.get_all_track_ids_by_queue_name_prefix(&format!("tts_{}", u64::from(user_id)))
                }
            })
            .unwrap_or_default();

        for track_id in track_ids {
            self.mixer.remove_source(track_id);

            if let Some(session) = session.as_mut() {
                session.remove_track(track_id);
            }
        }

        if let Some(session) = session {
            self.state_service.save_session(&session).await?;
        }

        self.reconcile().await?;

        Ok(())
    }

    #[instrument(skip(self))]
    pub async fn next_music(&self) -> ZakoResult<()> {
        let music_tracks = self
            .state_service
            .get_session(self.guild_id)
            .await?
            .map(|s| {
                s.get_all_track_ids_by_queue_name_prefix("music")
                    .into_iter()
                    .filter_map(|tid| s.find_track(tid).cloned())
                    .collect::<Vec<Track>>()
            })
            .unwrap_or_default();

        // two tracks: current and next
        if music_tracks.len() < 2 {
            return Ok(());
        }

        let current_track_id = music_tracks[0].track_id;
        self.mixer.remove_source(current_track_id);
        modify_state_session(&self.state_service, self.guild_id, move |session| {
            session.remove_track(current_track_id);
        })
        .await?;

        self.reconcile().await?;

        Ok(())
    }

    #[instrument(skip(self))]
    pub async fn session_state(&self) -> ZakoResult<Option<SessionState>> {
        self.state_service.get_session(self.guild_id).await
    }

    /// Reconcile the session state with the mixer state
    #[instrument(skip(self))]
    async fn reconcile(&self) -> ZakoResult<()> {
        let session = self.state_service.get_session(self.guild_id).await?;

        if let Some(session) = session {
            let active_tracks = session.get_active_tracks();

            for track in active_tracks {
                if !self.mixer.has_source(track.track_id).await {
                    self.play_now(track).await?;
                }
            }
        }

        Ok(())
    }

    #[instrument(skip(self))]
    async fn play_now(&self, track: Track) -> ZakoResult<()> {
        tracing::debug!(
            "Starting playback for track {:?} in guild {:?}",
            track.track_id,
            self.guild_id
        );

        let response = self
            .taphub_service
            .request_audio(track.request.clone())
            .await?;

        let consumer = self
            .decoder
            .start_decoding(track.track_id, response.stream)
            .await?;

        self.mixer
            .add_source(track.track_id, consumer, self.end_tx.clone());
        self.mixer.set_volume(track.track_id, track.volume.into());

        Ok(())
    }

    async fn handle_ended_track(&self, track_id: TrackId) -> ZakoResult<()> {
        self.stop(track_id).await?;
        self.preload_if_possible(track_id).await?;

        Ok(())
    }

    async fn preload_if_possible(&self, track_id: TrackId) -> ZakoResult<()> {
        let session = self.state_service.get_session(self.guild_id).await?;

        if let Some(session) = session {
            let queue_name = match session.find_track(track_id) {
                Some(track) => track.queue_name.clone(),
                None => return Ok(()),
            };

            let track_ids = session.get_all_track_ids_by_queue_name(&queue_name);
            if track_ids.len() >= 2 {
                let next_track_id = track_ids[1];
                let next_track = session.find_track(next_track_id);
                if let Some(track) = next_track {
                    self.taphub_service
                        .preload_audio(track.request.clone())
                        .await?;
                }
            }
        }

        Ok(())
    }
}

fn upsert_track(queues: &mut HashMap<QueueName, Vec<Track>>, queue_name: QueueName, track: Track) {
    if let Some(queue) = queues.get_mut(&queue_name) {
        queue.push(track);
    } else {
        queues.insert(queue_name.clone(), vec![track]);
    }
}

pub fn create_session_control(
    guild_id: GuildId,
    mixer: ArcMixer,
    decoder: ArcDecoder,
    state_service: ArcStateService,
    taphub_service: ArcTapHubService,
) -> Arc<SessionControl> {
    let (end_tx, end_rx) = tokio::sync::mpsc::channel(16);

    let session_control = Arc::new(SessionControl::new(
        guild_id,
        end_tx,
        mixer,
        decoder,
        state_service,
        taphub_service,
    ));

    let sc_clone = session_control.clone();
    tokio::spawn(async move {
        let mut end_rx = end_rx;
        while let Some(track_id) = end_rx.recv().await {
            if let Err(e) = sc_clone.handle_ended_track(track_id).await {
                tracing::warn!("Failed to handle ended track {:?}: {:?}", track_id, e);
            }
        }
    });

    session_control
}
