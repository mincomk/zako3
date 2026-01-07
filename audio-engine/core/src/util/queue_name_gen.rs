use crate::types::{QueueName, UserId};

pub fn music() -> QueueName {
    "music".to_string().into()
}

pub fn tts(user_id: UserId) -> QueueName {
    format!("tts_{}", u64::from(user_id)).into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::UserId;

    #[test]
    fn test_music_queue_name() {
        assert_eq!(music(), QueueName::from("music".to_string()));
    }

    #[test]
    fn test_tts_queue_name() {
        let user_id = UserId::from(123456789);
        assert_eq!(tts(user_id), QueueName::from("tts_123456789".to_string()));
    }
}
