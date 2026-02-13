use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TextMappingRule {
    pub pattern: String,
    pub replacement: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmojiMappingRule {
    pub emoji_id: String,
    pub emoji_name: String,
    pub emoji_image_url: String,
    pub replacement: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Settings {
    pub text_mappings: Vec<TextMappingRule>,
    pub emoji_mappings: Vec<EmojiMappingRule>,
    pub read_text_even_not_in_vc: bool,
}
