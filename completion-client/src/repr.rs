use serde::{Deserialize, Serialize};
use serde_json::Number;

#[derive(Deserialize)]
pub struct Prompt {
    name: String,
    language: String,
    prompt: String,
    pub original: String,
    tests: String,
    stop_tokens: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub name: String,
    pub language: String,
    pub prompt: String,
    pub tests: String,
    pub completion: String,
    pub attempts: u32,
    pub stop_tokens: Vec<String>,
    pub original: String,
}

#[derive(Serialize, Debug)]
pub struct PromptMessage {
    inputs: String,
    parameters: InferenceParams,
}

#[derive(Serialize, Debug)]
struct InferenceParams {
    best_of: usize,
    decoder_input_details: bool,
    details: bool,
    do_sample: bool,
    max_new_tokens: i32,
    repetition_penalty: Option<f64>,
    return_full_text: Option<bool>,
    seed: Option<i64>,
    stop: Vec<String>,
    temperature: Option<f64>,
    top_k: Option<i32>,
    top_p: Option<f64>,
    truncate: Option<usize>,
    typical_p: Option<f64>,
    watermark: bool,
}

#[derive(Deserialize, Debug)]
pub struct EvalResult {
    pub status: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct DatasetOutput {
    content: String,
    pub path: String,
    attempts: Number,
}

impl From<Prompt> for Program {
    fn from(value: Prompt) -> Self {
        Program {
            name: value.name,
            language: value.language,
            prompt: value.prompt,
            tests: value.tests,
            completion: String::from(""),
            attempts: 0,
            stop_tokens: value.stop_tokens,
            original: value.original,
        }
    }
}
impl From<Program> for PromptMessage {
    fn from(value: Program) -> Self {
        let inputs = value.prompt;
        let parameters = InferenceParams {
            best_of: 1,
            decoder_input_details: false,
            details: false,
            do_sample: true,
            max_new_tokens: 512,
            repetition_penalty: None,
            return_full_text: None,
            seed: None,
            stop: value.stop_tokens,
            temperature: Some(0.8),
            truncate: None,
            top_k: None,
            top_p: Some(0.95),
            typical_p: None,
            watermark: false,
        };
        PromptMessage { inputs, parameters }
    }
}

impl From<Program> for DatasetOutput {
    fn from(value: Program) -> Self {
        let Program {
            original,
            prompt,
            completion,
            attempts,
            ..
        } = value;
        let content = format!("{prompt}\n{completion}");
        DatasetOutput {
            content,
            path: original,
            attempts: Number::from(attempts),
        }
    }
}

impl Program {
    pub fn inc_attempts(&mut self, attempt_limit: u32) -> Option<()> {
        if self.attempts < attempt_limit {
            self.attempts = self.attempts + 1;
            Some(())
        } else {
            None
        }
    }
}
