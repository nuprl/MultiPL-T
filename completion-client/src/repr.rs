use serde::{Deserialize, Serialize};

#[derive(Debug, Clone)]
pub enum Lang {
    Python,
    Lua,
    Racket,
    Typescript,
    OCaml,
}

#[derive(Deserialize)]
pub struct Prompt {
    name: String,
    language: String,
    prompt: String,
    original: String,
    prompt_terminology: String,
    tests: String,
    stop_tokens: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub lang: Lang,
    pub prompt: String,
    pub tests: String,
    pub completion: String,
    pub attempts: usize,
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

#[derive(Serialize, Debug)]
pub struct DatasetOutput {
    content: String,
    path: String,
}

// TODO: Come up with temperature ratcheting heuristic
fn get_temp_from_attempts(attempts: usize) -> f64 {
    if attempts < 50 {
        0.2
    } else {
        0.8
    }
}

impl From<Program> for PromptMessage {
    fn from(value: Program) -> Self {
        let inputs = value.prompt;
        let parameters = InferenceParams {
            best_of: 1,
            decoder_input_details: false,
            details: false,
            do_sample: false,
            max_new_tokens: 512,
            repetition_penalty: None,
            return_full_text: None,
            seed: None,
            stop: value.stop_tokens,
            temperature: Some(get_temp_from_attempts(value.attempts)),
            truncate: None,
            top_k: Some(1),
            top_p: Some(0.95),
            typical_p: None,
            watermark: false,
        };
        PromptMessage { inputs, parameters }
    }
}
impl From<Prompt> for Program {
    fn from(value: Prompt) -> Self {
        let lang = match value.language.as_str() {
            "ml" => Lang::OCaml,
            "lua" => Lang::Lua,
            "rkt" => Lang::Racket,
            "ts" => Lang::Typescript,
            "py" => Lang::Python,
            _ => panic!("Unsupported language"),
        };
        let completion = String::from("");
        let attempts = 0;
        Program {
            lang,
            prompt: value.prompt,
            tests: value.tests,
            completion,
            attempts,
            stop_tokens: value.stop_tokens,
            original: value.original,
        }
    }
}

impl From<Program> for DatasetOutput {
    fn from(value: Program) -> Self {
        let Program {
            original,
            prompt,
            tests,
            completion,
            ..
        } = value;
        let content = format!("{prompt}\n{completion}\n{tests}");
        DatasetOutput {
            content,
            path: original,
        }
    }
}

impl Program {
    pub fn inc_attempts(&mut self) -> Option<()> {
        if self.attempts < 100 {
            self.attempts = self.attempts + 1;
            Some(())
        } else {
            None
        }
    }
}
