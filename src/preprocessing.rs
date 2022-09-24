use std::collections::HashMap;
use regex::Regex;

use crate::lexer::{Macro, Lexer};


pub struct Preprocessor {

}

fn substring(value: &str, start: usize, end: usize) -> &str {
    value
        .split_at(start).1
        .split_at(end).0
}

fn substring_upto(value: &str, end: usize) -> &str {
    value
        .split_at(end).0
}

fn substring_from(value: &str, start: usize) -> &str {
    value
        .split_at(start).1
}

impl Preprocessor {
    pub fn preprocess(content: &str, as_fragment: bool) -> Result<(HashMap<String, Macro>, usize), String>{
        let mut cursor: usize = 0;

        let mut macros = HashMap::<String, Macro>::new();

        while let Some(search_pos) = substring_from(content, cursor).find("*") {
            let macro_start = cursor + search_pos;
            if let Some(name_length) = substring_from(content, macro_start).find("{") {
                let name = substring(content, macro_start+1, name_length-1).trim();

                println!("Macro: `{}`", name);

                let name_end = macro_start + name_length;

                if let Some(body_length) = content.split_at(name_end).1.find("}") {
                    let body = substring(content, name_end + 1, body_length - 1);

                    let mut lexer = Lexer::new(body.to_string(), true)?;
                    let tokens = lexer.generar_tokens()?;
                    
                    let the_macro = Macro {
                        name: name.to_string(),
                        tokens
                    };

                    macros.insert(name.to_string(), the_macro);

                    cursor = name_end + body_length;
                } else {
                    return Err("Macro invalida. Se esperaba un `}` para cerrar el cuerpo.".into());
                }
            } else {
                return Err("Macro invalida. Se esperaba un `{` para abrir el cuerpo.".into());
            }
        }

        if as_fragment {
            Ok((macros, 0))
        } else {
            let program_bounds = Preprocessor::find_program_bounds(content)?;
            Ok((macros, program_bounds.0))
        }
    }

    fn find_program_bounds(content: &str) -> Result<(usize, usize), String> {
        lazy_static::lazy_static! {
            static ref HOLARE: Regex = Regex::new(r"(^hola)|(\n\s*hola)").unwrap();
            static ref ADIOSRE: Regex = Regex::new(r"adios").unwrap();
        }

        let holas: Vec<_> = HOLARE.find_iter(content).collect();

        if holas.len() > 1 {
            Err("Se esperaba un solo inicio del programa (`hola`), pero hay mas de uno".to_string())
        } else if holas.len() < 1 {
            Err("No se encontró un inicio del programa (`hola`)".to_string())
        } else {
            let start = holas.first().unwrap().start();

            let from_start = substring_from(content, start);

            let adios_tag = "adios";
            if let Some(adios) = from_start.find(adios_tag) {
                Ok((start, start + adios + adios_tag.len()))
            } else {
                Err("No se encontró el fin del programa (`adios`)".to_string())
            }
        }
    }
}