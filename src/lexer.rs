use std::fmt::{Display};
use std::collections::HashMap;

use crate::preprocessing::Preprocessor;

#[derive(Debug, Clone)]
pub struct Span(usize, usize);

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub lexeme: String,
    pub kind: TokenKind,
    pub span: Span
}

#[derive(Debug, Clone)]
pub struct Macro {
    pub name: String,
    pub tokens: Vec<Token>
}

// Lexer - Analizador 'lexicográfico'?
pub struct Lexer {
    content: String,
    pos: usize,
    line: usize,
    col: usize,
    is_fragment: bool,
    macros: HashMap<String, Macro>,
    keywords: HashMap<String, TokenKind>
}

struct TokenizationState(usize, usize);

impl Lexer {
    pub fn new(content: String, is_fragment: bool) -> Result<Self, String> {
        let preprocessed = Preprocessor::preprocess(&content, is_fragment)?;
        Ok(Self {
            content: content,
            pos: preprocessed.1, // Posición al inicio de `hola`
            line: 1, // TODO: Calcular linea y columna de la posición inicial
            col: 0,
            is_fragment,
            macros: preprocessed.0,
            keywords: HashMap::from([
                ("hola".to_string(), TokenKind::Hola),
                ("adios".to_string(), TokenKind::Adios),
                ("por favor".to_string(), TokenKind::PorFavor),
                ("gracias".to_string(), TokenKind::Gracias),
                ("muestra".to_string(), TokenKind::Muestra),
                ("repite".to_string(), TokenKind::Repite),
                ("veces".to_string(), TokenKind::Veces),
                ("mientras".into(), TokenKind::Mientras),
                ("di".to_string(), TokenKind::Di),
                ("baja".to_string(), TokenKind::Baja),
                ("mas".into(), TokenKind::Mas),
                ("menos".into(), TokenKind::Menos),
                ("por".into(), TokenKind::Por),
                ("entre".into(), TokenKind::Entre),
                ("y".into(), TokenKind::Y),
                ("cierto".into(), TokenKind::Cierto),
                ("falso".into(), TokenKind::Falso),
            ])
        })
    }

    fn caracter_actual(&self) -> Option<char> {
        self.mirar_caracter(0)
    }

    fn caracter_es(&self, comp: char) -> bool {
        match self.caracter_actual() {
            Some(c) => c == comp,
            None => false
        }
    }

    fn mirar_caracter(&self, n: usize) -> Option<char> {
        if self.pos + n > self.content.len()-1 {
            None
        } else {
            self.content.chars().nth(self.pos + n)
        }
    }

    fn siguiente(&mut self) {
        self.col += 1;
        self.pos += 1
    }

    fn store_state(&self) -> TokenizationState {
        TokenizationState(self.col, self.pos)
    }

    fn restore_state(&mut self, t: TokenizationState) {
        self.col = t.0;
        self.pos = t.1;
    }

    fn ignorar_espacios(&mut self) {
        // Saltar mientras caracter sea un espacio.
        while let Some(x) = self.caracter_actual() {
            if x.is_whitespace() {
                if x == '\n' {
                    self.line += 1;
                    self.col = 0;
                }
                self.siguiente()
            }
            else { break }
        }
    }

    fn ignorar_comentarios(&mut self) {
        while self.caracter_es('(') {
            self.siguiente();

            while self.caracter_actual().is_some()
            && self.caracter_actual().unwrap() != ')' {
                self.siguiente();
            }
            
            if self.caracter_es(')') { self.siguiente(); }

            self.ignorar_espacios();
        }
    }

    fn es_inicio_identificador(&self, c: char) -> bool {
        c.is_alphabetic()
    }

    fn identificador(&mut self, prefix: &str) -> Result<Vec<Token>, String> {
        let span = Span(self.line, self.col);
        let mut result = String::new();
        result.push_str(prefix);

        while let Some(c) = self.caracter_actual() {
            if c.is_alphanumeric() {
                result.push(c);
                self.siguiente();
            } else {
                break
            }
        }
        
        if self.caracter_es(' ') && self.es_inicio_identificador(self.mirar_caracter(1).unwrap_or('\0')) {
            let prev_state = self.store_state();

            self.siguiente();
            let mut compose_prefix = result.clone();
            
            compose_prefix.push(' ');

            if let Ok(composed) = self.identificador(&compose_prefix) {
                return Ok(composed);
            }

            // En caso de que no se encuentre una compuesta,
            // regresar al estado anterior
            self.restore_state(prev_state);
        }
        
        if let Some(existing_macro) = self.macros.get(&result) {
            Ok(existing_macro.tokens.clone())
        } else if let Some(palabra_clave) = self.keywords.get(&result) {
            Ok(vec![Token{ lexeme: result, kind: palabra_clave.clone(), span: span }])
        } else {
            // Por ahora, cualquier identificador que no sea
            // Una palabra clave, es un error
            Err(format!("Identificador invalido: `{}`", result))
        }
    }

    fn texto(&mut self) -> Result<Token, String> {
        let span = Span(self.line, self.col);
        let mut result = String::new();

        self.siguiente();
        while let Some(c) = self.caracter_actual() {
            if c == '"' { break; }
            result.push(c);
            self.siguiente();
        }

        if self.caracter_es('"') {
            self.siguiente();
            Ok(Token { lexeme: result, kind: TokenKind::Texto, span: span })
        } else {
            Err("String no fue cerrado".to_string())
        }
    }

    fn numero(&mut self, first: char) -> Result<Token, String> {
        let span = Span(self.line, self.col);

        let mut result = String::new();
        result.push(first);

        self.siguiente();

        let mut punto_encontrado = false;
        while let Some(c) = self.caracter_actual() {
            if !punto_encontrado && c == '.' {
                punto_encontrado = true
            }

            if !c.is_numeric() { break; }
            result.push(c);
            self.siguiente();
        }

        Ok(Token {lexeme: result, kind: TokenKind::Numero, span })
    }

    pub fn generar_tokens(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens: Vec<Token> = vec![];

        while self.caracter_actual().is_some() {
            // Ignorar espacios en blanco
            self.ignorar_espacios();

            // Ignorar comentarios
            self.ignorar_comentarios();

            let span = Span(self.line, self.col);

            let c = self.caracter_actual();
            if c.is_none() { break }
            
            let c = c.unwrap();

            if self.es_inicio_identificador(c) {
                let mut resulting_tokens: Vec<_> = self.identificador("")?.into();
                tokens.append(&mut resulting_tokens);
                continue;
            }

            let token =  if c == '"' {
                // Procesar string
                self.texto()?
            } else if c == '.' {
                self.siguiente();
                Token{ lexeme: c.to_string(), kind: TokenKind::Punto, span }
            } else if c == ',' {
                self.siguiente();
                Token{ lexeme: c.to_string(), kind: TokenKind::Coma, span }
            } else if c == ':' {
                self.siguiente();
                Token { lexeme: ':'.to_string(), kind: TokenKind::DosPuntos, span }
            } else if c.is_numeric() {
                self.numero(c)?
            } else {
                return Err(
                    format!("Caracter invalido: {}", self.caracter_actual().unwrap()).to_string()
                )
            };

            if !self.is_fragment && token.kind == TokenKind::Adios {
                tokens.push(token);
                break
            } else {
                tokens.push(token);
            }
        }

        Ok(tokens)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Hola,
    Adios,
    PorFavor,
    Gracias,
    Muestra,
    Di,
    Repite,
    Mientras,
    Veces,
    Punto,
    DosPuntos,
    Coma,
    Y,
    Numero,
    Texto,
    Cierto,
    Falso,
    Baja,

    Mas,
    Menos,
    Por,
    Entre
}