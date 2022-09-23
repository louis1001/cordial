use std::collections::HashMap;
use std::env;
use std::fmt::{Display};
use std::fs;
use std::iter::Peekable;

#[derive(Debug, Clone, PartialEq)]
enum TokenKind {
    Hola,
    Adios,
    PorFavor,
    Gracias,
    Muestra,
    Di,
    Repite,
    Veces,
    Punto,
    DosPuntos,
    Coma,
    Numero,
    Texto,
    Baja,
    Y
}

#[derive(Debug, Clone)]
struct Span(usize, usize);

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

#[derive(Debug, Clone)]
struct Token {
    lexeme: String,
    kind: TokenKind,
    span: Span
}
// Lexer - Analizador 'lexicográfico'?
struct Lexer {
    content: String,
    pos: usize,
    line: usize,
    col: usize,
    keywords: HashMap<String, TokenKind>
}

impl Lexer {
    fn new(content: String) -> Self {
        Self {
            content,
            pos: 0,
            line: 1,
            col: 0,
            keywords: HashMap::from([
                ("hola".to_string(), TokenKind::Hola),
                ("adios".to_string(), TokenKind::Adios),
                ("porfavor".to_string(), TokenKind::PorFavor),
                ("gracias".to_string(), TokenKind::Gracias),
                ("muestra".to_string(), TokenKind::Muestra),
                ("repite".to_string(), TokenKind::Repite),
                ("veces".to_string(), TokenKind::Veces),
                ("di".to_string(), TokenKind::Di),
                ("baja".to_string(), TokenKind::Baja),
            ])
        }
    }

    fn caracter_actual(&self) -> Option<char> {
        if self.pos > self.content.len() {
            None
        } else {
            self.content.chars().nth(self.pos)
        }
    }

    fn caracter_es(&self, comp: char) -> bool {
        match self.caracter_actual() {
            Some(c) => c == comp,
            None => false
        }
    }

    fn siguiente(&mut self) {
        self.col += 1;
        self.pos += 1
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

    fn identificador(&mut self) -> Result<Token, String> {
        let span = Span(self.line, self.col);
        let mut result = String::new();

        while let Some(c) = self.caracter_actual() {
            if c.is_alphanumeric() {
                result.push(c);
                self.siguiente();
            } else {
                break
            }
        }

        if let Some(palabra_clave) = self.keywords.get(&result) {
            Ok(Token{ lexeme: result, kind: palabra_clave.clone(), span: span })
        } else {
            // Por ahora, cualquier identificador que no sea
            // Una palabra clave, es un error
            todo!("Identificador invalido: {}", result)
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

    fn generar_tokens(&mut self) -> Result<Vec<Token>, String> {
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
            let token =  if c == '"' {
                // Procesar string
                self.texto()?
            } else if c == '.' {
                self.siguiente();
                Token{ lexeme: c.to_string(), kind: TokenKind::Punto, span }
            } else if c == ',' {
                self.siguiente();
                Token{ lexeme: c.to_string(), kind: TokenKind::Coma, span }
            } else if c == 'y' {
                self.siguiente();
                Token{ lexeme: c.to_string(), kind: TokenKind::Y, span }
            } else if c == ':' {
                self.siguiente();
                Token { lexeme: ':'.to_string(), kind: TokenKind::DosPuntos, span }
            } else if c.is_alphabetic() {
                self.identificador()?
            } else if c.is_numeric() {
                self.numero(c)?
            } else {
                return Err(
                    format!("Caracter invalido: {}", self.caracter_actual().unwrap()).to_string()
                )
            };

            tokens.push(token);
        }

        Ok(tokens)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Ast {
    Programa(Node),
    Bloque(Vec<Node>),
    Di(Node),
    Repite(Node, Node),
    Texto(String),
    Numero(f64),
    NoOp
}

type Node = Box<Ast>;

struct Parser<TokenIter> 
    where TokenIter: Iterator<Item = Token>
{
    tokens: Peekable<TokenIter>
}

impl<TokenIter> Parser<TokenIter>
    where TokenIter: Iterator<Item = Token>
{
    fn new(tokens: TokenIter) -> Self {
        Self { tokens: tokens.peekable() }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<(), String> {
        if let Some(_) = self.tokens.next_if(|x| { x.kind == kind }) {
            Ok(())
        } else {
            Err(
                format!("Error de sintaxis. Esperaba token de tipo `{:?}`", kind)
                .to_string()
            )
        }
    }

    fn match_kind(&mut self, kind: TokenKind) -> Option<Token> {
        self.tokens.next_if(|x| x.kind == kind)
    }

    fn parse(&mut self) -> Result<Node, String> {
        self.programa()
    }

    fn puntuacion(&mut self) -> Result<(), String> {
        if self.match_kind(TokenKind::Punto).is_some() {
            Ok(())
        } else if self.match_kind(TokenKind::Coma).is_some() {
            Ok(())
        } else if self.match_kind(TokenKind::Y).is_some() {
            Ok(())
        } else {
            if let Some(t) = self.tokens.peek() {
                Err(format!("Se esperaba puntuación en {}, pero se encontró `{:?}`", t.span, t.kind).to_string())
            } else {
                Err(format!("Se esperaba puntuación en {}, pero no se encontró nada.", self.tokens.peek().unwrap().span).to_string())
            }
        }
    }

    fn programa(&mut self) -> Result<Node, String> {
        self.expect(TokenKind::Hola)?;
        self.puntuacion()?;

        let body = self.sentencia()?;

        self.expect(TokenKind::Adios)?;
        let _ = self.puntuacion(); // Puntuación opcional luego de adios
        Ok(Box::new(Ast::Programa(body)))
    }

    fn bloque(&mut self) -> Result<Node, String> {
        self.expect(TokenKind::PorFavor)?;
        self.puntuacion()?;

        let mut body = vec![];

        while self.match_kind(TokenKind::Gracias).is_none() {
            if self.tokens.peek().is_none() {
                return Err("Bloque de `porfavor` no fue cerrado con `gracias`".to_string());
            }

            body.push(self.sentencia()?);
        }

        Ok(Box::new(Ast::Bloque(body)))
    }

    fn di(&mut self) -> Result<Node, String> {
        self.expect(TokenKind::Di)?;

        let expr = self.termino()?;
        
        Ok(Box::new(Ast::Di(expr)))
    }

    fn repite(&mut self) -> Result<Node, String> {
        self.expect(TokenKind::Repite)?;

        let ammount = self.atomo()?;

        self.expect(TokenKind::Veces)?;

        self.expect(TokenKind::DosPuntos)?;

        let body = self.sentencia()?;

        Ok(Box::new(Ast::Repite(ammount, body)))
    }

    fn termino(&mut self) -> Result <Node, String> {
        let node = self.atomo()?;
        
        // Operadores

        Ok(node)
    }

    fn atomo(&mut self) -> Result<Node, String> {
        let node = match self.tokens.next() {
            Some(t) => match t.kind {
                TokenKind::Texto => {
                    Ast::Texto(t.lexeme.clone())
                },
                TokenKind::Numero => {
                    let val: f64 = match t.lexeme.parse() {
                        Ok(v) => v,
                        Err(_) => {
                            return Err(format!("Valor `{}` no es un número valido.", t.lexeme));
                        }
                    };

                    Ast::Numero(val)
                },
                _ => Ast::NoOp
            }
            None => Ast::NoOp
        };

        Ok(Box::new(node))
    }
    
    fn sentencia(&mut self) -> Result<Node, String> {
        let token = self.tokens.peek()
            .ok_or("Se esperaba un token, pero no se encontró nada")?;

        let mut has_puntuation = true;

        let token = match token.kind {
            TokenKind::PorFavor => {
                self.bloque()
            }
            TokenKind::Di => {
                self.di()
            }
            TokenKind::Baja => {
                self.tokens.next();
                Ok(Box::new(Ast::Di(Box::new(Ast::Texto("\n".to_string())))))
            }
            TokenKind::Repite => {
                has_puntuation = false;
                self.repite()
            }
            _ => {
                Err("Token invalido.".to_string())
            }
        }?;

        if has_puntuation { self.puntuacion()?; }

        Ok(token)
    }
}

enum Valor {
    Texto(String),
    Numero(f64),
    Nada
}

struct Runner {

}

impl Runner {
    fn run(&mut self, text: String) -> Result<(), String> {
        // Generar los tokens (seccionar el código)
        let mut lexer = Lexer::new(text);

        let tokens = lexer.generar_tokens()?;

        let mut parser = Parser::new(tokens.into_iter());

        let ast = parser.parse()?;

        let _ = self.visit(ast)?;

        Ok(())
    }

    fn repite(&mut self, times: Node, body: Node) -> Result<Valor, String> {
        let times_value = self.visit(times)?;
        
        let count = if let Valor::Numero(valor) = times_value {
            valor
        } else {
            return Err("Cantidad de repeticiones en sentencia `Repite` debe ser un número valido".into());
        };

        let int_count = count.trunc() as i64;

        if int_count as f64 != count {
            return Err(format!("Cantidad de repeticiones en sentencia `Repite` debe ser un número entero. Se obtuvo `{}`.", count));
        }

        for _ in 0..int_count {
            self.visit(body.clone())?;
        }

        Ok(Valor::Nada)
    }

    fn visit(&mut self, node: Node) -> Result<Valor, String> {
        match *node {
            Ast::Programa(body) => {
                self.visit(body)
            }
            Ast::Bloque(nodes) => {
                // Crear scope
                for n in nodes {
                    let _ = self.visit(n)?;
                }
                Ok(Valor::Nada)
                // Destruir scope
            }
            Ast::Di(valor) => {
                let expr = self.visit(valor)?;

                match expr {
                    Valor::Texto(t) => print!("{}", t),
                    _ => panic!("Error! `Di` necesita un texto para decir.")
                }

                Ok(Valor::Nada)
            }
            Ast::Texto(t) => Ok(Valor::Texto(t)),
            Ast::Numero(n) => Ok(Valor::Numero(n)),
            Ast::Repite(times, body) => self.repite(times, body),
            Ast::NoOp => Ok(Valor::Nada),
            _ => todo!("{:?}", *node)
        }
    }
}

fn main() -> Result<(), String>{
    let args: Vec<_> = env::args().collect();
    if args.len() < 2 {
        return Err("Se necesita un archivo a ejecutar".to_string());
    }

    // Archivo a ejecutar
    let filename = &args[1];

    // Leer el archivo
    let contenido =
        fs::read_to_string(filename)
        .expect("No se pudo leer el archivo");

    let mut runner = Runner{};
    runner.run(contenido)?;

    Ok(())
}
