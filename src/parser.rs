use std::iter::Peekable;

use crate::lexer::{Token, TokenKind};

#[derive(Debug, Clone, PartialEq)]
pub enum Ast {
    Programa(Node),
    Bloque(Vec<Node>),
    Di(Node),
    Repite(Node, Node),
    Mientras(Node, Node),
    Texto(String),
    Numero(f64),
    Verdad(bool),

    AdditionOp(TokenKind, Node, Node),
    MultiplicationOp(TokenKind, Node, Node),

    NoOp
}

pub type Node = Box<Ast>;

pub struct Parser<TokenIter> 
    where TokenIter: Iterator<Item = Token>
{
    tokens: Peekable<TokenIter>
}

impl<TokenIter> Parser<TokenIter>
    where TokenIter: Iterator<Item = Token>
{
    pub fn new(tokens: TokenIter) -> Self {
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

    fn match_kinds(&mut self, kinds: &[TokenKind]) -> Option<Token> {
        let mut result = None;
        for kind in kinds {
            if let Some(token) = self.tokens.next_if(|x| x.kind == *kind) {
                result = Some(token);
            }
        }

        result
    }

    pub fn parse(&mut self) -> Result<Node, String> {
        self.programa()
    }

    fn puntuacion(&mut self) -> Result<(), String> {
        if self.match_kind(TokenKind::Punto).is_some() {
            Ok(())
        } else if self.match_kind(TokenKind::Coma).is_some() {
            Ok(())
        } else if self.match_kind(TokenKind::PuntoYComa).is_some() {
            Ok(())
        } else if self.match_kind(TokenKind::Y).is_some() {
            Ok(())
        } else {
            if let Some(t) = self.tokens.peek() {
                Err(format!("Se esperaba puntuación en {}, pero se encontró `{:?}`", t.span, t.kind).to_string())
            } else {
                if let Some(next_token) = self.tokens.peek() {
                    Err(format!("Se esperaba puntuación en {}, pero no se encontró nada.", next_token.span.to_string()))
                } else {
                    Err(format!("Se esperaba puntuación, pero no se encontró nada."))
                }
                
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

        let ammount = self.termino()?;

        self.expect(TokenKind::Veces)?;

        self.expect(TokenKind::DosPuntos)?;

        let body = self.sentencia()?;

        Ok(Box::new(Ast::Repite(ammount, body)))
    }

    fn mientras(&mut self) -> Result<Node, String> {
        self.expect(TokenKind::Mientras)?;

        let condition = self.termino()?;

        self.expect(TokenKind::DosPuntos)?;

        let body = self.sentencia()?;

        Ok(Box::new(Ast::Mientras(condition, body)))
    }

    fn termino(&mut self) -> Result <Node, String> {
        let mut node = self.producto()?;

        while let Some(t) = self.match_kinds(&[TokenKind::Mas, TokenKind::Menos]) {
            let rhs = self.producto()?;

            node = Box::new(Ast::AdditionOp(t.kind, node, rhs));
        }
        // Operadores

        Ok(node)
    }

    fn producto(&mut self) -> Result<Node, String> {
        let mut node = self.atomo()?;

        while let Some(t) = self.match_kinds(&[TokenKind::Por, TokenKind::Entre]) {
            let rhs = self.atomo()?;

            node = Box::new(Ast::MultiplicationOp(t.kind, node, rhs));
        }
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
                TokenKind::Cierto => Ast::Verdad(true),
                TokenKind::Falso => Ast::Verdad(false),
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
            TokenKind::Mientras => {
                has_puntuation = false;
                self.mientras()
            }
            _ => {
                Err(format!("Token invalido: {:?}.", token.kind))
            }
        }?;

        if has_puntuation { self.puntuacion()?; }

        Ok(token)
    }
}
