use crate::lexer;
use crate::parser;
use parser::{Node, Ast};

enum Valor {
    Texto(String),
    Numero(f64),
    Verdad(bool),
    Nada
}

pub struct Runner {

}

impl Runner {
    pub fn run(&mut self, text: String) -> Result<(), String> {
        // Generar los tokens (seccionar el código)
        let mut lexer = lexer::Lexer::new(text);

        let tokens = lexer.generar_tokens()?;

        let mut parser = parser::Parser::new(tokens.into_iter());

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

    fn mientras(&mut self, cond: Node, body: Node) -> Result<Valor, String> {
        let cond_value = self.visit(cond.clone())?;
        
        let mut condition = if let Valor::Verdad(valor) = cond_value {
            valor
        } else {
            return Err("La condición en una sentencia `Mientras` debe ser una verdad.".into());
        };

        while condition {
            self.visit(body.clone())?;

            let cond_value = self.visit(cond.clone())?;
            condition = if let Valor::Verdad(valor) = cond_value { valor }
            else {
                return Err("La condición en una sentencia `Mientras` debe ser una verdad.".into());
            }
        }

        Ok(Valor::Nada)
    }

    fn addition(&mut self, op: lexer::TokenKind, lhs: Node, rhs: Node) -> Result<Valor, String> {
        let lhs_visited = self.visit(lhs)?;
        let rhs_visited = self.visit(rhs)?;

        match (lhs_visited, rhs_visited) {
            (Valor::Numero(a), Valor::Numero(b)) => {
                match op {
                    lexer::TokenKind::Mas => Ok(Valor::Numero(a + b)),
                    lexer::TokenKind::Menos => Ok(Valor::Numero(a - b)),

                    _ => Err(format!("Operación binaria invalida: {:?}", op))
                }
            }
            _ => {
                Err(format!("Operación {:?} solo es posible entre números", op))
            }
        }
    }

    fn multiplication(&mut self, op: lexer::TokenKind, lhs: Node, rhs: Node) -> Result<Valor, String> {
        let lhs_visited = self.visit(lhs)?;
        let rhs_visited = self.visit(rhs)?;

        match (lhs_visited, rhs_visited) {
            (Valor::Numero(a), Valor::Numero(b)) => {
                match op {
                    lexer::TokenKind::Por => Ok(Valor::Numero(a * b)),
                    lexer::TokenKind::Entre => Ok(Valor::Numero(a / b)),

                    _ => Err(format!("Operación binaria invalida: {:?}", op))
                }
            }
            _ => {
                Err(format!("Operación {:?} solo es posible entre números", op))
            }
        }
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

            Ast::Verdad(v) => Ok(Valor::Verdad(v)),

            Ast::Repite(times, body) => self.repite(times, body),
            Ast::Mientras(cond, body) => self.mientras(cond, body),
            
            Ast::NoOp => Ok(Valor::Nada),
            Ast::AdditionOp(op, lhs, rhs) => self.addition(op, lhs, rhs),
            Ast::MultiplicationOp(op, lhs, rhs) => self.multiplication(op, lhs, rhs),
            _ => todo!("{:?}", *node)
        }
    }
}
