use std::collections::HashMap;

use crate::lexer;
use crate::parser;
use parser::{Node, Ast};

#[derive(Clone)]
enum Valor {
    Texto(String),
    Numero(f64),
    Verdad(bool),
    Nada
}

struct Simbolo {
    name: String,
    value_index: Option<u32>
}

// TODO: Normalize naming language conventions
struct Scope {
    name: String,
    symbols: HashMap<String, Simbolo>,
    values: HashMap<u32, Valor>
}

impl Scope {
    fn max_value_index(&self) -> u32 {
        *self.values.keys().max().unwrap_or(&0)
    }
}

impl Scope {
    fn new(name: &str) -> Self {
        Scope { name: name.to_string(), symbols: HashMap::new(), values: HashMap::new() }
    }
}

pub struct Runner {
    scope_stack: Vec<Scope>
}

impl Runner {
    pub fn new() -> Self {
        Runner { scope_stack: vec![] }
    }

    fn current_scope(&mut self) -> Option<&mut Scope> {
        self.scope_stack.last_mut()
    }
}

impl Runner {
    pub fn run(&mut self, text: String) -> Result<(), String> {
        // Generar los tokens (seccionar el código)
        let mut lexer = lexer::Lexer::new(text, false)?;

        let tokens = lexer.generar_tokens()?;

        let mut parser = parser::Parser::new(tokens.into_iter());

        let ast = parser.parse()?;

        let top_scope = Scope { name: "global".to_string(), symbols: HashMap::new(), values: HashMap::new() };
        self.scope_stack.push(top_scope);

        let _ = self.visit(ast)?;

        self.scope_stack.pop();

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

    fn nombre(&mut self, nombre: String) -> Result<Valor, String> {
        let scope = self.current_scope().ok_or("No se encontró un entorno válido.")?;

        let value_index = scope.symbols
            .get(&nombre)
                .ok_or(format!("El nombre {nombre} no está definido en el entorno."))?
            .value_index
                .ok_or(format!("El nombre {nombre} no ha sido inicializado."))?;

        let value = scope.values.get(&value_index)
            .expect(&format!("El `value_index` del simbolo {nombre} apunta a un valor válido."));

        Ok(value.clone())
    }

    fn asignacion(&mut self, value: Node, name: String) -> Result<Valor, String> {
        let value = self.visit(value)?;
        let scope = self.current_scope().ok_or("No se encontró un entorno válido.")?;

        let value_index: u32 = scope.max_value_index() + 1;
        scope.values.insert(value_index, value);

        let symbol = Simbolo { name, value_index: Some(value_index) };
        scope.symbols.insert(symbol.name.clone(), symbol);

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

            Ast::Verdad(v) => Ok(Valor::Verdad(v)),

            Ast::Repite(times, body) => self.repite(times, body),
            Ast::Mientras(cond, body) => self.mientras(cond, body),

            Ast::Asignacion(value, name) => self.asignacion(value, name),
            Ast::Nombre(nombre) => self.nombre(nombre),
            
            Ast::NoOp => Ok(Valor::Nada),
            Ast::AdditionOp(op, lhs, rhs) => self.addition(op, lhs, rhs),
            Ast::MultiplicationOp(op, lhs, rhs) => self.multiplication(op, lhs, rhs),
            _ => todo!("{:?}", *node)
        }
    }
}
