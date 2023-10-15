use std::env;
use std::fs;

mod preprocessing;
mod lexer;
mod parser;
mod execution;

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

    let mut runner = execution::Runner::new();
    runner.run(contenido)?;

    Ok(())
}
