
//  main.cpp
//  cordial
//
//  Created by Luis Gonzalez on 5/2/22.
//

#include <iostream>
#include "lexer.hpp"

int main(int argc, const char * argv[]) {
    // TODO: Debug position for tokens is not correct.
    std::string code = R"(
Esto no debe registrarse como un programa. Ni siquiera si escribo hola.
La verdad, no se qué tan confuso pueda verse.
    
Código empezaría aquí:
hola. porfavor
    muestra "Hola, mundo"
  y muestra 20,
gracias. adios.

Y esto estaría fuera de código. El programa termina luego del primer `adios`.
)";
    Cordial::Lexer lx;
    
    lx.setup(code, "anonymus");
    
    auto tk = lx.get_next();
    while (tk.has_value()) {
        std::cout << tk.value().get_str() << "\n";
        tk = lx.get_next();
    }
    
    return 0;
}
