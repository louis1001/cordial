//
//  parser.cpp
//  Cordial
//
//  Created by Luis Gonzalez on 6/2/22.
//

#include <iostream>

#include "parser.hpp"
#include "lexer.hpp"
#include "ast.hpp"

namespace Cordial {

nodo_ptr Parser::parse(std::string code, std::string file) {
    lexer = Lexer();
    lexer.setup(code, file);

    current_token = lexer.get_next();

    auto root = program();

    return root;
}

nodo_ptr Parser::nodo(std::function<contenido_nodo()> generator) {
    NodoMeta meta { lexer.debug_position(), lexer.file_name() };
    auto contenido = generator();

    return std::make_shared<Nodo>(meta, contenido);
}

void Parser::statement_separator(bool optional) {
    std::vector<Token::Type> separators {
        Token::Type::punto,
        Token::Type::coma,
        Token::Type::y
    };

    // I hate this logic
    if (is_of_type(separators) || !optional) {
        eat(separators);
    }

    while(is_of_type(separators)) {
        eat(current_token->type);
    }
}

std::vector<Token::Type> Parser::statement_types{
    Token::Type::muestra,
    Token::Type::porfavor,
    Token::Type::baja,
    Token::Type::si,
    // Temporary
    Token::Type::numero
};

std::vector<nodo_ptr> Parser::statement_list() {
    std::vector<nodo_ptr> children;
    while (is_of_type(statement_types)) {
        children.push_back(statement());
        statement_separator();
    }

    return children;
}

bool Parser::is_of_type(Token::Type type) {
    if (!current_token.has_value()) { return false; }

    return current_token->type == type;
}

bool Parser::is_of_type(const std::vector<Token::Type>& types) {
    if (!current_token.has_value() || types.empty()) { return false; }

    for (auto typeA : types) {
        if (current_token->type == typeA) { return true; }
    }

    return false;
}

void Parser::eat(Token::Type type) {
    if (!current_token.has_value()) {
        std::cout << "Error de sintaxis. Esperaba token de tipo `" << type.name()
                  << "`, pero se encontró el fin del archivo.\n";
        throw;
    }

    if (type == current_token->type) {
        current_token = lexer.get_next();
    } else {
        std::cout << "Token inesperado. Encontró `" << current_token->type.name()
                  << "` cuando esperaba `" << type.name() << "`.\n";
        throw;
    }
}

void Parser::eat(std::vector<Token::Type> types) {
    std::stringstream names;
    auto end_of_file = !current_token.has_value();
    for (auto it = types.begin(); it < types.end(); it++) {

        if (!end_of_file && *it == current_token->type) {
            current_token = lexer.get_next();
            return;
        }

        names << "`" << it->name() << "`";
        if (types.size() > 1) {
            if (it == types.end() - 2) {
                names << " o ";
            } else if (it != types.end()-1) {
                names << ", ";
            }
        }
    }

    if (end_of_file) {
        std::cout << "Error de sintaxis. Esperaba token de un tipo como: " << names.str() << ", pero se encontró el fin del archivo.\n";
        throw;
    } else {
        std::cout << "Token inesperado. Encontró `" << current_token->type.name() << "` cuando esperaba cualquiera de: " << names.str() << ".\n";
        throw;
    }
}

nodo_ptr Parser::program() {
    return nodo([&] {
        eat(Token::Type::hola);
        statement_separator();

        // Conseguir lista de hijos
        auto hijos = statement_list();

        eat(Token::Type::adios);
        return NodoPrograma{hijos};
    });
}

nodo_ptr Parser::block() {
    return nodo([&] {
        eat(Token::Type::porfavor);
        statement_separator(true);

        // Conseguir lista de hijos
        auto hijos = statement_list();

        eat(Token::Type::gracias);
        return NodoBloque{hijos};
    });
}

nodo_ptr Parser::statement() {
    if (!current_token.has_value()) {
        std::cerr << "Esperaba una oración, pero se llegó al final del archivo.";
        throw;
    }

    if (is_of_type(Token::Type::muestra)) {
        return muestra();
    } else if (is_of_type(Token::Type::porfavor)) {
        return block();
    } else if (is_of_type(Token::Type::baja)) {
        return baja();
    } else if (is_of_type(Token::Type::si)) {
        return si();
    } else {
        // Temporarily enable expressions as statements.
        return igualdad();
    }

    std::cerr << "Esperaba una oración, pero encontró un token de tipo: `" << current_token->type.name() << "`\n";
    throw;
}

nodo_ptr Parser::si() {
    return nodo([&]{
        eat(Token::Type::si);

        auto cond = igualdad();

        eat(Token::Type::coma);

        auto body = statement();

        return NodoSi { cond, body };
    });
}

nodo_ptr Parser::muestra() {
    return nodo([&]{
        eat(Token::Type::muestra);
        auto expr = igualdad();
        return NodoMuestra{expr};
    });
}

nodo_ptr Parser::baja() {
    return nodo([&]{
        eat(Token::Type::baja);
        return NodoBaja();
    });
}

nodo_ptr Parser::igualdad() {
    std::vector<Token::Type> igual_op {
            Token::Type::es
    };
    return nodo([&]() {
        auto current = expresion();
        while (is_of_type(igual_op)) {
            auto type = current_token->type;
            eat(type);

            auto pos = lexer.debug_position();
            auto file = lexer.file_name();
            current = std::make_shared<Nodo>(
                    NodoMeta{pos, file},
                    NodoIgual{current, expresion() }
            );
        }

        return current->contenido;
    });
}

nodo_ptr Parser::expresion() {
    std::vector<Token::Type> expr_op {
        Token::Type::mas,
        Token::Type::menos
    };
    return nodo([&]() {
        auto current = termino();
        while (is_of_type(expr_op)) {
            auto type = current_token->type;
            eat(type);

            auto pos = lexer.debug_position();
            auto file = lexer.file_name();
            if (type == Token::Type::mas) {
                current = std::make_shared<Nodo>(
                        NodoMeta{pos, file},
                        NodoSuma{current, termino() }
                        );
            } else {
                // Menos node
                current = std::make_shared<Nodo>(
                        NodoMeta{pos, file},
                        NodoResta{current, termino() }
                        );
            }
        }

        return current->contenido;
    });
}

nodo_ptr Parser::termino() {
    std::vector<Token::Type> term_op {
            Token::Type::por,
            Token::Type::entre
    };
    return nodo([&]() {
        auto current = literal();
        while (is_of_type(term_op)) {
            auto type = current_token->type;
            eat(type);

            auto pos = lexer.debug_position();
            auto file = lexer.file_name();
            if (type == Token::Type::por) {
                current = std::make_shared<Nodo>(
                        NodoMeta{pos, file},
                        NodoMulti{current, literal() }
                        );
            } else {
                // Menos node
                current = std::make_shared<Nodo>(
                        NodoMeta{pos, file},
                        NodoDivi{current, termino() }
                        );
            }
        }

        return current->contenido;
    });
}

nodo_ptr Parser::literal() {
    if (!current_token.has_value()) {
        std::cerr << "Esperaba un literal, pero se llegó al final del archivo.";
    }

    if (is_of_type(Token::Type::texto)) {
        return nodo([&, token=current_token.value()] {
            eat(Token::Type::texto);
            return NodoTexto{token.lexeme};
        });
    } else if (is_of_type(Token::Type::numero)) {
        return nodo([&, token=current_token.value()] {
            eat(Token::Type::numero);
            return NodoNumero{token.lexeme};
        });
    } else if (is_of_type({Token::Type::cierto, Token::Type::falso})) {
        return nodo([&, tk=current_token] {
            eat(tk->type);

            return NodoVerdad { tk->type == Token::Type::cierto };
        });
    }

    std::cerr << "Esperaba una literal, pero encontró un token de tipo: `" << current_token->type.name() << "`\n";
    throw;
}

}
