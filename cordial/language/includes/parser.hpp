//
//  parser.hpp
//  Cordial
//
//  Created by Luis Gonzalez on 6/2/22.
//

#ifndef parser_hpp
#define parser_hpp

#include <string>
#include <vector>
#include <optional>
#include <functional>

#include "ast.hpp"
#include "lexer.hpp"

namespace Cordial {

struct Parser {
private:
    nodo_ptr program();
    nodo_ptr block();
    nodo_ptr statement();
    nodo_ptr si();
    nodo_ptr muestra();
    nodo_ptr baja();
    nodo_ptr igualdad();
    nodo_ptr expresion();
    nodo_ptr termino();
    nodo_ptr literal();
    std::vector<nodo_ptr> statement_list();

    static std::vector<Token::Type> statement_types;

    void statement_separator(bool optional = false);

    void eat(Token::Type type);
    void eat(std::vector<Token::Type> types);
    bool is_of_type(Token::Type type);
    bool is_of_type(const std::vector<Token::Type>& types);

    nodo_ptr nodo(std::function<contenido_nodo()> generator);

    std::optional<Token> current_token;
    Lexer lexer;
public:
    Parser() = default;
    nodo_ptr parse(std::string code, std::string file);
};

}

#endif /* parser_hpp */
