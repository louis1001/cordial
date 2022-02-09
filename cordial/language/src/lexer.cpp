//
//  lexer.cpp
//  cordial
//
//  Created by Luis Gonzalez on 5/2/22.
//

#include <iostream>
#include <regex>
#include "lexer.hpp"

namespace Cordial {

void Lexer::setup(const std::string &code_, const std::string &file_) {
    code = code_;
    file = file_;
    position = 0;
    debugPos = {};
    is_at_start = false;
    found_end = false;
}

char Lexer::current_char() {
    if (position >= code.size()) {
        return '\0';
    }

    return code.at(position);
}

void Lexer::get_to_start() {
    std::regex hola_regex(R"(((^\s*)|(\n[\s\n]*))hola)");
    std::smatch m;
    std::regex_search(code, m, hola_regex);

    if (m.empty()) {
        std::cerr << "No se encontró el inicio del programa: `hola`" << std::endl;
        exit(1);
    }

    auto current_position = m.position(0);
    auto length = m.length(0);
    auto new_pos = current_position + length - 4;
    advance(new_pos);

    is_at_start = true;
}

char Lexer::advance() {
    if (position < code.size()) {
        auto current = current_char();
        if (current == '\n') {
            debugPos.bump_line();
        } else {
            debugPos.bump_col();
        }
        position++;

        return current;
    }

    return '\0';
}

std::string_view Lexer::advance(size_t by) {
    auto max_offset = std::min(code.size()-1, position + by);

    auto actual_change = max_offset - position;
    auto taken = std::string_view(code).substr(position, actual_change);

    // do the actual advancing
    position += actual_change;

    // count the new lines in taken
    size_t lines{0};
    size_t cols{0};
    for(auto c : taken) {
        cols++;
        if (c == '\n') {
            lines++;
            cols = 0;
        }
    }
    debugPos.bump_line(lines);
    debugPos.bump_col(cols);

    return taken;
}

Token Lexer::token(Token::Type type, const std::string &lexeme, std::optional<DebugPosition> pos) {
    return {type, lexeme, pos.value_or(debugPos), file};
}

Token Lexer::texto() {
    advance();
    std::stringstream result;
    auto error_en_multilinea = current_char() != '\n';
    result << advance();
    bool es_multilinea{false};
    while (is_valid() && current_char() != '"') {
        result << advance();
        if (current_char() == '\n') {
            es_multilinea = true;
        }
    }
    if (current_char() != '"') {
        std::cerr << "Falta doble comillas `\"` para finalizar texto.\n";
        throw;
    }

    advance();

    if (error_en_multilinea && es_multilinea) {
        std::cerr << "Texto multilinea debe empezar con un salto de linea luego de las doble comillas.\n";
        throw;
    }

    avoid_alphanum("un texto");

    return token(Token::Type::texto, result.str());
}

Token Lexer::numero() {
    std::stringstream result;
    result << advance();
    while (is_valid() && is_num()) {
        result << advance();
    }

    avoid_alphanum("un texto");

    return token(Token::Type::numero, result.str());
}

Token Lexer::palabra() {
    std::stringstream result;
    auto pos = debugPos;
    result << advance();

    while (is_valid() &&
           (isdigit(current_char()) ||
            isalpha(current_char()) ||
            current_char() == '_')) {
        result << advance();
    }

    auto valor = result.str();
    avoid_alphanum("la palabra `" + valor + "`");

    auto builtin = token_para_palabra(valor, pos);
    if (builtin.has_value()) {
        auto value = builtin.value();
        if (value.type == Token::Type::adios) {
            // Terminar de procesar
            found_end = true;
        }
        return builtin.value();
    } else {
        // TODO: Enable variables
        //return token(Token::Type::variable, valor, position);
        std::cerr << "Palabra desconocida: `" << valor << "`.\n";
        throw;
    }
}

// TODO: Find out how to be able to use ñ and áéíóúü
std::locale Lexer::spanish_locale{""};

void Lexer::avoid_alphanum(std::string_view para) {
    if (!ignore_whitespace() && is_identifier_char()) {
        std::cerr << "Caracter inesperado: '" << current_char() << "' mientras procesaba " << para << ".\n";
        throw;
    }
}

bool Lexer::is_valid() {
    return current_char() != '\0';
}

bool Lexer::ignore_whitespace() {
    bool did_anything{false};
    while (is_valid() && is_space()) {
        advance();
        did_anything = true;
    }

    return did_anything;
}

std::optional<Token> Lexer::get_next() {
    if (found_end) { return {}; }
    if (!is_at_start) {
        get_to_start();
        auto tk = token(Token::Type::hola, "hola");
        advance(4);
        return tk;
    }

    ignore_whitespace();

    if (is_num()) {
        return numero();
    } if (is_identifier_char()) {
        return palabra();
    } else {
        switch (current_char()) {
            case '.': {
                auto tk = token(Token::Type::punto, ".");
                advance();
                return tk;
            }
            case ',': {
                auto tk = token(Token::Type::coma, ",");
                advance();
                return tk;
            }
            case '"':
                return texto();
        }
    }

    // TODO: enable exception when no token matches
    //throw;

    return {};
}

std::optional<Token> Lexer::token_para_palabra(const std::string &name, DebugPosition pos) {
    for(uint8_t i = 0; i < Token::Type::COUNT; i++) {
        Token::Type type = static_cast<Token::Type::Value>(i);
        // For every token type
        // Find one with matching name.
        if (type.name() == name) { return token(type, name, pos); }
    }

    return {};
}

}
