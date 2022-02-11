//
//  lexer.hpp
//  cordial
//
//  Created by Luis Gonzalez on 5/2/22.
//

#ifndef lexer_hpp
#define lexer_hpp

#include <string>
#include <locale>
#include <optional>
#include "token.hpp"
#include "utils.hpp"

namespace Cordial {

struct Lexer {
private:
    size_t position{};
    DebugPosition debugPos;
    std::string code;
    std::string file;

    bool is_at_start{false};
    bool found_end{false};

    std::optional<Token> token_para_palabra(const std::string &name, DebugPosition pos);
    Token token(Token::Type type, const std::string &lexeme, std::optional<DebugPosition> pos = {});
    char current_char();
    char advance();
    std::string_view advance(size_t by);
    bool ignore_whitespace();

    Token texto();
    Token numero();
    Token palabra();

    static std::locale spanish_locale;

    inline bool is_alpha() { return std::isalpha(current_char(), spanish_locale); }
    inline bool is_num() { return std::isdigit(current_char(), spanish_locale); }
    inline bool is_space() { return std::isspace(current_char(), spanish_locale); }
    [[maybe_unused]] inline bool is_alphanum() { return is_alpha() || is_num(); }
    inline bool is_identifier_char() { return is_alpha() || current_char() == '_'; }

    void avoid_alphanum(std::string_view para);

    bool is_valid();
    void get_to_start();

public:
    [[nodiscard]] DebugPosition debug_position() const { return debugPos; }
    [[nodiscard]] std::string file_name() const { return file; }

    Lexer() = default;
    void setup(const std::string &code_, const std::string &file_);
    std::optional<Token> get_next();
};

}

#endif /* lexer_hpp */
