//
// Created by Luis Gonzalez on 10/2/22.
//

#ifndef CORDIAL_SYMBOL_HPP
#define CORDIAL_SYMBOL_HPP
#include <string>
#include <utility>
#include <vector>

namespace Cordial {
    class Symbol {
        std::shared_ptr<Symbol> parent{};
        std::string name;

    public:
        Symbol(std::string name_)
            : parent({}),
              name(std::move(name_)){}

        Symbol(std::string name_, std::shared_ptr<Symbol> parent_)
                : parent(std::move(parent_)),
                  name(std::move(name_)){}
    };

    class SymbolTable {
        std::string name;
        std::weak_ptr<SymbolTable> parent;
        std::vector<Symbol> symbols;
    };

    inline std::shared_ptr<Symbol> tipo_texto() {
        static const auto texto = std::make_shared<Symbol>("texto");
        return texto;
    }

    inline std::shared_ptr<Symbol> tipo_numero() {
        static const auto numero = std::make_shared<Symbol>("numero");
        return numero;
    }
}


#endif //CORDIAL_SYMBOL_HPP
