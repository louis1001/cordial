//
// Created by Luis Gonzalez on 10/2/22.
//

#ifndef CORDIAL_SIMBOLO_HPP
#define CORDIAL_SIMBOLO_HPP
#include <string>
#include <utility>
#include <vector>

namespace Cordial {
    class Simbolo {
        std::shared_ptr<Simbolo> padre{};
        std::string nombre;

    public:
        Simbolo(std::string name_)
            : padre({}),
              nombre(std::move(name_)){}

        Simbolo(std::string name_, std::shared_ptr<Simbolo> parent_)
                : padre(std::move(parent_)),
                  nombre(std::move(name_)){}
    };

    class SymbolTable {
        std::string name;
        std::weak_ptr<SymbolTable> parent;
        std::vector<Simbolo> symbols;
    };

    inline std::shared_ptr<Simbolo> tipo_texto() {
        static const auto texto = std::make_shared<Simbolo>("texto");
        return texto;
    }

    inline std::shared_ptr<Simbolo> tipo_numero() {
        static const auto numero = std::make_shared<Simbolo>("numero");
        return numero;
    }
}


#endif //CORDIAL_SIMBOLO_HPP
