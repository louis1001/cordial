//
// Created by Luis Gonzalez on 10/2/22.
//

#ifndef CORDIAL_VALOR_HPP
#define CORDIAL_VALOR_HPP
#include <utility>
#include <variant>
#include "simbolo.hpp"

namespace Cordial {
    struct Valor;
    using val_ptr = std::shared_ptr<Valor>;

    struct ValorNumero {
        int valor;
    };

    struct ValorTexto {
        std::string valor;
    };

    using contenido_val = std::variant<
        ValorNumero,
        ValorTexto
    >;

    struct Valor {
        std::shared_ptr<Simbolo> tipo;
        contenido_val contenido;

        Valor(std::shared_ptr<Simbolo> tipo_, contenido_val contenido_)
        : tipo(std::move(tipo_)),
          contenido(std::move(contenido_))
        {}
    };
}

#endif //CORDIAL_VALOR_HPP
