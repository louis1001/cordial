//
//  token.cpp
//  cordial
//
//  Created by Luis Gonzalez on 6/2/22.
//

#include <string>
#include "token.hpp"

namespace Cordial {

std::string_view Token::Type::name() const {
    switch (value) {
        case Type::hola:
            return "hola";
        case Type::adios:
            return "adios";
        case Type::porfavor:
            return "porfavor";
        case Type::gracias:
            return "gracias";
        case Type::coma:
            return "coma";
        case Type::punto:
            return "punto";
        case Type::y:
            return "y";
        case Type::numero:
            return "numero";
        case Type::texto:
            return "texto";
        case Type::variable:
            return "variable";
        case Type::muestra:
            return "muestra";
        case Type::baja:
            return "baja";

        case mas:
            return "mas";
        case menos:
            return "menos";
        case entre:
            return "entre";
        case por:
            return "por";

        case es:
            return "es";

        case Type::COUNT:
            return "OPEIUWGB {UHW4B9   AWEG";
    }
}

}
