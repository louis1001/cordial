//
// Created by Luis Gonzalez on 9/2/22.
//

#ifndef CORDIAL_SEMANTICS_HPP
#define CORDIAL_SEMANTICS_HPP
#include "ast.hpp"
#include "utils.hpp"
#include "simbolo.hpp"

namespace Cordial {
    class Semantics {
    public:
        std::shared_ptr<Simbolo> visit(nodo_ptr &node) const;
    };
}

#endif //CORDIAL_SEMANTICS_HPP
