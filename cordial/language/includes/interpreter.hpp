//
// Created by Luis Gonzalez on 9/2/22.
//

#ifndef CORDIAL_INTERPRETER_HPP
#define CORDIAL_INTERPRETER_HPP
#include "ast.hpp"
#include "utils.hpp"
#include "simbolo.hpp"
#include "valor.hpp"

namespace Cordial {
    class Interpreter {
    public:
        val_ptr visit(const nodo_ptr &node) const;
    };
}

#endif //CORDIAL_INTERPRETER_HPP
