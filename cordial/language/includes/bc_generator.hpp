//
// Created by Luis Gonzalez on 12/3/22.
//

#ifndef CORDIAL_BC_GENERATOR_HPP
#define CORDIAL_BC_GENERATOR_HPP

#include "ast.hpp"
#include "bcprogram.hpp"

namespace Cordial {
    class BCGenerator {
        void visit(const nodo_ptr&, const std::shared_ptr<Bytecode::ProgramBuilder>&);

    public:
        std::shared_ptr<Bytecode::ProgramBuilder> generate(const nodo_ptr&);
    };
}

#endif //CORDIAL_BC_GENERATOR_HPP
