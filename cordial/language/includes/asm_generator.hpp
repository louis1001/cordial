//
// Created by Luis Gonzalez on 9/2/22.
//

#ifndef CORDIAL_ASM_HPP
#define CORDIAL_ASM_HPP
#include <sstream>
#include "ast.hpp"
#include "utils.hpp"
#include "simbolo.hpp"
#include "valor.hpp"

namespace Cordial {
    class AsmGenerator {
        std::vector<std::string> strings;
    public:
        void visit(const nodo_ptr &node, std::stringstream& output) const;
    };
}

#endif //CORDIAL_ASM_HPP
