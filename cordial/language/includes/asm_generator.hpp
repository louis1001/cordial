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
        int tag_counter = 0;
        std::vector<std::string> strings;

        std::string new_tag(const std::string& prefix);
    public:
        void visit(const nodo_ptr &node, std::stringstream& output) const;
    };
}

#endif //CORDIAL_ASM_HPP
