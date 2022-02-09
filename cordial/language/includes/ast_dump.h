//
// Created by Luis Gonzalez on 8/2/22.
//

#ifndef CORDIAL_AST_DUMP_H
#define CORDIAL_AST_DUMP_H
#include <string>

#include "ast.hpp"

namespace Cordial {

class ASTDumper {
public:
    ASTDumper() = default;
    [[nodiscard]] std::string visit(const nodo_ptr &node, int counter) const;
};

}


#endif //CORDIAL_AST_DUMP_H
