//
//  ast.hpp
//  Cordial
//
//  Created by Luis Gonzalez on 6/2/22.
//

#ifndef ast_hpp
#define ast_hpp

#include <string>
#include <vector>
#include <variant>

#include "utils.hpp"

namespace Cordial {

struct NodoMeta {
    DebugPosition position;
    std::string file;
};

struct Nodo;
using nodo_ptr = std::shared_ptr<Nodo>;

struct NodoPrograma {
    std::vector<nodo_ptr> hijos;

    explicit NodoPrograma(std::vector<nodo_ptr> hijos_):
        hijos(hijos_)
    {}
};

struct NodoBloque {
    std::vector<nodo_ptr> hijos;
};

struct NodoMuestra {
    nodo_ptr expr;
};

struct NodoTexto {
    std::string contenido;
};

struct NodoNumero {
    std::string contenido;
};

struct NodoSuma {
    nodo_ptr lhs;
    nodo_ptr rhs;
};

struct NodoResta {
    nodo_ptr lhs;
    nodo_ptr rhs;
};

struct NodoMulti {
    nodo_ptr lhs;
    nodo_ptr rhs;
};

struct NodoDivi {
    nodo_ptr lhs;
    nodo_ptr rhs;
};

// Make more clear with macros?
using contenido_nodo = std::variant<
    NodoPrograma,
    NodoBloque,
    NodoMuestra,
    NodoTexto,
    NodoNumero,
    NodoSuma,
    NodoResta,
    NodoMulti,
    NodoDivi
>;

struct Nodo {
    NodoMeta meta;
    contenido_nodo nodo;
    Nodo(NodoMeta meta_, contenido_nodo nodo_):
        meta(meta_),
        nodo(nodo_)
    {}
};

}

#endif /* ast_hpp */
