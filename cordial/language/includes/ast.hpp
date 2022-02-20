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

struct NodoSi {
    nodo_ptr cond;
    nodo_ptr cuerpo;
};

struct NodoMuestra {
    nodo_ptr expr;
};

struct NodoBaja {};

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

struct NodoIgual {
    nodo_ptr lhs;
    nodo_ptr rhs;
};

// Make more clear with macros?
using contenido_nodo = std::variant<
    NodoPrograma,
    NodoBloque,
    NodoSi,
    NodoMuestra,
    NodoBaja,
    NodoTexto,
    NodoNumero,
    NodoSuma,
    NodoResta,
    NodoMulti,
    NodoDivi,

    NodoIgual
>;

struct Nodo {
    NodoMeta meta;
    contenido_nodo contenido;
    Nodo(NodoMeta meta_, contenido_nodo nodo_):
            meta(meta_),
            contenido(nodo_)
    {}
};

}

#endif /* ast_hpp */
