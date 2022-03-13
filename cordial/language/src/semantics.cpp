//
// Created by Luis Gonzalez on 9/2/22.
//

#include "semantics.hpp"

namespace Cordial {
    std::shared_ptr<Simbolo> Semantics::visit(nodo_ptr &node) const {
        std::shared_ptr<Simbolo> result;
        std::visit(
            overloaded{
               [this](NodoPrograma& programa) {
                   for (auto& hijo : programa.hijos) {
                       visit(hijo);
                   }
               },
               [this](NodoBloque& bloque) {
                   for (auto& hijo : bloque.hijos) {
                       visit(hijo);
                   }
               },
               [this](NodoSi& si) {
                   auto cond_type = visit(si.cond);
                   assert(cond_type == tipo_verdad());

                   visit(si.cuerpo);
               },
               [this](NodoMuestra& muestra) {
                   auto ret_type = visit(muestra.expr);

                   // Only types string for now
                   muestra.es_txt = ret_type == tipo_texto();
               },
               [](NodoBaja&) { /* This one just prints */},
               [&result](NodoTexto&) {
                   result = tipo_texto();
               },
               [&result](NodoNumero&) {
                   result = tipo_numero();
               },
               [&result](NodoVerdad&) {
                   result = tipo_verdad();
               },
               [this, &result](NodoSuma& suma) {
                   auto lhs = visit(suma.lhs);
                   assert(lhs == tipo_numero());
                   auto rhs = visit(suma.rhs);
                   assert(rhs == tipo_numero());
                   result = tipo_numero();
               },
               [this, &result](NodoResta& resta) {
                   auto lhs = visit(resta.lhs);
                   assert(lhs == tipo_numero());
                   auto rhs = visit(resta.rhs);
                   assert(rhs == tipo_numero());
                   result = tipo_numero();
               },
               [this, &result](NodoMulti& multi) {
                   auto lhs = visit(multi.lhs);
                   assert(lhs == tipo_numero());
                   auto rhs = visit(multi.rhs);
                   assert(rhs == tipo_numero());
                   result = tipo_numero();
               },
               [this, &result](NodoDivi& divis) {
                   auto lhs = visit(divis.lhs);
                   assert(lhs == tipo_numero());
                   auto rhs = visit(divis.rhs);
                   assert(rhs == tipo_numero());
                   result = tipo_numero();
               },
               [this, &result](NodoIgual& igual) {
                   auto lhs = visit(igual.lhs);
                   auto rhs = visit(igual.rhs);
                   assert(lhs == rhs);
                   assert(lhs == tipo_numero() || lhs == tipo_verdad());
                   result = tipo_verdad();
               }
           },
           node->contenido
        );

        return result;
    }
};