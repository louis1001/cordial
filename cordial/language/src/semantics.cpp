//
// Created by Luis Gonzalez on 9/2/22.
//

#include "semantics.hpp"

namespace Cordial {
    std::shared_ptr<Simbolo> Semantics::visit(const nodo_ptr &node) const {
        std::shared_ptr<Simbolo> result;
        std::visit(
            overloaded{
               [this](const NodoPrograma& programa) {
                   for (const auto& hijo : programa.hijos) {
                       visit(hijo);
                   }
               },
               [this](const NodoBloque& bloque) {
                   for (const auto& hijo : bloque.hijos) {
                       visit(hijo);
                   }
               },
               [this](const NodoSi& si) {
                   auto cond_type = visit(si.cond);
                   assert(cond_type == tipo_verdad());

                   visit(si.cuerpo);
               },
               [this](const NodoMuestra& muestra) {
                   auto ret_type = visit(muestra.expr);

                   // Only types string for now
                   assert(ret_type != nullptr);
                   assert(ret_type == tipo_texto());
               },
               [](const NodoBaja&) { /* This one just prints */},
               [&result](const NodoTexto& texto) {
                   result = tipo_texto();
               },
               [&result](const NodoNumero& numero) {
                   result = tipo_numero();
               },
               [this, &result](const NodoSuma& suma) {
                   auto lhs = visit(suma.lhs);
                   assert(lhs == tipo_numero());
                   auto rhs = visit(suma.rhs);
                   assert(rhs == tipo_numero());
                   result = tipo_numero();
               },
               [this, &result](const NodoResta& resta) {
                   auto lhs = visit(resta.lhs);
                   assert(lhs == tipo_numero());
                   auto rhs = visit(resta.rhs);
                   assert(rhs == tipo_numero());
                   result = tipo_numero();
               },
               [this, &result](const NodoMulti& multi) {
                   auto lhs = visit(multi.lhs);
                   assert(lhs == tipo_numero());
                   auto rhs = visit(multi.rhs);
                   assert(rhs == tipo_numero());
                   result = tipo_numero();
               },
               [this, &result](const NodoDivi& divis) {
                   auto lhs = visit(divis.lhs);
                   assert(lhs == tipo_numero());
                   auto rhs = visit(divis.rhs);
                   assert(rhs == tipo_numero());
                   result = tipo_numero();
               },
               [this, &result](const NodoIgual& igual) {
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