//
// Created by Luis Gonzalez on 9/2/22.
//

#include <iostream>

#include "interpreter.hpp"
#include "valor.hpp"

namespace Cordial {
    val_ptr Interpreter::visit(const nodo_ptr &node) const {
        val_ptr result;
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
               [this](const NodoMuestra& muestra) {
                   auto value = visit(muestra.expr);
                   if (const auto num_value = get_if<ValorNumero>(&value->contenido)) {
                       std::cout << num_value->valor;
                   } else if (const auto text_value = get_if<ValorTexto>(&value->contenido)) {
                        std::cout << text_value->valor;
                   }
               },
               [](const NodoBaja& ) {
                   std::cout << "\n";
               },
               [&result](const NodoTexto& texto) {
                   result = std::make_shared<Valor>(tipo_texto(), ValorTexto{texto.contenido});
               },
               [&result](const NodoNumero& numero) {
                   auto como_entero = std::stoi(numero.contenido);
                   result = std::make_shared<Valor>(tipo_numero(), ValorNumero{como_entero});
               },
               [this, &result](const NodoSuma& suma) {
                   auto lhs = visit(suma.lhs);
                   auto operando_iz = get<ValorNumero>(lhs->contenido).valor;

                   auto rhs = visit(suma.rhs);
                   auto operando_de = get<ValorNumero>(rhs->contenido).valor;

                   result = std::make_shared<Valor>(tipo_numero(), ValorNumero{ operando_iz + operando_de });
               },
               [this, &result](const NodoResta& resta) {
                   auto lhs = visit(resta.lhs);
                   auto operando_iz = get<ValorNumero>(lhs->contenido).valor;

                   auto rhs = visit(resta.rhs);
                   auto operando_de = get<ValorNumero>(rhs->contenido).valor;

                   result = std::make_shared<Valor>(tipo_numero(), ValorNumero{ operando_iz - operando_de });
               },
               [this, &result](const NodoMulti& multi) {
                   auto lhs = visit(multi.lhs);
                   auto operando_iz = get<ValorNumero>(lhs->contenido).valor;

                   auto rhs = visit(multi.rhs);
                   auto operando_de = get<ValorNumero>(rhs->contenido).valor;

                   result = std::make_shared<Valor>(tipo_numero(), ValorNumero{ operando_iz * operando_de });
               },
               [this, &result](const NodoDivi& divis) {
                   auto lhs = visit(divis.lhs);
                   auto operando_iz = get<ValorNumero>(lhs->contenido).valor;

                   auto rhs = visit(divis.rhs);
                   auto operando_de = get<ValorNumero>(rhs->contenido).valor;

                   result = std::make_shared<Valor>(tipo_numero(), ValorNumero{ operando_iz / operando_de });
               },
               [this, &result](const NodoIgual& igual) {
                   auto lhs = visit(igual.lhs);
                   auto rhs = visit(igual.rhs);

                   if (lhs->tipo == tipo_numero()) {
                       auto operando_iz = get<ValorNumero>(lhs->contenido).valor;
                       auto operando_de = get<ValorNumero>(rhs->contenido).valor;

                       result = std::make_shared<Valor>(tipo_verdad(), ValorVerdad{ operando_iz == operando_de });
                   } else { // Tipo verdad
                       auto operando_iz = get<ValorVerdad>(lhs->contenido).valor;
                       auto operando_de = get<ValorVerdad>(rhs->contenido).valor;

                       result = std::make_shared<Valor>(tipo_verdad(), ValorVerdad{ operando_iz == operando_de });
                   }
               }
           },
           node->contenido
        );

        return result;
    }
};