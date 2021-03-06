//
// Created by Luis Gonzalez on 8/2/22.
//

#include "ast_dump.h"
#include <sstream>
#include <variant>

namespace Cordial {

    std::string indentation(int count) {
        static int num_per_level{4};
        std::stringstream spaces;

        for (int i = 0; i < count; i++) {
            for (int j = 0; j < num_per_level; j++) {
                spaces << " ";
            }
        }

        return spaces.str();
    }

    std::string ASTDumper::visit(const nodo_ptr &node, int counter) const {
        std::stringstream result;

        auto id = indentation(counter);

        std::visit(overloaded{
               [&result, id, counter, this](const NodoPrograma& programa) {
                   result << id << "Programa {\n";

                    for (const auto& hijo : programa.hijos) {
                        result << visit(hijo, counter+1);
                    }

                   result << id << "}";
               },
               [&result, id, counter, this](const NodoBloque& bloque) {
                   result << id << "Bloque {\n";

                   for (const auto& hijo : bloque.hijos) {
                       result << visit(hijo, counter+1);
                   }

                   result << id << "}";
               },
               [&result, id, counter, this](const NodoSi& si) {
                   result << id << "Si (\n";
                   result << visit(si.cond, counter+1);
                   result << id << ") Entonces {\n";
                   result << visit(si.cuerpo, counter+1);
                   result << id << "}";
               },
               [&result, id, counter, this](const NodoMuestra& muestra) {
                   result << id << "Muestra (\n";
                   result << visit(muestra.expr, counter+1);
                   result << id << ")";
               },
               [&result, id](const NodoBaja&) {
                   result << id << "SaltoDeLinea ()";
               },
               [&result, id](const NodoTexto& texto) {
                   result << id << "Texto(`" << texto.contenido << "`)";
               },
               [&result, id](const NodoNumero& numero) {
                   result << id << "Numero(" << numero.contenido << ")";
               },
               [&result, id](const NodoVerdad& verdad) {
                   result << id << "Verdad(" << (verdad.contenido ? "cierto" : "falso") << ")";
               },
               [&result, id, counter, this](const NodoSuma& suma) {
                   result << id << "Suma(\n";
                   result << visit(suma.lhs, counter+1);
                   result << visit(suma.rhs, counter+1);
                   result << id << ")";
               },
               [&result, id, counter, this](const NodoResta& resta) {
                   result << id << "Resta(\n";
                   result << visit(resta.lhs, counter+1);
                   result << visit(resta.rhs, counter+1);
                   result << id << ")";
               },
               [&result, id, counter, this](const NodoMulti& multiplicacion) {
                   result << id << "Multiplicaci??n(\n";
                   result << visit(multiplicacion.lhs, counter+1);
                   result << visit(multiplicacion.rhs, counter+1);
                   result << id << ")";
               },
               [&result, id, counter, this](const NodoDivi& division) {
                   result << id << "Divisi??n(\n";
                   result << visit(division.lhs, counter+1);
                   result << visit(division.rhs, counter+1);
                   result << id << ")";
               },
               [&result, id, counter, this](const NodoIgual& igual) {
                   result << id << "Igualdad(\n";
                   result << visit(igual.lhs, counter+1);
                   result << visit(igual.rhs, counter+1);
                   result << id << ")";
               }
           },
            node->contenido
        );

        auto position = node->meta.position;
        auto file = node->meta.file;

        result << "\t\t\t\t\t (" << file << ":" << position.line << ":" << position.col << ")";

        result << "\n";

        return result.str();
    }

}