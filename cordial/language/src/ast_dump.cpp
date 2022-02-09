//
// Created by Luis Gonzalez on 8/2/22.
//

#include "ast_dump.h"
#include <sstream>
#include <iostream>
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

    template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; }; // (1)
    template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;
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
               [&result, id, counter, this](const NodoMuestra& muestra) {
                   result << id << "Muestra (\n";
                   result << visit(muestra.expr, counter+1);
                   result << id << ")";
               },
               [&result, id](const NodoTexto& texto) {
                   result << id << "Texto(" << texto.contenido << ")";
               },
               [&result, id](const NodoNumero& numero) {
                   result << id << "Numero(" << numero.contenido << ")";
               }
           },
            node->nodo
        );

        auto position = node->meta.position;
        auto file = node->meta.file;

        result << " (" << file << ":" << position.line << ":" << position.col << ")";

        result << "\n";

        return result.str();
    }

}