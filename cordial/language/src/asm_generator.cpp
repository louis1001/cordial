//
// Created by Luis Gonzalez on 9/2/22.
//

#include <iostream>

#include "asm_generator.hpp"
#include "valor.hpp"

#define INST(name, args) output << "  "<< #name << "\t\t" << args << "\n"
#define COMM(txt) output << "//\t" txt << "\n"

namespace Cordial {
    void AsmGenerator::visit(const nodo_ptr &node, std::stringstream& output) const {
        val_ptr result;
        std::visit(
            overloaded{
               [this, &output](const NodoPrograma& programa) {
                   output << ".global _start\n";
                   output << ".p2align 2\n";
                   output << "_start:\n";
                   COMM("Code starts\n");
                   output << ".text\n";
                   const_cast<std::vector<std::string>&>(strings).clear();

                   for (const auto& hijo : programa.hijos) {
                       visit(hijo, output);
                   }
                   output << "\n";
                   COMM("Código de salida (exit code)");
                   INST(mov, "X0, #0");
                   INST(mov, "X16, #1");
                   INST(svc, "#0x18");

                   output << "\n\n";
                   COMM("Constante para saltos de linea");
                   output << "__new_line:\t\t\t";
                   output << ".byte 10\n";
                   for (int i = 0; i < strings.size(); i++) {
                       auto str = strings.at(i);
                       output << ".p2align 2\n";
                       output << "string_literal_" << i << ":\t";
                       output << ".ascii ";
                       output << "\"" << str << "\"\n";
                   }
               },
               [this, &output](const NodoBloque& bloque) {
                   for (const auto& hijo : bloque.hijos) {
                       visit(hijo, output);
                   }
               },
               [this, &output](const NodoMuestra& muestra) {
                   COMM("Muestra {");
                   INST(mov, "X0, #1");
                   visit(muestra.expr, output);

                   INST(mov, "X16, #4");
                   INST(svc, "#0x18");
                   COMM("}");
               },
               [&output](const NodoBaja& ) {
                   COMM("Baja {");
                   INST(mov, "X0, #1");
                   INST(adr, "x1, __new_line");
                   INST(mov, "x2, 1");
                   INST(mov, "X16, #4");
                   INST(svc, "#0x18");
                   COMM("}");
               },
               [this, &output](const NodoTexto& texto) {
                   COMM("Texto '" + texto.contenido + "' (");
                   auto con = texto.contenido;
                   const_cast<std::vector<std::string>&>(strings).push_back(con);
                   INST(adr, "X1, string_literal_" << strings.size()-1);
                   INST(mov, "X2, " << con.size());
                   COMM(")");
               },
               [/*&output*/](const NodoNumero& numero) {
//                   auto como_entero = std::stoi(numero.contenido);
//                   result = std::make_shared<Valor>(tipo_numero(), ValorNumero{como_entero});
               },
               [/*this, &output*/](const NodoSuma& suma) {
//                   auto lhs = visit(suma.lhs, output);
//                   auto operando_iz = get<ValorNumero>(lhs->contenido).valor;
//
//                   auto rhs = visit(suma.rhs, output);
//                   auto operando_de = get<ValorNumero>(rhs->contenido).valor;
//
//                   result = std::make_shared<Valor>(tipo_numero(), ValorNumero{ operando_iz + operando_de });
               },
               [/*this, &output*/](const NodoResta& resta) {
//                   auto lhs = visit(resta.lhs, output);
//                   auto operando_iz = get<ValorNumero>(lhs->contenido).valor;
//
//                   auto rhs = visit(resta.rhs, output);
//                   auto operando_de = get<ValorNumero>(rhs->contenido).valor;
//
//                   result = std::make_shared<Valor>(tipo_numero(), ValorNumero{ operando_iz - operando_de });
               },
               [/*this, &output*/](const NodoMulti& multi) {
//                   auto lhs = visit(multi.lhs, output);
//                   auto operando_iz = get<ValorNumero>(lhs->contenido).valor;
//
//                   auto rhs = visit(multi.rhs, output);
//                   auto operando_de = get<ValorNumero>(rhs->contenido).valor;
//
//                   result = std::make_shared<Valor>(tipo_numero(), ValorNumero{ operando_iz * operando_de });
               },
               [/*this, &output*/](const NodoDivi& divis) {
//                   auto lhs = visit(divis.lhs, output);
//                   auto operando_iz = get<ValorNumero>(lhs->contenido).valor;
//
//                   auto rhs = visit(divis.rhs, output);
//                   auto operando_de = get<ValorNumero>(rhs->contenido).valor;
//
//                   result = std::make_shared<Valor>(tipo_numero(), ValorNumero{ operando_iz / operando_de });
               }
           },
           node->contenido
        );
    }
};