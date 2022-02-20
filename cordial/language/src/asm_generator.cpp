//
// Created by Luis Gonzalez on 9/2/22.
//

#include <iostream>

#include "asm_generator.hpp"
#include "valor.hpp"

#define INST(name, args) output << "  "<< #name << "\t\t" << args << "\n"
#define COMM(txt) output << "//\t" txt << "\n"

#define PUSH(reg) COMM("Push: " #reg "{");\
                  INST(str, #reg ", [sp, #-16]!"); \
                  COMM("}")

#define POP(reg) COMM("Pop to: " #reg "{");\
                  INST(ldr, #reg ", [sp], #16"); \
                  COMM("}")

#define TAG(name) output << name << ":\n"

namespace Cordial {
    std::string AsmGenerator::new_tag(const std::string& prefix) {
        std::stringstream result;
        result << prefix << tag_counter++;
        return result.str();
    }

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
                   // Temporalmente usar el resultado de X1
                   // Para exit code
                   INST(mov, "X0, X1");
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

                   output << "\n";
                   COMM("Boolean literals");
                   output << ".p2align 2\n";
                   output << "TRUE:\t\t\t";
                   output << ".byte 1\n\n";

                   output << ".p2align 2\n";
                   output << "FALSE:\t\t\t";
                   output << ".byte 0\n";
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
               [&output](const NodoNumero& numero) {
                   auto num = std::stoi(numero.contenido);
                   COMM("Numero literal: `" << num << "`(");
                   INST(mov, "X1, #" << num);
                   COMM(")");
               },
               [this, &output](const NodoSuma& suma) {
                   COMM("Suma (");
                   visit(suma.lhs, output);
                   PUSH(x1);

                   visit(suma.rhs, output);
                   POP(x2);
                   INST(add, "X1, X1, X2");
                   COMM(")");
               },
               [this, &output](const NodoResta& resta) {
                   COMM("Resta (");
                   visit(resta.lhs, output);
                   PUSH(x1);

                   visit(resta.rhs, output);
                   POP(x2);
                   INST(sub, "X1, X1, X2");
                   COMM(")");
               },
               [this, &output](const NodoMulti& multi) {
                   COMM("Multiplicación (");
                   visit(multi.lhs, output);
                   PUSH(x1);

                   visit(multi.rhs, output);
                   POP(x2);
                   INST(mul, "X1, X1, X2");
                   COMM(")");
               },
               [/*this, &output*/](const NodoDivi& divis) {
//                   auto lhs = visit(divis.lhs, output);
//                   auto operando_iz = get<ValorNumero>(lhs->contenido).valor;
//
//                   auto rhs = visit(divis.rhs, output);
//                   auto operando_de = get<ValorNumero>(rhs->contenido).valor;
//
//                   result = std::make_shared<Valor>(tipo_numero(), ValorNumero{ operando_iz / operando_de });
               },
               [this, &output](const NodoIgual& igual) {
                   COMM("Igual (");
                   visit(igual.lhs, output);
                   PUSH(x1);

                   visit(igual.rhs, output);
                   POP(x2);
                   INST(cmp, "X1, X2");

                   auto non_const_this = const_cast<AsmGenerator*>(this);
                   auto false_tag = non_const_this->new_tag("igualdad_es_false_");
                   non_const_this->tag_counter--;
                   auto exit_tag = non_const_this->new_tag("salida_igualdad_");
                   INST(bne, false_tag);
                   COMM("If X1 is equal to X2");
                   INST(adr, "X1, TRUE");
                   INST(b, exit_tag);

                   TAG(false_tag);
                   COMM("If X1 is not equal to X2");
                   INST(adr, "X1, FALSE");

                   TAG(exit_tag);

                   COMM(")");
               }
           },
           node->contenido
        );
    }
};