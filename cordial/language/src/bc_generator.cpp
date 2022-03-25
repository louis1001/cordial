//
// Created by Luis Gonzalez on 12/3/22.
//

#include "utils.hpp"
#include "bc_generator.hpp"

void Cordial::BCGenerator::visit(const Cordial::nodo_ptr & node, const std::shared_ptr<Bytecode::ProgramBuilder>& p) {
    std::visit(overloaded{
               [this, p](const NodoPrograma& programa) {
                   for (const auto& child : programa.hijos) {
                       visit(child, p);
                   }
               },
               [this, p](const NodoBloque& bloque) {
                   for (const auto& child : bloque.hijos) {
                       visit(child, p);
                   }
               },
               [](const NodoSi& si) {
                   TODO("Control de flujo sin implementar.");
               },
               [this, p](const NodoMuestra& muestra) {
                   visit(muestra.expr, p);
                   if (muestra.es_txt) {
                       p->inst(Bytecode::PNT, 0x1);
                   } else {
                       p->print_int(0x1);
                   }
               },
               [p](const NodoBaja&) {
                   p->inst(Bytecode::LDR, 0x3, Bytecode::ProgramBuilder::repr<u64>('\n'));
                   p->inst(Bytecode::PCH, 0x3);
               },
               [p](const NodoTexto& texto) {
                   p->str(0x1, texto.contenido);
               },
               [p](const NodoNumero& numero) {
                   auto n = std::stoi(numero.contenido);
                   p->inst(Bytecode::LDR, 0x1, Bytecode::ProgramBuilder::repr<u64>(n));
               },
               [](const NodoVerdad& verdad) {
                   TODO("Manejar verdades");
               },
               [this, p](const NodoSuma& suma) {
                   visit(suma.lhs, p);

                   p->push(0x1);

                   visit(suma.rhs, p);
                   p->pop(0x2);
                   p->inst(Bytecode::ADD, 0x1, 0x2);
               },
               [this, p](const NodoResta& resta) {
                   visit(resta.lhs, p);

                   p->push(0x1);

                   visit(resta.rhs, p);
                   // Hacer que vayan en el orden correcto antes de restar.
                   p->inst(Bytecode::CP, 0x2, 0x1);
                   p->pop(0x1);
                   p->inst(Bytecode::SUB, 0x1, 0x2);
               },
               [](const NodoMulti& multiplicacion) {
                   TODO("Manejar aritmetica");
               },
               [](const NodoDivi& division) {
                   TODO("Manejar aritmetica");
               },
               [](const NodoIgual& igual) {
                   TODO("Manejar comparaciones");
               }
       },
       node->contenido
    );
}

std::shared_ptr<Cordial::Bytecode::ProgramBuilder> Cordial::BCGenerator::generate(const Cordial::nodo_ptr & node) {
    auto p = std::make_shared<Bytecode::ProgramBuilder>();
    visit(node, p);

    return p;
}
