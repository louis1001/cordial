
//  main.cpp
//  cordial
//
//  Created by Luis Gonzalez on 5/2/22.
//

#include <iostream>
#include <fstream>

#include "parser.hpp"
#include "semantics.hpp"
#include "interpreter.hpp"
#include "ast_dump.h"
#include "argh.h"

void usage(const std::string& command = "cordial") {
    std::cout   << "Uso: "
                << command << " [opciones] <archivo>\n\n"
                << "OPCIONES:\n"
                << "  -h, --help  \t\t" <<   "Muestra esta guía\n"
                << "  -u, --unsafe\t\t" <<   "Ejecuta sin utilizar analizador semántico\n"
                << "  -d, --dump  \t\t" <<   "Muestra el árbol de sintaxis\n";
}

int main(int argc, const char * argv[]) {
    auto cmdl = argh::parser(argc, argv);

    if (cmdl[{"-h", "--help"}]) {
        usage();
        return 0;
    }

    std::string filename;
    cmdl(1) >> filename;

    if (filename.empty()) {
        std::cerr << "Falta nombre del archivo." << std::endl;
        usage();
        return 1;
    }

    std::cout << filename << "\n";

    std::fstream source_file(filename);

    std::string code((std::istreambuf_iterator<char>(source_file)),
                     (std::istreambuf_iterator<char>()));

    source_file.close();

    // TODO: Debug position for tokens is not correct.

    Cordial::Parser parser;
    auto result = parser.parse(code, filename);

    if (!cmdl[{"-u", "--unsafe"}]) {
        Cordial::Semantics semAn;
        (void)semAn.visit(result);
    }

    if (cmdl[{"-d", "--dump"}]) {
        Cordial::ASTDumper visitor;
        auto tree = visitor.visit(result, 0);
        std::cout << tree;
        return 0;
    }

    Cordial::Interpreter inter;
    inter.visit(result);

//    Cordial::Lexer lx;
//
//    lx.setup(code, "anonymus");
//
//    auto tk = lx.get_next();
//    while (tk.has_value()) {
//        std::cout << tk.value().get_str() << "\n";
//        tk = lx.get_next();
//    }

    return 0;
}
