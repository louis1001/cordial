
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
#include "asm_generator.hpp"
#include "ast_dump.h"
#include "argh.h"
#include "bc_generator.hpp"
#include "vm.hpp"

void usage(const std::string& command = "cordial") {
    std::cout   << "Uso: "
                << command << " [opciones] <archivo>\n\n"
                << "OPCIONES:\n"
                << "  -c, --compiles <salida>\t" <<   "Genera asm y compila a un binario nativo\n"
                << "  -h, --help             \t" <<   "Muestra esta guía\n"
                << "  -u, --unsafe           \t" <<   "Ejecuta sin utilizar analizador semántico\n"
                << "  -d, --dump             \t" <<   "Muestra el árbol de sintaxis\n";
}

void execute(std::string name, std::string command, int separation = 0) {
    for (int i = 0; i < separation; ++i) {
        std::cout << "\n";
    }

    std::cout << "[Executing] " << command << std::endl;
    int status = std::system(command.c_str());
    if (status != 0) {
        std::cerr << "Error running " + name + "." << std::endl;
        exit(1);
    }
}

int main(int argc, const char * argv[]) {
    argh::parser cmdl{"--compile", "-c"};
    cmdl.parse(argc, argv);

    if (cmdl[{"-h", "--help"}]) {
        usage();
        return 0;
    }

    std::string filename;
    cmdl(1) >> filename;

    std::string compilation_output;
    cmdl({"--compile", "-c"}, "") >> compilation_output;

    if (filename.empty()) {
        std::cerr << "Falta nombre del archivo." << std::endl;
        usage();
        return 1;
    }

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

    Cordial::BCGenerator bc;
    auto p = bc.generate(result);

//    p->print();

    Cordial::Bytecode::VM vm{ p->program() };

    vm.execute();

    return 0;

    if (cmdl[{"-d", "--dump"}]) {
        Cordial::ASTDumper visitor;
        auto tree = visitor.visit(result, 0);
        std::cout << tree;
        return 0;
    }

    if (compilation_output.empty()) {
        Cordial::Interpreter inter;
        inter.visit(result);
    } else {
        std::filesystem::path file_ptr{filename};
        auto source_name = file_ptr.stem().string();
        auto asm_name = "./" + source_name + ".s";
        auto obj_output_name = "./" + source_name + ".o";

        std::stringstream output;
        Cordial::AsmGenerator gen;
        gen.visit(result, output);

        std::fstream asm_output(asm_name, std::fstream::out);

        if (asm_output.good()) {
            asm_output.clear();

            asm_output << output.str();
            asm_output.close();
        } else {
            std::cerr << "No se pudo abrir el archivo `" << asm_name << "` para generar el ensamblador.";
            asm_output.close();
            exit(1);
        }

        auto assembler_command = "as -arch arm64 -o " + obj_output_name + " " + asm_name;

        auto linker_command = "ld -o " + source_name  + " " + obj_output_name + " -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _start -arch arm64";

        execute("assembler", assembler_command);

        execute("linker", linker_command);

        execute("compiled program", "./" + source_name, 1);
    }

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
