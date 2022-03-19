//
// Created by Luis Gonzalez on 12/3/22.
//

#include <fstream>
#include <sstream>
#include <iostream>
#include <utility>

#include "utils.hpp"
#include "bcprogram.hpp"

Cordial::Bytecode::Program::Program(const std::string &filename) {
    std::ifstream f(filename);

    if (!f.is_open()){
        std::cerr << "Couldn't open file `" << filename << "`\n";
        exit(1);
    }

    char byte;
    while(f.get(byte)) {
        code.push_back(byte);
    }
}

Cordial::Bytecode::Program::Program(std::vector<u8>  code_): code(std::move(code_)) {}

template<typename ut>
void Cordial::Bytecode::Program::set(u64 addr, ut val) {

    auto sz = sizeof(ut);
    assert(code.size() - addr > sz);

    for (int i = sz-1; i >= 0; i--) {
        u8 byte = val & 0xFF;
        code[addr+i] = byte;

        val >>= 8;
    }
}

void Cordial::Bytecode::Program::inst(Op inst, const std::vector<u8>& operands) {
    code.push_back(inst);

    for(auto n : operands) { code.push_back(n); }
}

void Cordial::Bytecode::Program::inst(Op inst, u8 arg, const std::vector<u8>& operands) {
    code.push_back(inst);

    code.push_back(arg);

    for(auto n : operands) { code.push_back(n); }
}

void Cordial::Bytecode::Program::inst(Op inst, u8 arg1, u8 arg2) {
    code.push_back(inst);

    code.push_back(arg1);
    code.push_back(arg2);
}

void Cordial::Bytecode::Program::str(u8 reg, const std::string_view new_string) {
    code.push_back(STR);

    code.push_back(reg);

    for (auto c : new_string) {
        code.push_back(c);
    }

    code.push_back('\0');
}

void Cordial::Bytecode::Program::whl(const std::function<void()> &preparation, u8 comparisson,
                                     const std::function<void()> &body) {
    u64 while_start = pc();
    preparation();

    inst(JPF, comparisson, repr<u64>(0));
    u64 jump_target = pc() - sizeof(u64);

    body();

    inst(JMP, repr<u64>(while_start));
    set<u64>(jump_target, pc());
}

template<typename ut>
std::vector<u8> Cordial::Bytecode::Program::repr(ut val) {
    auto sz = sizeof(ut);
    std::vector<u8> result(sz);
    for (auto i = sz-1; val > 0; i--) {
        u8 byte = val & 0xFF;
        result[i] = byte;

        val >>= 8;
    }

    return result;
}

void Cordial::Bytecode::Program::dump(const std::string &file) {
    std::fstream f(file, std::ios_base::out | std::ios_base::binary);

    for (auto b : bytes()) {
        f << b;
    }

    f.close();
}

void Cordial::Bytecode::Program::print_int(u8 reg)  {
    u8 ten = 0x1;
    u8 counter = 0x2;
    u8 zero_char = 0x3;
    u8 character = 0x4;
    u8 comp = 0x5;
    u8 zero = 0x6;
    u8 input = 0x7;

    push(reg);
    push(ten);
    push(zero_char);
    push(counter);
    push(character);
    push(comp);
    push(zero);
    push(input);

    inst(CP, input, reg);
    inst(LDR, counter, Program::repr<u64>(0));
    inst(LDR, ten, Program::repr<u64>(10));
    inst(LDR, zero, Program::repr<u64>(0));
    inst(LDR, zero_char, Program::repr<u64>('0'));

    inst(NOP);


    whl([&]{
            inst(GT, comp, {input, zero});
        }, comp,
        [&]{
            inst(CP, character, input);
            inst(MOD, character, ten);
            inst(ADD, character, zero_char);

            push(character);
            inst(INC, counter);

            inst(DIV, input, ten);
        });

    whl(
            [&]{ inst(GT, comp, {counter, zero}); },
            comp,
            [&]{
                inst(DEC, counter);

                pop(character);
                inst(PCH, character);
            });

    pop(input);
    pop(zero);
    pop(comp);
    pop(character);
    pop(counter);
    pop(zero_char);
    pop(ten);
    pop(reg);
}

void Cordial::Bytecode::Program::print()  {
    std::stringstream ss;
    for(auto i = 0; i < bytes().size(); i++) {
        ss << std::dec << std::setw(4) << i << ":\t";

        auto b = static_cast<Op>(bytes()[i]);

        ss << op_name(b) << "\t";
        switch (b) {
            case EXT:
                break;
            case NOP:
                break;
            case LDR: {
                print_hex(static_cast<int>(bytes()[++i]), 2, ss);
                ss << ", ";

                auto result = 0;
                for (int j = 0; j < sizeof(u64); j++) {
                    result <<= 8;
                    result |= bytes()[++i];
                }

                ss << "#" << result;
                break;
            }
            case ADD: {
                print_hex(static_cast<int>(bytes()[++i]), 2, ss);
                ss << ", ";
                print_hex(static_cast<int>(bytes()[++i]), 2, ss);
                break;
            }
            case SUB: {
                print_hex(static_cast<int>(bytes()[++i]), 2, ss);
                ss << ", ";
                print_hex(static_cast<int>(bytes()[++i]), 2, ss);
                break;
            }
            case PSH:
                print_hex(static_cast<int>(bytes()[++i]), 2, ss);
                break;
            case POP:
                print_hex(static_cast<int>(bytes()[++i]), 2, ss);
                break;
            default:
                ss << "(unhandled)";
                break;
        }

        ss << "\n";
    }

    std::cout << ss.str() << "\n";
}