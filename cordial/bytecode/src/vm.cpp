//
// Created by Luis Gonzalez on 12/3/22.
//

#include "vm.hpp"
#include "utils.hpp"

#include <iostream>

std::vector<u8> Cordial::Bytecode::VM::take(u64 bytes)  {
    std::vector <u8> result(bytes);
    for (auto i = 0; i < bytes; i++) {
        result.push_back(take_byte());
    }

    return result;
}

template<typename ut>
ut Cordial::Bytecode::VM::take() {
    ut result{0};
    for (auto i = 0; i < sizeof(ut); i++) {
        auto byte = take_byte();
        result = (result << 8) | byte;
    }

    return result;
}

std::vector<u8> Cordial::Bytecode::VM::take_until(u8 delimeter)  {
    std::vector <u8> result{};

    while (code()[pc] != delimeter) {
        result.push_back(take_byte());
    }

    result.push_back(take_byte());

    return result;
}

void Cordial::Bytecode::VM::dump() {
    std::cout << "\n*********************************\n";

    for (auto i = 0; i < registers.size(); i++) {
        auto value = registers[i];
        if (value != 0) {
            std::cout << "R" << i << ":\t";
            print_hex(value);
            std::cout << "\n";
        }
    }

    std::cout << "\n*********************************\n";
}

void Cordial::Bytecode::VM::execute_byte() {
    switch (take_byte()) {
        case NOP:
            break;
        case EXT:
            // end program.
            pc = (int) code().size();
            break;

        case LDR: {
            // load 64 bit integer into something?
            auto reg = take_byte();

            auto loaded = take<u64>();
#if DEBUG
            std::cout << "Put ";
                print_hex(loaded);
                std::cout << " into R" << static_cast<int>(reg) << "\n";
#endif
            registers[reg] = loaded;

            break;
        }
        case ADD: {
            auto dst = take_byte();
            auto src = take_byte();

            auto result = registers[dst] + registers[src];
            registers[dst] = result;
#if DEBUG
            std::cout << "Adding R" << static_cast<int>(dst) << " and R" << static_cast<int>(src) << " = ";
                print_hex(result);
                std::cout << "\n";
#endif
            break;
        }
        case SUB: {
            auto dst = take_byte();
            auto src = take_byte();

            auto result = registers[dst] - registers[src];
            registers[dst] = result;
#if DEBUG
            std::cout << "Adding R" << static_cast<int>(dst) << " and R" << static_cast<int>(src) << " = ";
                print_hex(result);
                std::cout << "\n";
#endif
            break;
        }
        case EQU: {
            auto dst = take_byte();

            auto lhs = take_byte();
            auto rhs = take_byte();

            registers[dst] = registers[lhs] == registers[rhs];
            break;
        }
        case GT: {
            auto dst = take_byte();

            auto lhs = take_byte();
            auto rhs = take_byte();

            auto result = registers[lhs] > registers[rhs];
            registers[dst] = result;
#if DEBUG
            std::cout << "Is ";
                print_hex(registers[lhs]);
                std::cout << " greater than ";
                print_hex(registers[rhs]);

                std::cout << "? " << (result ? "yes" : "no") << "\n";
#endif
            break;
        }
        case LT: {
            auto dst = take_byte();

            auto lhs = take_byte();
            auto rhs = take_byte();

            auto result = registers[lhs] < registers[rhs];
            registers[dst] = result;
#if DEBUG
            std::cout << "Is ";
                print_hex(registers[lhs]);
                std::cout << " less than ";
                print_hex(registers[rhs]);

                std::cout << "? " << (result ? "yes" : "no") << "\n";
#endif
            break;
        }
        case CP: {
            auto dst = take_byte();
            auto src = take_byte();
#if DEBUG
            std::cout << "Copy R" << static_cast<int>(src) << " into R" << static_cast<int>(dst) << "\n";
#endif
            registers[dst] = registers[src];
            break;
        }
        case JMP: {
            auto new_pc = take<u64>();

            pc = new_pc;

            break;
        }
        case JPF: {
            auto reg = take_byte();
            u64 new_pc = take<u64>();

            auto result = registers[reg];
            if (!result) {
                pc = new_pc;
            }

            break;
        }
        case JPT: {
            auto reg = take_byte();
            auto new_pc = take<u64>();

            auto result = registers[reg];
            if (result) {
                pc = new_pc;
            }

            break;
        }
        case SHR: {
            auto reg = take_byte();
            std::cout << registers[reg] << "\n";
            break;
        }
        case STR: {
            auto reg = take_byte();

            auto chars = take_until('\0');

            std::string str(chars.begin(), chars.end() - 1);

            // Store the string
            auto str_address = strings.size();
            strings.push_back(str);

            registers[reg] = str_address;
            break;
        }
        case PNT: {
            auto reg = take_byte();

            auto str_address = registers[reg];

            auto str = strings[str_address];

            std::cout << str;

            break;
        }
        case PCH: {
            auto reg = take_byte();

            std::cout << static_cast<u8>(registers[reg] & 0xFF);
            break;
        }
        case INC: {
            auto reg = take_byte();
            registers[reg]++;
            break;
        }
        case DEC: {
            auto reg = take_byte();
            registers[reg]--;
            break;
        }
        case PSH: {
            auto reg = take_byte();
            stack.push(registers[reg]);
            break;
        }
        case POP: {
            auto reg = take_byte();
            registers[reg] = stack.top();
            stack.pop();
            break;
        }
        case DIV: {
            auto dst = take_byte();
            auto src = take_byte();

            auto result = registers[dst] / registers[src];
            registers[dst] = result;
#if DEBUG
            std::cout << "Dviding R" << static_cast<int>(dst) << " and R" << static_cast<int>(src) << " = ";
                print_hex(result);
                std::cout << "\n";
#endif
            break;
        }
        case MOD: {
            auto dst = take_byte();
            auto src = take_byte();

            auto result = registers[dst] % registers[src];
            registers[dst] = result;
#if DEBUG
            std::cout << "Adding R" << static_cast<int>(dst) << " and R" << static_cast<int>(src) << " = ";
                print_hex(result);
                std::cout << "\n";
#endif
            break;
        }
        default:
            std::cout << "Código invalido (pc: " << pc-1 << "): ";
            print_hex(static_cast<int>(code()[pc - 1]), 2);
            auto name = op_name((Op) code()[pc - 1]);
            if (!name.empty()) {
                std::cout << " (" << name << " not handled)";
            }
            std::cout << "\n";
            exit(1);
    }
}

void Cordial::Bytecode::VM::execute() {
    while (pc < code().size()) {
        // Travel the whole code

        if (pc < 0) {
            throw std::exception();
        }
        execute_byte();
    }
}