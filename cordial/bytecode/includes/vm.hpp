//
// Created by Luis Gonzalez on 12/3/22.
//

#ifndef CORDIAL_VM_HPP
#define CORDIAL_VM_HPP
#include "bcprogram.hpp"

#include <array>
#include <stack>

/*
 *  Fibonacci sequence built on bytecode.
 *
int main() {
    u8 a = 0x0;
    u8 b = 0x1;
    u8 c = 0x2;
    u8 limite = 0x5;

    auto p = std::make_shared<Program>();

    p->str(0x12, "Fibonacci:\n");
    p->inst(PNT, 0x12);

    p->inst(LDR, limite, Program::repr<u64>(10000000));
    p->inst(LDR, a, Program::repr<u64>(0));
    p->inst(LDR, b, Program::repr<u64>(1));
    p->inst(LDR, c, Program::repr<u64>(0));

    p->inst(LDR, 0xA, Program::repr<u64>(1));

    p->whl([=]{ p->inst(LT, 0x7, {c, limite}); },
   0x7,
    [=]{
        p->inst(CP, a, b);
        p->inst(CP, b, c);
        p->inst(ADD, c, a);

        p->print_int(0xA);
        p->inst(INC, 0xA);

        p->inst(LDR, 0xB, Program::repr<u64>(':'));
        p->inst(PCH, 0xB);
        p->inst(LDR, 0xB, Program::repr<u64>(' '));
        p->inst(PCH, 0xB);

        p->print_int(c);
        p->inst(LDR, 0x0, Program::repr<u64>('\n'));
        p->inst(PCH, 0x0);
    });

    p->ext();

    p->dump("./fib.bycord");

    auto loaded = std::make_shared<Program>("./fib.bycord");

    BytecodeVM vm(loaded);
    vm.execute();

    return 0;
}*/

namespace Cordial::Bytecode {
    struct VM {
        explicit VM(Program program_)
            : program(std::move(program_)) {}

    private:
        std::array<u64, 32> registers{};
        std::stack <u64> stack;
        u64 pc = 0;

        std::vector <std::string> strings{};

        const Program program;

        const std::vector <u8>& code() { return program.code(); }

        std::vector<u8> take(u64 bytes);

        inline u8 take_byte() { return code()[pc++]; }

        template<typename ut>
        ut take();

        std::vector <u8> take_until(u8 delimeter);

        void execute_byte();

    public:

        void execute();

        void dump();
    };

}

#endif //CORDIAL_VM_HPP
