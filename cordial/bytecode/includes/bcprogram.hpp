//
// Created by Luis Gonzalez on 12/3/22.
//

#ifndef CORDIAL_BCPROGRAM_H
#define CORDIAL_BCPROGRAM_H

#include "OpCodes.hpp"

#include <utility>
#include <vector>
#include <string>

namespace Cordial::Bytecode {
    struct Program {
    protected:
        std::vector<u8> m_code;

    public:
        Program() = default;
        explicit Program(const std::string& filename);
        explicit Program(std::vector<u8>  code);

        u64 size() { return m_code.size(); }

        void insert(u8 val) { m_code.push_back(val); }
        template<typename ut>
        void set(u64 addr, ut val);

        const std::vector<u8> & code() const { return m_code; }
    };

    struct ProgramBuilder {
    private:
        Program the_program;

        void insert(const std::vector<u8>& bytes) {
            for(auto n : bytes) { the_program.insert(n); }
        }

    public:
        u64 pc() { return the_program.size(); }

        explicit ProgramBuilder(Program prg): the_program(std::move(prg)) {}
        ProgramBuilder() = default;

        template<typename ut>
        void set(u64 addr, ut val);

        void inst(Op inst, const std::vector<u8>& operands = {});

        void inst(Op inst, u8 arg, const std::vector<u8>& operands = {});

        void inst(Op inst, u8 arg1, u8 arg2);

        void str(u8 reg, const std::string_view new_string);

        inline void ext() { the_program.insert(EXT); }

        inline void push(u8 reg) { inst(PSH, reg); }

        inline void pop(u8 reg) { inst(POP, reg); }

        void whl(const std::function<void()>& preparation, u8 comparisson, const std::function<void()>& body);

        template<typename ut>
        static std::vector<u8> repr(ut val);

        void dump(const std::string& file);

        void print_int(u8 reg);

        void print();

        const Program& program() { return the_program; }
    };
}

#endif //CORDIAL_BCPROGRAM_H
