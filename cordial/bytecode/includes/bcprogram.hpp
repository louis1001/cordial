//
// Created by Luis Gonzalez on 12/3/22.
//

#ifndef CORDIAL_BCPROGRAM_H
#define CORDIAL_BCPROGRAM_H

#include "OpCodes.hpp"

#include <vector>
#include <string>

namespace Cordial::Bytecode {
    struct Program {
    private:
        std::vector<u8> code;

        void insert(const std::vector<u8>& bytes) {
            for(auto n : bytes) { code.push_back(n); }
        }

    public:
        u64 pc() { return code.size(); }
        explicit Program(const std::string& filename);

        Program() = default;

        template<typename ut>
        void set(u64 addr, ut val);

        void inst(Op inst, const std::vector<u8>& operands = {});

        void inst(Op inst, u8 arg, const std::vector<u8>& operands = {});

        void inst(Op inst, u8 arg1, u8 arg2);

        void str(u8 reg, const std::string_view new_string);

        inline void ext() { code.push_back(EXT); }

        inline void push(u8 reg) { inst(PSH, reg); }

        inline void pop(u8 reg) { inst(POP, reg); }

        void whl(const std::function<void()>& preparation, u8 comparisson, const std::function<void()>& body);

        template<typename ut>
        static std::vector<u8> repr(ut val);

        const std::vector<u8>& bytes() { return code; }

        void dump(const std::string& file);

        void print_int(u8 reg);

        void print();
    };
}

#endif //CORDIAL_BCPROGRAM_H
