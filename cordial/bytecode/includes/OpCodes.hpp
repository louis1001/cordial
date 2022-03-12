//
// Created by Luis Gonzalez on 12/3/22.
//

#ifndef CORDIAL_OPCODES_H
#define CORDIAL_OPCODES_H

#include <inttypes.h>

using u64 = uint64_t;
using u8 = uint8_t;

namespace Cordial::Bytecode {
    enum Op: u8 {
        EXT = 0xFF,
        NOP = 69,
        LDR = 0x2,
        ADD = 0x3,
        EQU = 0x6,
        CP  = 0x7,
        GT  = 0x8,
        LT  = 0x9,
        JMP = 0xA,
        JPT = 0xB,
        JPF = 0xC,
        SHR = 0xD,
        STR = 0xE,
        PNT = 0xF,
        PCH = 0x10,
        INC = 0x11,
        DEC = 0x12,
        PSH = 0x13,
        POP = 0x14,
        DIV = 0x15,
        MOD = 0x16,
    };

}

#endif //CORDIAL_OPCODES_H
