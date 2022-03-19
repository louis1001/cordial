//
// Created by Luis Gonzalez on 13/3/22.
//

#include "OpCodes.hpp"

std::string Cordial::Bytecode::op_name(Op op) {
    switch (op) {
        case EXT:
            return "EXT";
        case NOP:
            return "NOP";
        case LDR:
            return "LDR";
        case ADD:
            return "ADD";
        case SUB:
            return "SUB";
        case EQU:
            return "EQU";
        case CP:
            return "CP";
        case GT:
            return "GT";
        case LT:
            return "LT";
        case JMP:
            return "JMP";
        case JPT:
            return "JPT";
        case JPF:
            return "JPF";
        case SHR:
            return "SHR";
        case STR:
            return "STR";
        case PNT:
            return "PNT";
        case PCH:
            return "PCH";
        case INC:
            return "INC";
        case DEC:
            return "DEC";
        case PSH:
            return "PSH";
        case POP:
            return "POP";
        case DIV:
            return "DIV";
        case MOD:
            return "MOD";
        default:
            return "";
    }
}