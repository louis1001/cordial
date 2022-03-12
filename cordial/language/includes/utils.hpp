//
//  utils.hpp
//  cordial
//
//  Created by Luis Gonzalez on 6/2/22.
//

#pragma once
#include <iomanip>
#include <iostream>
#define TODO(reason) \
    std::cerr << "TODO: " << reason << std::endl; \
    throw

namespace Cordial {
    template<class... Ts> struct overloaded : Ts... {
        using Ts::operator()...;
    };
    template<class... Ts> overloaded(Ts...)
                -> overloaded<Ts...>;

struct DebugPosition {
    size_t line{1};
    size_t col{1};

    inline void bump_line(size_t by = 1) {
        line += by;
        if (by > 0) {
            col = 0;
        }
    }

    inline void bump_col(size_t by = 1) { col += by; }
};

template<typename T>
void print_hex(T val, int pad, std::ostream& output = std::cout) {
    output << "0x" << std::setfill('0') << std::setw(pad) << std::hex << val;
    output << std::dec << std::setw(0);
}

template<typename T>
void print_hex(T val, std::ostream& output = std::cout) {
    std::cout << "0x" << std::setfill('0') << std::setw(sizeof(T)) << std::hex << val;
    std::cout << std::dec << std::setw(0);
}

}

