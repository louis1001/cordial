//
//  utils.hpp
//  cordial
//
//  Created by Luis Gonzalez on 6/2/22.
//

#pragma once
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

}

