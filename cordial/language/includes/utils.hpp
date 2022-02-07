//
//  utils.hpp
//  cordial
//
//  Created by Luis Gonzalez on 6/2/22.
//

#pragma once

namespace Cordial {

struct DebugPosition {
    size_t line{1};
    size_t col;
    
    inline void bump_line(size_t by = 1) {
        line += by;
        if (by > 0) {
            col = 0;
        }
    }
    
    inline void bump_col(size_t by = 1) { col += by; }
};

}

