//
//  token.hpp
//  cordial
//
//  Created by Luis Gonzalez on 5/2/22.
//

#ifndef token_h
#define token_h

#include <string>
#include <sstream>
#include <map>

#include "utils.hpp"

namespace Cordial {

struct Token {
    struct Type {
        enum Value: uint8_t {
            hola = 0,
            adios,
            
            porfavor,
            gracias,
            
            muestra,
            punto,
            coma,
            y,
            
            numero,
            texto,
            variable,
            
            COUNT
        };
        
        Type() = default;
        constexpr Type(Value aType) : value(aType) { }

        #if 1
          // Allow switch and comparisons.
          constexpr operator Value() const { return value; }

          // Prevent usage: if(type)
          explicit operator bool() = delete;
        #else
          constexpr bool operator==(Type a) const { return value == a.value; }
          constexpr bool operator!=(Type a) const { return value != a.value; }
        #endif
        
        std::string_view name() const;

        private:
          Value value;
    };
    
    Type type;
    std::string lexeme;
    DebugPosition position;
    std::string file;
    
    ~Token() = default;
    
    std::string get_str() const {
        std::stringstream result;
        
        result << "Token{ ";
        
        result << "." << type.name() << ", ";
        result << "'" << lexeme << "', ";
        
        result << "en: (lin: " << position.line << ", ";
        result <<      "col: " << position.col  << ") ";
        
        result << "}";
        
        return result.str();
    }
};

}

#endif /* token_h */
