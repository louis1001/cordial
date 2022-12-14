cmake_minimum_required(VERSION 3.10)
project(cordial)

set(CMAKE_CXX_STANDARD 20)

if (UNIX)
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall -Werror")
else()
    string(REGEX REPLACE "/W[3|4]" "/W4" CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -WX")
endif()
include_directories(cordial/language/includes cordial/external/include cordial/bytecode/includes)

add_executable(cordial
        cordial/main.cpp
        cordial/language/src/lexer.cpp
        cordial/language/src/parser.cpp
        cordial/language/src/token.cpp
        cordial/language/src/ast_dump.cpp
        cordial/language/src/semantics.cpp
        cordial/language/src/simbolo.cpp
        cordial/language/src/interpreter.cpp
        cordial/language/src/valor.cpp
        cordial/language/src/asm_generator.cpp
        cordial/bytecode/src/bcprogram.cpp
        cordial/bytecode/src/vm.cpp
        cordial/language/src/bc_generator.cpp
        cordial/bytecode/src/OpCodes.cpp)

target_compile_definitions(cordial PUBLIC LANG_USE_ES=0)

if(CMAKE_BUILD_TYPE MATCHES Debug)
    target_compile_definitions(cordial PUBLIC DEBUG_MODE=1)
elseif(CMAKE_BUILD_TYPE MATCHES Release)
    target_compile_definitions(cordial PUBLIC DEBUG_MODE=0)
endif()