cmake_minimum_required(VERSION 3.10)
project(cordial)

set(CMAKE_CXX_STANDARD 20)

if (UNIX)
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall -Werror")
else()
    string(REGEX REPLACE "/W[3|4]" "/W4" CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -WX")
endif()
include_directories(cordial/language/includes)

add_executable(cordial
        cordial/main.cpp
        cordial/language/src/lexer.cpp
        cordial/language/src/token.cpp)

target_compile_definitions(cordial PUBLIC LANG_USE_ES=0)

if(CMAKE_BUILD_TYPE MATCHES Debug)
    target_compile_definitions(cordial PUBLIC DEBUG_MODE=1)
elseif(CMAKE_BUILD_TYPE MATCHES Release)
    target_compile_definitions(cordial PUBLIC DEBUG_MODE=0)
endif()