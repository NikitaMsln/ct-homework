cmake_minimum_required(VERSION 3.21)
project(huffman_lib VERSION 1.0.0 DESCRIPTION "Compress and decompress with huffman code")

set(CMAKE_CXX_STANDARD 20)

file(GLOB SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/lib/*.cpp)
option(LIBRARY_TYPE "Type of the library (STATIC or SHARED)")
if(LIBRARY_TYPE STREQUAL "SHARED")
    add_library(huffman_lib SHARED ${SOURCES})
else()
    add_library(huffman_lib STATIC ${SOURCES})
endif()

target_include_directories(huffman_lib
        PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/lib/
        PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include/)

set_target_properties(huffman_lib PROPERTIES VERSION ${PROJECT_VERSION} PUBLIC_HEADER huffman_lib.h)
install(TARGETS huffman_lib
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
