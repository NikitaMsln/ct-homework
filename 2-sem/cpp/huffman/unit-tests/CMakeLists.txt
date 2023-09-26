cmake_minimum_required(VERSION 3.21)
project(huffman-unit-tests)

set(CMAKE_CXX_STANDARD 20)

find_package(GTest REQUIRED)

add_executable(tests tests.cpp)
target_link_libraries(tests PUBLIC huffman_lib GTest::gtest GTest::gtest_main)
