cmake_minimum_required(VERSION 3.15)
project(aoc)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_EXPORT_COMPILE_COMMANDS 1)
set(CMAKE_CXX_FLAGS "-O3")

add_executable(day8 app/Day8.cpp)
