cmake_minimum_required(VERSION 3.2)
project(icfpc2006)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES main.cpp)
add_executable(icfpc2006 ${SOURCE_FILES})

add_custom_command(TARGET icfpc2006 POST_BUILD COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/icfpc2006 ~/source/icfpc2006/ )