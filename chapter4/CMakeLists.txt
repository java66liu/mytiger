cmake_minimum_required(VERSION 2.8.4)
project(chapter4)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES
    absyn.c
    absyn.h
    errormsg.c
    errormsg.h
    makefile
    parse.c
    parse.h
    symbol.c
    symbol.h
    table.c
    table.h
    tiger.lex
    tiger.y
    util.c
    util.h)

add_executable(chapter4 ${SOURCE_FILES})