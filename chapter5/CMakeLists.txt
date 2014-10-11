cmake_minimum_required(VERSION 2.8.4)
project(chapter5)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES
    absyn.c
    absyn.h
    env.c
    env.h
    errormsg.c
    errormsg.h
    parse.c
    parse.h
    semant.c
    semant.h
    symbol.c
    symbol.h
    table.c
    table.h
    tiger.lex
    tiger.y
    types.c
    types.h
    util.c
    util.h)

add_executable(chapter5 ${SOURCE_FILES})