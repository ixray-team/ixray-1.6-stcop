# Project for MiniLua
project(minilua LANGUAGES C)

set(MINILUA_SRC
    ${CMAKE_CURRENT_SOURCE_DIR}/minilua.c
)

source_group("" FILES ${MINILUA_SRC})

add_executable(${PROJECT_NAME} ${MINILUA_SRC})

target_compile_options(${PROJECT_NAME} PRIVATE ${LJCOMPILE_FLAGS})
target_compile_definitions(${PROJECT_NAME} PRIVATE ${LJCOMPILE_DEFS})

if(LIBM_LIBRARY)
    target_link_libraries(${PROJECT_NAME} PRIVATE ${LIBM_LIBRARY})
endif()

# Project for DynAsm
project(dynasm)

if(NOT LUA_COMMAND)
    set(MINILUA_COMMAND $<TARGET_FILE:minilua>)
    set(LUA_COMMAND ${MINILUA_COMMAND})
endif()

add_custom_command(
    COMMAND ${LUA_COMMAND} ${DASM}
        -LN ${DASMFLAGS}
        -o ${CMAKE_SOURCE_DIR}/src/host/buildvm_arch.h
        ${DASC}
    OUTPUT ${CMAKE_SOURCE_DIR}/src/host/buildvm_arch.h
    COMMENT "Generating buildvm_arch.h..."
)

add_custom_target(${PROJECT_NAME}
    DEPENDS ${CMAKE_SOURCE_DIR}/src/host/buildvm_arch.h
)

if(NOT LUA_COMMAND)
    add_dependencies(${PROJECT_NAME} minilua)
endif()

# Project for BuildVM
project(buildvm LANGUAGES C)

file(GLOB BUILDVM_SRC
    ${CMAKE_CURRENT_SOURCE_DIR}/buildvm*.c
    ${CMAKE_CURRENT_SOURCE_DIR}/buildvm*.h
)

source_group("" FILES ${BUILDVM_SRC})

add_executable(${PROJECT_NAME} ${BUILDVM_SRC})

target_compile_options(${PROJECT_NAME} PRIVATE ${LJCOMPILE_FLAGS})
target_compile_definitions(${PROJECT_NAME} PRIVATE ${LJCOMPILE_DEFS})

target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_SOURCE_DIR}/src)
target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_SOURCE_DIR}/dynasm)

add_dependencies(${PROJECT_NAME} dynasm)

# Project for GenDef
project(gendef)

set(ALL_LIB_SRC
    ${CMAKE_SOURCE_DIR}/src/lib_base.c
    ${CMAKE_SOURCE_DIR}/src/lib_math.c
    ${CMAKE_SOURCE_DIR}/src/lib_bit.c
    ${CMAKE_SOURCE_DIR}/src/lib_string.c
    ${CMAKE_SOURCE_DIR}/src/lib_table.c
    ${CMAKE_SOURCE_DIR}/src/lib_io.c
    ${CMAKE_SOURCE_DIR}/src/lib_os.c
    ${CMAKE_SOURCE_DIR}/src/lib_package.c
    ${CMAKE_SOURCE_DIR}/src/lib_debug.c
    ${CMAKE_SOURCE_DIR}/src/lib_jit.c
    ${CMAKE_SOURCE_DIR}/src/lib_ffi.c
)

if(WIN32)
    add_custom_command(
        COMMAND $<TARGET_FILE:buildvm>
            -m ${LJVM_MODE}
            -o ${CMAKE_LIBRARY_OUTPUT_DIRECTORY_EX}/lj_vm.obj
        OUTPUT ${CMAKE_LIBRARY_OUTPUT_DIRECTORY_EX}/lj_vm.obj
        COMMENT "Generating lj_vm.obj..."
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    )
elseif(UNIX)
    add_custom_command(
        COMMAND $<TARGET_FILE:buildvm>
            -m ${LJVM_MODE}
            -o ${CMAKE_SOURCE_DIR}/src/lj_vm.S
        OUTPUT ${CMAKE_SOURCE_DIR}/src/lj_vm.S
        COMMENT "Generating lj_vm.S..."
    )
endif()

add_custom_command(
    COMMAND $<TARGET_FILE:buildvm>
        -m bcdef
        -o ${CMAKE_SOURCE_DIR}/src/lj_bcdef.h
        ${ALL_LIB_SRC}
    OUTPUT ${CMAKE_SOURCE_DIR}/src/lj_bcdef.h
    COMMENT "Generating lj_bcdef.h..."
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)
add_custom_command(
    COMMAND $<TARGET_FILE:buildvm>
        -m ffdef
        -o ${CMAKE_SOURCE_DIR}/src/lj_ffdef.h
        ${ALL_LIB_SRC}
    OUTPUT ${CMAKE_SOURCE_DIR}/src/lj_ffdef.h
    COMMENT "Generating lj_ffdef.h..."
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)
add_custom_command(
    COMMAND $<TARGET_FILE:buildvm>
        -m libdef
        -o ${CMAKE_SOURCE_DIR}/src/lj_libdef.h
        ${ALL_LIB_SRC}
    OUTPUT ${CMAKE_SOURCE_DIR}/src/lj_libdef.h
    COMMENT "Generating lj_libdef.h..."
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)
add_custom_command(
    COMMAND $<TARGET_FILE:buildvm>
        -m recdef
        -o ${CMAKE_SOURCE_DIR}/src/lj_recdef.h
        ${ALL_LIB_SRC}
    OUTPUT ${CMAKE_SOURCE_DIR}/src/lj_recdef.h
    COMMENT "Generating lj_recdef.h..."
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)
add_custom_command(
    COMMAND $<TARGET_FILE:buildvm>
        -m vmdef
        -o ${CMAKE_SOURCE_DIR}/src/jit/vmdef.lua
        ${ALL_LIB_SRC}
    OUTPUT ${CMAKE_SOURCE_DIR}/src/jit/vmdef.lua
    COMMENT "Generating vmdef.lua..."
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)
add_custom_command(
    COMMAND $<TARGET_FILE:buildvm>
        -m folddef
        -o ${CMAKE_SOURCE_DIR}/src/lj_folddef.h
        ${CMAKE_SOURCE_DIR}/src/lj_opt_fold.c
    OUTPUT ${CMAKE_SOURCE_DIR}/src/lj_folddef.h
    COMMENT "Generating lj_folddef.h..."
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)

if(WIN32)
    add_custom_target(${PROJECT_NAME}
        DEPENDS
            ${CMAKE_LIBRARY_OUTPUT_DIRECTORY_EX}/lj_vm.obj
            ${CMAKE_SOURCE_DIR}/src/lj_bcdef.h
            ${CMAKE_SOURCE_DIR}/src/lj_ffdef.h
            ${CMAKE_SOURCE_DIR}/src/lj_libdef.h
            ${CMAKE_SOURCE_DIR}/src/lj_recdef.h
            ${CMAKE_SOURCE_DIR}/src/jit/vmdef.lua
            ${CMAKE_SOURCE_DIR}/src/lj_folddef.h
    )
elseif(UNIX)
    add_custom_target(${PROJECT_NAME}
        DEPENDS
            ${CMAKE_SOURCE_DIR}/src/lj_bcdef.h
            ${CMAKE_SOURCE_DIR}/src/lj_ffdef.h
            ${CMAKE_SOURCE_DIR}/src/lj_libdef.h
            ${CMAKE_SOURCE_DIR}/src/lj_recdef.h
            ${CMAKE_SOURCE_DIR}/src/jit/vmdef.lua
            ${CMAKE_SOURCE_DIR}/src/lj_folddef.h
    )

    enable_language(ASM)

    add_library(lj_vm OBJECT ${CMAKE_SOURCE_DIR}/src/lj_vm.S)

    target_compile_options(lj_vm PRIVATE ${LJCOMPILE_FLAGS})
    target_compile_definitions(lj_vm PRIVATE ${LJCOMPILE_DEFS})
endif()

add_dependencies(${PROJECT_NAME} buildvm)
