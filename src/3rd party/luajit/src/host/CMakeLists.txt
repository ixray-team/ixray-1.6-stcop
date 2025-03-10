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
        -o ${CMAKE_CURRENT_SOURCE_DIR}/src/host/buildvm_arch.h
        ${DASC}
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/src/host/buildvm_arch.h
    COMMENT "Generating buildvm_arch.h..."
)

add_custom_target(${PROJECT_NAME}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/src/host/buildvm_arch.h
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
target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/..)
target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../../dynasm)

add_dependencies(${PROJECT_NAME} dynasm)

# Project for GenDef
project(gendef)

set(ALL_LIB_SRC
    ${CMAKE_CURRENT_SOURCE_DIR}/../lib_base.c
    ${CMAKE_CURRENT_SOURCE_DIR}/../lib_math.c
    ${CMAKE_CURRENT_SOURCE_DIR}/../lib_bit.c
    ${CMAKE_CURRENT_SOURCE_DIR}/../lib_string.c
    ${CMAKE_CURRENT_SOURCE_DIR}/../lib_table.c
    ${CMAKE_CURRENT_SOURCE_DIR}/../lib_io.c
    ${CMAKE_CURRENT_SOURCE_DIR}/../lib_os.c
    ${CMAKE_CURRENT_SOURCE_DIR}/../lib_package.c
    ${CMAKE_CURRENT_SOURCE_DIR}/../lib_debug.c
    ${CMAKE_CURRENT_SOURCE_DIR}/../lib_jit.c
    ${CMAKE_CURRENT_SOURCE_DIR}/../lib_ffi.c
)

if(WIN32)
    add_custom_command(
        COMMAND $<TARGET_FILE:buildvm>
            -m ${LJVM_MODE}
            -o ${CMAKE_LIBRARY_OUTPUT_DIRECTORY_EX}/lj_vm.obj
        OUTPUT ${CMAKE_LIBRARY_OUTPUT_DIRECTORY_EX}/lj_vm.obj
        COMMENT "Generating lj_vm.obj..."
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )
elseif(UNIX)
    add_custom_command(
        COMMAND $<TARGET_FILE:buildvm>
            -m ${LJVM_MODE}
            -o ${CMAKE_CURRENT_SOURCE_DIR}/../lj_vm.S
        OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/../lj_vm.S
        COMMENT "Generating lj_vm.S..."
    )
endif()

add_custom_command(
    COMMAND $<TARGET_FILE:buildvm>
        -m bcdef
        -o ${CMAKE_CURRENT_SOURCE_DIR}/../lj_bcdef.h
        ${ALL_LIB_SRC}
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/../lj_bcdef.h
    COMMENT "Generating lj_bcdef.h..."
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
add_custom_command(
    COMMAND $<TARGET_FILE:buildvm>
        -m ffdef
        -o ${CMAKE_CURRENT_SOURCE_DIR}/../lj_ffdef.h
        ${ALL_LIB_SRC}
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/../lj_ffdef.h
    COMMENT "Generating lj_ffdef.h..."
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
add_custom_command(
    COMMAND $<TARGET_FILE:buildvm>
        -m libdef
        -o ${CMAKE_CURRENT_SOURCE_DIR}/../lj_libdef.h
        ${ALL_LIB_SRC}
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/../lj_libdef.h
    COMMENT "Generating lj_libdef.h..."
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
add_custom_command(
    COMMAND $<TARGET_FILE:buildvm>
        -m recdef
        -o ${CMAKE_CURRENT_SOURCE_DIR}/../lj_recdef.h
        ${ALL_LIB_SRC}
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/../lj_recdef.h
    COMMENT "Generating lj_recdef.h..."
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
add_custom_command(
    COMMAND $<TARGET_FILE:buildvm>
        -m vmdef
        -o ${CMAKE_CURRENT_SOURCE_DIR}/../jit/vmdef.lua
        ${ALL_LIB_SRC}
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/../jit/vmdef.lua
    COMMENT "Generating vmdef.lua..."
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
add_custom_command(
    COMMAND $<TARGET_FILE:buildvm>
        -m folddef
        -o ${CMAKE_CURRENT_SOURCE_DIR}/../lj_folddef.h
        ${CMAKE_CURRENT_SOURCE_DIR}/../lj_opt_fold.c
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/../lj_folddef.h
    COMMENT "Generating lj_folddef.h..."
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

if(WIN32)
    add_custom_target(${PROJECT_NAME}
        DEPENDS
            ${CMAKE_LIBRARY_OUTPUT_DIRECTORY_EX}/lj_vm.obj
            ${CMAKE_CURRENT_SOURCE_DIR}/../lj_bcdef.h
            ${CMAKE_CURRENT_SOURCE_DIR}/../lj_ffdef.h
            ${CMAKE_CURRENT_SOURCE_DIR}/../lj_libdef.h
            ${CMAKE_CURRENT_SOURCE_DIR}/../lj_recdef.h
            ${CMAKE_CURRENT_SOURCE_DIR}/../jit/vmdef.lua
            ${CMAKE_CURRENT_SOURCE_DIR}/../lj_folddef.h
    )
elseif(UNIX)
    add_custom_target(${PROJECT_NAME}
        DEPENDS
            ${CMAKE_CURRENT_SOURCE_DIR}/../lj_bcdef.h
            ${CMAKE_CURRENT_SOURCE_DIR}/../lj_ffdef.h
            ${CMAKE_CURRENT_SOURCE_DIR}/../lj_libdef.h
            ${CMAKE_CURRENT_SOURCE_DIR}/../lj_recdef.h
            ${CMAKE_CURRENT_SOURCE_DIR}/../jit/vmdef.lua
            ${CMAKE_CURRENT_SOURCE_DIR}/../lj_folddef.h
    )

    enable_language(ASM)

    add_library(lj_vm OBJECT ${CMAKE_CURRENT_SOURCE_DIR}/../lj_vm.S)

    target_compile_options(lj_vm PRIVATE ${LJCOMPILE_FLAGS})
    target_compile_definitions(lj_vm PRIVATE ${LJCOMPILE_DEFS})
endif()

add_dependencies(${PROJECT_NAME} buildvm)
