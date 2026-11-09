# Project for MiniLua
project(minilua
    LANGUAGES C
    VERSION ${LUAJIT_VERSION_MAJOR}.${LUAJIT_VERSION_MINOR}
)

set(MINILUA_SOURCES
    ${LUAJIT_ROOT_FOLDER}/src/host/minilua.c
)

source_group("MiniLua" FILES ${MINILUA_SOURCES})

if(NOT LUA_COMMAND)
    add_executable(${PROJECT_NAME} ${MINILUA_SOURCES})
    add_executable(LuaJIT::MiniLua ALIAS ${PROJECT_NAME})

    target_compile_options(${PROJECT_NAME} PRIVATE ${LUAJIT_COMPILE_FLAGS})
    target_compile_definitions(${PROJECT_NAME} PRIVATE ${LUAJIT_COMPILE_DEFINES})

    target_link_options(${PROJECT_NAME} PRIVATE ${LUAJIT_LINK_FLAGS})

    set(MINILUA_TARGET $<TARGET_FILE:LuaJIT::MiniLua>)
    set(LUA_COMMAND ${MINILUA_TARGET})

    if(LINUX)
        target_link_libraries(${PROJECT_NAME} PRIVATE LibM::Library)
    endif()
endif()

# Project for DynAsm
project(dynasm
    VERSION ${LUAJIT_VERSION_MAJOR}.${LUAJIT_VERSION_MINOR}
)

set(DASM_COMMAND ${LUAJIT_ROOT_FOLDER}/dynasm/dynasm.lua)

add_custom_command(
    COMMAND
        ${LUA_COMMAND}
        ${DASM_COMMAND}
        -LN ${DASM_FLAGS}
        -o ${LUAJIT_BUILD_FOLDER}/src/host/buildvm_arch.h
        ${DASC_FILE}
    OUTPUT ${LUAJIT_BUILD_FOLDER}/src/host/buildvm_arch.h
    COMMENT "Generating buildvm_arch.h..."
)

add_custom_target(${PROJECT_NAME}
    DEPENDS
        ${LUAJIT_BUILD_FOLDER}/src/host/buildvm_arch.h
)

if(NOT LUA_COMMAND)
    add_dependencies(${PROJECT_NAME} LuaJIT::MiniLua)
endif()

# Project for BuildVM
project(buildvm
    LANGUAGES C
    VERSION ${LUAJIT_VERSION_MAJOR}.${LUAJIT_VERSION_MINOR}
)

file(GLOB BUILDVM_HEADERS
    ${LUAJIT_ROOT_FOLDER}/src/host/buildvm*.h
)
file(GLOB BUILDVM_SOURCES
    ${LUAJIT_ROOT_FOLDER}/src/host/buildvm*.c
)

source_group("BuildVM" FILES ${BUILDVM_HEADERS})
source_group("BuildVM" FILES ${BUILDVM_SOURCES})

add_executable(${PROJECT_NAME}
    ${BUILDVM_HEADERS}
    ${BUILDVM_SOURCES}
)
add_executable(LuaJIT::BuildVM ALIAS ${PROJECT_NAME})

target_include_directories(${PROJECT_NAME} PRIVATE ${LUAJIT_ROOT_FOLDER}/src)
target_include_directories(${PROJECT_NAME} PRIVATE ${LUAJIT_ROOT_FOLDER}/src/host)
target_include_directories(${PROJECT_NAME} PRIVATE ${LUAJIT_ROOT_FOLDER}/dynasm)
target_include_directories(${PROJECT_NAME} PRIVATE ${LUAJIT_BUILD_FOLDER}/src/host)

target_compile_options(${PROJECT_NAME} PRIVATE ${LUAJIT_COMPILE_FLAGS})
target_compile_definitions(${PROJECT_NAME} PRIVATE ${LUAJIT_COMPILE_DEFINES})

target_link_options(${PROJECT_NAME} PRIVATE ${LUAJIT_LINK_FLAGS})

add_dependencies(${PROJECT_NAME} dynasm)

set(BUILDVM_TARGET $<TARGET_FILE:LuaJIT::BuildVM>)
set(BUILDVM_COMMAND ${BUILDVM_TARGET})

# Project for GenDef
project(gendef
    VERSION ${LUAJIT_VERSION_MAJOR}.${LUAJIT_VERSION_MINOR}
)

set(GENDEF_INPUT_FILES
    ${LUAJIT_ROOT_FOLDER}/src/lib_base.c
    ${LUAJIT_ROOT_FOLDER}/src/lib_math.c
    ${LUAJIT_ROOT_FOLDER}/src/lib_bit.c
    ${LUAJIT_ROOT_FOLDER}/src/lib_string.c
    ${LUAJIT_ROOT_FOLDER}/src/lib_table.c
    ${LUAJIT_ROOT_FOLDER}/src/lib_io.c
    ${LUAJIT_ROOT_FOLDER}/src/lib_os.c
    ${LUAJIT_ROOT_FOLDER}/src/lib_package.c
    ${LUAJIT_ROOT_FOLDER}/src/lib_debug.c
    ${LUAJIT_ROOT_FOLDER}/src/lib_jit.c
    ${LUAJIT_ROOT_FOLDER}/src/lib_ffi.c
    ${LUAJIT_ROOT_FOLDER}/src/lib_buffer.c
)

file(MAKE_DIRECTORY ${LUAJIT_BUILD_FOLDER}/src/jit)

add_custom_command(
    COMMAND
        ${BUILDVM_COMMAND}
            -m bcdef
            -o ${LUAJIT_BUILD_FOLDER}/src/lj_bcdef.h
            ${GENDEF_INPUT_FILES}
    OUTPUT ${LUAJIT_BUILD_FOLDER}/src/lj_bcdef.h
    DEPENDS buildvm ${GENDEF_INPUT_FILES} ${LUAJIT_ROOT_FOLDER}/src/lj_opt_fold.c
    COMMENT "Generating lj_bcdef.h..."
    WORKING_DIRECTORY ${LUAJIT_ROOT_FOLDER}
)
add_custom_command(
    COMMAND
        ${BUILDVM_COMMAND}
        -m ffdef
        -o ${LUAJIT_BUILD_FOLDER}/src/lj_ffdef.h
        ${GENDEF_INPUT_FILES}
    OUTPUT ${LUAJIT_BUILD_FOLDER}/src/lj_ffdef.h
    DEPENDS buildvm ${GENDEF_INPUT_FILES} ${LUAJIT_ROOT_FOLDER}/src/lj_opt_fold.c
    COMMENT "Generating lj_ffdef.h..."
    WORKING_DIRECTORY ${LUAJIT_ROOT_FOLDER}
)
add_custom_command(
    COMMAND
        ${BUILDVM_COMMAND}
        -m libdef
        -o ${LUAJIT_BUILD_FOLDER}/src/lj_libdef.h
        ${GENDEF_INPUT_FILES}
    OUTPUT ${LUAJIT_BUILD_FOLDER}/src/lj_libdef.h
    DEPENDS buildvm ${GENDEF_INPUT_FILES} ${LUAJIT_ROOT_FOLDER}/src/lj_opt_fold.c
    COMMENT "Generating lj_libdef.h..."
    WORKING_DIRECTORY ${LUAJIT_ROOT_FOLDER}
)
add_custom_command(
    COMMAND
        ${BUILDVM_COMMAND}
        -m recdef
        -o ${LUAJIT_BUILD_FOLDER}/src/lj_recdef.h
        ${GENDEF_INPUT_FILES}
    OUTPUT ${LUAJIT_BUILD_FOLDER}/src/lj_recdef.h
    DEPENDS buildvm ${GENDEF_INPUT_FILES} ${LUAJIT_ROOT_FOLDER}/src/lj_opt_fold.c
    COMMENT "Generating lj_recdef.h..."
    WORKING_DIRECTORY ${LUAJIT_ROOT_FOLDER}
)
add_custom_command(
    COMMAND
        ${BUILDVM_COMMAND}
        -m vmdef
        -o ${LUAJIT_BUILD_FOLDER}/src/jit/vmdef.lua
        ${GENDEF_INPUT_FILES}
    OUTPUT ${LUAJIT_BUILD_FOLDER}/src/jit/vmdef.lua
    DEPENDS buildvm ${GENDEF_INPUT_FILES} ${LUAJIT_ROOT_FOLDER}/src/lj_opt_fold.c
    COMMENT "Generating vmdef.lua..."
    WORKING_DIRECTORY ${LUAJIT_ROOT_FOLDER}
)

add_custom_command(
    COMMAND
        ${BUILDVM_COMMAND}
        -m folddef
        -o ${LUAJIT_BUILD_FOLDER}/src/lj_folddef.h
        ${LUAJIT_ROOT_FOLDER}/src/lj_opt_fold.c
    OUTPUT ${LUAJIT_BUILD_FOLDER}/src/lj_folddef.h
    DEPENDS buildvm ${GENDEF_INPUT_FILES} ${LUAJIT_ROOT_FOLDER}/src/lj_opt_fold.c
    COMMENT "Generating lj_folddef.h..."
    WORKING_DIRECTORY ${LUAJIT_ROOT_FOLDER}
)

set(GENDEF_OUTPUT_FILES
    ${LUAJIT_BUILD_FOLDER}/src/lj_bcdef.h
    ${LUAJIT_BUILD_FOLDER}/src/lj_ffdef.h
    ${LUAJIT_BUILD_FOLDER}/src/lj_libdef.h
    ${LUAJIT_BUILD_FOLDER}/src/lj_recdef.h
    ${LUAJIT_BUILD_FOLDER}/src/jit/vmdef.lua
    ${LUAJIT_BUILD_FOLDER}/src/lj_folddef.h
)

if(WIN32)
    add_custom_command(
        COMMAND
            ${BUILDVM_COMMAND}
            -m ${LUAJIT_VM_MODE}
            -o ${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}/$<CONFIG>/lj_vm.obj
        OUTPUT ${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}/$<CONFIG>/lj_vm.obj
        DEPENDS buildvm ${GENDEF_INPUT_FILES} ${LUAJIT_ROOT_FOLDER}/src/lj_opt_fold.c
    COMMENT "Generating lj_vm.obj..."
        WORKING_DIRECTORY ${LUAJIT_ROOT_FOLDER}
    )

    add_custom_target(${PROJECT_NAME}
        DEPENDS
            ${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}/$<CONFIG>/lj_vm.obj
            ${GENDEF_OUTPUT_FILES}
    )
elseif(UNIX)
    add_custom_command(
        COMMAND
            ${BUILDVM_COMMAND}
            -m ${LUAJIT_VM_MODE}
            -o ${LUAJIT_BUILD_FOLDER}/src/lj_vm.S
        OUTPUT ${LUAJIT_BUILD_FOLDER}/src/lj_vm.S
        DEPENDS buildvm ${GENDEF_INPUT_FILES} ${LUAJIT_ROOT_FOLDER}/src/lj_opt_fold.c
    COMMENT "Generating lj_vm.S..."
    )

    add_custom_target(${PROJECT_NAME}
        DEPENDS
            ${GENDEF_OUTPUT_FILES}
    )

    enable_language(ASM)

    add_library(lj_vm OBJECT ${LUAJIT_BUILD_FOLDER}/src/lj_vm.S)

    target_compile_options(lj_vm PRIVATE ${LUAJIT_COMPILE_FLAGS})
    target_compile_definitions(lj_vm PRIVATE ${LUAJIT_COMPILE_DEFINES})

    target_link_options(lj_vm PRIVATE ${LUAJIT_LINK_FLAGS})
endif()

add_dependencies(${PROJECT_NAME} LuaJIT::BuildVM)
