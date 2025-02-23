# Project for LuaJIT
project(luajit LANGUAGES C)

file(GLOB LIBLUAJIT_SRC
    ${CMAKE_CURRENT_SOURCE_DIR}/lj_*.c
    ${CMAKE_CURRENT_SOURCE_DIR}/lj_*.h
    ${CMAKE_CURRENT_SOURCE_DIR}/lib_*.c
    ${CMAKE_CURRENT_SOURCE_DIR}/lib_*.h
    ${CMAKE_CURRENT_SOURCE_DIR}/xr_*.c
    ${CMAKE_CURRENT_SOURCE_DIR}/xr_*.h
)

source_group("" FILES ${LIBLUAJIT_SRC})

add_library(${PROJECT_NAME} SHARED ${LIBLUAJIT_SRC})
add_dependencies(${PROJECT_NAME} gendef)

target_compile_options(${PROJECT_NAME} PRIVATE ${LJCOMPILE_FLAGS})
target_compile_definitions(${PROJECT_NAME} PRIVATE ${LJCOMPILE_DEFS})

target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_SOURCE_DIR}/src)

if(WIN32)
    target_compile_definitions(${PROJECT_NAME} PRIVATE ${LJDYNBUILD})
    target_link_options(${PROJECT_NAME} PRIVATE ${CMAKE_LIBRARY_OUTPUT_DIRECTORY_EX}/lj_vm.obj)
    set_target_properties(${PROJECT_NAME}
        PROPERTIES
            OUTPUT_NAME lua${NODOTABIVER}
    )
elseif(UNIX)
    target_compile_options(${PROJECT_NAME} PRIVATE ${LJDYNBUILD})
    target_link_libraries(${PROJECT_NAME} PRIVATE $<TARGET_OBJECTS:lj_vm>)
    set_target_properties(${PROJECT_NAME}
        PROPERTIES
            VERSION ${MAJVER}.${MINVER}
            SOVERSION ${MAJVER}
    )
endif()

install(
    TARGETS luajit
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    COMPONENT luajit
)
install(
    FILES
        ${CMAKE_CURRENT_SOURCE_DIR}/lua.h
        ${CMAKE_CURRENT_SOURCE_DIR}/lualib.h
        ${CMAKE_CURRENT_SOURCE_DIR}/lauxlib.h
        ${CMAKE_CURRENT_SOURCE_DIR}/luaconf.h
        ${CMAKE_CURRENT_SOURCE_DIR}/lua.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/luajit.h
    DESTINATION include/luajit-${MAJVER}.${MINVER}
    COMPONENT luajit
)
install(
    FILES
        ${CMAKE_CURRENT_SOURCE_DIR}/jit/bc.lua
        ${CMAKE_CURRENT_SOURCE_DIR}/jit/bcsave.lua
        ${CMAKE_CURRENT_SOURCE_DIR}/jit/dis_arm.lua
        ${CMAKE_CURRENT_SOURCE_DIR}/jit/dis_mips.lua
        ${CMAKE_CURRENT_SOURCE_DIR}/jit/dis_mipsel.lua
        ${CMAKE_CURRENT_SOURCE_DIR}/jit/dis_ppc.lua
        ${CMAKE_CURRENT_SOURCE_DIR}/jit/dis_x64.lua
        ${CMAKE_CURRENT_SOURCE_DIR}/jit/dis_x86.lua
        ${CMAKE_CURRENT_SOURCE_DIR}/jit/dump.lua
        ${CMAKE_CURRENT_SOURCE_DIR}/jit/v.lua
        ${CMAKE_CURRENT_SOURCE_DIR}/jit/vmdef.lua
    DESTINATION share/luajit-${MAJVER}.${MINVER}
    COMPONENT luajit
)

if(WIN32)
    install(
        FILES ${CMAKE_LIBRARY_OUTPUT_DIRECTORY_EX}/$<TARGET_FILE_BASE_NAME:luajit>.exp
        DESTINATION lib
        COMPONENT luajit
    )
endif()

# Project for LuaJIT CLI
project(luajitcli LANGUAGES C)

set(LUAJIT_SRC
    ${CMAKE_CURRENT_SOURCE_DIR}/luajit.c
)

source_group("" FILES ${LUAJIT_SRC})

if(LUAJIT_ENABLE_CLI)
    add_executable(${PROJECT_NAME} ${LUAJIT_SRC})
    set_target_properties(${PROJECT_NAME}
        PROPERTIES
            OUTPUT_NAME "luajit"
    )

    add_dependencies(${PROJECT_NAME} luajit)

    target_compile_options(${PROJECT_NAME} PRIVATE ${LJCOMPILE_FLAGS})
    target_compile_definitions(${PROJECT_NAME} PRIVATE ${LJCOMPILE_DEFS})

    target_link_libraries(${PROJECT_NAME} PRIVATE luajit)

    if(LIBM_LIBRARY)
        target_link_libraries(${PROJECT_NAME} PRIVATE ${LIBM_LIBRARY})
    endif()
    if(LIBDL_LIBRARY)
        target_link_libraries(${PROJECT_NAME} PRIVATE ${LIBDL_LIBRARY})
    endif()

    install(
        TARGETS luajitcli
        RUNTIME DESTINATION bin
        COMPONENT luajit-cli
    )
endif()

if (MSVC)
    set_target_properties(luajit PROPERTIES LINK_FLAGS "/BASE:0x28000000 /FIXED /DYNAMICBASE:NO")
elseif (CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    set_target_properties(luajit PROPERTIES LINK_FLAGS "-Wl,--image-base=0x28000000")
endif()