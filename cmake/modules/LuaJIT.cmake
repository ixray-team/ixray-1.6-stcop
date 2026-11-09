# LuaJIT is built from the pinned, patched sources kept in this repository.
set(LUAJIT_SOURCE_DIR "${CMAKE_SOURCE_DIR}/src/3rd-party/luajit")
set(LUAJIT_ENABLE_INSTALL OFF CACHE BOOL "Install the bundled LuaJIT separately")
add_subdirectory("${LUAJIT_SOURCE_DIR}" "${CMAKE_BINARY_DIR}/_deps/luajit-build")

set_target_properties(libluajit PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}"
    FOLDER "3rd Party"
)

set(LUAJIT_INC "${LUAJIT_SOURCE_DIR}/src")
set(LUAJIT_LIB libluajit)
set(LUAJIT_BIN "$<TARGET_FILE:libluajit>")
set(LUAJIT_NAME "$<TARGET_FILE_NAME:libluajit>")
set(LUA_INCLUDE_DIR "${LUAJIT_INC}" CACHE PATH "" FORCE)
set(LUA_LIBRARIES "${LUAJIT_LIB}" CACHE STRING "" FORCE)
set(LUA_LIBRARY "${LUAJIT_LIB}" CACHE STRING "" FORCE)

add_library(Lua::Lua ALIAS libluajit)
add_library(LuaJIT::lua51 ALIAS libluajit)
