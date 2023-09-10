project(luajit)
set(LUA_SRC_FILES)

# Files import
file(GLOB_RECURSE LUA_SOURCE_ALL_FILES
    "src/*.c"
)
file(GLOB_RECURSE LUA_INCL_ALL_FILES
    "src/*.h"
)
file(GLOB_RECURSE LUA_DASM_ALL_FILES
    "dynasm/*.*"
)

source_group("src" FILES ${LUA_SOURCE_ALL_FILES})
source_group("incl" FILES ${LUA_INCL_ALL_FILES})
source_group("dynasm" FILES ${LUA_DASM_ALL_FILES})

# Apply list
list(APPEND LUA_SRC_FILES ${LUA_SOURCE_ALL_FILES})
list(APPEND LUA_SRC_FILES ${LUA_INCL_ALL_FILES})

# Remove files
list(FILTER LUA_SRC_FILES EXCLUDE REGEX "src/lua.c$")

add_library(luajit SHARED ${LUA_SRC_FILES})
target_include_directories(luajit PUBLIC "${IXRAY_SDK_INC}")
target_include_directories(luajit PUBLIC "${IXRAY_SDK_INC}/lua/")
target_include_directories(luajit PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/dynasm")
