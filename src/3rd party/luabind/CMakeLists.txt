project(luabind)
set(LUABIND_SRC_FILES)

# Files import
file(GLOB_RECURSE LUABIND_SOURCE_ALL_FILES
    "luabind_memory.cpp"
    "luabind/src/*.*"
)

source_group("src" FILES ${LUABIND_SOURCE_ALL_FILES})

# Apply list
list(APPEND LUABIND_SRC_FILES ${LUABIND_SOURCE_ALL_FILES})
list(APPEND LUABIND_SRC_FILES ${LUABIND_INCL_ALL_FILES})

# Remove files
list(FILTER LUABIND_SRC_FILES EXCLUDE REGEX "src/lua.c$")

add_library(luabind SHARED ${LUABIND_SRC_FILES})
target_include_directories(luabind PUBLIC "${IXRAY_SDK_INC}/")
target_include_directories(luabind PUBLIC "${IXRAY_SDK_INC}/lua/")
target_include_directories(luabind PUBLIC "${IXRAY_SDK_INC}/luabind/")
target_include_directories(luabind PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/luabind/src")
target_include_directories(luabind PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/")

target_precompile_headers(luabind PUBLIC "pch.h")
target_compile_definitions(luabind PRIVATE LUABIND_BUILDING)
target_compile_definitions(luabind PUBLIC LUABIND_NO_EXCEPTIONS)
target_link_libraries(luabind PUBLIC luajit)
