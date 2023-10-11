project(luabind)
set(LUABIND_SRC_FILES)

# Files import
file(GLOB_RECURSE LUABIND_SOURCE_ALL_FILES
    "luabind/src/*.*"
)

file(GLOB_RECURSE LUABIND_INCL_ALL_FILES
    "${IXRAY_SDK_INC}/luabind/*.*"
)

source_group("src" FILES ${LUABIND_SOURCE_ALL_FILES})
source_group("include" FILES ${LUABIND_INCL_ALL_FILES})

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

target_link_directories(luabind PUBLIC "${IXRAY_SDK_LIB}/")

target_compile_definitions(luabind PRIVATE LUABIND_BUILDING)
target_compile_definitions(luabind PUBLIC LUABIND_NO_EXCEPTIONS)
target_link_libraries(luabind PUBLIC "lua51.lib")

add_custom_command(TARGET ${PROJECT_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${IXRAY_SDK_BIN}lua51.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)
