project(luabind)
set(LUABIND_SRC_FILES)

# Files import
file(GLOB_RECURSE LUABIND_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "luabind/src/*.*"
)
file(GLOB_RECURSE LUABIND_INCL_ALL_FILES CONFIGURE_DEPENDS
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
target_include_directories(luabind PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/luabind/src")
target_include_directories(luabind PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/")

target_link_directories(luabind PUBLIC "${IXRAY_SDK_LIB}/")

target_compile_definitions(luabind PRIVATE LUABIND_BUILDING)
target_compile_definitions(luabind PUBLIC "$<$<CONFIG:RELWITHDEBINFO>:LUABIND_NO_EXCEPTIONS>")
target_compile_definitions(luabind PUBLIC "$<$<CONFIG:RELEASE>:LUABIND_NO_EXCEPTIONS>")

## LuaJIT
add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${LUAJIT_BIN} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/${LUAJIT_NAME}
)
target_link_libraries(luabind PUBLIC ${LUAJIT_LIB})
