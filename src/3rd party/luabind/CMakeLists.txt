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
target_compile_definitions(luabind PUBLIC "$<$<CONFIG:RELWITHDEBINFO>:LUABIND_NO_EXCEPTIONS>")
target_compile_definitions(luabind PUBLIC "$<$<CONFIG:RELEASE>:LUABIND_NO_EXCEPTIONS>")

# NuGet
set(LUAJIT ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LuaJIT.2.1.0-beta3/)

add_custom_command(TARGET ${PROJECT_NAME}
    PRE_BUILD
    COMMAND ${NUGET_COMMAND} restore ${CMAKE_CURRENT_SOURCE_DIR}/packages.config -SolutionDirectory ${CMAKE_BINARY_DIR}
)

if(NOT "${CMAKE_VS_PLATFORM_NAME}" MATCHES "(x64)")
    add_custom_command(TARGET ${PROJECT_NAME}
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${LUAJIT}native/bin/x86/Release/lua51.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    )
else()
    add_custom_command(TARGET ${PROJECT_NAME}
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${LUAJIT}native/bin/x64/Release/lua51.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    )
endif()

## LuaJIT
if(NOT "${CMAKE_VS_PLATFORM_NAME}" MATCHES "(x64)")
    target_link_libraries(luabind PUBLIC ${LUAJIT}native/lib/x86/release/lua51.lib)
else()
    target_link_libraries(luabind PUBLIC ${LUAJIT}native/lib/x64/release/lua51.lib)
endif()
