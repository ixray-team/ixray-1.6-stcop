# Project for LuaJIT library
project(libluajit
    LANGUAGES C
    VERSION ${LUAJIT_VERSION_MAJOR}.${LUAJIT_VERSION_MINOR}
)

file(GLOB LIBRARY_HEADERS
    ${LUAJIT_ROOT_FOLDER}/src/lj_*.h
    ${LUAJIT_ROOT_FOLDER}/src/lib_*.h
)
file(GLOB LIBRARY_SOURCES
    ${LUAJIT_ROOT_FOLDER}/src/lj_*.c
    ${LUAJIT_ROOT_FOLDER}/src/lib_*.c
)

if(WIN32 AND MSVC)
    configure_file(
        ${LUAJIT_ROOT_FOLDER}/cmake/libluajit.rc.in
        ${LUAJIT_BUILD_FOLDER}/res/libluajit.rc
    )
    set(LIBRARY_RESOURCES
        ${LUAJIT_BUILD_FOLDER}/res/libluajit.rc
    )
endif()

source_group("Library" FILES ${LIBRARY_HEADERS})
source_group("Library" FILES ${LIBRARY_SOURCES})
source_group("Resources" FILES ${LIBRARY_RESOURCES})

add_library(${PROJECT_NAME} SHARED
    ${LIBRARY_HEADERS}
    ${LIBRARY_SOURCES}
    ${LIBRARY_RESOURCES}
)
add_library(LuaJIT::Library ALIAS ${PROJECT_NAME})

set_target_properties(${PROJECT_NAME}
    PROPERTIES
        ENABLE_EXPORTS TRUE
)

target_include_directories(${PROJECT_NAME}
    PUBLIC
        $<BUILD_INTERFACE:${LUAJIT_ROOT_FOLDER}/src>
        $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/luajit-${LUAJIT_VERSION_MAJOR}.${LUAJIT_VERSION_MINOR}>
)
target_include_directories(${PROJECT_NAME} PRIVATE ${LUAJIT_ROOT_FOLDER}/src)
target_include_directories(${PROJECT_NAME} PRIVATE ${LUAJIT_BUILD_FOLDER}/src)

target_compile_options(${PROJECT_NAME} PRIVATE ${LUAJIT_COMPILE_FLAGS})
target_compile_definitions(${PROJECT_NAME} PRIVATE ${LUAJIT_COMPILE_DEFINES})

target_link_options(${PROJECT_NAME} PRIVATE ${LUAJIT_LINK_FLAGS})

generate_symbols(${PROJECT_NAME})

generate_manifest(
    ${LUAJIT_ROOT_FOLDER}/cmake/libluajit.dll.manifest.in
    ${LUAJIT_BUILD_FOLDER}/res/libluajit.dll.manifest
)
embed_manifest(${PROJECT_NAME}
    DLL
    ${LUAJIT_BUILD_FOLDER}/res/libluajit.dll.manifest
)

add_dependencies(${PROJECT_NAME} gendef)

if(WIN32)
    target_compile_definitions(${PROJECT_NAME} PRIVATE ${LUAJIT_COMPILE_DEFINES_DYNAMIC})
    target_link_options(${PROJECT_NAME} PRIVATE ${LUAJIT_LINK_FLAGS})
    target_link_options(${PROJECT_NAME} PRIVATE ${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}/$<CONFIG>/lj_vm.obj)

    set_target_properties(${PROJECT_NAME}
        PROPERTIES
            OUTPUT_NAME lua${LUAJIT_NODOTABI_VERSION}
    )
elseif(LINUX AND CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(${PROJECT_NAME} PRIVATE ${LUAJIT_COMPILE_DEFINES_DYNAMIC})
    target_link_libraries(${PROJECT_NAME} PRIVATE $<TARGET_OBJECTS:lj_vm>)

    set_target_properties(${PROJECT_NAME}
        PROPERTIES
            OUTPUT_NAME luajit
            VERSION ${LUAJIT_VERSION_MAJOR}.${LUAJIT_VERSION_MINOR}
            SOVERSION ${LUAJIT_VERSION_MAJOR}
            INSTALL_RPATH "$ORIGIN"
            BUILD_WITH_INSTALL_RPATH TRUE
    )
elseif(APPLE AND CMAKE_C_COMPILER_ID MATCHES "AppleClang")
    target_link_libraries(${PROJECT_NAME} PRIVATE $<TARGET_OBJECTS:lj_vm>)

    set_target_properties(${PROJECT_NAME}
        PROPERTIES
            OUTPUT_NAME luajit
            VERSION ${LUAJIT_VERSION_MAJOR}.${LUAJIT_VERSION_MINOR}
            SOVERSION ${LUAJIT_VERSION_MAJOR}
            MACOSX_RPATH TRUE
            INSTALL_NAME_DIR "@rpath"
            INSTALL_RPATH "@loader_path;@loader_path/../lib;@loader_path/../../lib/$<CONFIG>"
            BUILD_WITH_INSTALL_RPATH TRUE
            XCODE_ATTRIBUTE_DEBUG_INFORMATION_FORMAT "dwarf-with-dsym"
    )
endif()

# Project for LuaJIT application
project(luajit
    LANGUAGES C
    VERSION ${LUAJIT_VERSION_MAJOR}.${LUAJIT_VERSION_MINOR}
)

set(APPLICATION_SOURCES
    ${LUAJIT_ROOT_FOLDER}/src/luajit.c
)

if(WIN32 AND MSVC)
    configure_file(
        ${LUAJIT_ROOT_FOLDER}/cmake/luajit.rc.in
        ${LUAJIT_BUILD_FOLDER}/res/luajit.rc
    )
    set(APPLICATION_RESOURCES
        ${LUAJIT_BUILD_FOLDER}/res/luajit.rc
    )
endif()

source_group("Application" FILES ${APPLICATION_SOURCES})
source_group("Resources" FILES ${APPLICATION_RESOURCES})

if(LUAJIT_ENABLE_APPLICATION)
    add_executable(${PROJECT_NAME}
        ${APPLICATION_SOURCES}
        ${APPLICATION_RESOURCES}
    )
    add_executable(LuaJIT::Application ALIAS ${PROJECT_NAME})

    target_compile_options(${PROJECT_NAME} PRIVATE ${LUAJIT_COMPILE_FLAGS})
    target_compile_definitions(${PROJECT_NAME} PRIVATE ${LUAJIT_COMPILE_DEFINES})

    target_link_options(${PROJECT_NAME} PRIVATE ${LUAJIT_LINK_FLAGS})

    generate_symbols(${PROJECT_NAME})

    generate_manifest(
        ${LUAJIT_ROOT_FOLDER}/cmake/luajit.exe.manifest.in
        ${LUAJIT_BUILD_FOLDER}/res/luajit.exe.manifest
    )
    embed_manifest(${PROJECT_NAME}
        EXE
        ${LUAJIT_BUILD_FOLDER}/res/luajit.exe.manifest
    )

    add_dependencies(${PROJECT_NAME} LuaJIT::Library)
    target_link_libraries(${PROJECT_NAME} PRIVATE LuaJIT::Library)

    if(WIN32 AND MSVC)
        target_link_options(${PROJECT_NAME} PRIVATE ${LUA_LINK_FLAGS})

        set_target_properties(${PROJECT_NAME}
            PROPERTIES
                OUTPUT_NAME luajit
        )
    elseif(LINUX AND CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
        set_target_properties(${PROJECT_NAME}
            PROPERTIES
                OUTPUT_NAME luajit
                INSTALL_RPATH "$ORIGIN:$ORIGIN/../lib:$ORIGIN/../../lib/$<CONFIG>"
                BUILD_WITH_INSTALL_RPATH TRUE
        )
    elseif(APPLE AND CMAKE_C_COMPILER_ID MATCHES "AppleClang")
        set_target_properties(${PROJECT_NAME}
            PROPERTIES
                OUTPUT_NAME luajit
                MACOSX_RPATH TRUE
                INSTALL_NAME_DIR "@rpath"
                INSTALL_RPATH "@executable_path;@executable_path/../lib;@executable_path/../../lib/$<CONFIG>"
                BUILD_WITH_INSTALL_RPATH TRUE
                XCODE_ATTRIBUTE_DEBUG_INFORMATION_FORMAT "dwarf-with-dsym"
        )
    endif()

    if(LINUX)
        target_link_libraries(${PROJECT_NAME} PRIVATE LibM::Library)
        target_link_libraries(${PROJECT_NAME} PRIVATE LibDl::Library)
    endif()
endif()
