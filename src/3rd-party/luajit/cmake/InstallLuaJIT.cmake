# Return if not enabled installing
if(NOT LUAJIT_ENABLE_INSTALL)
    return()
endif()

# Install library
install(
    TARGETS libluajit
    EXPORT LuaJITTargets
    RUNTIME
        COMPONENT Library
        DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY
        COMPONENT Library
        DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE
        COMPONENT Library
        DESTINATION ${CMAKE_INSTALL_LIBDIR}
    PERMISSIONS
        OWNER_READ OWNER_WRITE
        GROUP_READ
        WORLD_READ
)
if(WIN32 AND MSVC)
    install(
        FILES
            $<TARGET_PDB_FILE:LuaJIT::Library>
        DESTINATION ${CMAKE_INSTALL_BINDIR}
        COMPONENT Library
    )
elseif(LINUX)
    install(
        FILES
            $<TARGET_FILE_DIR:LuaJIT::Library>/$<TARGET_FILE_NAME:LuaJIT::Library>.debug
        DESTINATION ${CMAKE_INSTALL_LIBDIR}
        COMPONENT Library
    )
elseif(APPLE AND CMAKE_GENERATOR MATCHES "Xcode")
    install(
        DIRECTORY
            $<TARGET_FILE_DIR:LuaJIT::Library>/$<TARGET_FILE_NAME:LuaJIT::Library>.dSYM
        DESTINATION ${CMAKE_INSTALL_LIBDIR}
        COMPONENT Library
    )
endif()

# Install headers
install(
    FILES
        ${LUAJIT_ROOT_FOLDER}/src/lua.h
        ${LUAJIT_ROOT_FOLDER}/src/lualib.h
        ${LUAJIT_ROOT_FOLDER}/src/lauxlib.h
        ${LUAJIT_ROOT_FOLDER}/src/luaconf.h
        ${LUAJIT_ROOT_FOLDER}/src/lua.hpp
        ${LUAJIT_ROOT_FOLDER}/src/luajit.h
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/luajit-${LUAJIT_VERSION_MAJOR}.${LUAJIT_VERSION_MINOR}
    COMPONENT Library
)

# Install scripts
install(
    FILES
        ${LUAJIT_ROOT_FOLDER}/src/jit/bc.lua
        ${LUAJIT_ROOT_FOLDER}/src/jit/bcsave.lua
        ${LUAJIT_ROOT_FOLDER}/src/jit/dis_arm.lua
        ${LUAJIT_ROOT_FOLDER}/src/jit/dis_arm64.lua
        ${LUAJIT_ROOT_FOLDER}/src/jit/dis_arm64be.lua
        ${LUAJIT_ROOT_FOLDER}/src/jit/dis_mips.lua
        ${LUAJIT_ROOT_FOLDER}/src/jit/dis_mips64.lua
        ${LUAJIT_ROOT_FOLDER}/src/jit/dis_mips64el.lua
        ${LUAJIT_ROOT_FOLDER}/src/jit/dis_mips64r6.lua
        ${LUAJIT_ROOT_FOLDER}/src/jit/dis_mips64r6el.lua
        ${LUAJIT_ROOT_FOLDER}/src/jit/dis_mipsel.lua
        ${LUAJIT_ROOT_FOLDER}/src/jit/dis_ppc.lua
        ${LUAJIT_ROOT_FOLDER}/src/jit/dis_x64.lua
        ${LUAJIT_ROOT_FOLDER}/src/jit/dis_x86.lua
        ${LUAJIT_ROOT_FOLDER}/src/jit/dump.lua
        ${LUAJIT_ROOT_FOLDER}/src/jit/p.lua
        ${LUAJIT_ROOT_FOLDER}/src/jit/v.lua
        ${LUAJIT_ROOT_FOLDER}/src/jit/zone.lua
        ${LUAJIT_BUILD_FOLDER}/src/jit/vmdef.lua
    DESTINATION ${CMAKE_INSTALL_DATADIR}/luajit-${LUAJIT_VERSION_MAJOR}.${LUAJIT_VERSION_MINOR}
    COMPONENT Library
)

# Install application
if(LUAJIT_ENABLE_APPLICATION)
    install(
        TARGETS luajit
        EXPORT LuaJITTargets
        RUNTIME
            COMPONENT Application
            DESTINATION ${CMAKE_INSTALL_BINDIR}
        PERMISSIONS
            OWNER_READ OWNER_WRITE OWNER_EXECUTE
            GROUP_READ GROUP_EXECUTE
            WORLD_READ WORLD_EXECUTE
    )

    if(WIN32 AND MSVC)
        install(
            FILES
                $<TARGET_PDB_FILE:LuaJIT::Application>
            DESTINATION ${CMAKE_INSTALL_BINDIR}
            COMPONENT Interpreter
        )
    elseif(LINUX)
        install(
            FILES
                $<TARGET_FILE_DIR:LuaJIT::Application>/$<TARGET_FILE_NAME:LuaJIT::Application>.debug
            DESTINATION ${CMAKE_INSTALL_BINDIR}
            COMPONENT Interpreter
        )
    elseif(APPLE AND CMAKE_GENERATOR MATCHES "Xcode")
        install(
            DIRECTORY
                $<TARGET_FILE_DIR:LuaJIT::Application>/$<TARGET_FILE_NAME:LuaJIT::Application>.dSYM
            DESTINATION ${CMAKE_INSTALL_BINDIR}
            COMPONENT Interpreter
        )
    endif()
endif()

# Install package config
configure_file(
    ${LUAJIT_ROOT_FOLDER}/cmake/luajit.pc.in
    ${LUAJIT_BUILD_FOLDER}/pkgconfig/luajit.pc
    @ONLY
)
install(
    FILES ${LUAJIT_BUILD_FOLDER}/pkgconfig/luajit.pc
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig
    COMPONENT Library
)

# Install documentation
configure_file(
    ${LUAJIT_ROOT_FOLDER}/COPYRIGHT
    ${LUAJIT_BUILD_FOLDER}/copyright
    COPYONLY
)
install(
    FILES ${LUAJIT_BUILD_FOLDER}/copyright
    DESTINATION ${CMAKE_INSTALL_DATADIR}/doc/luajit-${LUAJIT_VERSION_MAJOR}.${LUAJIT_VERSION_MINOR}
)
if(LUAJIT_ENABLE_APPLICATION)
    install(
        FILES ${LUAJIT_ROOT_FOLDER}/etc/luajit.1
        DESTINATION ${CMAKE_INSTALL_MANDIR}/man1
        COMPONENT Application
    )
endif()

# Export targets
install(
    EXPORT LuaJITTargets
    FILE LuaJITTargets.cmake
    NAMESPACE LuaJIT::
    DESTINATION ${LUAJIT_BUILD_FOLDER}/cmake
)

# Export config
include(CMakePackageConfigHelpers)
configure_package_config_file(
    ${LUAJIT_ROOT_FOLDER}/cmake/LuaJITConfig.cmake.in
    ${LUAJIT_BUILD_FOLDER}/cmake/LuaJITConfig.cmake
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/luajit-${LUAJIT_VERSION_MAJOR}.${LUAJIT_VERSION_MINOR}
    NO_SET_AND_CHECK_MACRO
    NO_CHECK_REQUIRED_COMPONENTS_MACRO
)
write_basic_package_version_file(
    ${LUAJIT_BUILD_FOLDER}/cmake/LuaJITConfigVersion.cmake
    VERSION ${LUAJIT_VERSION_MAJOR}.${LUAJIT_VERSION_MINOR}.${LUAJIT_VERSION_PATCH}
    COMPATIBILITY AnyNewerVersion
)
install(
    FILES
        ${LUAJIT_BUILD_FOLDER}/cmake/LuaJITConfig.cmake
        ${LUAJIT_BUILD_FOLDER}/cmake/LuaJITConfigVersion.cmake
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/luajit-${LUAJIT_VERSION_MAJOR}.${LUAJIT_VERSION_MINOR}
)
install(
    DIRECTORY ${LUAJIT_BUILD_FOLDER}/cmake/
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/luajit-${LUAJIT_VERSION_MAJOR}.${LUAJIT_VERSION_MINOR}
    FILES_MATCHING PATTERN "LuaJITTargets*.cmake"
)

# Replace prefix
configure_file(
    ${LUAJIT_ROOT_FOLDER}/cmake/ReplacePrefix.cmake.in
    ${LUAJIT_BUILD_FOLDER}/ReplacePrefix.cmake
    @ONLY
)
install(SCRIPT ${LUAJIT_BUILD_FOLDER}/ReplacePrefix.cmake)
