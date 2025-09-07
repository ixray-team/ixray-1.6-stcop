# Nuget entry
find_program(NUGET_COMMAND nuget)
if(NOT NUGET_COMMAND)
    message("NuGet not found in PATH!")
    message("Downloading NuGet...")
    if(NOT EXISTS "${CMAKE_BINARY_DIR}/dep/nuget")
        execute_process(COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_BINARY_DIR}/dep/nuget")
        file(DOWNLOAD https://dist.nuget.org/win-x86-commandline/latest/nuget.exe
             "${CMAKE_BINARY_DIR}/dep/nuget/nuget.exe")
    endif()
    set(NUGET_COMMAND "${CMAKE_BINARY_DIR}/dep/nuget/nuget.exe")
    message("NuGet downloaded: ${NUGET_COMMAND}")
else()
    message("NuGet found: ${NUGET_COMMAND}")
endif()

# Download packages
execute_process(
    COMMAND ${NUGET_COMMAND} restore ${CMAKE_CURRENT_SOURCE_DIR}/cmake/linux/Packages.config -SolutionDirectory ${CMAKE_BINARY_DIR}
)

# Helper
set(NUGET_PACKAGE_PLATFORM x64)
set(NUGET_PACKAGE_PLATFORM_EX x86_64)

# Optick
set(CORE_OPT ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Optick.1.4.0/)

# Theora
set(ENGINE_THRA ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibTheora.1.1.1.3/)

# OGG
set(SND_OGG ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibOgg.1.3.5.4/)

# OpenAL
set(SND_OAL ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.OpenALSoft.1.23.1.1/)

# Vorbis
set(SND_VOB ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibVorbis.1.3.7.4/)

# FreeType
set(ENGINE_FREETYPE ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.FreeType.2.13.2/)

# Steam Sockets
set(STEAM_SOCKETS_PATH ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.GameNetworkingSockets.Vcpkg.Runtimes.linux-x64.1.4.1)

# LuaJIT 
set(LUAJIT ${CMAKE_BINARY_DIR}/packages/IXRay.Packages.LuaJIT.Runtimes.linux-x64.2023.8.23.1-open/)

set(LUAJIT_NAME libluajit.so)
set(LUAJIT_LIB ${LUAJIT}runtimes/linux-x64/native/${LUAJIT_NAME})
set(LUAJIT_BIN ${LUAJIT}runtimes/linux-x64/native/${LUAJIT_NAME})

# Validation / fallback : si le fichier standard libluajit.so n'existe pas mais qu'une variante versionnée est présente
if(NOT EXISTS "${LUAJIT_LIB}")
    file(GLOB _LJ_CAND "${LUAJIT}runtimes/linux-x64/native/libluajit*.so")
    list(FILTER _LJ_CAND EXCLUDE REGEX ".*/libluajit.so$")
    list(LENGTH _LJ_CAND _LJ_COUNT)
    if(_LJ_COUNT GREATER 0)
        list(GET _LJ_CAND 0 _LJ_VERSIONED)
        message(STATUS "LuaJIT: libluajit.so absent, utilisation candidate: ${_LJ_VERSIONED}")
        # Créer un lien symbolique pour normaliser le nom attendu par le reste du build
        execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ${_LJ_VERSIONED} ${LUAJIT_LIB} RESULT_VARIABLE _LJ_SYM_RES OUTPUT_QUIET ERROR_QUIET)
        if(_LJ_SYM_RES EQUAL 0)
            message(STATUS "LuaJIT: lien symbolique créé -> ${LUAJIT_LIB}")
        else()
            message(WARNING "LuaJIT: impossible de créer le lien symbolique vers ${_LJ_VERSIONED}; le link peut échouer si aucune règle ne copie la lib.")
            # Fallback: pointer directement sur la versionnée pour la phase link
            set(LUAJIT_LIB ${_LJ_VERSIONED})
            set(LUAJIT_BIN ${_LJ_VERSIONED})
        endif()
    else()
        message(WARNING "LuaJIT: no NuGet binary found in ${LUAJIT}runtimes/linux-x64/native/ (expected: libluajit.so). Attempting system detection...")
        # Recherche système (Debian/Ubuntu): libluajit-5.1.so.2
        find_library(SYS_LUAJIT libluajit.so PATHS /usr/lib /usr/lib64 /usr/local/lib NO_DEFAULT_PATH)
        if(NOT SYS_LUAJIT)
            find_library(SYS_LUAJIT NAMES libluajit-5.1.so libluajit-5.1.so.2 libluajit.so.2 PATHS /usr/lib /usr/lib64 /usr/local/lib)
        endif()
        if(SYS_LUAJIT)
            get_filename_component(_SYS_DIR ${SYS_LUAJIT} DIRECTORY)
            message(STATUS "LuaJIT: using system library ${SYS_LUAJIT}")
            set(LUAJIT_LIB ${SYS_LUAJIT})
            set(LUAJIT_BIN ${SYS_LUAJIT})
            set(LUAJIT_SYSTEM TRUE)
        else()
            message(WARNING "LuaJIT: no system library found. Install libluajit-5.1-dev or check NuGet.")
        endif()
    endif()
endif()

# FreeImage
set(FREEIMAGE ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.FreeImage.WinMerge.2023.8.19-open)

# Nuget
set(NVTT ${CMAKE_BINARY_DIR}/packages/IXRay.Packages.Nvtt.2020.12.21-open/)