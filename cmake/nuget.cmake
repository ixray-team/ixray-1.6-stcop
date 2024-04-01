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
	COMMAND ${NUGET_COMMAND} restore ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Packages.config -SolutionDirectory ${CMAKE_BINARY_DIR}
)

# Helper
if (WIN32 AND NOT "${CMAKE_VS_PLATFORM_NAME}" MATCHES "(x64)")
    set(NUGET_PACKAGE_PLATFORM x86)
    set(NUGET_PACKAGE_PLATFORM_EX x86)
else()
    set(NUGET_PACKAGE_PLATFORM x64)
    set(NUGET_PACKAGE_PLATFORM_EX x86_64)
endif()

# SDL3
if (WIN32)
    set(CORE_SDL3_PLATFORM win-${NUGET_PACKAGE_PLATFORM})
else()
    set(CORE_SDL3_PLATFORM linux-${NUGET_PACKAGE_PLATFORM})
endif()

set(CORE_SDL3 ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Sdl.Runtimes.${CORE_SDL3_PLATFORM}.2024.3.16-open/)

# Optick
set(CORE_OPT ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Optick.1.4.0/)

# DxMath
set(CORE_DXMATH ${CMAKE_BINARY_DIR}/packages/directxmath.2022.12.12.1/)

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

# DxMath
set(CORE_DXMATH ${CMAKE_BINARY_DIR}/packages/directxmath.2022.12.12.1/)

# Steam Sockets
set(STEAM_SOCKETS_PATH ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.GameNetworkingSockets.Vcpkg.Runtimes.win-${NUGET_PACKAGE_PLATFORM}.1.4.1)

# LuaJIT 
set(LUAJIT ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LuaJIT.2.1.0-beta3/)

# FreeImage
set(FREEIMAGE ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.FreeImage.WinMerge.2023.8.19-open)

# Nuget
set(NVTT ${CMAKE_BINARY_DIR}/packages/IXRay.Packages.Nvtt.2020.12.21-open/)