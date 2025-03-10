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
    COMMAND ${NUGET_COMMAND} restore ${CMAKE_CURRENT_SOURCE_DIR}/cmake/windows/Packages.config -SolutionDirectory ${CMAKE_BINARY_DIR}
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
set(CORE_SDL3_PLATFORM win-${NUGET_PACKAGE_PLATFORM})
set(CORE_SDL3 ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Sdl.Runtimes.${CORE_SDL3_PLATFORM}.2024.3.16-open/)
set(CORE_SDL3_LIBRARY SDL3.lib)

# Optick
set(CORE_OPT ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Optick.Runtimes.win-${NUGET_PACKAGE_PLATFORM}.1.4.0.1/)

# DxMath
set(CORE_DXMATH ${CMAKE_BINARY_DIR}/packages/directxmath.2024.2.15.1/)

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
set(CORE_DXMATH ${CMAKE_BINARY_DIR}/packages/directxmath.2024.2.15.1/)

# Steam Sockets
set(STEAM_SOCKETS_PATH ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.GameNetworkingSockets.Vcpkg.Runtimes.win-${NUGET_PACKAGE_PLATFORM}.1.4.1)

# RedImage
set(REDIMAGE_PATH ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.RedImageTool.Runtimes.win-x64.0.1.0)

# LuaJIT 
set(LUAJIT ${CMAKE_BINARY_DIR}/packages/IXRay.Packages.LuaJIT.Runtimes.win-${NUGET_PACKAGE_PLATFORM}.2017.8.11-open/)

set(LUAJIT_NAME lua51.dll)
set(LUAJIT_LIB ${LUAJIT}runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/lua51.lib)
set(LUAJIT_BIN ${LUAJIT}runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/${LUAJIT_NAME})

# FreeImage
set(FREEIMAGE ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.FreeImage.WinMerge.2023.8.19-open)

# Nuget
set(NVTT ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Nvtt.Runtimes.win-x64.2024.6.1-open/)

# TBB
set(IXR_TBB_SDK ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.OneTbb.Runtimes.win7-${NUGET_PACKAGE_PLATFORM}.2021.11.0/)
set(IXR_TBB_INC ${IXR_TBB_SDK}build/native/include/)
set(IXR_TBB_BIN ${IXR_TBB_SDK}runtimes/win7-${NUGET_PACKAGE_PLATFORM}/native/Release/${IXR_TBB_NAME})
set(IXR_TBB_LIB ${IXR_TBB_SDK}/runtimes/win7-${NUGET_PACKAGE_PLATFORM}/native/Release/tbb12.lib)

# AMD FidelityFX FSR2
set(AMD_FSR2 ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.FidelityFX.FSR2.DirectX11.Runtimes.win-${NUGET_PACKAGE_PLATFORM}.2.2.1.1)

# SpeexDSP
set(SPEEXDSP ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.SpeexDsp.Runtimes.win-${NUGET_PACKAGE_PLATFORM}.2024.6.4.1-open)

# OPUS
set(OPUS ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Opus.Runtimes.win-${NUGET_PACKAGE_PLATFORM}.2024.5.22-open)

# Mimalloc
set(MIMALLOC ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Mimalloc.Runtimes.win-${NUGET_PACKAGE_PLATFORM}.2.1.7.3)
if("${NUGET_PACKAGE_PLATFORM}" MATCHES "(x86)")
    set(MIMALLOC_POSTFIX "32")
endif()

# LZO
set(LZO ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Lzo.Runtimes.win-${NUGET_PACKAGE_PLATFORM}.2.10.0)
set(LZO_LIB ${LZO}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/lzo2.lib)

# YAML
set(YAML_CORE ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.YamlCpp.Runtimes.win-x64.0.8.0)
set(YAML_INCL ${YAML_CORE}/build/native/include)
set(YAML_LIB  ${YAML_CORE}/runtimes/win-x64/native/Release/yaml-cpp.lib)
set(YAML_BIN  ${YAML_CORE}/runtimes/win-x64/native/Release/yaml-cpp.dll)
set(YAML_LIB_NAME yaml-cpp.dll)