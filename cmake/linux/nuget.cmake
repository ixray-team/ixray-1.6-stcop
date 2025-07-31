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

# SDL3
set(CORE_SDL3_PLATFORM linux-${NUGET_PACKAGE_PLATFORM})
set(CORE_SDL3 ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Sdl.Runtimes.${CORE_SDL3_PLATFORM}.2024.3.16-open/)
set(CORE_SDL3_LIBRARY libSDL3.so.0.0.0)

# Optick
set(CORE_OPT ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Optick.1.4.0/)

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

# Steam Sockets
set(STEAM_SOCKETS_PATH ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.GameNetworkingSockets.Vcpkg.Runtimes.linux-x64.1.4.1)

# LuaJIT 
set(LUAJIT ${CMAKE_BINARY_DIR}/packages/IXRay.Packages.LuaJIT.Runtimes.linux-x64.2023.8.23.1-open/)

set(LUAJIT_NAME libluajit.so)
set(LUAJIT_LIB ${LUAJIT}runtimes/linux-x64/native/${LUAJIT_NAME})
set(LUAJIT_BIN ${LUAJIT}runtimes/linux-x64/native/${LUAJIT_NAME})

# FreeImage
set(FREEIMAGE ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.FreeImage.WinMerge.2023.8.19-open)

# Nuget
set(NVTT ${CMAKE_BINARY_DIR}/packages/IXRay.Packages.Nvtt.2020.12.21-open/)

# TBB
set(IXR_TBB_SDK ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.OneTbb.Runtimes.linux-x64.2024.3.26-open/)
set(IXR_TBB_INC ${IXR_TBB_SDK}build/native/include/)
set(IXR_TBB_LIBDIR linux-${NUGET_PACKAGE_PLATFORM})
set(IXR_TBB_LIB  ${IXR_TBB_SDK}/runtimes/${IXR_TBB_LIBDIR}/native/Release/libtbb.so.12.13)
set(IXR_TBB_BIN ${IXR_TBB_LIB})

# Mimalloc
set(MIMALLOC ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Mimalloc.Runtimes.linux-x64.2.1.7.3)

# LZO
set(LZO ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Lzo.Runtimes.linux-x64.2.10.0)
set(LZO_LIB ${LZO}/runtimes/linux-${NUGET_PACKAGE_PLATFORM}/native/Release/liblzo2.so.2.0.0)

# YAML
set(YAML_CORE ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.YamlCpp.Runtimes.linux-x64.0.8.0)
set(YAML_INCL ${YAML_CORE}/build/native/include)
set(YAML_LIB  ${YAML_CORE}/runtimes/linux-x64/native/Release/libyaml-cpp.so.0.8.0)
set(YAML_BIN  ${YAML_LIB})
set(YAML_LIB_NAME libyaml-cpp.so.0.8.0)