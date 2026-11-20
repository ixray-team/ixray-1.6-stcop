# Nuget entry
find_program(NUGET_COMMAND nuget)
if(NOT NUGET_COMMAND)
    if(NOT EXISTS "${CMAKE_BINARY_DIR}/dep/nuget")
        message("Downloading NuGet...")
        execute_process(COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_BINARY_DIR}/dep/nuget")
        file(DOWNLOAD https://dist.nuget.org/win-x86-commandline/latest/nuget.exe
             "${CMAKE_BINARY_DIR}/dep/nuget/nuget.exe")
        message("NuGet downloaded: ${NUGET_COMMAND}")
    endif()
    set(NUGET_COMMAND "${CMAKE_BINARY_DIR}/dep/nuget/nuget.exe")
else()
    message("NuGet found: ${NUGET_COMMAND}")
endif()

# Download packages
if (WIN32 AND IXRAY_CROSS_COMPILATION)
    set(PACKAGES_CONFIG "${CMAKE_CURRENT_SOURCE_DIR}/cmake/packages/nuget/Packages.config")
    file(READ "${PACKAGES_CONFIG}" PACKAGES_CONTENT)
    string(REGEX MATCHALL "<package id=\"[^\"]+\" version=\"[^\"]+\"" PACKAGE_ENTRIES "${PACKAGES_CONTENT}")

    set(NUGET_CACHE_DIR "${CMAKE_BINARY_DIR}/dep/nuget/cache")
    file(MAKE_DIRECTORY "${NUGET_CACHE_DIR}")

    foreach(ENTRY IN LISTS PACKAGE_ENTRIES)
        string(REGEX REPLACE "<package id=\"([^\"]+)\" version=\"([^\"]+)\"" "\\1" PKG_ID "${ENTRY}")
        string(REGEX REPLACE "<package id=\"([^\"]+)\" version=\"([^\"]+)\"" "\\2" PKG_VER "${ENTRY}")

        set(PKG_TARGET_DIR "${CMAKE_BINARY_DIR}/packages/${PKG_ID}.${PKG_VER}")

        if (EXISTS "${PKG_TARGET_DIR}")
            message(STATUS "[OK] ${PKG_ID} ${PKG_VER}")
            continue()
        endif()

        message(STATUS "Downloading ${PKG_ID} ${PKG_VER}")

        # Приводим ID к нижнему регистру — именно так NuGet.org формирует URL.
        string(TOLOWER "${PKG_ID}" PKG_ID_LOWER)
        set(NUPKG_URL "https://api.nuget.org/v3-flatcontainer/${PKG_ID_LOWER}/${PKG_VER}/${PKG_ID_LOWER}.${PKG_VER}.nupkg")
        set(NUPKG_FILE "${NUGET_CACHE_DIR}/${PKG_ID}.${PKG_VER}.nupkg")

        file(DOWNLOAD "${NUPKG_URL}" "${NUPKG_FILE}"
                STATUS DOWNLOAD_STATUS
                TLS_VERIFY ON
        )
        list(GET DOWNLOAD_STATUS 0 DOWNLOAD_RESULT)
        if (NOT DOWNLOAD_RESULT EQUAL 0)
            list(GET DOWNLOAD_STATUS 1 DOWNLOAD_ERROR)
            message(FATAL_ERROR "Failed to download ${PKG_ID} ${PKG_VER}: ${DOWNLOAD_ERROR}")
        endif()

        file(MAKE_DIRECTORY "${PKG_TARGET_DIR}")
        execute_process(
                COMMAND ${CMAKE_COMMAND} -E tar xf "${NUPKG_FILE}"
                WORKING_DIRECTORY "${PKG_TARGET_DIR}"
                RESULT_VARIABLE EXTRACT_RESULT
        )
        if (NOT EXTRACT_RESULT EQUAL 0)
            message(FATAL_ERROR "Failed to extract ${PKG_ID} ${PKG_VER}")
        endif()

        message(STATUS "[OK] ${PKG_ID} ${PKG_VER}")
    endforeach()
else ()
    execute_process(
            COMMAND ${NUGET_COMMAND} restore ${CMAKE_CURRENT_SOURCE_DIR}/cmake/packages/nuget/Packages.config -SolutionDirectory ${CMAKE_BINARY_DIR}
    )
endif ()

# Helper
if (WIN32 AND NOT "${CMAKE_VS_PLATFORM_NAME}" MATCHES "(x64)")
    set(NUGET_PACKAGE_PLATFORM x86)
else()
    set(NUGET_PACKAGE_PLATFORM x64)
endif()

# Optick
set(CORE_OPT ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Optick.Runtimes.win-${NUGET_PACKAGE_PLATFORM}.1.4.0.1/)

# DxMath
set(CORE_DXMATH ${CMAKE_BINARY_DIR}/packages/directxmath.2024.2.15.1/)

# Theora
set(ENGINE_THRA ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibTheora.1.1.1.3/)

# OpenAL
set(SND_OAL ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.OpenALSoft.1.23.1.1/)

# LuaJIT 
set(LUAJIT ${CMAKE_BINARY_DIR}/packages/IXRay.LuaJIT.Binaries.win10.0.19041.0-${NUGET_PACKAGE_PLATFORM}.1626960173.0.0-open/)

set(LUAJIT_NAME lua51.dll)
set(LUAJIT_LIB ${LUAJIT}lib/lua51.lib)
set(LUAJIT_BIN ${LUAJIT}bin/${LUAJIT_NAME})

# Nuget
set(NVTT ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Nvtt.Runtimes.win-x64.2024.6.1-open/)

# TBB
set(IXR_TBB_SDK ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.OneTbb.Runtimes.win7-${NUGET_PACKAGE_PLATFORM}.2021.11.0/)
set(IXR_TBB_INC ${IXR_TBB_SDK}build/native/include/)
set(IXR_TBB_BIN ${IXR_TBB_SDK}runtimes/win7-${NUGET_PACKAGE_PLATFORM}/native/Release/${IXR_TBB_NAME})
set(IXR_TBB_LIB ${IXR_TBB_SDK}/runtimes/win7-${NUGET_PACKAGE_PLATFORM}/native/Release/tbb12.lib)

# LZO
set(LZO ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Lzo.Runtimes.win-${NUGET_PACKAGE_PLATFORM}.2.10.0)
set(LZO_LIB ${LZO}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/lzo2.lib)

# Intel XeSS
set(INTEL_XESS ${CMAKE_BINARY_DIR}/packages/IXRay.IntelXESS.2.0.1.1/include/)
set(INTEL_XESS_LIB ${CMAKE_BINARY_DIR}/packages/IXRay.IntelXESS.2.0.1.1/lib/libxess.lib)
set(INTEL_XESS_DX11_LIB ${CMAKE_BINARY_DIR}/packages/IXRay.IntelXESS.2.0.1.1/lib/libxess_dx11.lib)
set(INTEL_XESS_BIN ${CMAKE_BINARY_DIR}/packages/IXRay.IntelXESS.2.0.1.1/bin/libxess.dll)
set(INTEL_XESS_DX11_BIN ${CMAKE_BINARY_DIR}/packages/IXRay.IntelXESS.2.0.1.1/bin/libxess_dx11.dll)

# YAML
set(YAML_CORE ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.YamlCpp.Runtimes.win-x64.0.8.0)
set(YAML_INCL ${YAML_CORE}/build/native/include)
set(YAML_LIB  ${YAML_CORE}/runtimes/win-x64/native/Release/yaml-cpp.lib)
set(YAML_BIN  ${YAML_CORE}/runtimes/win-x64/native/Release/yaml-cpp.dll)
set(YAML_LIB_NAME yaml-cpp.dll)

# MySQL Connector
set(MYSQLCONNECTOR ${CMAKE_BINARY_DIR}/packages/IXRay.MySQLConnector.8.0.33/)
