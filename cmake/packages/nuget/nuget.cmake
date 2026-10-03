# Nuget entry
set(NUGET_LOCAL_EXE "${CMAKE_BINARY_DIR}/dep/nuget/nuget.exe")
set(NUGET_PACKAGES_CONFIG "${CMAKE_CURRENT_SOURCE_DIR}/cmake/packages/nuget/Packages.config")
set(NUGET_CONFIG_FILE "${CMAKE_CURRENT_SOURCE_DIR}/NuGet.config")

find_program(NUGET_COMMAND nuget)
if(NOT NUGET_COMMAND)
    if(NOT EXISTS "${NUGET_LOCAL_EXE}")
        if(IXRAY_OFFLINE)
            message(FATAL_ERROR "NuGet is missing and IXRAY_OFFLINE=ON. Place nuget.exe at ${NUGET_LOCAL_EXE} or install nuget in PATH.")
        endif()
        message(STATUS "Downloading NuGet...")
        file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/dep/nuget")
        file(DOWNLOAD
            https://dist.nuget.org/win-x86-commandline/latest/nuget.exe
            "${NUGET_LOCAL_EXE}"
            TLS_VERIFY ON
            TIMEOUT 30
            INACTIVITY_TIMEOUT 15
            STATUS nuget_download_status
        )
        list(GET nuget_download_status 0 nuget_download_code)
        if(NOT nuget_download_code EQUAL 0)
            file(REMOVE "${NUGET_LOCAL_EXE}")
            list(GET nuget_download_status 1 nuget_download_error)
            message(FATAL_ERROR "Failed to download nuget.exe: ${nuget_download_error}")
        endif()
        message(STATUS "NuGet downloaded: ${NUGET_LOCAL_EXE}")
    endif()
    set(NUGET_COMMAND "${NUGET_LOCAL_EXE}")
else()
    message(STATUS "NuGet found: ${NUGET_COMMAND}")
endif()

file(SHA256 "${NUGET_PACKAGES_CONFIG}" NUGET_PACKAGES_HASH)
set(NUGET_RESTORE_STAMP "${CMAKE_BINARY_DIR}/packages/.nuget-restore.stamp")
set(NUGET_NEED_RESTORE TRUE)
if(NOT IXRAY_NUGET_FORCE_RESTORE AND EXISTS "${NUGET_RESTORE_STAMP}")
    file(READ "${NUGET_RESTORE_STAMP}" NUGET_RESTORE_STAMP_HASH)
    string(STRIP "${NUGET_RESTORE_STAMP_HASH}" NUGET_RESTORE_STAMP_HASH)
    if(NUGET_RESTORE_STAMP_HASH STREQUAL NUGET_PACKAGES_HASH)
        set(NUGET_NEED_RESTORE FALSE)
        message(STATUS "NuGet packages already restored, skipping restore")
    endif()
endif()

if(IXRAY_OFFLINE)
    if(NUGET_NEED_RESTORE)
        message(FATAL_ERROR "NuGet restore is required but IXRAY_OFFLINE=ON. Restore packages once with network, or pass -DIXRAY_NUGET_FORCE_RESTORE=ON with connectivity.")
    endif()
    set(NUGET_NEED_RESTORE FALSE)
endif()

# Download packages
if(NUGET_NEED_RESTORE)
    message(STATUS "Restoring NuGet packages...")
    if (WIN32 AND IXRAY_CROSS_COMPILATION)
        execute_process(
                COMMAND winepath -w
                "${NUGET_PACKAGES_CONFIG}"
                OUTPUT_VARIABLE NUGET_CONFIG_WIN
                OUTPUT_STRIP_TRAILING_WHITESPACE
                TIMEOUT 30
                RESULT_VARIABLE NUGET_WINEPATH_CFG_RESULT
        )
        if(NOT NUGET_WINEPATH_CFG_RESULT EQUAL 0)
            message(FATAL_ERROR "winepath failed for Packages.config")
        endif()
        execute_process(
                COMMAND winepath -w
                "${CMAKE_BINARY_DIR}"
                OUTPUT_VARIABLE CMAKE_BINARY_DIR_WIN
                OUTPUT_STRIP_TRAILING_WHITESPACE
                TIMEOUT 30
                RESULT_VARIABLE NUGET_WINEPATH_BIN_RESULT
        )
        if(NOT NUGET_WINEPATH_BIN_RESULT EQUAL 0)
            message(FATAL_ERROR "winepath failed for CMAKE_BINARY_DIR")
        endif()
        execute_process(
                COMMAND winepath -w
                "${NUGET_CONFIG_FILE}"
                OUTPUT_VARIABLE NUGET_CONFIG_FILE_WIN
                OUTPUT_STRIP_TRAILING_WHITESPACE
                TIMEOUT 30
                RESULT_VARIABLE NUGET_WINEPATH_NCFG_RESULT
        )
        if(NOT NUGET_WINEPATH_NCFG_RESULT EQUAL 0)
            message(FATAL_ERROR "winepath failed for NuGet.config")
        endif()

        execute_process(
                COMMAND wine
                "${NUGET_COMMAND}"
                restore
                "${NUGET_CONFIG_WIN}"
                -SolutionDirectory
                "${CMAKE_BINARY_DIR_WIN}"
                -ConfigFile
                "${NUGET_CONFIG_FILE_WIN}"
                -NonInteractive
                RESULT_VARIABLE NUGET_RESTORE_RESULT
                TIMEOUT 600
        )
    else ()
        execute_process(
                COMMAND ${NUGET_COMMAND} restore "${NUGET_PACKAGES_CONFIG}"
                    -SolutionDirectory ${CMAKE_BINARY_DIR}
                    -ConfigFile "${NUGET_CONFIG_FILE}"
                    -NonInteractive
                RESULT_VARIABLE NUGET_RESTORE_RESULT
                TIMEOUT 600
        )
    endif ()

    if(NOT NUGET_RESTORE_RESULT EQUAL 0)
        message(FATAL_ERROR "NuGet restore failed (exit ${NUGET_RESTORE_RESULT}). Check the network, or reconfigure with -DIXRAY_OFFLINE=ON after a successful restore.")
    endif()

    file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/packages")
    file(WRITE "${NUGET_RESTORE_STAMP}" "${NUGET_PACKAGES_HASH}\n")
endif()

# Helper
if (WIN32 AND NOT "${CMAKE_VS_PLATFORM_NAME}" MATCHES "(x64)")
    set(NUGET_PACKAGE_PLATFORM x86)
else()
    set(NUGET_PACKAGE_PLATFORM x64)
endif()

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

# DLSS
set(NVIDIA_DLSS ${CMAKE_BINARY_DIR}/packages/IXRay.DLSS.310.4.0/)