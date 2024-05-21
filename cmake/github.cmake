# 3DS Max SDK 
set(IXR_3DS_MAX_SDK ${CMAKE_BINARY_DIR}/dep/max_2024.zip)

if(NOT EXISTS ${IXR_3DS_MAX_SDK})
    file(
        DOWNLOAD
        https://github.com/ixray-team/ixray-packages/releases/download/d2023.12.8/Autodesk.3dsMax.Sdk.2024.zip
        ${IXR_3DS_MAX_SDK}
        SHOW_PROGRESS
    )

    file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/dep/max_sdk)

    execute_process(
        COMMAND ${CMAKE_COMMAND} -E tar -xzf ${CMAKE_BINARY_DIR}/dep/max_2024.zip
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/dep/max_sdk
    )
endif()

set(IXR_3DS_MAX_SDK ${CMAKE_BINARY_DIR}/dep/max_sdk/)

# Maya SDK
set(IXR_MAYA_SDK ${CMAKE_BINARY_DIR}/dep/maya_clean_2024.7z)

if(NOT EXISTS ${IXR_MAYA_SDK})
    file(
        DOWNLOAD
        https://github.com/ixray-team/ixray-packages/releases/download/d2024.10.5/maya_clean_2024.7z
        ${IXR_MAYA_SDK}
        SHOW_PROGRESS
    )

    file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/dep/maya_sdk)

    execute_process(
        COMMAND ${CMAKE_COMMAND} -E tar -xzf ${CMAKE_BINARY_DIR}/dep/maya_clean_2024.7z
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/dep/maya_sdk
    )
endif()

set(IXR_MAYA_SDK ${CMAKE_BINARY_DIR}/dep/maya_sdk/)

# LightWave SDK
set(IXR_LW_SDK ${CMAKE_BINARY_DIR}/dep/lw_sdk_2020.zip)

if(NOT EXISTS ${IXR_LW_SDK})
    file(
        DOWNLOAD
        https://github.com/ixray-team/ixray-packages/releases/download/d2023.12.8/LightWave3D.Sdk.2020.zip
        ${CMAKE_BINARY_DIR}/dep/lw_sdk_2020.zip
        SHOW_PROGRESS
    )

    execute_process(
        COMMAND ${CMAKE_COMMAND} -E tar -xzf ${CMAKE_BINARY_DIR}/dep/lw_sdk_2020.zip
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/dep/
    )
endif()

set(IXR_LW_SDK ${CMAKE_BINARY_DIR}/dep/lw_sdk_2020/)

# Discord GameSDK
set(DISCORD_GAME_SDK_FILE ${CMAKE_BINARY_DIR}/dep/discord_gamesdk_3.2.1.zip)
if(NOT EXISTS ${DISCORD_GAME_SDK_FILE})
    file(
        DOWNLOAD
        https://github.com/ixray-team/ixray-packages/releases/download/d2023.12.8/Discord.GameSdk.3.2.1.zip
        ${CMAKE_BINARY_DIR}/dep/discord_gamesdk_3.2.1.zip
        SHOW_PROGRESS
    )

    file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/dep/discord_gamesdk/)

    execute_process(
        COMMAND ${CMAKE_COMMAND} -E tar -xzf ${CMAKE_BINARY_DIR}/dep/discord_gamesdk_3.2.1.zip
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/dep/discord_gamesdk/
    )
endif()
set(DISCORD_GAME_SDK ${CMAKE_BINARY_DIR}/dep/discord_gamesdk/)

# AMD AGS SDK
set(AMD_AGS_SDK_FILE ${CMAKE_BINARY_DIR}/dep/Amd.Ags.Sdk.5.4.2.zip)
if(NOT EXISTS ${AMD_AGS_SDK_FILE})
    file(
        DOWNLOAD
        https://github.com/ixray-team/ixray-packages/releases/download/d2023.12.8/Amd.Ags.Sdk.5.4.2.zip
        ${CMAKE_BINARY_DIR}/dep/Amd.Ags.Sdk.5.4.2.zip
        SHOW_PROGRESS
    )

    file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/dep/amd_ags_sdk/)

    execute_process(
        COMMAND ${CMAKE_COMMAND} -E tar -xzf ${CMAKE_BINARY_DIR}/dep/Amd.Ags.Sdk.5.4.2.zip
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/dep/amd_ags_sdk/
    )
endif()
set(AMD_AGS_SDK ${CMAKE_BINARY_DIR}/dep/amd_ags_sdk/)

# NVIDIA DLSS SDK
set(NVIDIA_DLSS_SDK_FILE ${CMAKE_BINARY_DIR}/dep/Nvidia.Dlss.Sdk.Libraries.Windows.3.7.0.zip)
if(NOT EXISTS ${NVIDIA_DLSS_SDK_FILE})
    file(
        DOWNLOAD
        https://github.com/ixray-team/ixray-packages/releases/download/d2024.5.3/Nvidia.Dlss.Sdk.Libraries.Windows.3.7.0.zip
        ${CMAKE_BINARY_DIR}/dep/Nvidia.Dlss.Sdk.Libraries.Windows.3.7.0.zip
        SHOW_PROGRESS
    )
    file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/dep/nvngx_dlss_sdk/)
    execute_process(
        COMMAND ${CMAKE_COMMAND} -E tar -xzf ${CMAKE_BINARY_DIR}/dep/Nvidia.Dlss.Sdk.Libraries.Windows.3.7.0.zip
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/dep/nvngx_dlss_sdk/
    )
endif()
set(NVIDIA_DLSS_SDK ${CMAKE_BINARY_DIR}/dep/nvngx_dlss_sdk/)
