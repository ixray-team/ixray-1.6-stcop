# Общая функция для скачивания и распаковки SDK
function(download_and_extract_sdk url zip_file out_dir)
    if(NOT EXISTS "${zip_file}")
        message(STATUS "Downloading ${url} ...")
        file(DOWNLOAD
            "${url}"
            "${zip_file}"
            SHOW_PROGRESS
        )

        file(MAKE_DIRECTORY "${out_dir}")

        execute_process(
            COMMAND ${CMAKE_COMMAND} -E tar -xzf "${zip_file}"
            WORKING_DIRECTORY "${out_dir}"
        )
    endif()
endfunction()

# Папка с зависимостями
set(DEP_DIR ${CMAKE_BINARY_DIR}/dep)

# 3DS Max SDK (закомментировано распаковкой, т.к. уже локальный файл)
#set(IXR_3DS_MAX_SDK_ZIP ${DEP_DIR}/max_2024.zip)
#set(IXR_3DS_MAX_SDK_URL "https://github.com/ixray-team/ixray-packages/releases/download/d2023.12.8/Autodesk.3dsMax.Sdk.2024.zip")
#set(IXR_3DS_MAX_SDK_DIR ${DEP_DIR}/max_sdk)
#download_and_extract_sdk(${IXR_3DS_MAX_SDK_URL} ${IXR_3DS_MAX_SDK_ZIP} ${IXR_3DS_MAX_SDK_DIR})
#set(IXR_3DS_MAX_SDK ${DEP_DIR}/max_sdk/)

# Maya SDK
set(IXR_MAYA_SDK_ZIP ${DEP_DIR}/Autodesk.Maya.Sdk.2024.zip)
set(IXR_MAYA_SDK_URL https://github.com/ixray-team/ixray-packages/releases/download/d2024.5.3/Autodesk.Maya.Sdk.2024.zip)
set(IXR_MAYA_SDK_DIR ${DEP_DIR}/maya_sdk)
download_and_extract_sdk(${IXR_MAYA_SDK_URL} ${IXR_MAYA_SDK_ZIP} ${IXR_MAYA_SDK_DIR})
set(IXR_MAYA_SDK ${IXR_MAYA_SDK_DIR}/)

# LightWave SDK (закомментировано)
#set(IXR_LW_SDK_ZIP ${DEP_DIR}/lw_sdk_2020.zip)
#set(IXR_LW_SDK_URL https://github.com/ixray-team/ixray-packages/releases/download/d2023.12.8/LightWave3D.Sdk.2020.zip)
#set(IXR_LW_SDK_DIR ${DEP_DIR}/lw_sdk_2020)
#download_and_extract_sdk(${IXR_LW_SDK_URL} ${IXR_LW_SDK_ZIP} ${IXR_LW_SDK_DIR})
#set(IXR_LW_SDK ${IXR_LW_SDK_DIR}/)

# Discord GameSDK
set(DISCORD_GAME_SDK_ZIP ${DEP_DIR}/discord_gamesdk_3.2.1.zip)
set(DISCORD_GAME_SDK_URL https://github.com/ixray-team/ixray-packages/releases/download/d2023.12.8/Discord.GameSdk.3.2.1.zip)
set(DISCORD_GAME_SDK_DIR ${DEP_DIR}/discord_gamesdk)
download_and_extract_sdk(${DISCORD_GAME_SDK_URL} ${DISCORD_GAME_SDK_ZIP} ${DISCORD_GAME_SDK_DIR})
set(DISCORD_GAME_SDK ${DISCORD_GAME_SDK_DIR}/)

# AMD AGS SDK
set(AMD_AGS_SDK_ZIP ${DEP_DIR}/Amd.Ags.Sdk.5.4.2.zip)
set(AMD_AGS_SDK_URL https://github.com/ixray-team/ixray-packages/releases/download/d2023.12.8/Amd.Ags.Sdk.5.4.2.zip)
set(AMD_AGS_SDK_DIR ${DEP_DIR}/amd_ags_sdk)
download_and_extract_sdk(${AMD_AGS_SDK_URL} ${AMD_AGS_SDK_ZIP} ${AMD_AGS_SDK_DIR})
set(AMD_AGS_SDK ${AMD_AGS_SDK_DIR}/)