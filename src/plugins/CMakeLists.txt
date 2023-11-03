set(IXRAY_FOLDER_PLUGIN FOLDER "Plugin")

if(IXRAY_PLUGIN_MAX)
    set(IXR_3DS_MAX_SDK ${CMAKE_BINARY_DIR}/dep/max_2024.zip)

    if(NOT EXISTS ${IXR_3DS_MAX_SDK})
        file(
            DOWNLOAD
            https://github.com/ixray-team/ixray-packages/releases/download/d2023.12.8/Autodesk.3dsMax.Sdk.2024.zip
            ${CMAKE_BINARY_DIR}/dep/max_2024.zip
            SHOW_PROGRESS
        )

        file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/dep/max_sdk/)

        execute_process(
            COMMAND ${CMAKE_COMMAND} -E tar -xzf ${CMAKE_BINARY_DIR}/dep/max_2024.zip
            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/dep/max_sdk/
        )
    endif()

    set(IXR_3DS_MAX_SDK ${CMAKE_BINARY_DIR}/dep/max_sdk/)

    add_subdirectory("Max/Material")
    #add_subdirectory("Max/Export")

    set_target_properties(xrMax_Materials PROPERTIES ${IXRAY_FOLDER_PLUGIN})
    #set_target_properties(xrMax_Export PROPERTIES ${IXRAY_FOLDER_PLUGIN})
endif()

if(IXRAY_PLUGIN_LW)
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

    add_subdirectory("lw/Server")
    set_target_properties(xrLW_Server PROPERTIES ${IXRAY_FOLDER_PLUGIN})
endif()
