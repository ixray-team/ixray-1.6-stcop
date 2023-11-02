set(IXR_3DS_MAX_SDK ${CMAKE_BINARY_DIR}/dep/sdk-3dsdk-3dsmax.zip)

if (IXRAY_PLUGIN_MAX)
    if (NOT EXISTS ${IXR_3DS_MAX_SDK})
        file(
            DOWNLOAD
            https://github.com/ixray-team/ixray-1.6-stcop/releases/download/v1.6.02-2/sdk-3dsdk-3dsmax.zip
            ${CMAKE_BINARY_DIR}/dep/sdk-3dsdk-3dsmax.zip 
            SHOW_PROGRESS
        )

        execute_process(
            COMMAND ${CMAKE_COMMAND} -E tar -xzf ${CMAKE_BINARY_DIR}/dep/sdk-3dsdk-3dsmax.zip 
            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/dep/
        )
    
    endif()

    set(IXR_3DS_MAX_SDK ${CMAKE_BINARY_DIR}/dep/sdk/3d_sdk/3dsmax/ver-6.0/)

    add_subdirectory("Max/Material")
endif()
