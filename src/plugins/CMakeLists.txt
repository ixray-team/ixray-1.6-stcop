set(IXRAY_FOLDER_PLUGIN FOLDER "Plugin")

if(CMAKE_SIZEOF_VOID_P EQUAL 8)
    if(IXRAY_PLUGIN_MAX)
        add_subdirectory("Max/Material")
        add_subdirectory("Max/Export")

        set_target_properties(xrMax_Materials PROPERTIES ${IXRAY_FOLDER_PLUGIN})
        set_target_properties(xrMax_Export PROPERTIES ${IXRAY_FOLDER_PLUGIN})
    endif()

    if(IXRAY_PLUGIN_LW)
        add_subdirectory("lw/Server")
        add_subdirectory("lw/Export/LWO")
        add_subdirectory("lw/Export")
        add_subdirectory("lw/Shader")

        set_target_properties(xrLW_Server PROPERTIES ${IXRAY_FOLDER_PLUGIN})
        set_target_properties(xrLW_Shader PROPERTIES ${IXRAY_FOLDER_PLUGIN})
        set_target_properties(xrLW_Export PROPERTIES ${IXRAY_FOLDER_PLUGIN})
        set_target_properties(lwo PROPERTIES ${IXRAY_FOLDER_3RD})
    endif()
endif()
