set(IXRAY_FOLDER_PLUGIN FOLDER "Plugin")

if(CMAKE_SIZEOF_VOID_P EQUAL 8)
    add_subdirectory("PCore")
    add_subdirectory("Maya")
    set_target_properties(PCore PROPERTIES ${IXRAY_FOLDER_PLUGIN})
    set_target_properties(MayaTools PROPERTIES ${IXRAY_FOLDER_PLUGIN})
endif()
