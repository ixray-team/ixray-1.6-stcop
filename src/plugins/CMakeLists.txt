set(IXRAY_FOLDER_PLUGIN FOLDER "Plugins")

if(CMAKE_SIZEOF_VOID_P EQUAL 8)
    add_subdirectory("PCore")
    add_subdirectory("Maya")
    #add_subdirectory("Max")
    
    set_target_properties(PCore PROPERTIES ${IXRAY_FOLDER_PLUGIN})
    set_target_properties(MayaTools_2024 PROPERTIES ${IXRAY_FOLDER_PLUGIN})
    set_target_properties(MayaTools_2025 PROPERTIES ${IXRAY_FOLDER_PLUGIN})
    set_target_properties(MayaTools_2026 PROPERTIES ${IXRAY_FOLDER_PLUGIN})
    #set_target_properties(MaxTools PROPERTIES ${IXRAY_FOLDER_PLUGIN})
endif()
