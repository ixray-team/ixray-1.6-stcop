set(IXRAY_FOLDER_EDITORS FOLDER "Editors/")
set(IXRAY_FOLDER_EDITOR_CORE FOLDER "Editors/Core")
set(IXRAY_FOLDER_EDITOR_EXCORE FOLDER "Editors/External")

# Core libs 
if (IXRAY_EDITORS OR IXRAY_UTILS)
    add_subdirectory("xrEProps")
endif()

if (IXRAY_EDITORS)
    add_subdirectory("xrECore")

    # Main Editors
    add_subdirectory("LevelEditor")
    if(WIN32)
        option(IXRAY_SDK_MCP "Include the Python MCP server with Level Editor" OFF)
        if(IXRAY_SDK_MCP)
            add_subdirectory("${IXRay_SOURCE_DIR}/util/sdk-mcp" "${CMAKE_CURRENT_BINARY_DIR}/sdk-mcp")
            add_dependencies(LevelEditor sdk_mcp)
        endif()
    endif()
endif()

# Groups
if (IXRAY_EDITORS OR IXRAY_UTILS)
    set_target_properties(xrEProps PROPERTIES ${IXRAY_FOLDER_EDITOR_CORE})
endif()

if (IXRAY_EDITORS)
    set_target_properties(xrECore PROPERTIES ${IXRAY_FOLDER_EDITOR_CORE})

    set_target_properties(LevelEditor PROPERTIES ${IXRAY_FOLDER_EDITORS})

    if (NOT IXRAY_UTILS)
        set_target_properties(xrSE_Factory PROPERTIES ${IXRAY_FOLDER_EDITOR_EXCORE})
        set_target_properties(xrDXT PROPERTIES ${IXRAY_FOLDER_EDITOR_EXCORE})
    endif()
endif()
