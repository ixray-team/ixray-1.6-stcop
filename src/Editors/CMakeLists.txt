set(IXRAY_FOLDER_EDITORS FOLDER "Editors/")
set(IXRAY_FOLDER_EDITOR_CORE FOLDER "Editors/Core")
set(IXRAY_FOLDER_EDITOR_EXCORE FOLDER "Editors/External")

# Core libs 
if (IXRAY_EDITORS OR IXRAY_UTILS)
    add_subdirectory("xrEUI")
    add_subdirectory("xrEProps")
endif()

if (IXRAY_EDITORS)
    add_subdirectory("xrECore")

    # Main Editors
    add_subdirectory("ParticleEditor")
    add_subdirectory("ActorEditor")
    add_subdirectory("ShaderEditor")
    add_subdirectory("LevelEditor")
endif()

# Groups
if (IXRAY_EDITORS OR IXRAY_UTILS)
    set_target_properties(xrEUI PROPERTIES ${IXRAY_FOLDER_EDITOR_CORE})
    set_target_properties(xrEProps PROPERTIES ${IXRAY_FOLDER_EDITOR_CORE})
endif()

if (IXRAY_EDITORS)
    set_target_properties(xrECore PROPERTIES ${IXRAY_FOLDER_EDITOR_CORE})

    set_target_properties(ParticlesEditor PROPERTIES ${IXRAY_FOLDER_EDITORS})
    set_target_properties(ActorEditor PROPERTIES ${IXRAY_FOLDER_EDITORS})
    set_target_properties(ShaderEditor PROPERTIES ${IXRAY_FOLDER_EDITORS})
    set_target_properties(LevelEditor PROPERTIES ${IXRAY_FOLDER_EDITORS})

    if (NOT IXRAY_UTILS)
        set_target_properties(xrSE_Factory PROPERTIES ${IXRAY_FOLDER_EDITOR_EXCORE})
        set_target_properties(ETools PROPERTIES ${IXRAY_FOLDER_EDITOR_EXCORE})
        set_target_properties(xrDXT PROPERTIES ${IXRAY_FOLDER_EDITOR_EXCORE})
    endif()
endif()
