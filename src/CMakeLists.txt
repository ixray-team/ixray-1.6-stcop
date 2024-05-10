# Solution folders
set(IXRAY_FOLDER_ENGINE FOLDER "Engine")
set(IXRAY_FOLDER_PREBUILD FOLDER "Engine/Pre Build")
set(IXRAY_FOLDER_RENDER FOLDER "Engine/Renders")
set(IXRAY_FOLDER_GAME FOLDER "Engine/Game")

# Engine libraries
add_subdirectory("xrCore")
add_subdirectory("xrCDB")
add_subdirectory("xrSound")
add_subdirectory("xrNetServer")
add_subdirectory("xrEngine")
add_subdirectory("xrPhysics")
add_subdirectory("xrParticles")
add_subdirectory("xrScripts")

if (IXRAY_USE_R1 OR IXRAY_MP)
    add_subdirectory("Layers/xrRenderPC_R1")
endif()

if (IXRAY_USE_R2)
    add_subdirectory("Layers/xrRenderPC_R2")
endif()

add_subdirectory("Layers/xrRenderPC_R4")
add_subdirectory("xrGame")

if(IXRAY_MP)
    add_subdirectory("xrServer")
    add_subdirectory("xrGameSpy")
endif()

add_subdirectory("xrPlay")

# Move libraries
set_target_properties(xrCore PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrCDB PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrSound PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrNetServer PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrPhysics PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrParticles PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrAbstractions PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrScripts PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrGame PROPERTIES ${IXRAY_FOLDER_GAME})

if(IXRAY_MP)
    set_target_properties(xrGameSpy PROPERTIES ${IXRAY_FOLDER_GAME})
endif()

if(WIN32)
    if (IXRAY_USE_R1)
        set_target_properties(xrRender_R1 PROPERTIES ${IXRAY_FOLDER_RENDER})
    endif()

    if (IXRAY_USE_R2)
        set_target_properties(xrRender_R2 PROPERTIES ${IXRAY_FOLDER_RENDER})
    endif()

    set_target_properties(xrRender_R4 PROPERTIES ${IXRAY_FOLDER_RENDER})

    # Set default project
    set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT xrEngine)
endif()