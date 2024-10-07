# Solution folders
set(IXRAY_FOLDER_ENGINE FOLDER "Engine")
set(IXRAY_FOLDER_PREBUILD FOLDER "Engine/Pre Build")
set(IXRAY_FOLDER_RENDER FOLDER "Engine/Renders")
set(IXRAY_FOLDER_GAME FOLDER "Engine/Game")

# Engine libraries
add_subdirectory("xrCore")

if(NOT IXRAY_COMPRESSOR_ONLY)
    add_subdirectory("xrCDB")
    add_subdirectory("xrSound")
    add_subdirectory("xrNetServer")
    add_subdirectory("xrEngine")
    add_subdirectory("xrPhysics")
    add_subdirectory("xrParticles")
    add_subdirectory("xrScripts")
    add_subdirectory("xrUI")
endif()

if(NOT IXRAY_COMPRESSOR_ONLY)
    if(IXRAY_USE_R1 OR IXRAY_MP)
        add_subdirectory("Layers/xrRenderPC_R1")
    endif()

    if(IXRAY_USE_R2)
        add_subdirectory("Layers/xrRenderPC_R2")
    endif()

    if(IXR_TEST_CI)
        add_subdirectory("Layers/xrRenderInterface")
        add_subdirectory("Layers/xrRenderPC_R4")
        add_subdirectory("xrGame")
    endif()
endif()

if(IXRAY_MP AND NOT IXRAY_COMPRESSOR_ONLY AND IXR_TEST_CI)
    add_subdirectory("xrServer")
    add_subdirectory("xrGameSpy")
endif()

if(NOT IXRAY_COMPRESSOR_ONLY AND IXR_TEST_CI)
    add_subdirectory("xrPlay")
endif()

# Move libraries
set_target_properties(xrCore PROPERTIES ${IXRAY_FOLDER_ENGINE})

if(NOT IXRAY_COMPRESSOR_ONLY)
    set_target_properties(xrCDB PROPERTIES ${IXRAY_FOLDER_ENGINE})
    set_target_properties(xrSound PROPERTIES ${IXRAY_FOLDER_ENGINE})
    set_target_properties(xrNetServer PROPERTIES ${IXRAY_FOLDER_ENGINE})
    set_target_properties(xrPhysics PROPERTIES ${IXRAY_FOLDER_ENGINE})
    set_target_properties(xrParticles PROPERTIES ${IXRAY_FOLDER_ENGINE})
    set_target_properties(xrAbstractions PROPERTIES ${IXRAY_FOLDER_ENGINE})
    set_target_properties(xrScripts PROPERTIES ${IXRAY_FOLDER_ENGINE})
    set_target_properties(xrUI PROPERTIES ${IXRAY_FOLDER_ENGINE})

    if(IXR_TEST_CI)
        set_target_properties(xrGame PROPERTIES ${IXRAY_FOLDER_GAME})
    endif()
endif()

if(IXRAY_MP AND NOT IXRAY_COMPRESSOR_ONLY AND IXR_TEST_CI)
    set_target_properties(xrGameSpy PROPERTIES ${IXRAY_FOLDER_GAME})
endif()

if(WIN32)
    if (IXRAY_USE_R1 AND NOT IXRAY_COMPRESSOR_ONLY)
        set_target_properties(xrRender_R1 PROPERTIES ${IXRAY_FOLDER_RENDER})
    endif()

    if (IXRAY_USE_R2 AND NOT IXRAY_COMPRESSOR_ONLY)
        set_target_properties(xrRender_R2 PROPERTIES ${IXRAY_FOLDER_RENDER})
    endif()

    if (NOT IXRAY_COMPRESSOR_ONLY AND IXR_TEST_CI)
        set_target_properties(xrRender_R4 PROPERTIES ${IXRAY_FOLDER_RENDER})

        # Set default project
        set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT xrEngine)
    endif()
endif()
