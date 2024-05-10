# Solution folders
set(IXRAY_FOLDER_UTILS FOLDER "Utils")

# Compressor
if (IXRAY_USE_COMPRESSOR OR IXRAY_UTILS)
    add_subdirectory("xrCompress")
    set_target_properties(xrCompress PROPERTIES ${IXRAY_FOLDER_UTILS})
endif()

# Engine Utils
if(IXRAY_UTILS)
    add_subdirectory("xrDXT")
    add_subdirectory("xrQSlim")
    add_subdirectory("xrLC_Light")
    add_subdirectory("xrLC")
    add_subdirectory("xrForms")
    add_subdirectory("ETools")
    add_subdirectory("xrSE_Factory")
    add_subdirectory("TextureCloner")
    add_subdirectory("xrAI")
    add_subdirectory("efc")

    if(IXRAY_MP)
        add_subdirectory("mp_balancer")
        add_subdirectory("mp_configs_verifyer")
        add_subdirectory("mp_screenshots_info")
        add_subdirectory("mp_gpprof_server")
    endif()
else() 
    if (IXRAY_EDITORS)
        add_subdirectory("xrSE_Factory")
        add_subdirectory("ETools")
        add_subdirectory("xrDXT")
        add_subdirectory("xrQSlim")
    endif()
endif()

# Setup properties
if(IXRAY_UTILS)
    set_target_properties(xrDXT PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(QSlim PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(xrLC_Light PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(xrLC PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(ETools PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(EFC PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(xrSE_Factory PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(xrAI PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(xrForms PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(TextureCloner PROPERTIES ${IXRAY_FOLDER_UTILS})

    if(IXRAY_MP)
        set_target_properties(mp_balancer PROPERTIES ${IXRAY_FOLDER_UTILS})
        set_target_properties(mp_configs_verifyer PROPERTIES ${IXRAY_FOLDER_UTILS})
        set_target_properties(mp_screenshots_info PROPERTIES ${IXRAY_FOLDER_UTILS})
        set_target_properties(mp_gpprof_server PROPERTIES ${IXRAY_FOLDER_UTILS})
    endif()
endif()

# Set default project
if(IXRAY_COMPRESSOR_ONLY)
    set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT xrCompress)
    set_target_properties(xrEngine xrCDB xrScripts xrSound xrPhysics xrParticles xrGame xrRender_R4 xrAbstractions xrNetServer PROPERTIES EXCLUDE_FROM_ALL 1 EXCLUDE_FROM_DEFAULT_BUILD 1)
    set_target_properties(ODE luabind crypto CxImage PROPERTIES EXCLUDE_FROM_ALL 1 EXCLUDE_FROM_DEFAULT_BUILD 1)

    #set_target_properties(xrEngine PROPERTIES ${IXRAY_FOLDER_ENGINE})

    if (IXRAY_USE_R1)
        set_target_properties(xrRender_R1 PROPERTIES EXCLUDE_FROM_ALL 1 EXCLUDE_FROM_DEFAULT_BUILD 1)
    endif()
    
    if (IXRAY_USE_R2)
        set_target_properties(xrRender_R2 PROPERTIES EXCLUDE_FROM_ALL 1 EXCLUDE_FROM_DEFAULT_BUILD 1)
    endif()

    if(IXRAY_UTILS)
        set_target_properties(QSlim MagicFM xrDXT xrForms xrLC_Light xrLC ETools xrSE_Factory xrAI PROPERTIES EXCLUDE_FROM_ALL 1 EXCLUDE_FROM_DEFAULT_BUILD 1)

        if (IXRAY_MP)
            set_target_properties(mp_configs_verifyer mp_balancer mp_screenshots_info mp_gpprof_server PROPERTIES EXCLUDE_FROM_ALL 1 EXCLUDE_FROM_DEFAULT_BUILD 1)
        endif()
    endif()
endif()