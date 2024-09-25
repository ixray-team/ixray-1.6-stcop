# Solution folders
set(IXRAY_FOLDER_3RD FOLDER "3rd Party")

# 3rd libraries
if(NOT IXRAY_COMPRESSOR_ONLY)
    add_subdirectory("luabind")
    add_subdirectory("ode")
    add_subdirectory("crypto")
    add_subdirectory("imgui")
    add_subdirectory("opcode")
    if(IXRAY_MP)
        add_subdirectory("cximage")
    endif()
endif()

if(WIN32)
    add_subdirectory("dxerr")
endif()

if(NOT IXRAY_COMPRESSOR_ONLY)
    if(IXRAY_UTILS OR IXRAY_EDITORS)
        add_subdirectory("MagicSoftware/FreeMagic/Source")
        set_target_properties(MagicFM PROPERTIES ${IXRAY_FOLDER_3RD})
    endif()
endif()

if(DEVIXRAY_ENABLE_OGL_RENDER AND NOT IXRAY_COMPRESSOR_ONLY)
    add_subdirectory("glew")
endif()

# Move 3rd
if(NOT IXRAY_COMPRESSOR_ONLY)
    set_target_properties(luabind PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(ODE PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(crypto PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(imgui PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(opcode-prosectors PROPERTIES ${IXRAY_FOLDER_3RD})

    if(IXRAY_MP)
        set_target_properties(CxImage PROPERTIES ${IXRAY_FOLDER_3RD})
    endif()
endif()

if(WIN32)
    set_target_properties(dxerr PROPERTIES ${IXRAY_FOLDER_3RD})
endif()
