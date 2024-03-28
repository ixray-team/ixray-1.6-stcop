# Solution folders
set(IXRAY_FOLDER_3RD FOLDER "3rd Party")

# 3rd libraries
add_subdirectory("luabind")
add_subdirectory("ode")
add_subdirectory("crypto")
add_subdirectory("cximage")
add_subdirectory("imgui")
add_subdirectory("lzo")

if (DEVIXRAY_ENABLE_OGL_RENDER)
    add_subdirectory("glew")
endif()

# Move 3rd
set_target_properties(luabind PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(ODE PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(crypto PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(CxImage PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(imgui PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(lzo_static_lib PROPERTIES ${IXRAY_FOLDER_3RD})

if (DEVIXRAY_ENABLE_OGL_RENDER)
    set_target_properties(glew PROPERTIES ${IXRAY_FOLDER_3RD})
endif()

# Plarform libs
if (WIN32)
    add_subdirectory("dxerr")
    set_target_properties(dxerr PROPERTIES ${IXRAY_FOLDER_3RD})
endif()