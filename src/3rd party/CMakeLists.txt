# Solution folders
set(IXRAY_FOLDER_3RD FOLDER "3rd Party")

# 3rd libraries
add_subdirectory("luabind")
add_subdirectory("ode")
add_subdirectory("crypto")
add_subdirectory("cximage")
add_subdirectory("imgui")
add_subdirectory("dxerr")

# Move 3rd
set_target_properties(luabind PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(ODE PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(crypto PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(CxImage PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(imgui PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(dxerr PROPERTIES ${IXRAY_FOLDER_3RD})