set(IXRAY_FOLDER_EDITORS FOLDER "Editors/")
set(IXRAY_FOLDER_EDITOR_CORE FOLDER "Editors/Core")

# Core libs 
add_subdirectory("xrEUI")
add_subdirectory("xrEProps")
add_subdirectory("xrECore")

# Main Editors
add_subdirectory("ParticleEditor")
add_subdirectory("ActorEditor")
add_subdirectory("ShaderEditor")

# Groups
set_target_properties(xrEUI PROPERTIES ${IXRAY_FOLDER_EDITOR_CORE})
set_target_properties(xrEProps PROPERTIES ${IXRAY_FOLDER_EDITOR_CORE})
set_target_properties(xrECore PROPERTIES ${IXRAY_FOLDER_EDITOR_CORE})

set_target_properties(ParticlesEditor PROPERTIES ${IXRAY_FOLDER_EDITORS})
set_target_properties(ActorEditor PROPERTIES ${IXRAY_FOLDER_EDITORS})
set_target_properties(ShaderEditor PROPERTIES ${IXRAY_FOLDER_EDITORS})