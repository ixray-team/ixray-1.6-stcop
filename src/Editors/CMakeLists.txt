# Solution folders
set(IXRAY_FOLDER_EDITORS FOLDER "Editors")
set(IXRAY_FOLDER_EDITORS_COREE FOLDER "Editors/Core")

# Editors projects
if(IXRAY_EDITORS)
	add_subdirectory("EditorRender")
	add_subdirectory("EditorEngineAPI")
	add_subdirectory("EditorProps")
	add_subdirectory("EditorCore")
	add_subdirectory("PostProcessEditor")

	# Setup properties (Core libs)
    set_target_properties(EditorRender		PROPERTIES ${IXRAY_FOLDER_EDITORS_COREE})
    set_target_properties(EditorEngineAPI	PROPERTIES ${IXRAY_FOLDER_EDITORS_COREE})
    set_target_properties(EditorProps		PROPERTIES ${IXRAY_FOLDER_EDITORS_COREE})
	
	# Setup properties (Editors)
    set_target_properties(PPEditor PROPERTIES ${IXRAY_FOLDER_EDITORS})
endif()