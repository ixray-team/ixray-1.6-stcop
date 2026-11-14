add_custom_target(sdk_mcp
    COMMAND "${CMAKE_COMMAND}" -E make_directory "${IXRAY_OUTPUT}/mcp"
    COMMAND "${CMAKE_COMMAND}" -E copy_if_different
        "${CMAKE_CURRENT_SOURCE_DIR}/server.py"
        "${CMAKE_CURRENT_SOURCE_DIR}/requirements.txt"
        "${CMAKE_CURRENT_SOURCE_DIR}/README.md"
        "${IXRAY_OUTPUT}/mcp"
    VERBATIM
)
set_target_properties(sdk_mcp PROPERTIES FOLDER "Editors/")
