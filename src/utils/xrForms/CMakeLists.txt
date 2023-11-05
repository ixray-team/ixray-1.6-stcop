project(xrForms)
set(UTILS_FORMS_SRC_FILES)

# Files import
file(GLOB_RECURSE UTILS_FORMS_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
    "*.hpp"
)

file(GLOB_RECURSE UTILS_FORMS_RES_FILES
    "*.ico"
    "*.rc"
    "*.bmp"
)

# Source groups
source_group("core" FILES ${UTILS_FORMS_SOURCE_ALL_FILES})
source_group("res" FILES ${UTILS_FORMS_RES_FILES})

# Apply list
list(APPEND UTILS_FORMS_SRC_FILES ${UTILS_FORMS_SOURCE_ALL_FILES})
list(APPEND UTILS_FORMS_SRC_FILES ${UTILS_FORMS_RES_FILES})

# Remove unused files
#list(FILTER UTILS_FORMS_SRC_FILES EXCLUDE REGEX "nvdxt.cpp$")

# xrForms project
add_executable(xrForms WIN32 ${UTILS_FORMS_SRC_FILES})
target_include_directories(xrForms PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_link_directories(xrForms PUBLIC "${IXRAY_SDK_LIB}/")
set_target_properties(xrForms PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
target_include_directories(xrForms PRIVATE "../../xrServerEntities/")

# Project defines
target_compile_definitions(xrForms PRIVATE UTILS_FORMS_EXPORTS)
target_compile_definitions(xrForms PRIVATE _WINDOWS)
target_compile_definitions(xrForms PRIVATE _USRDLL)
target_compile_definitions(xrForms PRIVATE ECORE_API=)
target_compile_definitions(xrForms PRIVATE ENGINE_API=)
target_compile_definitions(xrForms PRIVATE XRLC_LIGHT_API=)
target_compile_definitions(xrForms PRIVATE AI_COMPILER)

add_compile_options(/fp:fast)
target_compile_definitions(xrForms PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrForms PUBLIC xrCore)
target_link_libraries(xrForms PUBLIC xrAI)
target_link_libraries(xrForms PUBLIC xrLC)
target_link_libraries(xrForms PUBLIC "Winmm.lib")
target_link_libraries(xrForms PUBLIC "comctl32.lib")

# sdk bins
add_custom_command(TARGET ${PROJECT_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${IXRAY_SDK_BIN}FreeImage.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${IXRAY_SDK_BIN}MagicFM.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)