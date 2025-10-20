project(xrLevelBuilder)
set(UTILS_FORMS_SRC_FILES)

# Files import
file(GLOB_RECURSE UTILS_FORMS_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
    "*.hpp"
)
file(GLOB_RECURSE UTILS_FORMS_IMGUI_FILES "imgui*.*")
file(GLOB_RECURSE UTILS_FORMS_RES_FILES
    "*.ico"
    "*.rc"
    "*.bmp"
)

# Source groups
source_group("core" FILES ${UTILS_FORMS_SOURCE_ALL_FILES})
source_group("core/imgui" FILES ${UTILS_FORMS_IMGUI_FILES})
source_group("res" FILES ${UTILS_FORMS_RES_FILES})

# Apply list
list(APPEND UTILS_FORMS_SRC_FILES ${UTILS_FORMS_SOURCE_ALL_FILES})
list(APPEND UTILS_FORMS_SRC_FILES ${UTILS_FORMS_RES_FILES})

# Remove unused files
#list(FILTER UTILS_FORMS_SRC_FILES EXCLUDE REGEX "nvdxt.cpp$")

# xrLevelBuilder project
add_executable(xrLevelBuilder WIN32 ${UTILS_FORMS_SRC_FILES})
target_include_directories(xrLevelBuilder PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_link_directories(xrLevelBuilder PUBLIC "${IXRAY_SDK_LIB}/")
set_target_properties(xrLevelBuilder PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
target_include_directories(xrLevelBuilder PRIVATE "../../xrServerEntities/")

# Project defines
target_compile_definitions(xrLevelBuilder PRIVATE UTILS_FORMS_EXPORTS)
target_compile_definitions(xrLevelBuilder PRIVATE ECORE_API=)
target_compile_definitions(xrLevelBuilder PRIVATE ENGINE_API=)
target_compile_definitions(xrLevelBuilder PRIVATE XRLC_LIGHT_API=)
target_compile_definitions(xrLevelBuilder PRIVATE AI_COMPILER)

# Linker list
target_link_libraries(xrLevelBuilder PUBLIC xrCore)
target_link_libraries(xrLevelBuilder PUBLIC xrAI)
target_link_libraries(xrLevelBuilder PUBLIC xrLC)
target_link_libraries(xrLevelBuilder PUBLIC MagicFM)
target_link_libraries(xrLevelBuilder PUBLIC imgui)
target_link_libraries(xrLevelBuilder PUBLIC xrAbstractions)
target_link_libraries(xrLevelBuilder PUBLIC "Winmm.lib")
target_link_libraries(xrLevelBuilder PUBLIC "comctl32.lib")