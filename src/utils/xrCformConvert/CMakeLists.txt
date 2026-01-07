project(xrCformConvert)
set(UTILS_CFORMCONV_SRC_FILES)

# Files import
file(GLOB_RECURSE UTILS_CFORMCONV_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
    "*.hpp"
)
file(GLOB_RECURSE UTILS_CFORMCONV_IMGUI_FILES "imgui*.*")
file(GLOB_RECURSE UTILS_CFORMCONV_RES_FILES
    "*.ico"
    "*.rc"
    "*.bmp"
)
file(GLOB_RECURSE UTILS_CFORMCONV_SOURCE_LOG_FILES CONFIGURE_DEPENDS
    "../xrForms/cl_log.cpp"
    "../xrForms/cl_log.h"
    "../xrForms/imgui_impl_sdlrenderer3.cpp"
    "../xrForms/imgui_impl_sdlrenderer3.h"
    "../../Editors/xrEUI/imgui_impl_sdl3.h" # I don't understand why I don't need to explicitly include this in xrLevelBuilder
    "../../Editors/xrEUI/imgui_impl_sdl3.cpp"
)

# Source groups
source_group("core" FILES ${UTILS_CFORMCONV_SOURCE_ALL_FILES})
source_group("core/imgui" FILES ${UTILS_CFORMCONV_IMGUI_FILES})
source_group("res" FILES ${UTILS_CFORMCONV_RES_FILES})
source_group("extra" FILES ${UTILS_CFORMCONV_SOURCE_LOG_FILES})

# Apply list
list(APPEND UTILS_CFORMCONV_SRC_FILES ${UTILS_CFORMCONV_SOURCE_ALL_FILES})
list(APPEND UTILS_CFORMCONV_SRC_FILES ${UTILS_CFORMCONV_RES_FILES})
list(APPEND UTILS_CFORMCONV_SRC_FILES ${UTILS_CFORMCONV_SOURCE_LOG_FILES})

# Remove unused files
#list(FILTER UTILS_CFORMCONV_SRC_FILES EXCLUDE REGEX "nvdxt.cpp$")

# xrLevelBuilder project
add_executable(xrCformConvert WIN32 ${UTILS_CFORMCONV_SRC_FILES})
target_include_directories(xrCformConvert PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_link_directories(xrCformConvert PUBLIC "${IXRAY_SDK_LIB}/")
set_target_properties(xrCformConvert PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
target_include_directories(xrCformConvert PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../../xrEngine/")
target_include_directories(xrCformConvert PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../../xrCore/")

# Project defines
target_compile_definitions(xrCformConvert PRIVATE UTILS_FORMS_EXPORTS)
target_compile_definitions(xrCformConvert PRIVATE ECORE_API=)
target_compile_definitions(xrCformConvert PRIVATE ENGINE_API=)
target_compile_definitions(xrCformConvert PRIVATE XRLC_LIGHT_API=)

# Linker list
target_link_libraries(xrCformConvert PUBLIC xrCore)
target_link_libraries(xrCformConvert PUBLIC MagicFM)
target_link_libraries(xrCformConvert PUBLIC imgui)
target_link_libraries(xrCformConvert PUBLIC xrEngineCore)

if (WIN32)
    target_link_libraries(xrCformConvert PUBLIC WindowsSDK::WinMM)
endif()
#target_link_libraries(xrCformConvert PUBLIC "comctl32.lib")