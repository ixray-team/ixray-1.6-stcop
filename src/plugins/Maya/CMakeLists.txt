project(MayaTools)

# Files import
file(GLOB_RECURSE MayaTools_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.cxx"
    "*.h"
)

set(MayaTools_SRC_FILES)

source_group("src" FILES ${MayaTools_SOURCE_ALL_FILES})

# Apply list
list(APPEND MayaTools_SRC_FILES ${MayaTools_SOURCE_ALL_FILES})

# Remove unused files
#list(FILTER MayaTools_SRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# MayaTools project
add_library(MayaTools SHARED ${MayaTools_SRC_FILES})
target_include_directories(MayaTools PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(MayaTools PRIVATE "${IXR_MAYA_SDK}")
target_link_directories(MayaTools PRIVATE "${IXR_MAYA_SDK}/lib")
set_target_properties(MayaTools PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

set_target_properties(
    MayaTools
    PROPERTIES
        OUTPUT_NAME "ixray_maya_tools"
        SUFFIX ".mll"
)

target_link_libraries(MayaTools PUBLIC PCore)
target_link_libraries(MayaTools PUBLIC OpenMaya)
target_link_libraries(MayaTools PUBLIC OpenMayaAnim)
target_link_libraries(MayaTools PUBLIC Foundation)