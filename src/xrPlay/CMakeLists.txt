project(xrEngine)
set(XPLAY_SRC_FILES)

# Files import
file(GLOB_RECURSE XPLAY_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.*"
)
file(GLOB_RECURSE XPLAY_SOURCE_CPP_FILES
    "*.cpp"
    "*.h"
)
file(GLOB_RECURSE XPLAY_SOURCE_RES_FILES
    "*.ico"
    "*.rc"
    "*.bmp"
)
file(GLOB_RECURSE XPLAY_SOURCE_UI_FILES
    "UI*.cpp"
    "UI*.h"
)

# File groups
source_group("Source Files" FILES ${XPLAY_SOURCE_CPP_FILES})
source_group("Resources" FILES ${XPLAY_SOURCE_RES_FILES})
source_group("UI" FILES ${XPLAY_SOURCE_UI_FILES})

# Apply list
list(APPEND XPLAY_SRC_FILES ${XPLAY_SOURCE_ALL_FILES})

# xrEngine project
add_executable(xrEngine WIN32 ${XPLAY_SOURCE_ALL_FILES})
target_include_directories(xrEngine PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrEngine PRIVATE "${IXRAY_SDK_INC}")
set_target_properties(xrEngine PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

# Project defines
target_compile_definitions(xrEngine PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrEngine PUBLIC xrAbstractions)
target_link_libraries(xrEngine PUBLIC xrScripts)

if (WIN32)
    add_dependencies(xrEngine xrGame xrRender_R4)
    if(IXRAY_USE_R1)
        add_dependencies(xrEngine xrRender_R1)
    endif()

    if(IXRAY_USE_R2)
        add_dependencies(xrEngine xrRender_R2)
    endif()
endif()

if(IXRAY_MP)
    add_dependencies(xrEngine xrGameSpy)
endif()

# AMD AGS SDK
target_include_directories(xrEngine PUBLIC ${CMAKE_BINARY_DIR}/dep/amd_ags_sdk/inc/)

add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_BINARY_DIR}/dep/amd_ags_sdk/lib/amd_ags_${NUGET_PACKAGE_PLATFORM}.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)