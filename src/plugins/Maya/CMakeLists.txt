function(setup_maya_sdk version)
    set(IXR_MAYA_SDK_${version}_ZIP ${DEP_DIR}/Autodesk.Maya.Sdk.${version}.zip)
    set(IXR_MAYA_SDK_${version}_URL https://github.com/ixray-team/ixray-packages/releases/download/d2024.5.3/Autodesk.Maya.Sdk.${version}.zip)
    set(IXR_MAYA_SDK_${version}_DIR ${DEP_DIR}/maya_sdk_${version})
    download_and_extract_sdk(${IXR_MAYA_SDK_${version}_URL} ${IXR_MAYA_SDK_${version}_ZIP} ${IXR_MAYA_SDK_${version}_DIR})
    set(IXR_MAYA_SDK_${version} ${IXR_MAYA_SDK_${version}_DIR}/ PARENT_SCOPE)
endfunction()

foreach(version 2024 2025 2026 2027)
    setup_maya_sdk(${version})
endforeach()

project(MayaTools)

file(GLOB_RECURSE MAYA_TOOLS_ALL_FILES CONFIGURE_DEPENDS "*.cpp" "*.cxx" "*.h")
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}" FILES ${MAYA_TOOLS_ALL_FILES})

function(add_maya_tools_target TARGET_NAME MAYA_VERSION MAYA_SDK_DIR)
    add_library(${TARGET_NAME} SHARED ${MAYA_TOOLS_ALL_FILES})

    target_include_directories(${TARGET_NAME} PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}" "${MAYA_SDK_DIR}")
    target_link_directories(${TARGET_NAME} PRIVATE "${MAYA_SDK_DIR}/lib")

    target_link_libraries(${TARGET_NAME} PUBLIC
        PCore
        OpenMaya
        OpenMayaAnim
        Foundation
    )

    set_target_properties(${TARGET_NAME} PROPERTIES
        DISABLE_PRECOMPILE_HEADERS ON
        OUTPUT_NAME "ixray_maya_tools_${MAYA_VERSION}"
        SUFFIX ".mll"
    )

    target_compile_definitions(${TARGET_NAME} PRIVATE IXR_MAYA_VERSION=${MAYA_VERSION})
endfunction()

add_maya_tools_target(MayaTools_2024 2024 "${IXR_MAYA_SDK_2024}")
add_maya_tools_target(MayaTools_2025 2025 "${IXR_MAYA_SDK_2025}")
add_maya_tools_target(MayaTools_2026 2026 "${IXR_MAYA_SDK_2026}")
add_maya_tools_target(MayaTools_2027 2027 "${IXR_MAYA_SDK_2027}")