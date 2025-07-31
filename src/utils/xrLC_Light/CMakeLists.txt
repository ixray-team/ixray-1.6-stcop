project(xrLC_Light)
set(LCL_SRC_FILES)

# Files import
file(GLOB_RECURSE LCL_SOURCE_ALL_FILES CONFIGURE_DEPENDS
#    "${IXRAY_SDK_INC}/hxgrid/Interface/hxGridInterface.cpp"
    
    "*.cpp"
    "*.h"
)

file(GLOB_RECURSE LCL_SOURCE_MUMODELS_FILES
    "xrMU_Model.*"
    "mu_model_*"
    "xrMU_Model_*"
    "fitter.*"
)

file(GLOB_RECURSE LCL_SOURCE_IMAGE_FILES
    "xrImage_Filter.*"
    "xrLoadSurface.*"
    "xrUVpoint.*"
    "xrDXTC.*"
    "tga.*"
    "../../xrEngine/xrLoadSurface.cpp"
)



file(GLOB_RECURSE LCL_SOURCE_LIGHT_FILES
    "xrLC_GlobalData.*"
    "xrLC_Light.*"
    "xrLightVertex.*"
)

file(GLOB_RECURSE LCL_SOURCE_LIGHT_LIGHTMAP_FILES
      "Lightmap*"   
      "lm_layer*"
)
file(GLOB_RECURSE LCL_SOURCE_LIGHT_IMPLICIT_FILES
      "xrLight_*"   
)

file(GLOB_RECURSE LCL_SOURCE_LIGHT_DEFLECTORS_FILES
    "xrDeflector*"
    "xrDeflectoL_Direct.cpp"
)
 
file(GLOB_RECURSE LCL_SOURCE_BASE_FILES
    "base_*"
    "b_build_texture.*"
    "ETextureParams.*"
    "R_light.*"
    "light_point.h"
)

file(GLOB_RECURSE LCL_SOURCE_DOLIGHT_FILES
    "detail_slot_calculate.*"
    "global_calculation_data.*"
    "global_slots_data.*"
    "recalculation.*"
    "LightThread.*"
    "DetailFormat.*"
    "compiler.*"
)

file(GLOB_RECURSE LCL_SOURCE_NETWORK_FILES
     "lightstab_interface.*"
     "light_execute.*"
)

file(GLOB_RECURSE LCL_SOURCE_MESHDATA_FILES
     "face_smoth_flags.*"
     "tcf.*"
     "uv_tri.*"
     "xrFace*"
     "MeshStaic*"
     "MeshStructure*"
     "itterate_adjacents*"
     "calculate_normals.*"
     "vector_clear.*"
)

file(GLOB_RECURSE XLC_SOURCE_EMBREE_FILES
    "Embree*"
)

# Source groups
source_group("kernel" FILES ${LCL_SOURCE_ALL_FILES})
source_group("mu-models" FILES ${LCL_SOURCE_MUMODELS_FILES})
source_group("light_files" FILES ${LCL_SOURCE_LIGHT_FILES})


source_group("light_files\\EmbreeRayTracing" FILES ${XLC_SOURCE_EMBREE_FILES})
source_group("light_files\\deflectors" FILES ${LCL_SOURCE_LIGHT_DEFLECTORS_FILES})
source_group("light_files\\implicit" FILES ${LCL_SOURCE_LIGHT_IMPLICIT_FILES})
source_group("light_files\\lightmap" FILES ${LCL_SOURCE_LIGHT_LIGHTMAP_FILES})


source_group("base_files" FILES ${LCL_SOURCE_BASE_FILES})
source_group("dolight_files" FILES ${LCL_SOURCE_DOLIGHT_FILES})
source_group("network_files" FILES ${LCL_SOURCE_NETWORK_FILES})
source_group("mesh_files" FILES ${LCL_SOURCE_MESHDATA_FILES})
source_group("image_files" FILES ${LCL_SOURCE_IMAGE_FILES})


# Apply list
list(APPEND LCL_SRC_FILES ${LCL_SOURCE_ALL_FILES})
list(APPEND LCL_SRC_FILES ${LCL_SOURCE_MUMODELS_FILES})
list(APPEND LCL_SRC_FILES ${LCL_SOURCE_LIGHT_FILES})
list(APPEND LCL_SRC_FILES ${LCL_SOURCE_BASE_FILES})
list(APPEND LCL_SRC_FILES ${LCL_SOURCE_DOLIGHT_FILES})
list(APPEND LCL_SRC_FILES ${LCL_SOURCE_MESHDATA_FILES})
list(APPEND LCL_SRC_FILES ${LCL_SOURCE_IMAGE_FILES})

# Remove unused files
list(FILTER LCL_SRC_FILES EXCLUDE REGEX "ptmalloc3/*")
list(FILTER LCL_SRC_FILES EXCLUDE REGEX "file_compress.*")
list(FILTER LCL_SRC_FILES EXCLUDE REGEX "xrIsect.h")
list(FILTER LCL_SRC_FILES EXCLUDE REGEX "hxGridInterface.cpp")
list(FILTER LCL_SRC_FILES EXCLUDE REGEX "_d3d_extensions.h")

# xrLC_Light project
add_library(xrLC_Light ${LCL_SRC_FILES})
target_include_directories(xrLC_Light PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrLC_Light PUBLIC "${IXRAY_SDK_INC}")
target_link_directories(xrLC_Light PUBLIC "${IXRAY_SDK_LIB}")

target_precompile_headers(xrLC_Light PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(xrLC_Light PRIVATE _USRDLL)

# Linker list
target_link_libraries(xrLC_Light PUBLIC xrCore)
target_link_libraries(xrLC_Light PUBLIC xrDXT)
target_link_libraries(xrLC_Light PUBLIC xrCDB)
target_link_libraries(xrLC_Light PUBLIC imgui)

## FreeImage
set(FREEIMAGE ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.FreeImage.WinMerge.2023.8.19-open)
target_include_directories(xrLC_Light PUBLIC "${FREEIMAGE}/native/include/")

add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${FREEIMAGE}/native/bin/${NUGET_PACKAGE_PLATFORM}/Release/FreeImage.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${IXRAY_SDK_BIN}/embree4.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${IXRAY_SDK_BIN}/meshoptimizer.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Zlib.1.2.13.1/native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/zlib.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)
target_link_libraries(xrLC_Light PUBLIC ${FREEIMAGE}/native/lib/${NUGET_PACKAGE_PLATFORM}/Release/FreeImage.lib)

target_include_directories(xrLC_Light PUBLIC "${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/include/")
target_include_directories(xrLC_Light PUBLIC "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_2019.2024.6.5.1/include/")

# Zlib
target_include_directories(xrLC_Light PUBLIC "${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Zlib.1.2.13.1/native/include/")
target_link_libraries(xrLC_Light PUBLIC ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Zlib.1.2.13.1/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/zlib.lib)
