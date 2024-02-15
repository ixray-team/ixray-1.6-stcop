project(MagicFM)
set(MAGIC_FM_FILES)

# Files import
file(GLOB APPROXIMATION2D_FILES
    "Approximation2D/*.cpp"
    "Approximation2D/*.h"
    "Approximation2D/*.pkg"
)
file(GLOB APPROXIMATION3D_FILES
    "Approximation3D/*.cpp"
    "Approximation3D/*.h"
    "Approximation3D/*.pkg"
)
file(GLOB CONTAINMENT2D_FILES
    "Containment2D/*.cpp"
    "Containment2D/*.h"
    "Containment2D/*.inl"
    "Containment2D/*.pkg"
)
file(GLOB CONTAINMENT3D_FILES
    "Containment3D/*.cpp"
    "Containment3D/*.h"
    "Containment3D/*.inl"
    "Containment3D/*.pkg"
)
file(GLOB CORE_FILES
    "Core/*.cpp"
    "Core/*.h"
    "Core/*.inl"
    "Core/*.pkg"
)
file(GLOB CORE2D_FILES
    "Core2D/*.cpp"
    "Core2D/*.h"
    "Core2D/*.inl"
    "Core2D/*.pkg"
)
file(GLOB CORE3D_FILES
    "Core3D/*.cpp"
    "Core3D/*.h"
    "Core3D/*.inl"
    "Core3D/*.pkg"
)
file(GLOB CURVE2D_FILES
    "Curve2D/*.cpp"
    "Curve2D/*.h"
    "Curve2D/*.inl"
    "Curve2D/*.pkg"
)
file(GLOB CURVE3D_FILES
    "Curve3D/*.cpp"
    "Curve3D/*.h"
    "Curve3D/*.inl"
    "Curve3D/*.pkg"
)
file(GLOB DISTANCE2D_FILES
    "Distance2D/*.cpp"
    "Distance2D/*.h"
    "Distance2D/*.inl"
    "Distance2D/*.pkg"
)
file(GLOB DISTANCE3D_FILES
    "Distance3D/*.cpp"
    "Distance3D/*.h"
    "Distance3D/*.inl"
    "Distance3D/*.pkg"
)
file(GLOB IMAGEANALYSIS_FILES
    "ImageAnalysis/*.cpp"
    "ImageAnalysis/*.h"
    "ImageAnalysis/*.inl"
    "ImageAnalysis/*.pkg"
)
file(GLOB INTERPOLATION1D_FILES
    "Interpolation1D/*.cpp"
    "Interpolation1D/*.h"
    "Interpolation1D/*.inl"
    "Interpolation1D/*.pkg"
)
file(GLOB INTERPOLATION2D_FILES
    "Interpolation2D/*.cpp"
    "Interpolation2D/*.h"
    "Interpolation2D/*.inl"
    "Interpolation2D/*.pkg"
)
file(GLOB INTERPOLATION3D_FILES
    "Interpolation3D/*.cpp"
    "Interpolation3D/*.h"
    "Interpolation3D/*.inl"
    "Interpolation3D/*.pkg"
)
file(GLOB INTERSECTION2D_FILES
    "Intersection2D/*.cpp"
    "Intersection2D/*.h"
    "Intersection2D/*.inl"
    "Intersection2D/*.pkg"
)
file(GLOB INTERSECTION3D_FILES
    "Intersection3D/*.cpp"
    "Intersection3D/*.h"
    "Intersection3D/*.inl"
    "Intersection3D/*.pkg"
)
file(GLOB NUMERICS_FILES
    "Numerics/*.cpp"
    "Numerics/*.h"
    "Numerics/*.inl"
    "Numerics/*.pkg"
)
file(GLOB PROJECTION_FILES
    "Projection/*.cpp"
    "Projection/*.h"
    "Projection/*.inl"
    "Projection/*.pkg"
)
file(GLOB SURFACE_FILES
    "Surface/*.cpp"
    "Surface/*.h"
    "Surface/*.inl"
    "Surface/*.pkg"
)
file(GLOB TESSELLATION2D_FILES
    "Tessellation2D/*.cpp"
    "Tessellation2D/*.h"
    "Tessellation2D/*.inl"
    "Tessellation2D/*.pkg"
)
file(GLOB TESSELLATION3D_FILES
    "Tessellation3D/*.cpp"
    "Tessellation3D/*.h"
    "Tessellation3D/*.inl"
    "Tessellation3D/*.pkg"
)
file(GLOB MAGIC_FM_ROOT_FILES
    "MagicFM.pkg"
    "MagicFMLibType.h"
    "MgcRTLib.h"
    "stdafx.h"
)

# Define filters
source_group("Approximation2D" FILES ${APPROXIMATION2D_FILES})
source_group("Approximation3D" FILES ${APPROXIMATION3D_FILES})
source_group("Containment2D" FILES ${CONTAINMENT2D_FILES})
source_group("Containment3D" FILES ${CONTAINMENT3D_FILES})
source_group("Core" FILES ${CORE_FILES})
source_group("Core2D" FILES ${CORE2D_FILES})
source_group("Core3D" FILES ${CORE3D_FILES})
source_group("Curve2D" FILES ${CURVE2D_FILES})
source_group("Curve3D" FILES ${CURVE3D_FILES})
source_group("Distance2D" FILES ${DISTANCE2D_FILES})
source_group("Distance3D" FILES ${DISTANCE3D_FILES})
source_group("ImageAnalysis" FILES ${IMAGEANALYSIS_FILES})
source_group("Interpolation1D" FILES ${INTERPOLATION1D_FILES})
source_group("Interpolation2D" FILES ${INTERPOLATION2D_FILES})
source_group("Interpolation3D" FILES ${INTERPOLATION3D_FILES})
source_group("Intersection2D" FILES ${INTERSECTION2D_FILES})
source_group("Intersection3D" FILES ${INTERSECTION3D_FILES})
source_group("Numerics" FILES ${NUMERICS_FILES})
source_group("Projection" FILES ${PROJECTION_FILES})
source_group("Surface" FILES ${SURFACE_FILES})
source_group("Tessellation2D" FILES ${TESSELLATION2D_FILES})
source_group("Tessellation3D" FILES ${TESSELLATION3D_FILES})
source_group("" FILES ${MAGIC_FM_ROOT_FILES})

# Apply list
list(APPEND MAGIC_FM_FILES ${APPROXIMATION2D_FILES})
list(APPEND MAGIC_FM_FILES ${APPROXIMATION3D_FILES})
list(APPEND MAGIC_FM_FILES ${CONTAINMENT2D_FILES})
list(APPEND MAGIC_FM_FILES ${CONTAINMENT3D_FILES})
list(APPEND MAGIC_FM_FILES ${CORE_FILES})
list(APPEND MAGIC_FM_FILES ${CORE2D_FILES})
list(APPEND MAGIC_FM_FILES ${CORE3D_FILES})
list(APPEND MAGIC_FM_FILES ${CURVE2D_FILES})
list(APPEND MAGIC_FM_FILES ${CURVE3D_FILES})
list(APPEND MAGIC_FM_FILES ${DISTANCE2D_FILES})
list(APPEND MAGIC_FM_FILES ${DISTANCE3D_FILES})
list(APPEND MAGIC_FM_FILES ${IMAGEANALYSIS_FILES})
list(APPEND MAGIC_FM_FILES ${INTERPOLATION1D_FILES})
list(APPEND MAGIC_FM_FILES ${INTERPOLATION2D_FILES})
list(APPEND MAGIC_FM_FILES ${INTERPOLATION3D_FILES})
list(APPEND MAGIC_FM_FILES ${INTERSECTION2D_FILES})
list(APPEND MAGIC_FM_FILES ${INTERSECTION3D_FILES})
list(APPEND MAGIC_FM_FILES ${NUMERICS_FILES})
list(APPEND MAGIC_FM_FILES ${PROJECTION_FILES})
list(APPEND MAGIC_FM_FILES ${SURFACE_FILES})
list(APPEND MAGIC_FM_FILES ${TESSELLATION2D_FILES})
list(APPEND MAGIC_FM_FILES ${TESSELLATION3D_FILES})
list(APPEND MAGIC_FM_FILES ${MAGIC_FM_ROOT_FILES})

# MagicFM project
add_library(MagicFM SHARED ${MAGIC_FM_FILES})
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Approximation2D/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Approximation3D/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Containment2D/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Containment3D/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Core/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Core2D/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Core3D/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Curve2D/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Curve3D/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Distance2D/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Distance3D/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/ImageAnalysis/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Interpolation1D/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Interpolation2D/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Interpolation3D/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Intersection2D/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Intersection3D/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Numerics/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Projection/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Surface/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Tessellation2D/")
target_include_directories(MagicFM PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/Tessellation3D/")

target_precompile_headers(MagicFM PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(MagicFM PRIVATE _WINDOWS)
target_compile_definitions(MagicFM PRIVATE _USRDLL)
target_compile_definitions(MagicFM PRIVATE MAGICFMDLL_EXPORTS)
target_compile_definitions(MagicFM PRIVATE STRICT)

# Linker list
# target_link_libraries(MagicFM PRIVATE "dbghelp.lib")
# target_link_options(MagicFM PUBLIC "$<$<CONFIG:DEBUG>:/SAFESEH:NO>")

add_compile_options(/fp:fast)
target_compile_definitions(MagicFM PRIVATE "$<$<CONFIG:Debug>:DEBUG>")
