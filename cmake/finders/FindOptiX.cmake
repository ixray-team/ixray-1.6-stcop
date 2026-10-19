# Find the header-only OptiX API, including installed SDKs and optix-dev archives.
file(GLOB OptiXInstalledRoots
    "C:/ProgramData/NVIDIA Corporation/OptiX SDK *"
    "C:/Program Files/NVIDIA GPU Computing Toolkit/OptiX SDK *"
    "C:/Program Files/NVIDIA Corporation/OptiX SDK *"
)
list(SORT OptiXInstalledRoots COMPARE NATURAL ORDER DESCENDING)

set(OptixRootHint "${OptiX_ROOT}")
if (NOT OptixRootHint)
    set(OptixRootHint "$ENV{OptiX_ROOT}")
endif()
if (NOT OptixRootHint)
    set(OptixRootHint "$ENV{OPTIX_ROOT}")
endif()
if (OptixRootHint)
    # An explicit root takes precedence over an include path cached for another SDK.
    unset(OptixRootInclude)
    find_path(OptixRootInclude NAMES optix.h PATHS "${OptixRootHint}" PATH_SUFFIXES include NO_DEFAULT_PATH NO_CACHE)
    set(OptiX_INCLUDE_DIR "${OptixRootInclude}" CACHE PATH "OptiX include directory" FORCE)
endif()

find_path(OptiX_INCLUDE_DIR
    NAMES optix.h
    HINTS ${OptiX_ROOT} ENV OptiX_ROOT ENV OPTIX_ROOT
    PATHS ${OptiXInstalledRoots}
    PATH_SUFFIXES include
)

unset(OptiX_VERSION)
if (EXISTS "${OptiX_INCLUDE_DIR}/optix.h")
    file(STRINGS "${OptiX_INCLUDE_DIR}/optix.h" OptiXVersionLine REGEX "^#define OPTIX_VERSION [0-9]+")
    if (OptiXVersionLine MATCHES "OPTIX_VERSION +([0-9]+)")
        math(EXPR OptiXMajor "${CMAKE_MATCH_1} / 10000")
        math(EXPR OptiXMinor "(${CMAKE_MATCH_1} % 10000) / 100")
        math(EXPR OptiXPatch "${CMAKE_MATCH_1} % 100")
        set(OptiX_VERSION "${OptiXMajor}.${OptiXMinor}.${OptiXPatch}")
    endif()
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(OptiX REQUIRED_VARS OptiX_INCLUDE_DIR OptiX_VERSION VERSION_VAR OptiX_VERSION)
set(OptiX_INCLUDE_DIRS "${OptiX_INCLUDE_DIR}")
mark_as_advanced(OptiX_INCLUDE_DIR)
