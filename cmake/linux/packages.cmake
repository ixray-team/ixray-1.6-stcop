include(FetchContent)

# YamlCPP
FetchContent_Declare(
  yaml-cpp
  GIT_REPOSITORY https://github.com/jbeder/yaml-cpp.git
  GIT_TAG        0.8.0
)

FetchContent_MakeAvailable(yaml-cpp)

# TBB
find_package(TBB QUIET)
if(NOT TBB_FOUND)
  message(STATUS "TBB non trouvé via find_package, utilisation de FetchContent (oneTBB) ...")
  FetchContent_Declare(
    tbb
    GIT_REPOSITORY https://github.com/oneapi-src/oneTBB.git
    GIT_TAG v2021.5.0
  )
  # to limit build: disable tests/examples if known variables
  set(TBB_TEST OFF CACHE BOOL "Disable TBB tests" FORCE)
  set(TBB_STRICT OFF CACHE BOOL "Disable TBB strict warnings" FORCE)
  FetchContent_MakeAvailable(tbb)
  if(TARGET TBB::tbb)
    message(STATUS "TBB récupéré via FetchContent")
  else()
    message(FATAL_ERROR "Echec FetchContent TBB")
  endif()
endif()

# LZO
find_path(LZO_INCLUDE_DIR
	NAMES lzo/lzo1x.h
	PATHS /usr/include /usr/local/include
)
find_library(LZO_LIBRARY
	NAMES lzo2
	PATHS /usr/lib /usr/lib64 /usr/local/lib
)

if (NOT LZO_INCLUDE_DIR)
  message(FATAL_ERROR "Could not find lzo/lzo1x.h")
endif()

# NVTT
include(FetchContent)

FetchContent_Declare(
    nvtt
    GIT_REPOSITORY https://github.com/imesense-forks/castano-nvidia-texture-tools.git
    GIT_TAG        default
)

FetchContent_MakeAvailable(nvtt)