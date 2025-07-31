include(FetchContent)

# YamlCPP
FetchContent_Declare(
  yaml-cpp
  GIT_REPOSITORY https://github.com/jbeder/yaml-cpp.git
  GIT_TAG        0.8.0
)

FetchContent_MakeAvailable(yaml-cpp)

# TBB
find_package(TBB REQUIRED)

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