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

# NVTT
include(FetchContent)

FetchContent_Declare(
    nvtt
    GIT_REPOSITORY https://github.com/imesense-forks/castano-nvidia-texture-tools.git
    GIT_TAG        default
)

FetchContent_MakeAvailable(nvtt)


# Sound 3rd
function(setup_audio_libs target)
    target_link_libraries(${target} PRIVATE
        Ogg::ogg
        Vorbis::vorbis
        Vorbis::vorbisfile
        OpenAL
        Opus::opus
        SpeexDSP::speexdsp
    )
endfunction()