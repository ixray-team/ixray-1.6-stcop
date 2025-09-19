# FindOptiX.cmake — для OptiX 9+
# Не ищет .lib, только include и dll (runtime)

find_path(OptiX_INCLUDE_DIR
  NAMES optix.h
  PATHS
    ENV OPTIX_ROOT
    "C:/ProgramData/NVIDIA Corporation/OptiX SDK 9.0.0/include"
    "C:/Program Files/NVIDIA GPU Computing Toolkit/OptiX SDK 9.0.0/include"
    NO_DEFAULT_PATH
)

if(OptiX_INCLUDE_DIR)
  set(OptiX_FOUND TRUE)
else()
  set(OptiX_FOUND FALSE)
endif()

if(OptiX_FOUND)
  message(STATUS "Found OptiX include dir: ${OptiX_INCLUDE_DIR}")
else()
  message(WARNING "Could NOT find OptiX include directory")
endif()


set(OptiX_INCLUDE_DIRS ${OptiX_INCLUDE_DIR} CACHE PATH "OptiX include directory")

mark_as_advanced(OptiX_INCLUDE_DIR OptiX_DLL)

