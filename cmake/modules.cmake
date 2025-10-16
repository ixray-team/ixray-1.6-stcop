set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules" ${CMAKE_MODULE_PATH})

include("${CMAKE_MODULE_PATH}/SDL3.cmake")
include("${CMAKE_MODULE_PATH}/LuaBind.cmake")
include("${CMAKE_MODULE_PATH}/Ogg.cmake")
include("${CMAKE_MODULE_PATH}/Vorbis.cmake")
include("${CMAKE_MODULE_PATH}/OpenalSoft.cmake")

if (IXRAY_MP)
    include("${CMAKE_MODULE_PATH}/GameNetworkingSockets.cmake")
    include("${CMAKE_MODULE_PATH}/Opus.cmake")
    include("${CMAKE_MODULE_PATH}/SpeexDSP.cmake")
endif()