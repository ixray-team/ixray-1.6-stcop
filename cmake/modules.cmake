set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules" ${CMAKE_MODULE_PATH})

include("${CMAKE_MODULE_PATH}/SDL3.cmake")
include("${CMAKE_MODULE_PATH}/LuaJIT.cmake")
include("${CMAKE_MODULE_PATH}/LuaBind.cmake")
include("${CMAKE_MODULE_PATH}/Ogg.cmake")
include("${CMAKE_MODULE_PATH}/Vorbis.cmake")
include("${CMAKE_MODULE_PATH}/OpenalSoft.cmake")
include("${CMAKE_MODULE_PATH}/FreeImage.cmake")

if (IXRAY_MP)
    include("${CMAKE_MODULE_PATH}/GameNetworkingSockets.cmake")
    include("${CMAKE_MODULE_PATH}/Opus.cmake")
    include("${CMAKE_MODULE_PATH}/SpeexDSP.cmake")
endif()

# Headers only libraries
include("${CMAKE_MODULE_PATH}/FastDynamicCast.cmake")
include("${CMAKE_MODULE_PATH}/magic_enum.cmake")
include("${CMAKE_MODULE_PATH}/sse2neon.cmake")
include("${CMAKE_MODULE_PATH}/nlohmann_json.cmake")
include("${CMAKE_MODULE_PATH}/amd_adl.cmake")
include("${CMAKE_MODULE_PATH}/nvapi.cmake")
include("${CMAKE_MODULE_PATH}/stb.cmake")

if(WIN32)
    include("${CMAKE_MODULE_PATH}/dirent.cmake")
endif()

if (IXRAY_EDITORS)
    include("${CMAKE_MODULE_PATH}/FileWatch.cmake")
endif()
