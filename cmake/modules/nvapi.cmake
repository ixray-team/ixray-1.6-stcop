function(download_and_fix_nvapi)
    set(NVAPI_HEADERS
        nvapi.h
        nvapi_interface.h
        nvapi_lite_common.h
#        nvapi_lite_d3dext.h
        nvapi_lite_salend.h
        nvapi_lite_salstart.h
        nvapi_lite_sli.h
        nvapi_lite_stereo.h
        nvapi_lite_surround.h
    )
    
    foreach(header ${NVAPI_HEADERS})
        download_single_header(
            "https://raw.githubusercontent.com/NVIDIA/nvapi/refs/heads/main/${header}"
            "nvapi/${header}"
            TLS_VERIFY ON
        )
        
        file(READ "${EXTERNAL_INCLUDE_DIR}/nvapi/${header}" content)
        if(NOT content MATCHES "#pragma once")
            file(WRITE "${EXTERNAL_INCLUDE_DIR}/nvapi/${header}" "#pragma once\n\n${content}")
            message(STATUS "Added pragma once to ${header}")
        endif()
    endforeach()
endfunction()

# FX: Легендарный костыль
download_and_fix_nvapi()

if(NOT EXISTS "${EXTERNAL_INCLUDE_DIR}/nvapi/nvapi_lite_d3dext.h")
    file(WRITE "${EXTERNAL_INCLUDE_DIR}/nvapi/nvapi_lite_d3dext.h" "")
endif()