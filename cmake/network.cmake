# Limit hanging network I/O during configure (NuGet, FetchContent, git, file(DOWNLOAD)).

option(IXRAY_OFFLINE "Skip NuGet restore and FetchContent downloads (deps must already be present)" OFF)
option(IXRAY_NUGET_FORCE_RESTORE "Always run nuget restore even if packages were already restored" OFF)
option(IXRAY_FETCHCONTENT_DISCONNECTED "Do not git fetch / re-download FetchContent deps after the first successful populate" ON)

set(FETCHCONTENT_UPDATES_DISCONNECTED ${IXRAY_FETCHCONTENT_DISCONNECTED} CACHE BOOL "Skip FetchContent updates when the dependency is already populated" FORCE)

if(IXRAY_OFFLINE)
    set(FETCHCONTENT_FULLY_DISCONNECTED ON CACHE BOOL "Do not contact the network for FetchContent" FORCE)
    set(FETCHCONTENT_UPDATES_DISCONNECTED ON CACHE BOOL "" FORCE)
endif()

set(ENV{GIT_TERMINAL_PROMPT} 0)
set(ENV{GCM_INTERACTIVE} Never)

if(NOT DEFINED ENV{GIT_HTTP_LOW_SPEED_LIMIT})
    set(ENV{GIT_HTTP_LOW_SPEED_LIMIT} 1024)
endif()

if(NOT DEFINED ENV{GIT_HTTP_LOW_SPEED_TIME})
    set(ENV{GIT_HTTP_LOW_SPEED_TIME} 30)
endif()

include(FetchContent)

if(NOT COMMAND _ixr_FetchContent_Declare)
    macro(FetchContent_Declare name)
        if(FETCHCONTENT_UPDATES_DISCONNECTED OR IXRAY_OFFLINE)
            _FetchContent_Declare(
                ${name}
                ${ARGN}
                UPDATE_DISCONNECTED TRUE
                TIMEOUT 120
                INACTIVITY_TIMEOUT 30
            )
        else()
            _FetchContent_Declare(
                ${name}
                ${ARGN}
                TIMEOUT 120
                INACTIVITY_TIMEOUT 30
            )
        endif()
    endmacro()
    function(_ixr_FetchContent_Declare)
    endfunction()
endif()
