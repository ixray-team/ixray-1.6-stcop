set(archive "${THEORA_BUILD_DIR}/libtheora-1.1.1.tar.gz")
set(urls
    "https://downloads.xiph.org/releases/theora/libtheora-1.1.1.tar.gz"
    "https://ftp.osuosl.org/pub/xiph/releases/theora/libtheora-1.1.1.tar.gz"
)

foreach(url IN LISTS urls)
    message(STATUS "Downloading Theora from ${url}")
    file(DOWNLOAD "${url}" "${archive}" STATUS download_status TLS_VERIFY ON TIMEOUT 120)
    list(GET download_status 0 download_code)
    if(download_code EQUAL 0)
        return()
    endif()
    list(GET download_status 1 download_error)
    message(WARNING "Theora download failed: ${download_error}")
    file(REMOVE "${archive}")
endforeach()

message(FATAL_ERROR "Failed to download Theora from all configured sources")
