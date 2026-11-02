include(FetchContent)

function(ixray_add_package_zip)
    set(BUILD_EXAMPLES OFF)
    set(BUILD_TESTS OFF)
    set(BUILD_SHARED_LIBS OFF)
    set(INSTALL_PROJECT OFF)
    set(AMALGAMATE_SOURCES OFF)
    set(BUILD_HEADER_ONLY OFF)
    FetchContent_Declare(miniz
        URL https://github.com/richgel999/miniz/archive/refs/tags/3.1.0.tar.gz
        URL_HASH SHA256=09569fc19d060ac9f5999ba9356728c2494ebe6a24ac0eb0a6b6ae3d396cfea6
        DOWNLOAD_EXTRACT_TIMESTAMP TRUE
    )
    FetchContent_MakeAvailable(miniz)
    target_compile_definitions(miniz PUBLIC MINIZ_NO_ZLIB_COMPATIBLE_NAMES)
    set_target_properties(miniz PROPERTIES FOLDER "3rd Party")
endfunction()

ixray_add_package_zip()
