include(FetchContent)

function(ixray_add_package_archive)
    set(CMAKE_POLICY_DEFAULT_CMP0077 NEW)
    set(BUILD_SHARED_LIBS OFF)
    set(BUILD_TESTING OFF)
    set(ZLIB_BUILD_TESTING OFF)
    set(ZLIB_BUILD_SHARED OFF)
    set(ZLIB_BUILD_STATIC ON)
    set(ZLIB_INSTALL OFF)
    FetchContent_Declare(ZLIB
        URL https://zlib.net/fossils/zlib-1.3.2.tar.gz
        URL_HASH SHA256=bb329a0a2cd0274d05519d61c667c062e06990d72e125ee2dfa8de64f0119d16
        DOWNLOAD_EXTRACT_TIMESTAMP TRUE
        OVERRIDE_FIND_PACKAGE
    )
    FetchContent_MakeAvailable(ZLIB)
    if (NOT TARGET ZLIB::ZLIB)
        add_library(ZLIB::ZLIB ALIAS zlibstatic)
    endif()
    file(WRITE "${CMAKE_FIND_PACKAGE_REDIRECTS_DIR}/zlib-extra.cmake"
        "set(ZLIB_FOUND TRUE)\n"
        "set(ZLIB_INCLUDE_DIR \"${zlib_BINARY_DIR};${zlib_SOURCE_DIR}\")\n"
        "set(ZLIB_INCLUDE_DIRS \"${zlib_BINARY_DIR};${zlib_SOURCE_DIR}\")\n"
        "set(ZLIB_LIBRARIES ZLIB::ZLIB)\n"
    )
    # The bundled static zlib uses the default calling convention and no DLL import macros.
    set(ZLIB_WINAPI FALSE)
    set(ZLIB_DLL FALSE)
    set(WITHOUT_ZLIB_DLL TRUE)
    set(XZ_NLS OFF)
    set(XZ_TOOL_XZ OFF)
    set(XZ_TOOL_XZDEC OFF)
    set(XZ_TOOL_LZMADEC OFF)
    set(XZ_TOOL_LZMAINFO OFF)
    set(XZ_TOOL_SCRIPTS OFF)
    set(XZ_DOC OFF)
    FetchContent_Declare(LibLZMA
        URL https://github.com/tukaani-project/xz/releases/download/v5.8.4/xz-5.8.4.tar.gz
        URL_HASH SHA256=0014c7886930454fe8bd4228665b51af55eeae560ea135c9c4cd33f55b2591d9
        DOWNLOAD_EXTRACT_TIMESTAMP TRUE
        OVERRIDE_FIND_PACKAGE
    )
    FetchContent_MakeAvailable(LibLZMA)
    if (NOT TARGET LibLZMA::LibLZMA)
        add_library(LibLZMA::LibLZMA ALIAS liblzma)
    endif()
    # Supply legacy variables expected by libarchive's find_package call.
    file(WRITE "${CMAKE_FIND_PACKAGE_REDIRECTS_DIR}/liblzma-extra.cmake"
        "set(LIBLZMA_FOUND TRUE)\n"
        "set(LIBLZMA_INCLUDE_DIR \"${liblzma_SOURCE_DIR}/src/liblzma/api\")\n"
        "set(LIBLZMA_INCLUDE_DIRS \"${liblzma_SOURCE_DIR}/src/liblzma/api\")\n"
        "set(LIBLZMA_LIBRARIES LibLZMA::LibLZMA)\n"
    )
    # The bundled target is static and its interface supplies LZMA_API_STATIC.
    set(WITHOUT_LZMA_API_STATIC FALSE)
    set(LZMA_API_STATIC TRUE)
    set(HAVE_LZMA_STREAM_ENCODER_MT FALSE)
    foreach(Feature WERROR MBEDTLS NETTLE OPENSSL LIBB2 LZ4 LZO ZSTD ZLIB BZip2
            LIBXML2 EXPAT WIN32_XMLLITE PCREPOSIX PCRE2POSIX LIBGCC CNG TAR
            TAR_SHARED CPIO CPIO_SHARED CAT CAT_SHARED UNZIP UNZIP_SHARED
            XATTR ACL ICONV TEST COVERAGE INSTALL CLANG_TIDY)
        set(ENABLE_${Feature} OFF)
    endforeach()
    set(ENABLE_LZMA ON)
    set(ENABLE_ZLIB ON)
    set(MSVC_USE_STATIC_CRT OFF)
    set(POSIX_REGEX_LIB "NONE" CACHE STRING "No regular-expression provider is needed for package extraction" FORCE)
    FetchContent_Declare(package_archive
        URL https://github.com/libarchive/libarchive/releases/download/v3.8.9/libarchive-3.8.9.tar.gz
        URL_HASH SHA256=f5a6539059cf5e597dbeda37bfa4874b1e8dea063c8d93bf85a2b44af90a5bd4
        DOWNLOAD_EXTRACT_TIMESTAMP TRUE
    )
    FetchContent_MakeAvailable(package_archive)
    target_compile_definitions(archive_static INTERFACE LIBARCHIVE_STATIC)
    if(WIN32)
        target_link_libraries(archive_static bcrypt)
    endif()
    set_target_properties(zlibstatic liblzma archive_static PROPERTIES FOLDER "3rd Party")
    set_property(DIRECTORY "${liblzma_SOURCE_DIR}" PROPERTY EXCLUDE_FROM_ALL TRUE)
    set_property(DIRECTORY "${package_archive_SOURCE_DIR}" PROPERTY EXCLUDE_FROM_ALL TRUE)
    set(IXRAY_PACKAGE_ZLIB_SOURCE_DIR "${zlib_SOURCE_DIR}" PARENT_SCOPE)
    set(IXRAY_PACKAGE_ARCHIVE_SOURCE_DIR "${package_archive_SOURCE_DIR}" PARENT_SCOPE)
    set(IXRAY_PACKAGE_LZMA_SOURCE_DIR "${liblzma_SOURCE_DIR}" PARENT_SCOPE)
endfunction()

ixray_add_package_archive()
