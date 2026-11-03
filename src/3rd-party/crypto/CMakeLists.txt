project(crypto)

if(APPLE)
    # The bundled OpenSSL is configured for Win32 and cannot be built here,
    # so macOS uses the system CommonCrypto instead.
    add_library(crypto STATIC crypto_macos.cpp)
    target_link_libraries(crypto PUBLIC xrCore)
else()
    set(CRYPTO_SRC_FILES)
    file(GLOB_RECURSE CRYPTO_SRC
        "*.cpp"
        "*.c"
        "*.h"
    )

    list(APPEND CRYPTO_SRC_FILES ${CRYPTO_SRC})

    # crypto_macos.cpp is the macOS implementation and must not be compiled here.
    list(FILTER CRYPTO_SRC_FILES EXCLUDE REGEX "crypto_macos\\.cpp$")

    add_library(crypto STATIC ${CRYPTO_SRC_FILES})

    target_include_directories(crypto PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/openssl")
    target_link_libraries(crypto PUBLIC xrCore)
endif()

target_compile_definitions(crypto PRIVATE _CRT_NO_POSIX_ERROR_CODES)
