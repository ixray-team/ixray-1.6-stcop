project(crypto)

set(CRYPTO_SRC_FILES)
file(GLOB_RECURSE CRYPTO_SRC
    "*.cpp"
    "*.c"
    "*.h"
)

list(APPEND CRYPTO_SRC_FILES ${CRYPTO_SRC})
add_library(crypto STATIC ${CRYPTO_SRC_FILES})

target_include_directories(crypto PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/openssl")
add_compile_options(/fp:fast)
target_compile_definitions(crypto PRIVATE "$<$<CONFIG:Debug>:DEBUG>")
target_compile_definitions(crypto PRIVATE _CRT_NO_POSIX_ERROR_CODES)
