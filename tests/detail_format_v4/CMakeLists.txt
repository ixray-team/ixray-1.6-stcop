cmake_minimum_required(VERSION 3.16)
project(ixray_detail_format_codec_tests LANGUAGES CXX)

enable_testing()
add_executable(detail_format_codec_tests
    test_detail_format_codec.cpp
    ../../src/xrCore/DetailFormatCodec.cpp
)
target_compile_features(detail_format_codec_tests PRIVATE cxx_std_17)
target_compile_definitions(detail_format_codec_tests PRIVATE IXRAY_DETAIL_CODEC_STANDALONE=1)
if (MSVC)
    target_compile_options(detail_format_codec_tests PRIVATE /W4 /WX)
else()
    target_compile_options(detail_format_codec_tests PRIVATE -Wall -Wextra -Wpedantic -Werror)
endif()
add_test(NAME detail_format_codec COMMAND detail_format_codec_tests)

# An optional integration test using a real level.details v3 fixture.
# Never store the large copyrighted game/level asset in the source tree.
set(IXRAY_DETAIL_V3_FILE "" CACHE FILEPATH "Path to a real level.details v3 file for regression testing")
add_executable(detail_format_real_v3_tests
    test_real_v3_file.cpp
    ../../src/xrCore/DetailFormatCodec.cpp
)
target_compile_features(detail_format_real_v3_tests PRIVATE cxx_std_17)
target_compile_definitions(detail_format_real_v3_tests PRIVATE IXRAY_DETAIL_CODEC_STANDALONE=1)
if (MSVC)
    target_compile_options(detail_format_real_v3_tests PRIVATE /W4 /WX)
else()
    target_compile_options(detail_format_real_v3_tests PRIVATE -Wall -Wextra -Wpedantic -Werror)
endif()
if (IXRAY_DETAIL_V3_FILE)
    if (NOT EXISTS "${IXRAY_DETAIL_V3_FILE}")
        message(FATAL_ERROR "IXRAY_DETAIL_V3_FILE does not exist: ${IXRAY_DETAIL_V3_FILE}")
    endif()
    add_test(NAME detail_format_real_v3
             COMMAND detail_format_real_v3_tests "${IXRAY_DETAIL_V3_FILE}")
endif()

# Optional end-to-end fixture for a real v4 level.details with high (>=1024) IDs.
# Unlike the codec unit test, this validates outer chunks, model subchunks,
# serialized slot count and byte-exact roundtripping on the user's actual file.
set(IXRAY_DETAIL_V4_FILE "" CACHE FILEPATH "Path to a Detail v4 level.details with referenced IDs >=1024")
add_executable(detail_format_real_v4_tests
    test_real_v4_file.cpp
    ../../src/xrCore/DetailFormatCodec.cpp
)
target_compile_features(detail_format_real_v4_tests PRIVATE cxx_std_17)
target_compile_definitions(detail_format_real_v4_tests PRIVATE IXRAY_DETAIL_CODEC_STANDALONE=1)
if (MSVC)
    target_compile_options(detail_format_real_v4_tests PRIVATE /W4 /WX)
else()
    target_compile_options(detail_format_real_v4_tests PRIVATE -Wall -Wextra -Wpedantic -Werror)
endif()
if (IXRAY_DETAIL_V4_FILE)
    if (NOT EXISTS "${IXRAY_DETAIL_V4_FILE}")
        message(FATAL_ERROR "IXRAY_DETAIL_V4_FILE does not exist: ${IXRAY_DETAIL_V4_FILE}")
    endif()
    add_test(NAME detail_format_real_v4
             COMMAND detail_format_real_v4_tests "${IXRAY_DETAIL_V4_FILE}")
endif()
