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
