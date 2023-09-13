add_library(rapidjson INTERFACE)
add_library(RapidJSON::rapidjson ALIAS rapidjson)
target_include_directories(rapidjson INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)