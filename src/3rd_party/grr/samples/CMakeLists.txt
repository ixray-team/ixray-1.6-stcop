add_executable("big_sample" "big_sample.cpp")
target_link_libraries("big_sample" PRIVATE GRR::grr)
set_target_properties("big_sample" PROPERTIES LINKER_LANGUAGE CXX)