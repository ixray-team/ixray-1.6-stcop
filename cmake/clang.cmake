# General build options
add_compile_options(-Wno-switch -Wno-unused-result -Wno-deprecated-declarations -Wno-inconsistent-missing-override -Wno-inline-new-delete)
add_compile_options(-Wno-implicit-exception-spec-mismatch -Wno-macro-redefined -Wno-unused-local-typedef -Wno-microsoft-unqualified-friend)
add_compile_options(-Wno-comment)
add_compile_options(-msse4.2) # crc32

if (WIN32)
    add_compile_options(-Wno-microsoft-template-shadow -Wno-overloaded-virtual -Wno-unknown-pragmas -Wno-class-conversion -Wno-reorder-ctor)
    add_compile_options(-Wno-microsoft-cast -Wno-microsoft-enum-forward-reference -Wno-bitwise-op-parentheses -Wno-misleading-indentation -Wno-logical-op-parentheses)
    add_compile_options(-Wno-explicit-specialization-storage-class -Wno-microsoft-pure-definition -Wno-ignored-pragmas -Wno-exceptions -Wno-ignored-attributes)
    #add_compile_options()
    add_compile_definitions(_CRT_SECURE_NO_WARNINGS _UNICODE UNICODE)
    add_compile_options(/MP)
    add_compile_options("$<$<CONFIG:DEBUG>:/Od>" "$<$<CONFIG:DEBUG>:/MD>" "/Ob1")
else()
    set(CMAKE_CXX_STANDARD_LIBRARIES "-lstdc++")
    add_compile_options(-fms-extensions)
    #add_compile_options(-fexperimental-library) # stacktrace
    add_compile_options(
        -Wno-null-dereference -Wno-nonnull -Wno-format-truncation -Wno-attributes -Wno-format -Wno-undefined-inline -Wno-unknown-warning-option 
        -Wno-tautological-constant-out-of-range-compare -Wno-macro-redefined -Wno-int-to-pointer-cast -Wno-deprecated-literal-operator
        -Wno-exceptions
    )

    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++") # ranges
    add_compile_definitions(_GNU_SOURCE) # mremap
    
    find_program(LLD_PROGRAM lld)
    if(LLD_PROGRAM)
        add_link_options("-fuse-ld=lld")
        link_directories(/usr/lib64 /usr/local/lib64)
    endif(LLD_PROGRAM)
endif()

# Setup packages patches
if(${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD")
    include("cmake/freebsd/packages.cmake")
    add_compile_options(-mwaitpkg)
endif()

# Setup build patches
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin/$<CONFIG>)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin/$<CONFIG>)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib/$<CONFIG>)

# Hack for COPY
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_EX ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})

# Other 
function(target_validate_pch target target_path)
    file(GLOB_RECURSE CORE_SOURCE_PCH_FILES 
        "${target_path}/[sS][tT][dD][aA][fF][xX].*"
        "${target_path}/[sS][tT][dD][aA][fF][xX]/*"
    )
    
    file(GLOB_RECURSE CORE_SOURCE_ALL_C_FILES "${target_path}/*.c")
    set_source_files_properties(${CORE_SOURCE_ALL_C_FILES} PROPERTIES SKIP_PRECOMPILE_HEADERS ON)
    
    file(GLOB_RECURSE PCH_HEADER_FOUND "${target_path}/[sS][tT][dD][aA][fF][xX].h")
    
    if(PCH_HEADER_FOUND)
        list(GET PCH_HEADER_FOUND 0 PCH_HEADER_PATH)
        get_filename_component(PCH_HEADER_NAME "${PCH_HEADER_PATH}" NAME)
        target_precompile_headers(${target} PRIVATE "${PCH_HEADER_NAME}")
    else()
        target_precompile_headers(${target} PRIVATE "stdafx.h")
    endif()
    
    file(GLOB_RECURSE PCH_CPP_FOUND "${target_path}/[sS][tT][dD][aA][fF][xX].cpp")
    
    if(PCH_CPP_FOUND)
        foreach(cpp_file ${PCH_CPP_FOUND})
            get_filename_component(cpp_name "${cpp_file}" NAME)
            set_source_files_properties("${cpp_name}" PROPERTIES HEADER_FILE_ONLY TRUE)
        endforeach()
    endif()
    
    source_group("pch" FILES ${CORE_SOURCE_PCH_FILES})
endfunction()