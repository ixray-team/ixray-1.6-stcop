# General build options
add_compile_options(-Wno-reorder-ctor -Wno-switch -Wno-macro-redefined -Wno-implicit-exception-spec-mismatch -Wno-deprecated-volatile 
                    -Wno-inline-new-delete -Wno-c++11-narrowing -Wno-unused-local-typedef -Wno-defaulted-function-deleted -Wno-undefined-inline
                    -Wno-volatile)

if (WIN32)
    add_compile_options(--Wno-microsoft-template-shadow)
else()
    add_compile_options(-fms-extensions -Wno-null-conversion -Wno-null-dereference)
endif()

if (NOT WIN32)
    find_program(LLD_PROGRAM lld)
    if(LLD_PROGRAM)
        add_link_options("-fuse-ld=lld")
        link_directories(/usr/lib64 /usr/local/lib64)
    endif(LLD_PROGRAM)
endif()