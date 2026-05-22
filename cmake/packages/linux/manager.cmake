# ------------------------------------------------------------------------------
# Detect Linux package manager
# ------------------------------------------------------------------------------

find_program(APT_EXECUTABLE apt)
find_program(DNF_EXECUTABLE dnf)

if (APT_EXECUTABLE)
    include("cmake/packages/linux/dpkg.cmake")
elseif(DNF_EXECUTABLE)
    include("cmake/packages/linux/rpm.cmake")
else()
    message(FATAL_ERROR "Unsupported Linux package manager")
endif()