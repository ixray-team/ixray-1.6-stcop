# ------------------------------------------------------------------------------
# Detect Linux package manager
# ------------------------------------------------------------------------------

find_program(APT_EXECUTABLE apt)
find_program(DNF_EXECUTABLE dnf)
find_program(PACMAN_EXECUTABLE pacman)

if (APT_EXECUTABLE)
    include("cmake/packages/linux/dpkg.cmake")
elseif(DNF_EXECUTABLE)
    include("cmake/packages/linux/rpm.cmake")
elseif(PACMAN_EXECUTABLE)
    include("cmake/packages/linux/pacman.cmake")
else()
    message(FATAL_ERROR "Unsupported Linux package manager")
endif()
