# Fedora / DNF package check

find_program(DNF_EXECUTABLE dnf REQUIRED)

set(REQUIRED_PACKAGES
    clang
    lldb
    lld
    libcxx-devel
    libcxxabi-devel

    libX11-devel
    libXext-devel
    libXrandr-devel
    libXcursor-devel
    libXfixes-devel
    libXi-devel
    libXinerama-devel
    libXScrnSaver-devel
    wayland-devel
    libdrm-devel
    mesa-libgbm-devel
    systemd-devel
    pipewire-devel
    ibus-devel
    dbus-devel
    tbb-devel
    lzo
    lzo-devel
    libogg-devel
    meson
    glslang
    vulkan-headers
    vulkan-loader-devel
)

message(STATUS "Checking required Fedora packages...")

foreach(Package IN LISTS REQUIRED_PACKAGES)

    execute_process(
        COMMAND rpm -q ${Package}
        RESULT_VARIABLE PACKAGE_FOUND
        OUTPUT_QUIET
        ERROR_QUIET
    )

    if (PACKAGE_FOUND EQUAL 0)
        message(STATUS "[OK] ${Package}")
    else()
        message(ERROR "[MISSING] ${Package}")
    endif()

endforeach()