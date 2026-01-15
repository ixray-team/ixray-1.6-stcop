# Repository Build

## Windows {#windows}

To build on Windows you need:

- git
- cmake
- Visual Studio (2019, 2022, or 2026)

**Build**

``` sh
git clone https://github.com/ixray-team/ixray-1.6-stcop.git
cd ixray-1.6-stcop

cmake -B build
```

Then, in the build folder, open the .sln file in Visual Studio and build the project you need.

## Linux {#linux}

To build on Linux you need:

- git
- cmake
- clang

Additional dependencies:
### Ubuntu {#ubuntu}

``` sh
# SDL3 deps: (https://wiki.libsdl.org/SDL3/README-linux)
sudo apt-get install build-essential make pkg-config cmake ninja-build gnome-desktop-testing libasound2-dev libpulse-dev \
libaudio-dev libfribidi-dev libjack-dev libsndio-dev libx11-dev libxext-dev \
libxrandr-dev libxcursor-dev libxfixes-dev libxi-dev libxss-dev libxtst-dev \
libxkbcommon-dev libdrm-dev libgbm-dev libgl1-mesa-dev libgles2-mesa-dev \
libegl1-mesa-dev libdbus-1-dev libibus-1.0-dev libudev-dev

## Steam Networking
sudo apt install libssl-dev libprotobuf-dev protobuf-compiler

## Vorbis
sudo apt install libogg-dev

## IX-Ray
sudo apt install uuid-dev libtbb-dev liblzo2-dev
```

### Fedora {#fedora}

``` sh
# SDL3 deps: (https://wiki.libsdl.org/SDL3/README-linux)
sudo dnf install git-core make alsa-lib-devel fribidi-devel pulseaudio-libs-devel pipewire-devel \
libX11-devel libXext-devel libXrandr-devel libXcursor-devel libXfixes-devel \
libXi-devel libXScrnSaver-devel dbus-devel ibus-devel \
systemd-devel mesa-libGL-devel libxkbcommon-devel mesa-libGLES-devel \
mesa-libEGL-devel vulkan-devel wayland-devel wayland-protocols-devel \
libdrm-devel mesa-libgbm-devel libusb1-devel libdecor-devel pipewire-jack-audio-connection-kit-devel

## Steam Networking
sudo dnf install openssl-devel protobuf-devel

## Vorbis
sudo dnf install libogg-devel

## IX-Ray
sudo dnf install libuuid-devel tbb-devel lzo-devel
```

## Build

``` sh
git clone https://github.com/ixray-team/ixray-1.6-stcop.git
cd ixray-1.6-stcop

cmake -B build
cmake --build build
```
