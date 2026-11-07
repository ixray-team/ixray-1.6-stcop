# Native macOS dependencies

The native R4 renderer uses the dynamically linked libraries below. Keep their
copyright and license notices with redistributed binaries. The macOS build copies
the DXMT, vkd3d, LLVM, DirectXTex, LuaJIT and DXVK notices into `bin/Debug/licenses/`.

| Component | Pinned source | License |
| --- | --- | --- |
| DXMT | [647b0020ba5764e5593b12407afd76964fa11e20](https://github.com/3Shain/dxmt/tree/647b0020ba5764e5593b12407afd76964fa11e20) | LGPL 2.1 or later; see its `LICENSE`, `COPYING.LIB` and `LICENSE.OLD` |
| Native DirectX headers used by DXMT | [9df86f2341616ef1888ae59919feaa6d4fad693d](https://github.com/misyltoad/mingw-directx-headers/tree/9df86f2341616ef1888ae59919feaa6d4fad693d) | Preserve individual header notices |
| vkd3d | [2.1 source archive](https://dl.winehq.org/vkd3d/source/vkd3d-2.1.tar.xz), SHA-256 `7510146aff2adfb4ae07ab890701a607e5ff7c66e57100cfc9f630ec92eeda6a` | LGPL 2.1 or later; `COPYING` |
| LLVM linked into DXMT's shader translator | [llvmorg-15.0.7](https://github.com/llvm/llvm-project/tree/llvmorg-15.0.7) | Apache 2.0 with LLVM exceptions; [full notice](licenses/LLVM-15.0.7.txt) |
| DirectXTex native texture creation helper | Adapted from [c0cb97764379ebd20dbd2016c58965db85f5b307](https://github.com/microsoft/DirectXTex/tree/c0cb97764379ebd20dbd2016c58965db85f5b307) | MIT; [full notice](licenses/DirectXTex.txt) |
| DirectXMesh native build | [1e5a5e2bf005940202c3623bde57ea72ba76b416](https://github.com/microsoft/DirectXMesh/tree/1e5a5e2bf005940202c3623bde57ea72ba76b416) | MIT; preserve upstream `LICENSE` |
| LuaJIT | [015e23580d9f14ec1960beeebeaf0c5319d9135d](https://github.com/ixray-team/luajit-ixray/tree/015e23580d9f14ec1960beeebeaf0c5319d9135d) | MIT; preserve upstream copyright notices |
| DXVK retained for D3D9 and native DirectX headers | [829e377d2d5cf4b07455412fcabed66a1bf4390c](https://github.com/doitsujin/dxvk/tree/829e377d2d5cf4b07455412fcabed66a1bf4390c) plus `cmake/patches/dxvk-macos.patch` | zlib; preserve upstream `LICENSE` |

When distributing the LGPL libraries, provide the corresponding source and build
instructions under their licenses, including any changes, and preserve users'
ability to replace those libraries. DXMT sources are built without modifications;
the native Cocoa window bridge is engine code under `src/xrRHI/Platform/MacOS`.
Source downloads and build logs remain under the CMake build directory. Do not
include game assets in an engine binary distribution.

Apple's Xcode/Metal toolchain is a build prerequisite and is not bundled. Homebrew
provides other dependencies such as OpenAL Soft, FreeType, Ogg/Vorbis, TBB,
Vulkan Loader and MoltenVK under their respective licenses. SDL 3.2.22 is built
from the repository's existing CMake dependency and uses the zlib license. Existing Steamworks
SDK redistribution requirements continue to apply to `libsteam_api.dylib`.

