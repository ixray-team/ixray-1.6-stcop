#pragma once

#if defined(IXRAY_PROFILER_TRACY) && defined(IXR_WINDOWS)
#include <tracy/TracyD3D11.hpp>
extern TracyD3D11Ctx g_tracyD3D11GPUContext;
#endif
