#pragma once
#include "RHIProfiler.h"
#include <optional>

class InternalDX11GPUEventWrapper
{
private:
	int _index = -1;
#if defined(IXRAY_PROFILER_TRACY) && defined(IXR_WINDOWS)
	std::optional<tracy::D3D11ZoneScope> profiler_zone;
#endif

public:
	InternalDX11GPUEventWrapper(const char* name, const wchar_t* wname, const void* profiler_location);
	~InternalDX11GPUEventWrapper();
};