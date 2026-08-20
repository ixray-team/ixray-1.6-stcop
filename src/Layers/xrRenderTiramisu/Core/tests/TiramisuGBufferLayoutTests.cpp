#include "Core/Passes/Deferred/TiramisuGBufferLayout.h"

#include <iostream>

namespace
{
int Fail(const char* Message)
{
	std::cerr << Message << '\n';
	return 1;
}
} // namespace

int main()
{
	if (TiramisuGBufferLayout::Version != 1 ||
		TiramisuGBufferLayout::TargetCount != 4)
	{
		return Fail("G-buffer version and attachment count must remain explicit");
	}

	if (TiramisuGBufferLayout::RenderPassSignature !=
		"gbuffer:rgba8+rgba16f+rgba16f+rg16f:d24s8")
	{
		return Fail("G-buffer signature must match the material pass manifest");
	}

	if (TiramisuGBufferLayout::GetTargetFormat(
			ETiramisuGBufferTarget::BaseColorAmbientOcclusion
		) != nri::Format::RGBA8_UNORM ||
		TiramisuGBufferLayout::GetTargetFormat(
			ETiramisuGBufferTarget::NormalRoughnessMetallic
		) != nri::Format::RGBA16_SFLOAT ||
		TiramisuGBufferLayout::GetTargetFormat(
			ETiramisuGBufferTarget::EmissiveMaterialFlags
		) != nri::Format::RGBA16_SFLOAT ||
		TiramisuGBufferLayout::GetTargetFormat(
			ETiramisuGBufferTarget::Velocity
		) != nri::Format::RG16_SFLOAT)
	{
		return Fail("G-buffer attachment formats changed without a version bump");
	}

	if (TiramisuGBufferLayout::DepthFormat !=
		nri::Format::D24_UNORM_S8_UINT)
	{
		return Fail("G-buffer depth format must match its pass signature");
	}

	constexpr u32 ExpectedBytesPerPixel = 4 + 8 + 8 + 4;
	u32 ActualBytesPerPixel = 0;
	for (const u32 Bytes : TiramisuGBufferLayout::TargetBytesPerPixel)
	{
		ActualBytesPerPixel += Bytes;
	}
	if (ActualBytesPerPixel != ExpectedBytesPerPixel)
	{
		return Fail("G-buffer memory estimate must cover every MRT");
	}

	return 0;
}
