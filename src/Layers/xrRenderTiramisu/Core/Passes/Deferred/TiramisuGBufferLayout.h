#pragma once

#include "TiramisuRenderTypes.h"

#include <NRI.h>

// Стабильные индексы MRT, общие для geometry и deferred-lighting passes.
enum class ETiramisuGBufferTarget : u8
{
	BaseColorAmbientOcclusion,
	NormalRoughnessMetallic,
	EmissiveMaterialFlags,
	Velocity,
	Count
};

// Версионированный GPU-контракт production G-buffer Tiramisu.
class TiramisuGBufferLayout final
{
public:
	static constexpr u32 Version = 1;
	static constexpr u32 TargetCount =
		static_cast<u32>(ETiramisuGBufferTarget::Count);
	static constexpr xr_string_view RenderPassSignature =
		"gbuffer:rgba8+rgba16f+rgba16f+rg16f:d24s8";
	static constexpr nri::Format DepthFormat =
		nri::Format::D24_UNORM_S8_UINT;

	static constexpr xr_array<nri::Format, TargetCount> TargetFormats =
	{
		nri::Format::RGBA8_UNORM,
		nri::Format::RGBA16_SFLOAT,
		nri::Format::RGBA16_SFLOAT,
		nri::Format::RG16_SFLOAT
	};

	static constexpr xr_array<u32, TargetCount> TargetBytesPerPixel =
	{
		4,
		8,
		8,
		4
	};

	[[nodiscard]] static constexpr u32 GetTargetIndex(
		const ETiramisuGBufferTarget Target
	) noexcept
	{
		return static_cast<u32>(Target);
	}

	[[nodiscard]] static constexpr nri::Format GetTargetFormat(
		const ETiramisuGBufferTarget Target
	) noexcept
	{
		return TargetFormats[GetTargetIndex(Target)];
	}
};

static_assert(TiramisuGBufferLayout::TargetCount == 4);
