/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#pragma once

struct RHIViewport
{
	float TopLeftX = 0.0f;
	float TopLeftY = 0.0f;
	float Width = 0.0f;
	float Height = 0.0f;
	float MinDepth = 0.0f;
	float MaxDepth = 1.0f;

	RHIViewport() = default;

	RHIViewport(float x, float y, float w, float h, float minDepth = 0.0f, float maxDepth = 1.0f)
		: TopLeftX(x), TopLeftY(y), Width(w), Height(h), MinDepth(minDepth), MaxDepth(maxDepth)
	{
	}
};

// What the window's swap chain is made with: its size in the window's pixels, the frames it may queue (r_latency)
// and v-sync (r_vsync)
struct RHISwapChainDesc
{
	u32 Width = 0;
	u32 Height = 0;
	u32 FrameQueue = 2;
	bool IsVSync = false;

	bool operator==(const RHISwapChainDesc&) const = default;
};

struct RHIInputElementDesc
{
	const char* SemanticName;
	u32 SemanticIndex;
	ERHI_FORMAT Format;
	u32 InputSlot;
	u32 AlignedByteOffset;
	ERHI_INPUT_CLASSIFICATION InputSlotClass;
	u32 InstanceDataStepRate;
};

struct RHI_API RHISampleDesc
{
	ERHI_FILTER Filter;
	ERHI_TEXTURE_ADDRESS_MODE AddressU;
	ERHI_TEXTURE_ADDRESS_MODE AddressV;
	ERHI_TEXTURE_ADDRESS_MODE AddressW;
	float MipLODBias;
	u32 MaxAnisotropy;
	ERHI_COMPARISON_FUNC ComparisonFunc;
	float BorderColor[4];
	float MinLOD;
	float MaxLOD;

	bool operator==(const RHISampleDesc& desc2) const
	{
		if (Filter != desc2.Filter) return false;
		if (AddressU != desc2.AddressU) return false;
		if (AddressV != desc2.AddressV) return false;
		if (AddressW != desc2.AddressW) return false;
		if (ComparisonFunc != desc2.ComparisonFunc) return false;
		if (BorderColor[0] != desc2.BorderColor[0]) return false;
		if (BorderColor[1] != desc2.BorderColor[1]) return false;
		if (BorderColor[2] != desc2.BorderColor[2]) return false;
		if (BorderColor[3] != desc2.BorderColor[3]) return false;
		if (MinLOD != desc2.MinLOD) return false;
		if (MaxLOD != desc2.MaxLOD) return false;

		return true;
	}
};