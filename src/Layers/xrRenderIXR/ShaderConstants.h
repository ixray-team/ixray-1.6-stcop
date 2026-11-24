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
#include "RenderAPI.h"

struct GfxView;

namespace Hlsl
{
	struct alignas(16) float4 : Fvector4
	{
		float4& operator=(const Fvector4& V)
		{
			set(V);
			return *this;
		}
	};

	struct alignas(16) float3 : Fvector3
	{
		float3& operator=(const Fvector3& V)
		{
			set(V);
			return *this;
		}
	};

	// Shaders are compiled row-major and multiply mul(M, v): rows hold the transposed matrix
	template<u32 Rows>
	struct floatNx4
	{
		float4 Row[Rows];

		floatNx4& operator=(const Fmatrix& M)
		{
			for (u32 Index = 0; Index < Rows; ++Index)
			{
				Row[Index].set(M.m[0][Index], M.m[1][Index], M.m[2][Index], M.m[3][Index]);
			}
			return *this;
		}
	};

	using float3x4 = floatNx4<3>;
	using float4x4 = floatNx4<4>;
	using uint = u32;

#define CBUFFER(name, slot) struct alignas(16) name { static constexpr u32 Slot = slot;
#define CBUFFER_END };
#include "../../../shaders/common/fixed_cb.hlsli"
#include "../../../shaders/common/post_cb.hlsli"
#include "../../../shaders/common/detail_cb.hlsli"
#include "../../../shaders/common/fluid_cb.hlsli"
#include "../../../shaders/common/extra_cb.hlsli"
#undef CBUFFER
#undef CBUFFER_END
}

using Hlsl::CBFrame, Hlsl::CBView, Hlsl::CBObject, Hlsl::CBMaterial, Hlsl::CBLight, Hlsl::CBPass, Hlsl::CBSkin;
using Hlsl::CBPost, Hlsl::CBDetail, Hlsl::CBFluid, Hlsl::CBUIIcon, Hlsl::CBVisor, Hlsl::CBEditor, Hlsl::CBDraw;
static_assert(sizeof(CBDraw) == 96);
static_assert(offsetof(CBMaterial, m_lmap) == 32);

class RENDER_API CShaderConstants
{
public:
	void Create();

	template<class T>
	T& Write()
	{
		Dirty |= 1u << T::Slot;
		NriDirty |= 1u << T::Slot;
		return std::get<T>(Shadow);
	}

	// The blocks written since the last dynamic draw through NRI
	u32 TakeNriDirty()
	{
		return std::exchange(NriDirty, 0u);
	}

	template<class T>
	const T& Read() const
	{
		return std::get<T>(Shadow);
	}

	void Flush();

	template<class F>
	void ForEachBlock(F&& Function) const
	{
		std::apply([&](const auto&... Block) { (Function(std::remove_cvref_t<decltype(Block)>::Slot, static_cast<const void*>(&Block), u32(sizeof(Block))), ...); }, Shadow);
	}

	void UpdateFrame();
	void UpdateView(const GfxView& View);
	void UpdateSunShadows();
	void UpdateObject(const Fmatrix& World, const Fmatrix& WorldView, const Fmatrix& WorldViewProject, const Fmatrix& WorldViewProjectOld);
	void ResetMaterial();
	void UpdateScreen();

private:
	using Blocks = std::tuple<CBFrame, CBView, CBObject, CBMaterial, CBLight, CBPass, CBSkin, CBPost, CBDetail, CBFluid, CBUIIcon, CBVisor, CBEditor, CBDraw>;
	static constexpr u32 Count = std::tuple_size_v<Blocks>;

	template<class T>
	void Collect(const T& Block, u32 Force, RHIConstantWrite* Writes, u32& Num);

	Blocks Shadow{};
	Blocks Uploaded{};
	u32 Dirty = 0;
	u32 NriDirty = ~0u;
	u32 PushedEpoch = ~0u;
};
