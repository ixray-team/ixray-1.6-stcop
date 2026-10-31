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

#define CBUFFER(name, slot) struct alignas(16) name { static constexpr u32 Slot = slot;
#define CBUFFER_END };
#include "../../../gamedata/shaders/d3d11/shared/fixed_cb.hlsli"
#include "../../../gamedata/shaders/d3d11/shared/post_cb.hlsli"
#include "../../../gamedata/shaders/d3d11/shared/detail_cb.hlsli"
#include "../../../gamedata/shaders/d3d11/shared/fluid_cb.hlsli"
#include "../../../gamedata/shaders/d3d11/shared/extra_cb.hlsli"
#undef CBUFFER
#undef CBUFFER_END
}

using Hlsl::CBFrame, Hlsl::CBView, Hlsl::CBObject, Hlsl::CBMaterial, Hlsl::CBLight, Hlsl::CBPass, Hlsl::CBSkin;
using Hlsl::CBPost, Hlsl::CBDetail, Hlsl::CBFluid, Hlsl::CBUIIcon, Hlsl::CBVisor, Hlsl::CBEditor, Hlsl::CBDraw;
static_assert(sizeof(CBDraw) == 96);
static_assert(sizeof(CBSkin) <= 65536);

class RENDER_API CShaderConstants
{
public:
	void Create();

	template<class T>
	T& Write()
	{
		Dirty |= 1u << T::Slot;
		if constexpr (std::is_same_v<T, CBSkin>)
		{
			SkinSize = sizeof(CBSkin);
		}
		return std::get<T>(Shadow);
	}

	CBSkin& WriteSkin(u32 Bones)
	{
		CBSkin& Skin = Write<CBSkin>();
		SkinSize = offsetof(CBSkin, sbones_array_old) + Bones * sizeof(Hlsl::float3x4);
		return Skin;
	}

	template<class T>
	const T& Read() const
	{
		return std::get<T>(Shadow);
	}

	void Flush();

	void UpdateFrame();
	void UpdateView(const GfxView& View);
	void UpdateObject(const Fmatrix& World, const Fmatrix& WorldView, const Fmatrix& WorldViewProject, const Fmatrix& WorldViewProjectOld);
	void ResetMaterial();
	void SetLOD(float LOD);
	void UpdateScreen();

private:
	using Blocks = std::tuple<CBFrame, CBView, CBObject, CBMaterial, CBLight, CBPass, CBSkin, CBPost, CBDetail, CBFluid, CBUIIcon, CBVisor, CBEditor, CBDraw>;
	static constexpr u32 Count = std::tuple_size_v<Blocks>;

	template<class T>
	void Collect(const T& Block, u32 Force, RHIConstantWrite* Writes, u32& Num);

	Blocks Shadow{};
	Blocks Uploaded{};
	u32 Dirty = 0;
	u32 PushedEpoch = ~0u;
	u32 SkinSize = sizeof(CBSkin);
	u32 PushedSkinSize = 0;
};
