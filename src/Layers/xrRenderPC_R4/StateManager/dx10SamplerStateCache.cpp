#include "stdafx.h"
#include "dx10SamplerStateCache.h"

#include "../ShaderBindings.h"

dx10SamplerStateCache SSManager;

namespace
{
	using Filter = ERHI_FILTER;
	using Address = ERHI_TEXTURE_ADDRESS_MODE;

	constexpr struct
	{
		std::string_view Name;
		Filter Mode;
		Address UV;
		Address W;
		ERHI_COMPARISON_FUNC Compare;
	} Samplers[] =
	{
		{ "smp_nofilter", Filter::MIN_MAG_MIP_POINT, Address::CLAMP, Address::CLAMP, ERHI_COMPARISON_FUNC::NEVER },
		{ "smp_rtlinear", Filter::MIN_MAG_LINEAR_MIP_POINT, Address::CLAMP, Address::CLAMP, ERHI_COMPARISON_FUNC::NEVER },
		{ "smp_linear", Filter::MIN_MAG_MIP_LINEAR, Address::WRAP, Address::WRAP, ERHI_COMPARISON_FUNC::NEVER },
		{ "smp_base", Filter::ANISOTROPIC, Address::WRAP, Address::WRAP, ERHI_COMPARISON_FUNC::NEVER },
		{ "smp_material", Filter::MIN_MAG_LINEAR_MIP_POINT, Address::CLAMP, Address::WRAP, ERHI_COMPARISON_FUNC::NEVER },
		{ "smp_smap", Filter::COMPARISON_MIN_MAG_LINEAR_MIP_POINT, Address::CLAMP, Address::CLAMP, ERHI_COMPARISON_FUNC::LESS_EQUAL },
		{ "smp_jitter", Filter::MIN_MAG_MIP_POINT, Address::WRAP, Address::WRAP, ERHI_COMPARISON_FUNC::NEVER },
		{ "smp_bump_ds", Filter::ANISOTROPIC, Address::WRAP, Address::WRAP, ERHI_COMPARISON_FUNC::NEVER },
		{ "samPointClamp", Filter::MIN_MAG_MIP_POINT, Address::CLAMP, Address::CLAMP, ERHI_COMPARISON_FUNC::NEVER },
		{ "samLinear", Filter::MIN_MAG_MIP_LINEAR, Address::CLAMP, Address::CLAMP, ERHI_COMPARISON_FUNC::NEVER },
		{ "samLinearClamp", Filter::MIN_MAG_MIP_LINEAR, Address::CLAMP, Address::CLAMP, ERHI_COMPARISON_FUNC::NEVER },
		{ "samRepeat", Filter::MIN_MAG_MIP_LINEAR, Address::WRAP, Address::WRAP, ERHI_COMPARISON_FUNC::NEVER },
		{ "smp_base_clamp", Filter::ANISOTROPIC, Address::CLAMP, Address::CLAMP, ERHI_COMPARISON_FUNC::NEVER },
	};
	static_assert(std::size(Samplers) == ShaderBindings::SamplerCount);
}

void dx10SamplerStateCache::Create()
{
	Destroy();
	CreateSet(0);
	CreateSet(1);
	Bind();
}

void dx10SamplerStateCache::Destroy()
{
	for (auto& Set : States)
	{
		for (RHIObject*& State : Set)
		{
			_RELEASE(State);
		}
	}
}

void dx10SamplerStateCache::CreateSet(u32 Set)
{
	for (const auto& Sampler : Samplers)
	{
		const bool Anisotropic = Sampler.Mode == Filter::ANISOTROPIC;
		RHISampleDesc Desc = {};
		Desc.Filter = Sampler.Mode;
		Desc.AddressU = Sampler.UV;
		Desc.AddressV = Sampler.UV;
		Desc.AddressW = Sampler.W;
		Desc.MipLODBias = Anisotropic ? MipLodBias : 0.0f;
		Desc.MaxAnisotropy = Anisotropic && Set == 0 ? MaxAnisotropy : 1;
		Desc.ComparisonFunc = Sampler.Compare;
		Desc.MinLOD = -FLT_MAX;
		Desc.MaxLOD = FLT_MAX;

		RHIObject*& State = States[Set][ShaderBindings::Sampler(Sampler.Name)];
		_RELEASE(State);
		CHK_DX(GRHI->CreateSamplerState(Desc, &State));
	}
}

void dx10SamplerStateCache::Bind() const
{
	GRHI->SetSamplers(States[ActiveSet], ShaderBindings::SamplerCount);
}

void dx10SamplerStateCache::SetMaxAnisotropy(u32 MaxAniso)
{
	clamp(MaxAniso, 1u, 16u);
	const u32 Set = MaxAniso == 1;
	const bool Rebuild = !Set && MaxAnisotropy != MaxAniso;
	if (Set == ActiveSet && !Rebuild)
	{
		return;
	}

	ActiveSet = Set;
	if (Rebuild)
	{
		MaxAnisotropy = MaxAniso;
	}
	if (!States[0][0])
	{
		return;
	}
	if (Rebuild)
	{
		CreateSet(0);
	}
	Bind();
}

void dx10SamplerStateCache::SetMipLodBias(float Bias)
{
	if (ps_r_scale_mode > 1)
	{
		Bias += std::min(0.0f, log2(GfxCmdList::GetWidth() / GfxCmdList::GetTargetWidth()) - 1.0f + EPS);
	}
	clamp(Bias, -3.0f, 3.0f);
	if (MipLodBias == Bias)
	{
		return;
	}

	MipLodBias = Bias;
	if (States[0][0])
	{
		CreateSet(0);
		CreateSet(1);
		Bind();
	}
}
