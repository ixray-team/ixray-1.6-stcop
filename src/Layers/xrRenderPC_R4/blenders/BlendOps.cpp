#include "stdafx.h"
#include "BlendOps.h"
#include "Blender_Recorder.h"
#include "../uber_deffer.h"

namespace
{
	const char* const OpNames[] =
	{
		"pass",
		"pass_full",
		"uber_deffer",
		"sorting",
		"emissive",
		"scope",
		"scope_postprocess",
		"distort",
		"wmark",
		"fog",
		"zb",
		"blend",
		"iblend",
		"aref",
		"color_write",
		"texture",
		"sampler",
		"stencil",
		"stencil_ref",
		"zfunc",
		"shader_option",
		"clear_shader_options",
	};
	static_assert(std::size(OpNames) == size_t(EBlendOp::Count));

	void ReplaceAll(xr_string& Text, const char* Pattern, const char* Value)
	{
		const size_t PatternLength = xr_strlen(Pattern);
		const size_t ValueLength = xr_strlen(Value);
		for (size_t Position = Text.find(Pattern); Position != xr_string::npos; Position = Text.find(Pattern, Position + ValueLength))
		{
			Text.replace(Position, PatternLength, Value);
		}
	}

	void BeginPass(CBlender_Compile& C, bool& IsFirstPass)
	{
		if (!IsFirstPass)
			C.r_End();
		IsFirstPass = false;
	}

	const char* GeometryShader(const BlendOp& Op)
	{
		return Op.Strings[1].empty() ? nullptr : Op.Strings[1].c_str();
	}
}

xr_string BlendOps::ResolveTexture(const char* Texture, const TextureArgs& Args)
{
	xr_string Result = Texture;
	if (Result.find('{') == xr_string::npos)
		return Result;

	ReplaceAll(Result, "{base}", Args.Base);
	ReplaceAll(Result, "{second}", Args.Second);
	ReplaceAll(Result, "{detail}", Args.Detail);
	return Result;
}

void BlendOps::Apply(CBlender_Compile& C, const BlendOp& Op, bool& IsFirstPass, const TextureArgs& Args)
{
	const u32* V = Op.Values;
	switch (Op.Type)
	{
	case EBlendOp::Pass:
		BeginPass(C, IsFirstPass);
		if (const char* GS = GeometryShader(Op))
			C.r_Pass(Op.Strings[0].c_str(), GS, Op.Strings[2].c_str(), true);
		else
			C.r_Pass(Op.Strings[0].c_str(), Op.Strings[2].c_str(), true);
		break;

	case EBlendOp::PassFull:
		BeginPass(C, IsFirstPass);
		if (const char* GS = GeometryShader(Op))
			C.r_Pass(Op.Strings[0].c_str(), GS, Op.Strings[2].c_str(), !!V[0], !!V[1], !!V[2]);
		else
			C.r_Pass(Op.Strings[0].c_str(), Op.Strings[2].c_str(), !!V[0], !!V[1], !!V[2]);
		break;

	case EBlendOp::UberDeffer:
		uber_deffer(C, true, nullptr, nullptr, false, nullptr, true, true);
		break;

	case EBlendOp::Sorting:				C.SetParams(int(V[0]), !!V[1]);							break;
	case EBlendOp::Emissive:			C.SH->flags.bEmissive = !!V[0];							break;
	case EBlendOp::Scope:				C.SH->flags.bScopeMask = !!V[0];						break;
	case EBlendOp::ScopePostprocess:	C.SH->flags.ScopePostprocess = !!V[0];					break;
	case EBlendOp::Distort:				C.SH->flags.bDistort = !!V[0];							break;
	case EBlendOp::Wmark:				C.SH->flags.bWmark = !!V[0];							break;
	case EBlendOp::Fog:					C.PassSET_LightFog(false, !!V[0]);						break;
	case EBlendOp::ZB:					C.PassSET_ZB(!!V[0], !!V[1]);							break;
	case EBlendOp::Blend:				C.PassSET_ablend_mode(!!V[0], V[1], V[2]);				break;
	case EBlendOp::IBlend:				C.PassSET_ablend_mode(V[0], !!V[1], V[2], V[3]);		break;
	case EBlendOp::Aref:				C.PassSET_ablend_aref(!!V[0], V[1]);					break;
	case EBlendOp::ColorWrite:			C.r_ColorWriteEnable(!!V[0], !!V[1], !!V[2], !!V[3]);	break;
	case EBlendOp::Stencil:				C.r_Stencil(!!V[0], V[1], V[2], V[3], V[4], V[5], V[6]);	break;
	case EBlendOp::StencilRef:			C.r_StencilRef(V[0]);									break;
	case EBlendOp::ZFunc:				C.RS.SetRS(D3DRS_ZFUNC, V[0]);							break;
	case EBlendOp::Sampler:				C.r_dx10Sampler(Op.Strings[0].c_str());					break;
	case EBlendOp::ShaderOption:		RImplementation.addShaderOption(Op.Strings[0].c_str(), Op.Strings[1].c_str());	break;
	case EBlendOp::ClearShaderOptions:	RImplementation.clearAllShaderOptions();				break;

	case EBlendOp::Texture:
	{
		const xr_string Texture = ResolveTexture(Op.Strings[1].c_str(), Args);
		C.r_dx10Texture(Op.Strings[0].c_str(), Texture.c_str());
		break;
	}

	default:
		FATAL("Unknown blend operation");
		break;
	}
}

const char* BlendOps::GetName(EBlendOp Type)
{
	return OpNames[u32(Type)];
}

bool BlendOps::FindType(const char* Name, EBlendOp& Type)
{
	for (u32 Index = 0; Index < u32(EBlendOp::Count); ++Index)
	{
		if (0 == xr_strcmp(OpNames[Index], Name))
		{
			Type = EBlendOp(Index);
			return true;
		}
	}
	return false;
}
