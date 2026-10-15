#pragma once

class CBlender_Compile;

enum class EBlendOp : u8
{
	Pass,				 
	PassFull,			 
	UberDeffer,			 
	Sorting,			 
	Emissive,			 
	Scope,
	ScopePostprocess,
	Distort,
	Wmark,
	Fog,				 
	ZB,					 
	Blend,				 
	IBlend,				 
	Aref,				 
	ColorWrite,			 
	Texture,			 
	Sampler,			 
	Stencil,			 
	StencilRef,			 
	ZFunc,				 
	ShaderOption,		 
	ClearShaderOptions,
	Count
};

struct BlendOp
{
	EBlendOp	Type = EBlendOp::Pass;
	u32			Values[7] = {};
	xr_string	Strings[3];

	bool operator==(const BlendOp& Other) const
	{
		if (Type != Other.Type)
			return false;
		for (u32 Index = 0; Index < 7; ++Index)
		{
			if (Values[Index] != Other.Values[Index])
				return false;
		}
		for (u32 Index = 0; Index < 3; ++Index)
		{
			if (Strings[Index] != Other.Strings[Index])
				return false;
		}
		return true;
	}
};

using BlendOpList = xr_vector<BlendOp>;

namespace BlendOps
{
	struct TextureArgs
	{
		const char* Base = "null";
		const char* Second = "null";
		const char* Detail = "null";
	};

	xr_string ResolveTexture(const char* Texture, const TextureArgs& Args);

	void Apply(CBlender_Compile& C, const BlendOp& Op, bool& IsFirstPass, const TextureArgs& Args);

	const char* GetName(EBlendOp Type);
	bool FindType(const char* Name, EBlendOp& Type);
}
