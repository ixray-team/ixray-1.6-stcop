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

// The material features a pass was compiled with, one bit for each define that picks one. The pixel shaders
// get the defines; the GPU scene's material table gets these bits, so the two cannot disagree.
namespace MaterialFeature
{
enum : u32
{
	Pbr = 1u << 0,
	Bump = 1u << 1,
	AlphaTest = 1u << 2,
	Detail = 1u << 3,
	DetailBump = 1u << 4,
	LightmapHemi = 1u << 5,
	TreeWave = 1u << 6,
	Layers = 1u << 7,
	SteepParallax = 1u << 8,
	SpecularMap = 1u << 9,
	SnowMask = 1u << 10,
	HairMask = 1u << 11,
	VertexLit = 1u << 12,
	Placed = 1u << 13,
	Still = 1u << 14,
	Unlit = 1u << 15,
};

inline u32 FromDefines(const ShaderExternalMap& Defines)
{
	static const struct
	{
		const char* Name;
		u32 Bit;
	} Table[] = {
		{ "USE_PBR", Pbr },
		{ "USE_BUMP", Bump },
		{ "USE_AREF", AlphaTest },
		{ "USE_TDETAIL", Detail },
		{ "USE_TDETAIL_BUMP", DetailBump },
		{ "USE_LM_HEMI", LightmapHemi },
		{ "USE_TREEWAVE", TreeWave },
		{ "USE_4_BUMP", Layers },
		{ "USE_STEEPPARALLAX", SteepParallax },
		{ "USE_IOR_TEXTURE", SpecularMap },
		{ "USE_SNOW_TEXTURE", SnowMask },
		{ "USE_HAIRMASK", HairMask },
	};

	u32 Bits = 0;

	for (const auto& Item : Table)
	{
		Bits |= Defines.contains(xr_string(Item.Name)) ? Item.Bit : 0u;
	}

	return Bits;
}
} // namespace MaterialFeature
