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
#include "stdafx.h"
#include "GpuSceneInternal.h"

namespace GpuSceneInternal
{
xr_vector<Hlsl::SceneMaterial> LevelMaterials;

namespace
{
// The level's materials with the head of each streamed map as it is loaded now, a table a frame in flight: the
// resolve reads a head with its record. Read from the heads' table by every sample, it was a wait a map
nri::Buffer* LoadedMaterials[FrameCount] = {};
nri::Descriptor* LoadedViews[FrameCount] = {};
u32 LoadedKeys[FrameCount] = {};

nri::Descriptor* LoadMaterials()
{
	const u32 Slot = RFrame.dwFrame % FrameCount;
	const u64 Size = LevelMaterials.size() * sizeof(Hlsl::SceneMaterial);

	// Made for the level's materials and gone with the level (ReleaseResolve); its view is one of Views
	if (!LoadedMaterials[Slot])
	{
		nri::BufferDesc Desc = {};
		Desc.size = Size;
		Desc.structureStride = sizeof(Hlsl::SceneMaterial);
		Desc.usage = nri::BufferUsageBits::SHADER_RESOURCE;
		Api.CreateCommittedBuffer(*GRHI->Nri->Device, nri::MemoryLocation::HOST_UPLOAD, 0.0f, Desc, LoadedMaterials[Slot]);
		LoadedViews[Slot] = MakeView(LoadedMaterials[Slot], nri::BufferView::STRUCTURED_BUFFER, nri::Format::UNKNOWN, sizeof(Hlsl::SceneMaterial));
		LoadedKeys[Slot] = 0;
	}

	const xr_vector<u32>& Heads = Textures.GetHeads();
	const u32 Key = crc32(Heads.data(), Heads.size() * sizeof(u32)) | 1u;

	if (LoadedKeys[Slot] != Key)
	{
		LoadedKeys[Slot] = Key;
		Hlsl::SceneMaterial* Records = static_cast<Hlsl::SceneMaterial*>(Api.MapBuffer(*LoadedMaterials[Slot], 0, Size));
		std::memcpy(Records, LevelMaterials.data(), Size);

		for (size_t Material = 0; Material < LevelMaterials.size(); ++Material)
		{
			// A map: its tail, its head, its info (SceneMaterial)
			u32* Maps = &Records[Material].base;

			for (u32 Map = 0; Map < SCENE_MATERIAL_MAPS; ++Map)
			{
				if (const u32 Stream = Maps[Map * 4 + 2] >> SCENE_TEXTURE_STREAM_SHIFT)
				{
					Maps[Map * 4 + 1] = Stream - 1 < Heads.size() ? Heads[Stream - 1] : SCENE_TEXTURE_ABSENT;
				}
			}
		}

		Api.UnmapBuffer(*LoadedMaterials[Slot]);
	}

	return LoadedViews[Slot];
}

// The tiles of the screen by what they hold (scene_tiles): a count a list, the arguments of the dispatch that takes
// the list, and the lists
nri::Buffer* TileCounts = nullptr;
nri::Buffer* Tiles = nullptr;
nri::Descriptor* TileViews[3] = {}; // the counts and the lists as they are written, the lists as they are read
bool IsTilesNew = true;
ref_cs TileShaders[2]; // the counts set to none, the tiles listed
ref_cs ClassShaders[SCENE_SHADE_LISTS - 1];
ref_cs LightShader;
// The sky's colour along each axis (scene_sky_tint), made a frame for the light pass
ref_cs TintShader;
nri::Buffer* Tint = nullptr;
nri::Descriptor* TintViews[2] = {}; // as it is written, as it is read
bool IsTintNew = true;
ref_texture Occlusion;
ref_texture SunMask;
ref_texture Sky[2];

// A texture by its own view, or by its surface where it has none: one the frame fills
void ReadTexture(NriBinding* Reads, u32& Count, u32 Slot, CTexture* Texture)
{
	if (nri::Descriptor* View = Texture->BoundView())
	{
		Reads[Count++] = { Slot, nullptr, 0, View };
	}
	else if (Texture->pSurface)
	{
		Reads[Count++] = { Slot, Texture->pSurface };
	}
}

void CreateResolve()
{
	const nri::BufferUsageBits Usage = nri::BufferUsageBits::SHADER_RESOURCE_STORAGE | nri::BufferUsageBits::SHADER_RESOURCE;
	// The arguments of an indirect dispatch: a typed buffer, as D3D11 has no structured one that is also arguments
	TileCounts = CreateBuffer(SCENE_SHADE_LISTS * 3 * sizeof(u32), 0, Usage | nri::BufferUsageBits::ARGUMENT, "Scene shaded tile counts");
	Tiles = CreateBuffer(u64(SCENE_SHADE_LISTS) * SCENE_SHADE_TILES * sizeof(u32), sizeof(u32), Usage, "Scene shaded tiles");
	TileViews[0] = MakeView(TileCounts, nri::BufferView::STORAGE_BUFFER, nri::Format::R32_UINT);
	TileViews[1] = MakeView(Tiles, nri::BufferView::STORAGE_STRUCTURED_BUFFER, nri::Format::UNKNOWN, sizeof(u32));
	TileViews[2] = MakeView(Tiles, nri::BufferView::STRUCTURED_BUFFER, nri::Format::UNKNOWN, sizeof(u32));
	IsTilesNew = true;
	ShaderExternalMap Defines;
	Defines["TILES_RESET"] = "1";
	TileShaders[0] = DEV->_CreateCS("common\\scene_tiles", Defines);
	TileShaders[1] = DEV->_CreateCS("common\\scene_tiles");

	for (u32 Class = 0; Class < std::size(ClassShaders); ++Class)
	{
		string16 Value;
		xr_sprintf(Value, "%u", Class);
		ShaderExternalMap Kind;
		Kind["RESOLVE_CLASS"] = Value;
		ClassShaders[Class] = DEV->_CreateCS("common\\scene_resolve", Kind);
	}

	LightShader = DEV->_CreateCS("common\\scene_light");
	TintShader = DEV->_CreateCS("common\\scene_sky_tint");
	Tint = CreateBuffer(6 * sizeof(Fvector4), sizeof(Fvector4), Usage, "Scene sky tint");
	TintViews[0] = MakeView(Tint, nri::BufferView::STORAGE_STRUCTURED_BUFFER, nri::Format::UNKNOWN, sizeof(Fvector4));
	TintViews[1] = MakeView(Tint, nri::BufferView::STRUCTURED_BUFFER, nri::Format::UNKNOWN, sizeof(Fvector4));
	IsTintNew = true;
	Occlusion = DEV->_CreateTexture(r2_RT_ssao_temp);
	SunMask = DEV->_CreateTexture(r2_sunmask);
	Sky[0] = DEV->_CreateTexture(r2_T_sky0);
	Sky[1] = DEV->_CreateTexture(r2_T_sky1);
}
} // namespace

void ReleaseResolve()
{
	if (!Tiles)
	{
		return;
	}

	Api.DestroyBuffer(TileCounts);
	Api.DestroyBuffer(Tiles);
	Api.DestroyBuffer(Tint);
	Tint = nullptr;

	for (u32 Slot = 0; Slot < FrameCount; ++Slot)
	{
		if (LoadedMaterials[Slot])
		{
			Api.DestroyBuffer(LoadedMaterials[Slot]);
			LoadedMaterials[Slot] = nullptr;
		}
	}
	TileCounts = nullptr;
	Tiles = nullptr;

	for (ref_cs* Shader : { &TileShaders[0], &TileShaders[1], &ClassShaders[0], &ClassShaders[1], &ClassShaders[2], &ClassShaders[3], &LightShader, &TintShader })
	{
		*Shader = nullptr;
	}

	Occlusion = nullptr;
	SunMask = nullptr;
	Sky[0] = nullptr;
	Sky[1] = nullptr;
}
} // namespace GpuSceneInternal

bool GpuScene::IsVisibilityFrame() const
{
	// The views the pass that lights the frame's surfaces can show: what a surface is made of and the light on it. The
	// rest are of the draws themselves (overdraw, clusters, triangles, instances, materials, streaming) or read what
	// only a forward shader has (the lights that reach the pixel, the lightmap, the heightfield): the frame is drawn
	// forward for them, with the same clusters in the same order
	const u32 View = ps_r_debug_view;
	const bool IsShown = View == 0 || (View >= 8 && View <= 11) || (View >= 14 && View <= 16);
	return IsShown && RenderingPath() != 3 && IsForwardBinned() && ForwardOcclusionNormals() && !ps_r_msaa;
}

void GpuScene::Resolve(GfxCmdList& Cmd, IRHISurface* Ids, IRHISurface* Depth, IRHISurface* Surface, IRHISurface* Thin)
{
	PROF_EVENT("SceneResolve");

	if (!Tiles)
	{
		CreateResolve();
	}

	NriPass& Passes = RImplementation.NriPasses;
	u32 Width = Ids->GetWidth();
	u32 Height = Ids->GetHeight();
	ViewSize(Width, Height);
	NriBinding Lists[3] = { { 0, nullptr, 0, TileViews[0], TileCounts, IsTilesNew }, { 1, nullptr, 0, TileViews[1], Tiles, IsTilesNew }, { 2, Thin } };
	Passes.Dispatch(Cmd, TileShaders[0]._get(), nullptr, 0, Lists, 1, 1, 1, 1);
	Lists[0].IsNew = false;
	IsTilesNew = false;
	const NriBinding Listed[2] = { { 0, Ids }, { 1, Surface } };
	// A thread a tile, a group of them a tile across and a tile down
	constexpr u32 Block = SCENE_SHADE_TILE * SCENE_SHADE_TILE;
	Passes.Dispatch(Cmd, TileShaders[1]._get(), Listed, 2, Lists, 3, (Width + Block - 1) / Block, (Height + Block - 1) / Block, 1);

	// The detail texture's fade, as the bins' draws set it
	Cmd.Constants.Write<CBMaterial>().dt_params.w = 1.0f / r__dtex_range;
	CRenderTarget& Frame = *RImplementation.Target;
	constexpr u32 Tables = 9;
	NriBinding Reads[Tables + SCENE_TEXTURE_BUCKETS + 11] = { { 0, Ids }, { 1, Depth }, { 2, nullptr, 0, SceneTableViews[0] }, { 3, nullptr, 0, SceneTableViews[1] }, { 4, nullptr, 0, SceneTableViews[2] }, { 5, nullptr, 0, ClusterIndexView },
		{ 22, nullptr, 0, LoadMaterials() }, { 23, nullptr, 0, TreeLightView }, { 24, nullptr, 0, TileViews[2] } };

	for (u32 Bucket = 0; Bucket < SCENE_TEXTURE_BUCKETS; ++Bucket)
	{
		Reads[Tables + Bucket] = { 6 + Bucket, nullptr, 0, Textures.View(Bucket) };
	}

	u32 ReadCount = Tables + SCENE_TEXTURE_BUCKETS;
	// The rain's map is a slice of the sun's cascades; the decals lie by the tiles of the frame's lights
	Reads[ReadCount++] = { 50, Frame.rt_smap_depth_sun->pSurface };
	ReadTexture(Reads, ReadCount, 54, Frame.t_rain_splash._get());
	ReadTexture(Reads, ReadCount, 55, Frame.t_rain_streaks._get());

	if (Frame.rt_LightTiles && RImplementation.ForwardDecalRecords && RImplementation.DecalArray)
	{
		Reads[ReadCount++] = { 48, Frame.rt_LightTiles->pSurface };
		Reads[ReadCount++] = { 52, nullptr, 0, RImplementation.ForwardDecalRecords };
		Reads[ReadCount++] = { 53, RImplementation.DecalArray };
	}

	if (RImplementation.o.ProceduralRainPuddles)
	{
		ReadTexture(Reads, ReadCount, 31, Frame.t_puddle_normal._get());
		ReadTexture(Reads, ReadCount, 32, Frame.t_puddle_ripples._get());
		ReadTexture(Reads, ReadCount, 33, Frame.t_puddle_water._get());
	}

	if (Frame.SnowInMaterial)
	{
		ReadTexture(Reads, ReadCount, 58, Frame.t_snow_base._get());
		ReadTexture(Reads, ReadCount, 59, Frame.t_snow_bump_x._get());
	}

	const NriBinding Written[2] = { { 0, Surface }, { 1, Thin } };

	// A timer a class: what a class costs shows by name in a GPU trace and in the frame's own profile
	static const char* const Names[] = { "ResolvePlain", "ResolveTerrain", "ResolveTree", "ResolveSteep" };
	static const wchar_t* const WideNames[] = { L"ResolvePlain", L"ResolveTerrain", L"ResolveTree", L"ResolveSteep" };
	static_assert(std::size(Names) == std::size(ClassShaders));

	for (u32 Class = 0; Class < std::size(ClassShaders); ++Class)
	{
		CRHIGPUMark Mark(Names[Class], WideNames[Class]);
		Passes.Dispatch(Cmd, ClassShaders[Class]._get(), Reads, ReadCount, Written, 2, 0, 0, 0, TileCounts, Class * 3 * sizeof(u32));
	}
}

void GpuScene::Light(GfxCmdList& Cmd, IRHISurface* Surface, IRHISurface* Depth, IRHISurface* Thin, IRHISurface* Mirror, IRHISurface* SkyBehind, IRHISurface* Colour)
{
	PROF_EVENT("SceneLight");
	CRenderTarget& Frame = *RImplementation.Target;
	// The registers of the textures the frame's spots project (surface_light.hlsli)
	static const u32 ProjectorSlots[8] = { 36, 38, 39, 41, 42, 43, 44, 45 };
	NriBinding Reads[16 + std::size(ProjectorSlots)] = { { 0, Surface }, { 1, Depth }, { 2, nullptr, 0, TileViews[2] }, { 5, Thin }, { 50, Frame.rt_smap_depth_sun->pSurface }, { 51, Frame.rt_smap_depth_tex },
		{ 56, nullptr, 0, ForwardLightsView() } };
	u32 ReadCount = 7;

	if (Mirror)
	{
		Reads[ReadCount++] = { 8, Mirror };
	}

	if (Frame.rt_LightTiles)
	{
		Reads[ReadCount++] = { 9, Frame.rt_LightTiles->pSurface };
	}

	ReadTexture(Reads, ReadCount, 6, Sky[0]._get());
	ReadTexture(Reads, ReadCount, 7, Sky[1]._get());

	{
		NriBinding Skies[2];
		u32 SkyCount = 0;
		ReadTexture(Skies, SkyCount, 0, Sky[0]._get());
		ReadTexture(Skies, SkyCount, 1, Sky[1]._get());
		const NriBinding Made = { 0, nullptr, 0, TintViews[0], Tint, IsTintNew };
		RImplementation.NriPasses.Dispatch(Cmd, TintShader._get(), Skies, SkyCount, &Made, 1, 1, 1, 1);
		IsTintNew = false;
		Reads[ReadCount++] = { 10, nullptr, 0, TintViews[1] };
	}

	for (u32 Slot = 0; Slot < std::min(RImplementation.SurfaceProjectors.size(), std::size(ProjectorSlots)); ++Slot)
	{
		ReadTexture(Reads, ReadCount, ProjectorSlots[Slot], RImplementation.SurfaceProjectors[Slot]);
	}

	if (Occlusion->pSurface)
	{
		Reads[ReadCount++] = { 3, Occlusion->pSurface };
	}

	if (SkyBehind)
	{
		Reads[ReadCount++] = { 4, SkyBehind };
	}

	if (IRHISurface* Heights = RImplementation.HeightfieldShadows.Shadow())
	{
		Reads[ReadCount++] = { 49, Heights };
	}

	SunMask->EnsureLoaded();
	ReadTexture(Reads, ReadCount, 23, SunMask._get());

	const NriBinding Written = { 0, Colour };
	RImplementation.NriPasses.Dispatch(Cmd, LightShader._get(), Reads, ReadCount, &Written, 1, 0, 0, 0, TileCounts, (SCENE_SHADE_LISTS - 1) * 3 * sizeof(u32));
}
