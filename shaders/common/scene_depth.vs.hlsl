/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Shaders
***************************************************************************************
* Anyone may use, copy, modify and distribute this shader file, changed or
* unchanged, as part of the IX-Ray engine, its games and their modifications.
* It may not be sold on its own.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of this file.
**************************************************************************************/
#include "common\scene_tables.hlsli"

#include "common\scene_depth.hlsli"

#ifdef USE_GRASS
float2 calc_xz_wave(float2 dir2D, float frac)
{
	return dir2D * frac;
}

#include "common\detail_sway.hlsli"

Output main(uint Corner : SV_VertexID, uint Slot : SV_InstanceID)
{
	uint2 Entry = uint2(g_Visible[Slot * 2u], g_Visible[Slot * 2u + 1u]);
	Output O = (Output)0;
	O.hpos = float4(2.0f, 2.0f, 2.0f, 1.0f);
	SceneCluster C = g_Clusters[Entry.y & 0xFFFFFFu];

	[branch]
	if (Corner >= (C.triangles & SCENE_CLUSTER_COUNT_MASK) * 3u)
	{
		return O;
	}

	SceneVertex V = LoadVertex(C.vertices >> 24, (C.vertices & 0xFFFFFFu) + g_ClusterIndices[C.index_start + Corner]);
	SceneGrassItem Item = g_Items[Entry.x & (SCENE_GRASS_ITEMS - 1u)];
	float Scale = Item.scale;
	uint Variant = Entry.y >> 28;
	uint Waves = (max(Variant, 1u) - 1u) * 4u;

	float3 World = mul(QuaternionToMatrix(float4(Item.quat, sqrt(max(0.0f, 1.0f - dot(Item.quat, Item.quat))))), V.P * Scale) + Item.pos;
	float3 P = World;
	float3 Old = World;
	DetailSway(grass.wind, grass.waves[Waves], grass.waves[Waves + 1u], grass.waves[Waves + 2u], grass.waves[Waves + 3u], Variant == 0u, V.P, float(asint(V.X << 16) >> 16) * (1.0f / 2048.0f), Scale, Item.pos, Item.trample_visual, Item.trample_dir, World, P, Old);

	O.hpos = mul(grass.view_projection, float4(P, 1.0f));
	O.hpos.xy += grass.lod.zw * O.hpos.w;
	int2 Q = (asint(uint2(V.UV << 16, V.UV)) >> 16 << 8) | int2((V.TF.xx >> uint2(16, 24)) & 0xFFu);
	O.uv = float2(Q) * (1.0f / 262144.0f);
	O.alpha_slice = g_Materials[g_Instances[C.instance].material].alpha;
	O.fade = float2(GrassEntryFade(Entry.x), GrassPixelOffset(Item.pos, 0u));
	return O;
}
#else
Output main(uint Corner : SV_VertexID, uint Slot : SV_InstanceID)
{
	uint Entry = g_Visible[Slot];
	Output O = (Output)0;
	// The unused tail of a cluster: a triangle outside the view
	O.hpos = float4(2.0f, 2.0f, 2.0f, 1.0f);

	[branch]
	if (Entry & SCENE_VISIBLE_DYNAMIC)
	{
		SceneModel D = g_Models[view.second.z + (Entry & (SCENE_MODELS - 1u))];
#ifdef USE_TILES
		uint Run = (Entry >> SCENE_VISIBLE_MODEL_SHIFT) & SCENE_DYNAMIC_RUNS;
#else
		uint Run = (Entry >> SCENE_VISIBLE_MODEL_SHIFT) & SCENE_MODEL_RUNS;
#endif
		uint Offset = Run * SCENE_CLUSTER_TRIANGLES * 3u + Corner;

		[branch]
		if (Offset < D.triangle_count * 3u)
		{
			uint Page = D.source & SCENE_DYNAMIC_PAGE_MASK;
			uint Index = D.vertices + LoadIndex(Page, D.index_start + Offset);
			SceneVertex V;

			[branch]
			if (D.source & SCENE_DYNAMIC_SKINNED)
			{
				V = g_SkinnedVertices[Index];
			}
			else
			{
				V = LoadVertex(Page, Index);
			}

			float3 Local = V.P;

#ifdef USE_TILES
			// The run is drawn for one tile of the target: clipped to it, then moved into it
			uint Tile = (Entry >> SCENE_VISIBLE_TILE_SHIFT) & (SCENE_VIEW_TILES - 1u);
			float4 H = mul(view.tile_views[Tile], float4(mul(D.world, float4(Local, 1.0f)), 1.0f));
			O.clip = float4(H.w + H.x, H.w - H.x, H.w + H.y, H.w - H.y);
			H.xy = H.xy * view.tile_rects[Tile].xy + H.w * view.tile_rects[Tile].zw;
			O.hpos = H;
#else
			O.hpos = mul(view.view_projection, float4(mul(D.world, float4(Local, 1.0f)), 1.0f));
#endif
#ifdef USE_AREF
			int2 Q = (asint(uint2(V.UV << 16, V.UV)) >> 16 << 8) | int2((V.TF.xx >> uint2(16, 24)) & 0xFFu);
			O.uv = float2(Q) * (1.0f / 262144.0f);
			O.alpha_slice = D.alpha;
#endif
		}

		return O;
	}

	SceneCluster C = g_Clusters[Entry];

	[branch]
	if (Corner >= (C.triangles & SCENE_CLUSTER_COUNT_MASK) * 3u)
	{
		return O;
	}

	uint Vertex = scene_page_corner(C.index_start, Corner);
	float3 P = scene_page_position(C.vertices, Vertex);
	uint Transform = (C.triangles >> SCENE_CLUSTER_TRANSFORM_SHIFT) & SCENE_CLUSTER_TRANSFORM_MASK;

	[branch]
	if (Transform)
	{
		float3x4 World = g_Transforms[Transform].world;
		P = mul(World, float4(P, 1.0f));

		[branch]
		if (C.triangles & SCENE_CLUSTER_WAVE)
		{
			float Sway = (P.y - World._24) * calc_cyclic(view.wave.w + dot(P, view.wave.xyz));
			float Weight = float(asint(scene_page_baked(C.vertices, Vertex) << 16) >> 16) * view.wind.w;
			P.xz += view.wind.xz * (Sway * Weight);
		}
	}

	O.hpos = mul(view.view_projection, float4(P, 1.0f));
#ifdef USE_AREF
	O.uv = scene_page_uv(C.vertices, Vertex);
	O.alpha_slice = g_Materials[g_Instances[C.instance].material].alpha;
#endif
	return O;
}
#endif
