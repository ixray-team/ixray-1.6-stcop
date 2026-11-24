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

cbuffer SceneGrassBlock : register(b0)
{
	SceneGrassView grass;
}

// The main view, for the depth pyramid a blade of the forward frame is tested on (limits.w)
cbuffer SceneViewBlock : register(b1)
{
	SceneView view;
}

StructuredBuffer<SceneGrassItem> g_Items : register(t0);
Texture2D<float> g_HiZ : register(t1);
Buffer<uint> g_Runs : register(t2); // limits.y == 2: a word a blade, where it is stored and its bin above SCENE_GRASS_STORE_BITS

#include "common\scene_occlusion.hlsli"
RWBuffer<uint> g_Args : register(u0); // the arguments of an instanced indirect draw, one instance per entry; then SCENE_GRASS_ counts; then the tiles
RWBuffer<uint> g_Visible : register(u1); // the entries, two words each

[numthreads(64, 1, 1)]
void main(uint3 Id : SV_DispatchThreadID)
{
	if (Id.x == 0u)
	{
		g_Args[0] = grass.limits.x;
	}

	if (Id.x >= grass.counts.x)
	{
		return;
	}

	uint Index = Id.x;
	uint Low = 0u;

	[branch]
	if (grass.limits.y == 2u)
	{
		uint Run = g_Runs[Id.x];
		Index = Run & ((1u << SCENE_GRASS_STORE_BITS) - 1u);
		Low = Run >> SCENE_GRASS_STORE_BITS;
	}
	else
	{
		// The bins are in the order of their instances: the last one that starts at or before this one
		uint High = grass.counts.y;

		[loop]
		while (High - Low > 1u)
		{
			uint Middle = (Low + High) >> 1;
			if (grass.bins[Middle].first <= Id.x)
			{
				Low = Middle;
			}
			else
			{
				High = Middle;
			}
		}
	}

	SceneGrassBin Bin = grass.bins[Low];

	SceneGrassItem Item = g_Items[Index];
	float Lot;
	float Far;
	float Size = GrassSize(Item.pos, grass.thin, Lot, Far);

	// A list thinned where it was gathered keeps every blade it has, at the size it was given; a stored blade is
	// thinned here, by the lot it was put in order with
	if (grass.limits.y == 2u)
	{
		Size = GrassSizeStored(Item.pos, grass.thin, Lot, Far);
	}
	else if (grass.limits.y != 0u)
	{
		Size = 1.0f;
	}

	if (Size == 0.0f)
	{
		return;
	}

	// A stored blade is as large as it is drawn only with its size
	float Radius = Bin.radius * Item.scale * (grass.limits.y == 2u ? Size : 1.0f);

	for (uint Plane = 0; Plane < grass.counts.z; ++Plane)
	{
		if (dot(grass.planes[Plane].xyz, Item.pos) + grass.planes[Plane].w > Radius)
		{
			return;
		}
	}

	[branch]
	if (grass.limits.w != 0u)
	{
		// The sphere around the root is twice the model: the pyramid is asked about the model's own one
		float4 Turn = float4(Item.quat, sqrt(max(0.0f, 1.0f - dot(Item.quat, Item.quat))));
		float3 Up = float3(2.0f * (Turn.x * Turn.y + Turn.w * Turn.z), 1.0f - 2.0f * (Turn.x * Turn.x + Turn.z * Turn.z), 2.0f * (Turn.y * Turn.z - Turn.w * Turn.x));
		float Own = Radius * Bin.own_radius / Bin.radius;

		if (sphere_occluded(g_HiZ, float4(Item.pos + Up * (Radius - Own), Own)))
		{
			return;
		}

		// The budget of a tile of the screen: every blade adds the share of the tile it covers, and where the sum is
		// more layers than the budget each blade is kept by its lot. A blade larger than a tile is the front of the
		// picture and always stays. A blade is counted into and reads the four tiles around it by its place between
		// their centres, and one about to go shrinks first, so the share kept changes smoothly as the eye moves
		[branch]
		if (grass.budget.x != 0.0f)
		{
			float3 Centre = mul(view.view, float4(Item.pos + Up * (Radius - Own), 1.0f)).xyz;
			float Depth = max(Centre.z, view.hiz.z);
			float Reach = Own / Depth * view.hiz.y * 0.5f * grass.budget.w;

			if (Reach < 0.5f)
			{
				float2 Place = saturate(Centre.xy / Depth * view.hiz.xy * float2(0.5f, -0.5f) + 0.5f) * grass.budget.zw - 0.5f;
				float2 Weight = frac(Place);
				int2 First = int2(floor(Place));
				float Amount = (Reach * Reach * 512.0f + 1.0f) * 16.0f;
				float Count = 0.0f;

				[unroll]
				for (uint Corner = 0u; Corner < 4u; ++Corner)
				{
					uint2 Step = uint2(Corner & 1u, Corner >> 1u);
					uint2 Tile = uint2(clamp(First + int2(Step), int2(0, 0), int2(grass.budget.zw) - 1));
					uint Word = SCENE_GRASS_TILE_BASE + Tile.y * uint(grass.budget.z) + Tile.x;
					float2 Part = lerp(1.0f - Weight, Weight, float2(Step));

					if (grass.budget.y != 0.0f)
					{
						InterlockedAdd(g_Args[Word], uint(Amount * Part.x * Part.y + 0.5f));
					}
					else
					{
						Count += Part.x * Part.y * float(g_Args[Word]);
					}
				}

				float Kept = grass.budget.x * 4096.0f / max(Count, 1.0f);
				float Pick = frac(Lot * 251.0f);

				if (grass.budget.y == 0.0f)
				{
					if (Pick >= Kept)
					{
						return;
					}

					Size *= saturate((Kept - Pick) / (0.25f * Kept));
				}
			}

			if (grass.budget.y != 0.0f)
			{
				return;
			}
		}
	}

	// The far mesh takes over across a band of distances, a blade at a time
	// Every view takes the mesh the main view draws of the blade: a shadow of the far mesh alone was a quarter of a
	// near bush's faces, a sliver beside the bush
	bool IsFar = Bin.far_clusters != 0u && Far > grass.lod.x * (0.75f + 0.5f * frac(Lot * 17.0f));
	// A draw a model and mesh: an indexed draw of the model's own indices, an instance a blade. Its arguments follow
	// the SCENE_GRASS_ words, five words a draw, the far mesh after the whole one; its blades are a word each, the
	// whole meshes in the first half of the list, at the bin's own place
	[branch]
	if (grass.limits.z != 0u)
	{
		uint Args = SCENE_GRASS_WORDS + (Low * 2u + (IsFar ? 1u : 0u)) * 5u;
		uint Blade;
		// A far blade stands for the clump it was thinned from: as many copies as blades the thinning left it alone
		// of, the part of one by the blade's own lot, so the count grows a blade at a time with the distance
		float Share = grass.thin.w * grass.thin.w / max(Far * Far, 1e-4f);
		uint Copies = IsFar ? clamp(uint(1.0f / max(Share, 1.0f / 16.0f) + frac(Lot * 29.0f)), 1u, SCENE_GRASS_COPIES) : 1u;
		g_Args[Args] = (IsFar ? Bin.triangles & 0xFFFFu : Bin.triangles >> 16) * 3u;
		g_Args[Args + 2u] = Bin.index_start;
		InterlockedAdd(g_Args[Args + 1u], Copies, Blade);
		uint First = IsFar ? grass.counts.x + Bin.first * SCENE_GRASS_COPIES : Bin.first;

		for (uint Copy = 0u; Copy < Copies; ++Copy)
		{
			g_Visible[First + Blade + Copy] = Index | (Copy << 20) | (GrassSizeBits(Size) << 24);
		}

		return;
	}

	uint Run = IsFar ? 0u : Bin.far_clusters;
	uint Runs = IsFar ? Bin.far_clusters : Bin.clusters;

	uint Slot;
	InterlockedAdd(g_Args[1], Runs, Slot);

	if (Slot + Runs > grass.counts.w)
	{
		InterlockedAdd(g_Args[1], 0u - Runs);
		return;
	}

	InterlockedAdd(g_Args[SCENE_GRASS_KEPT], 1u);
	InterlockedAdd(g_Args[SCENE_GRASS_FAR], IsFar ? 1u : 0u);
	InterlockedAdd(g_Args[SCENE_GRASS_TRIANGLES], IsFar ? Bin.triangles & 0xFFFFu : Bin.triangles >> 16);

	for (uint Each = 0; Each < Runs; ++Each)
	{
		g_Visible[(Slot + Each) * 2u] = Index | (GrassSizeBits(Size) << 24);
		g_Visible[(Slot + Each) * 2u + 1u] = (Bin.cluster + Run + Each) | ((Run + Each) << 24) | (Bin.variant << 28);
	}
}
