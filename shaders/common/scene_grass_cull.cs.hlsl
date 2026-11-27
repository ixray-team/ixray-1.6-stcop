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
RWBuffer<uint> g_Visible : register(u1); // the entries, two words each; for draws a model see SCENE_GRASS_LIST

// What the cull left of a blade of a draw a model, a word at the list's end a thread (0: nothing): the fade, the share
// of the far mesh and the clump, each the value both the count and the write take
uint GrassMark(float Fade, float Switch, float Clump)
{
	return max(uint(saturate(Fade) * 255.0f + 0.5f), 1u) | (uint(Switch * 255.0f + 0.5f) << 8) | (uint(Clump * 4096.0f + 0.5f) << 16);
}

void GrassUnmark(uint Mark, out float Fade, out float Switch, out float Clump)
{
	Fade = float(Mark & 255u) * (1.0f / 255.0f);
	Switch = float((Mark >> 8) & 255u) * (1.0f / 255.0f);
	Clump = float(Mark >> 16) * (1.0f / 4096.0f);
}

[numthreads(64, 1, 1)]
void main(uint3 GroupId : SV_GroupID, uint Lane : SV_GroupIndex)
{
	uint Group = GroupId.y * grass.groups.z + GroupId.x;

	if (Group == 0u && Lane == 0u)
	{
		g_Args[0] = grass.limits.x;
	}

	// The draws' places in the list, one after another in the order of the draws, as many blades each as the list
	// has room for: an id names a blade by its place
	[branch]
	if (grass.limits.z == 2u)
	{
		if (Group == 0u && Lane == 0u)
		{
			uint Start = 0u;
			uint Room = min(SCENE_GRASS_ID_SLOTS, grass.counts.w * 5u - grass.counts.x - SCENE_GRASS_LIST);

			for (uint Draw = 0u; Draw < grass.counts.y * 2u; ++Draw)
			{
				uint Count = min(g_Args[SCENE_GRASS_WORDS + Draw * 5u + 1u], Room - Start);
				g_Args[SCENE_GRASS_WORDS + Draw * 5u + 1u] = Count;
				g_Visible[Draw] = Start;
				g_Visible[SCENE_GRASS_BINS * 2u + Draw] = 0u;
				Start += Count;
			}
		}

		return;
	}

	uint Id = Group * 64u + Lane;

	// A group of stored blades goes as one: the sphere around them outside the view, past where the grass ends or
	// hidden in the pyramid is every blade's. Every pass of a view tests it alike, so none reads a mark the cull left
	[branch]
	if (grass.limits.y == 2u)
	{
		uint Record = grass.groups.x + Group * 5u;

		if (Group >= grass.groups.y)
		{
			return;
		}

		float4 Bounds = asfloat(uint4(g_Runs[Record], g_Runs[Record + 1u], g_Runs[Record + 2u], g_Runs[Record + 3u]));
		uint Span = g_Runs[Record + 4u];

		if (Lane > (Span >> 24) || distance(Bounds.xyz, grass.thin.xyz) - Bounds.w > grass.reach.x)
		{
			return;
		}

		for (uint Plane = 0; Plane < grass.counts.z; ++Plane)
		{
			if (dot(grass.planes[Plane].xyz, Bounds.xyz) + grass.planes[Plane].w > Bounds.w)
			{
				return;
			}
		}

		[branch]
		if (grass.limits.w != 0u)
		{
			if (sphere_occluded(g_HiZ, Bounds))
			{
				return;
			}
		}

		Id = (Span & 0xFFFFFFu) + Lane;
	}

	if (Id >= grass.counts.x)
	{
		return;
	}

	uint Index = Id;
	uint Low = 0u;

	[branch]
	if (grass.limits.y == 2u)
	{
		uint Run = g_Runs[Id];
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
			if (grass.bins[Middle].first <= Id)
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
	uint Marked = grass.counts.w * 5u - grass.counts.x + Id;

	if (grass.limits.z == 1u)
	{
		g_Visible[Marked] = 0u;
	}

	// The blades are written from their marks: only those the cull kept read more than a word
	[branch]
	if (grass.limits.z == 3u)
	{
		uint Mark = g_Visible[Marked];

		if (Mark == 0u)
		{
			return;
		}

		float Fade;
		float Switch;
		float Clump;
		GrassUnmark(Mark, Fade, Switch, Clump);

		for (uint Mesh = 0u; Mesh < 2u; ++Mesh)
		{
			bool IsFar = Mesh != 0u;
			float Part = IsFar ? Switch : 1.0f - Switch;

			if (Part <= 0.0f)
			{
				continue;
			}

			uint Draw = Low * 2u + Mesh;
			uint Copies = IsFar ? uint(ceil(Clump - 1e-3f)) : 1u;
			uint Blade;
			InterlockedAdd(g_Visible[SCENE_GRASS_BINS * 2u + Draw], Copies, Blade);
			uint Kept = g_Args[SCENE_GRASS_WORDS + Draw * 5u + 1u];
			uint First = SCENE_GRASS_LIST + g_Visible[Draw] + Blade;

			for (uint Copy = 0u; Copy < Copies && Blade + Copy < Kept; ++Copy)
			{
				g_Visible[First + Copy] = Index | (Copy << 20) | GrassFadeBits(Fade * Part * saturate(Clump - float(Copy)), IsFar);
			}
		}

		return;
	}

	SceneGrassItem Item = g_Items[Index];
	float Lot;
	float Far;
	float Fade = GrassFade(Item.pos, grass.thin, Lot, Far);

	// A list thinned where it was gathered keeps every blade it has; a stored blade is thinned here, by the lot it
	// was put in order with
	if (grass.limits.y == 2u)
	{
		Fade = GrassFadeStored(Item.pos, grass.thin, Lot, Far);
	}
	else if (grass.limits.y != 0u)
	{
		Fade = 1.0f;
	}

	// Where the grass ends it dissolves, not at the edge of a slot
	Fade *= saturate((grass.reach.x - Far) * grass.reach.y);

	// A shadow view keeps every blade within 8 m of the eye and fewer farther out, a quarter from 16 m, each going by
	// its lot: what the cards cover in the shadow map is what its grass costs
	if (grass.limits.z == 0u)
	{
		float Share = clamp(64.0f / max(Far * Far, 1e-4f), 0.25f, 1.0f);
		Fade *= saturate((Share * 1.25f - Lot) / (0.25f * Share));
	}

	if (Fade <= 0.0f)
	{
		return;
	}

	float Radius = Bin.radius * Item.scale;

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

		// The budget of a tile of the screen: every blade adds the share of the tile it covers, and where the sum the
		// frame before left is more layers than the budget each blade is kept by its lot. A blade larger than a tile is
		// the front of the picture and always stays. A blade is counted into and reads the four tiles around it by its
		// place between their centres, and one about to go dissolves first, so the share kept changes smoothly as the
		// eye moves. The frames count into the two halves of the tiles in turn: budget.y, the one this frame writes
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
					InterlockedAdd(g_Args[Word + uint(grass.budget.y) * SCENE_GRASS_TILES], uint(Amount * Part.x * Part.y + 0.5f));
					Count += Part.x * Part.y * float(g_Args[Word + (1u - uint(grass.budget.y)) * SCENE_GRASS_TILES]);
				}

				float Kept = grass.budget.x * 4096.0f / max(Count, 1.0f);
				float Pick = frac(Lot * 251.0f);

				if (Pick >= Kept)
				{
					return;
				}

				Fade *= saturate((Kept - Pick) / (0.25f * Kept));
			}
		}
	}

	// The far mesh takes over across a band of distances, a blade at a time, each over a short stretch of its own
	// where both meshes are drawn and the far one keeps the pixels the whole one lets go
	// Every view takes the mesh the main view draws of the blade: a shadow of the far mesh alone was a quarter of a
	// near bush's faces, a sliver beside the bush
	float Switch = Bin.far_clusters != 0u ? saturate((Far / grass.lod.x - 0.75f - 0.5f * frac(Lot * 17.0f)) * 10.0f + 0.5f) : 0.0f;
	// A draw a model and mesh: an indexed draw of the model's own indices, an instance a blade. Its arguments follow
	// the SCENE_GRASS_ words, five words a draw, the far mesh after the whole one; its blades are a word each, at the
	// draw's place in the list. The blades are culled and counted (limits.z 1), the places given out, and the blades
	// the cull marked written (3)
	[branch]
	if (grass.limits.z != 0u)
	{
		// A far blade stands for the clump it was thinned from: a copy for every blade the thinning left it alone of,
		// and for the area its far mesh leaves out of the whole one, the last copy dissolving in, so the ground keeps
		// its cover and the clump grows smoothly with the distance
		float Share = clamp(grass.thin.w * grass.thin.w / max(Far * Far, 1e-4f), 1.0f / 16.0f, 1.0f);
		float Cover = float(Bin.variant >> 16) * (1.0f / 65535.0f);
		float Clump = clamp(1.0f / max(Share * Cover, 1e-4f), 1.0f, float(SCENE_GRASS_COPIES));
		uint Mark = GrassMark(Fade, Switch, Clump);
		g_Visible[Marked] = Mark;
		GrassUnmark(Mark, Fade, Switch, Clump);

		for (uint Mesh = 0u; Mesh < 2u; ++Mesh)
		{
			bool IsFar = Mesh != 0u;
			float Part = IsFar ? Switch : 1.0f - Switch;

			if (Part <= 0.0f)
			{
				continue;
			}

			uint Args = SCENE_GRASS_WORDS + (Low * 2u + Mesh) * 5u;
			g_Args[Args] = (IsFar ? Bin.triangles & 0xFFFFu : Bin.triangles >> 16) * 3u;
			g_Args[Args + 2u] = Bin.index_start;
			InterlockedAdd(g_Args[Args + 1u], IsFar ? uint(ceil(Clump - 1e-3f)) : 1u);
		}

		return;
	}

	bool IsFar = Switch >= 0.5f;
	uint Run = IsFar ? 0u : Bin.far_clusters;
	uint Runs = IsFar ? Bin.far_clusters : Bin.clusters;

	uint Slot = 0u;

	// Every blade a shadow view keeps asks the same word for its entries: a wave asks once for all of its blades
#if __SHADER_TARGET_MAJOR >= 6
	uint Before = WavePrefixSum(Runs);
	uint Need = WaveActiveSum(Runs);

	if (WaveIsFirstLane())
	{
		InterlockedAdd(g_Args[1], Need, Slot);
	}

	Slot = WaveReadLaneFirst(Slot) + Before;
#else
	InterlockedAdd(g_Args[1], Runs, Slot);
#endif

	if (Slot + Runs > grass.counts.w)
	{
		InterlockedAdd(g_Args[1], 0u - Runs);
		return;
	}

	for (uint Each = 0; Each < Runs; ++Each)
	{
		g_Visible[(Slot + Each) * 2u] = Index | GrassFadeBits(Fade, false);
		g_Visible[(Slot + Each) * 2u + 1u] = (Bin.cluster + Run + Each) | ((Run + Each) << 24) | ((Bin.variant & 0xFFFFu) << 28);
	}
}
