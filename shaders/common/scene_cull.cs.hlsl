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

cbuffer SceneViewBlock : register(b0)
{
	SceneView view;
}

StructuredBuffer<SceneInstance> g_Instances : register(t0);
StructuredBuffer<SceneCluster> g_Clusters : register(t1);
StructuredBuffer<SceneMaterial> g_Materials : register(t2);
StructuredBuffer<SceneTreeLight> g_TreeLights : register(t3);
Texture2D<float> g_HiZ : register(t4);
Buffer<uint> g_Chunks : register(t6); // of scene_ranges: the clusters the view may see, of the detail it draws
Buffer<float> g_Density : register(t7); // of scene_density: uv units a cluster lays over a metre
StructuredBuffer<SceneModel> g_Models : register(t8); // of a view with bins: the frame's models
RWBuffer<uint> g_Counters : register(u0);
RWBuffer<uint> g_Visible : register(u1); // draw instance -> cluster
RWBuffer<uint> g_VisibleAlpha : register(u2);
RWBuffer<uint> g_Seen : register(u3); // per cluster: the main view showed it the frame before
RWBuffer<uint> g_Feedback : register(u4); // per streamed texture: the frame its head was last asked for << 8, and how much it was needed that frame

#include "common\scene_occlusion.hlsli"

// Reach: the texels of a texture's tail a pixel takes, over what it may take before the head is needed
void AskHead(uint Info, float Reach)
{
	uint Stream = Info >> SCENE_TEXTURE_STREAM_SHIFT;

	if (Stream != 0u && Reach < 1.0f)
	{
		// How much: the levels of the head the nearest pixel takes, four of them being all there is to ask. The
		// latest frame wins, and in it the cluster that needs the head most
		uint Need = 1u + uint(saturate(-log2(max(Reach, 1e-4f)) * 0.25f) * 254.0f);
		InterlockedMax(g_Feedback[Stream - 1u], (asuint(view.thresholds.z) << 8) | Need);
	}
}

// A cluster the main view shows asks for the heads of its material's textures: the pixels of the forward frame ask
// for nothing, a store in their shader cost more than the whole of this pass
void AskMaterial(uint Material, float Reach)
{
	SceneMaterial M = g_Materials[Material];
	float Detail = Reach * M.detail_scale;
	AskHead(M.base_info, Reach);
	AskHead(M.bump_info, Reach);
	AskHead(M.bump_x_info, Reach);
	AskHead(M.specular_info, Reach);
	AskHead(M.detail_info, Detail);
	AskHead(M.detail_bump_info, Detail);
	AskHead(M.detail_bump_x_info, Detail);
	// The baked light has its own layout: asked for wherever it is seen
	AskHead(M.hemi_info, 0.0f);
	AskHead(M.lightmap_info, 0.0f);
	AskHead(M.terrain[0].z, Reach);

	for (uint Map = 1u; Map < SCENE_MATERIAL_MAPS - SCENE_MATERIAL_PLAIN_MAPS; ++Map)
	{
		AskHead(M.terrain[Map].z, Detail);
	}
}

void AskHeads(uint Material, uint Index, SceneCluster C)
{
	AskMaterial(Material, g_Density[Index] * max(distance(C.bounds.xyz, view.camera.xyz) - C.bounds.w, 0.0f) * view.thresholds.y);
}

bool IsInside(float4 Bounds)
{
	for (uint Plane = 0; Plane < view.counts.z; ++Plane)
	{
		if (dot(view.planes[Plane].xyz, Bounds.xyz) + view.planes[Plane].w > Bounds.w)
		{
			return false;
		}
	}

	return true;
}

bool IsSectorSeen(uint Sector)
{
	return Sector >= SCENE_VIEW_SECTORS || (view.sectors[Sector >> 7][(Sector >> 5) & 3u] & (1u << (Sector & 31u))) != 0u;
}

bool IsTileSector(uint Tile, uint Sector)
{
	return Sector >= SCENE_VIEW_SECTORS || (view.tile_sectors[Tile * 2u + (Sector >> 7)][(Sector >> 5) & 3u] & (1u << (Sector & 31u))) != 0u;
}

// Inside the tile's frustum but its near plane, as the lamp part's frustum on the CPU
bool IsInTile(uint Tile, float4 Bounds)
{
	float4x4 M = view.tile_views[Tile];
	float4 Planes[5] = { M[3] + M[0], M[3] - M[0], M[3] + M[1], M[3] - M[1], M[3] - M[2] };

	for (uint Plane = 0; Plane < 5u; ++Plane)
	{
		if (dot(Planes[Plane].xyz, Bounds.xyz) + Planes[Plane].w < -Bounds.w * length(Planes[Plane].xyz))
		{
			return false;
		}
	}

	return true;
}

// returns true for a model the frame before could not see: its sphere wholly on that frame's screen, with everything
// that frame's pyramid holds over it nearer. view.view and view.hiz are that frame's here
bool IsModelHidden(float4 Bounds)
{
	float3 Centre = mul(view.view, float4(Bounds.xyz, 1.0f)).xyz;
	float Nearest = Centre.z - Bounds.w;

	// At the screen's edge, or past it, that frame says nothing of it
	if (Nearest <= view.hiz.z || any((abs(Centre.xy) + Bounds.w) * view.hiz.xy > Nearest))
	{
		return false;
	}

	return sphere_occluded(g_HiZ, Bounds);
}

// A model is tested by the frustum, and by the pyramid the frame before left, in the main view's first part: it is
// drawn first and hides the rest. Every
// run of it goes to its draw, and its textures want their heads. A part of the HUD stands in a space of its own, in
// front of everything: it is always drawn
void AddModel(uint Model)
{
	SceneModel M = g_Models[Model];

	if (!(M.source & SCENE_MODEL_HUD) && (!IsInside(M.bounds) || (view.hiz.w > 0.0f && IsModelHidden(M.bounds))))
	{
		return;
	}

	uint Runs = (M.triangle_count + SCENE_CLUSTER_TRIANGLES - 1u) / SCENE_CLUSTER_TRIANGLES;
	uint Args = SCENE_COUNTER_WORDS + M.bin * 4u;
	uint First;
	g_Counters[Args + SCENE_COUNTER_VERTICES] = SCENE_CLUSTER_TRIANGLES * 3u;
	InterlockedAdd(g_Counters[Args + SCENE_COUNTER_CLUSTERS], Runs, First);

	for (uint Run = 0; Run < Runs; ++Run)
	{
		g_Visible[M.visible_start + First + Run] = SCENE_VISIBLE_DYNAMIC | (Run << SCENE_VISIBLE_MODEL_SHIFT) | Model;
	}

	AskMaterial(M.material, 0.0f);
}

// A moving caster of a shadow view: a record the view casts by its flags, in a sector the view reached, of an object
// other than the one the view leaves out, inside the view's planes. Its runs go to the list of what is cut out by its
// texture or to that of the rest, after the level's clusters there
void AddCaster(uint Model)
{
	SceneModel M = g_Models[view.second.z + Model];
	bool IsCast = (M.flags & SCENE_OBJECT_CASTER) && (!(M.flags & SCENE_OBJECT_TRANSLUCENT) || (view.counts.y & SCENE_VIEW_TRANSLUCENT)) &&
		(M.object == SCENE_OBJECT_NONE || M.object != view.second.w) && IsSectorSeen(M.sector);

	if (!IsCast || !IsInside(M.bounds))
	{
		return;
	}

	uint Runs = (M.triangle_count + SCENE_CLUSTER_TRIANGLES - 1u) / SCENE_CLUSTER_TRIANGLES;
	bool IsAlpha = M.alpha != SCENE_TEXTURE_ABSENT;
	uint Counter = (IsAlpha ? SCENE_COUNTER_ALPHA : 0u) + SCENE_COUNTER_CLUSTERS;
	uint First;
	InterlockedAdd(g_Counters[Counter], Runs, First);

	if (First + Runs > view.counts.x + SCENE_DYNAMIC_CLUSTERS)
	{
		InterlockedAdd(g_Counters[Counter], 0u - Runs);
		return;
	}

	for (uint Run = 0; Run < Runs; ++Run)
	{
		uint Entry = SCENE_VISIBLE_DYNAMIC | (Run << SCENE_VISIBLE_MODEL_SHIFT) | Model;

		[branch]
		if (IsAlpha)
		{
			g_VisibleAlpha[First + Run] = Entry;
		}
		else
		{
			g_Visible[First + Run] = Entry;
		}
	}
}

// A moving caster of the table for the lamp parts of one target: its runs go to the lists once for each tile that casts it,
// the tile in the entry
void AddTiles(uint Model)
{
	SceneModel M = g_Models[view.second.z + Model];

	if (!(M.flags & SCENE_OBJECT_CASTER) || ((M.flags & SCENE_OBJECT_TRANSLUCENT) && !(view.counts.y & SCENE_VIEW_TRANSLUCENT)))
	{
		return;
	}

	uint Runs = (M.triangle_count + SCENE_CLUSTER_TRIANGLES - 1u) / SCENE_CLUSTER_TRIANGLES;
	bool IsAlpha = M.alpha != SCENE_TEXTURE_ABSENT;
	uint Counter = (IsAlpha ? SCENE_COUNTER_ALPHA : 0u) + SCENE_COUNTER_CLUSTERS;

	for (uint Tile = 0; Tile < view.second.y; ++Tile)
	{
		uint2 Object = view.tile_objects[Tile].xy;
		bool IsCast = Object.y != 0u ? M.object == Object.x : M.object != Object.x || M.object == SCENE_OBJECT_NONE;

		if (!IsCast || !IsTileSector(Tile, M.sector) || !IsInTile(Tile, M.bounds))
		{
			continue;
		}

		uint First;
		InterlockedAdd(g_Counters[Counter], Runs, First);

		if (First + Runs > SCENE_DYNAMIC_CLUSTERS)
		{
			InterlockedAdd(g_Counters[Counter], 0u - Runs);
			return;
		}

		for (uint Run = 0; Run < Runs; ++Run)
		{
			uint Entry = SCENE_VISIBLE_DYNAMIC | (Tile << SCENE_VISIBLE_TILE_SHIFT) | (Run << SCENE_VISIBLE_MODEL_SHIFT) | Model;

			[branch]
			if (IsAlpha)
			{
				g_VisibleAlpha[First + Run] = Entry;
			}
			else
			{
				g_Visible[First + Run] = Entry;
			}
		}
	}
}

bool IsVisible(SceneCluster C)
{
	if (!IsInside(C.bounds))
	{
		return false;
	}

	SceneInstance I = g_Instances[C.instance];
	if (!IsSectorSeen(I.sector))
	{
		return false;
	}

	if ((I.flags & SCENE_INSTANCE_TRANSLUCENT) && !(view.counts.y & SCENE_VIEW_TRANSLUCENT))
	{
		return false;
	}

	if (view.counts.y & SCENE_VIEW_KEPT)
	{
		return true;
	}

	uint Transform = (C.triangles >> SCENE_CLUSTER_TRANSFORM_SHIFT) & SCENE_CLUSTER_TRANSFORM_MASK;

	[branch]
	if (Transform != 0u && view.lod.x > 0.0f)
	{
		float4 Group = g_TreeLights[Transform].lod;
		float3 Away = Group.xyz - view.camera.xyz;

		if (Group.w > 0.0f && Group.w / (dot(Away, Away) + 0.00001f) <= view.lod.x)
		{
			return false;
		}
	}

	float R = I.bounds.w;
	float3 Offset = I.bounds.xyz - view.camera.xyz;
	float DistanceSq = dot(Offset, Offset) + 0.00001f;
	float Fog = view.camera.w + R;

	if (R / DistanceSq <= view.thresholds.x || DistanceSq > Fog * Fog)
	{
		return false;
	}

	if (!(I.flags & SCENE_INSTANCE_KEEP))
	{
		float Volume = 4.1887902f * R * R * R;
		float Distance = sqrt(DistanceSq);

		for (uint Step = 0; Step < view.counts.w; ++Step)
		{
			if (Volume < view.steps[Step].x && Distance > view.steps[Step].y)
			{
				return false;
			}
		}
	}

	return true;
}

[numthreads(SCENE_CHUNK_CLUSTERS, 1, 1)]
void main(uint3 Thread : SV_DispatchThreadID)
{
	// Whichever pass of the view comes first: a view may have nothing but what is cut out
	if (Thread.x == 0u)
	{
		g_Counters[SCENE_COUNTER_VERTICES] = SCENE_CLUSTER_TRIANGLES * 3u;
		g_Counters[SCENE_COUNTER_ALPHA + SCENE_COUNTER_VERTICES] = SCENE_CLUSTER_TRIANGLES * 3u;
		// The solid list as meshlets is dispatched by the three words from its count: a group an entry, one deep. As a
		// draw's first vertex and instance the two change nothing, no stage reads a vertex buffer
		g_Counters[SCENE_COUNTER_CLUSTERS + 1u] = 1u;
		g_Counters[SCENE_COUNTER_CLUSTERS + 2u] = 1u;
	}

	// A pass of a band takes the list of what is cut out by its texture, the one pass before them the other list
	bool IsCut = view.thresholds.w >= 0.0f;

	if (Thread.x >= g_Chunks[IsCut ? 1u : 0u])
	{
		return;
	}

	uint Size;
	g_Chunks.GetDimensions(Size);
	uint Chunk = g_Chunks[IsCut ? Size - 1u - Thread.x : 2u + Thread.x];
	uint3 Id = uint3(Chunk & ((1u << SCENE_CHUNK_SHIFT) - 1u), 0u, 0u);

	if (Chunk & SCENE_CHUNK_HIDDEN)
	{
		g_Seen[Id.x] = 0u;
		return;
	}

	if (Id.x >= view.counts.x)
	{
		uint Mesh = Id.x - view.counts.x;

		[branch]
		if (view.counts.y & SCENE_VIEW_BINS)
		{
			AddModel(Mesh);
		}
		else if (view.counts.y & SCENE_VIEW_TILED)
		{
			AddTiles(Mesh);
		}
		else
		{
			AddCaster(Mesh);
		}

		return;
	}

	SceneCluster C = g_Clusters[Id.x];

	// One pass per band of distance, the nearest first: what is cut out by its texture comes out near to far, and
	// the depth of the near leaves rejects the far ones. What is solid gains nothing by it: one pass takes it all
	if (IsCut)
	{
		float Band = 0.0f;

		if (C.triangles & SCENE_CLUSTER_ALPHA)
		{
			float Near = length(max(abs(C.bounds.xyz - view.camera.xyz) - C.extent.xyz, 0.0f));
			Band = float(Near >= SCENE_BAND_FIRST) + float(Near >= SCENE_BAND_SECOND) + float(Near >= SCENE_BAND_THIRD);
		}

		if (Band != view.thresholds.w)
		{
			return;
		}
	}

	uint Wanted = (view.counts.y & SCENE_VIEW_MAIN) ? SCENE_CLUSTER_MAIN : SCENE_CLUSTER_DEPTH;

	// The frustum first: it drops most of a level, and the other tests read more records
	bool IsShown = (C.triangles & Wanted) != 0u;

	[branch]
	if (IsShown)
	{
		IsShown = IsVisible(C);
	}

	if (view.counts.y & SCENE_VIEW_SEEN)
	{
		IsShown = IsShown && g_Seen[Id.x] != 0u;
	}
	else if (view.counts.y & SCENE_VIEW_UNSEEN)
	{
		// What the first part drew is tested as well: it is drawn first again only while nothing hides it
		bool WasSeen = g_Seen[Id.x] != 0u;
		// Each shape fits some geometry closer, and what either one hides is hidden
		IsShown = IsShown && !sphere_occluded(g_HiZ, C.bounds);

		[branch]
		if (IsShown && any(C.extent.xyz > 0.0f))
		{
			IsShown = !box_occluded(g_HiZ, C.bounds.xyz, C.extent.xyz);
		}
		g_Seen[Id.x] = IsShown ? 1u : 0u;
		IsShown = IsShown && !WasSeen;
	}

	if (IsShown)
	{
		uint Slot;

		[branch]
		if (view.counts.y & SCENE_VIEW_BINS)
		{
			uint Material = g_Instances[C.instance].material;
			uint Bin = g_Materials[Material].pipeline;

			if (Bin != SCENE_MATERIAL_NO_PIPELINE)
			{
				uint Args = SCENE_COUNTER_WORDS + (Bin + view.second.x) * 4u;
				g_Counters[Args + SCENE_COUNTER_VERTICES] = SCENE_CLUSTER_TRIANGLES * 3u;
				InterlockedAdd(g_Counters[Args + SCENE_COUNTER_CLUSTERS], 1u, Slot);
				g_Visible[view.second.y + g_Materials[Material].pipeline_start + Slot] = Id.x;
				AskHeads(Material, Id.x, C);
			}
		}
		else if (C.triangles & SCENE_CLUSTER_ALPHA)
		{
			InterlockedAdd(g_Counters[SCENE_COUNTER_ALPHA + SCENE_COUNTER_CLUSTERS], 1u, Slot);
			g_VisibleAlpha[Slot] = Id.x;
		}
		else
		{
			InterlockedAdd(g_Counters[SCENE_COUNTER_CLUSTERS], 1u, Slot);
			g_Visible[Slot] = Id.x;
		}
	}
}
