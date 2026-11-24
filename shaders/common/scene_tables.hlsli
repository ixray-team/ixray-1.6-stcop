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
#ifndef SCENE_TABLES_HLSLI
#define SCENE_TABLES_HLSLI

// Records of the GPU scene tables. Shared with the renderer (GpuScene.h): keep both in step.

#define SCENE_INSTANCE_TREE 1u
#define SCENE_INSTANCE_PROGRESSIVE 2u
#define SCENE_INSTANCE_DEPTH_BATCH 4u // opaque caster drawn by the depth-only batch
#define SCENE_INSTANCE_KEEP 8u // never dropped by the size steps
// A caster whose shadow pass is of a later priority: a lamp casts it with translucent shadows on, no other shadow view
#define SCENE_INSTANCE_TRANSLUCENT 16u
#define SCENE_INSTANCE_STEEP 32u // its material has a height the eye's ray is marched through: a class of its own among the ids

#define SCENE_VIEW_PLANES 12u
// SceneView.counts.y
// It draws the instances of SCENE_INSTANCE_TRANSLUCENT
#define SCENE_VIEW_TRANSLUCENT 1u
// Lamp parts in tiles of one target (USE_TILES of scene_depth): the casters of the frame's table are tested against each
// of second.y tiles, by its own view, sectors and object (tile_objects, tile_sectors)
#define SCENE_VIEW_TILED 2u
// The target is kept between frames: nothing is dropped by its distance or size
#define SCENE_VIEW_KEPT 4u
// The main view draws clusters marked SCENE_CLUSTER_MAIN, any other view those marked SCENE_CLUSTER_DEPTH
#define SCENE_VIEW_MAIN 8u
// Above it the dynamic count: of a view without bins its moving casters, SceneModel records from second.z of the frame's models
#define SCENE_VIEW_DYNAMIC_SHIFT 4u
// The visible clusters go to their material's pipeline: its draw arguments after SCENE_COUNTER_WORDS, its own
// part of the visible list. Its dynamic count is of the frame's models (SceneModel), not of dynamic meshes.
#define SCENE_VIEW_BINS 0x80000000u
// The main view in two parts, so that what the frame draws first hides the rest: only the clusters the frame before
// showed, then the others, each tested against the depth pyramid of what is drawn by then
#define SCENE_VIEW_SEEN 0x40000000u
#define SCENE_VIEW_UNSEEN 0x20000000u
#define SCENE_VIEW_MODES 0xE0000000u
#define SCENE_MATERIAL_NO_PIPELINE 0xFFFFFFFFu
#define SCENE_VIEW_STEPS 13u
#define SCENE_VIEW_SECTORS 256u
// Views drawn into parts of one target by a single draw (USE_TILES of scene_depth)
#define SCENE_VIEW_TILES 16u
#define SCENE_CLUSTER_TRIANGLES 64u
// The lists of the cull's first level (scene_ranges), in one buffer: word 0 counts the entries that follow word 1,
// word 1 the entries from the buffer's end down, which are the clusters a view with pipeline lists cuts out by their
// texture. An entry: a cluster of the detail the view draws, below SCENE_CHUNK_SHIFT, and whether the depth pyramid
// hid its run as a whole: it is then only marked as not seen. A run has at most SCENE_CHUNK_CLUSTERS clusters
#define SCENE_CHUNK_CLUSTERS 64u
#define SCENE_CHUNK_SHIFT 24u
#define SCENE_CHUNK_HIDDEN 0x80000000u
// A run's flag: its clusters are cut out by their texture
#define SCENE_CHUNK_ALPHA 0x40000000u
#define SCENE_BANDS 4 // passes of the cull over the cut-out list, each taking the clusters this far from the eye in metres
#define SCENE_BAND_FIRST 4.0f
#define SCENE_BAND_SECOND 12.0f
#define SCENE_BAND_THIRD 40.0f
// SceneCluster.triangles: triangle count, the flags of geometry that sways with the wind and of
// geometry cut out by its texture's alpha, then the transform slot
#define SCENE_CLUSTER_COUNT_MASK 0x7Fu
#define SCENE_CLUSTER_WAVE 0x80u
#define SCENE_CLUSTER_ALPHA 0x100u
#define SCENE_CLUSTER_TRANSFORM_SHIFT 9u
#define SCENE_CLUSTER_TRANSFORM_MASK 0x1FFFFFu
#define SCENE_CLUSTER_MAIN 0x40000000u
#define SCENE_CLUSTER_DEPTH 0x80000000u
#define SCENE_TEXTURE_BUCKETS 16u
// The level's pixels shaded from their ids: the screen in tiles, a list of tiles a class of material and one of every
// tile that holds an id; the tiles a list has room for
#define SCENE_SHADE_TILE 8u
#define SCENE_SHADE_LISTS 5u
#define SCENE_SHADE_TILES 0x80000u
// Textures the buckets have no place for, bound as they are for every forward draw: a map's slot names one as a bucket
// from SCENE_TEXTURE_BUCKETS on, slice 0
#define SCENE_TEXTURE_BOUND 16u
#define SCENE_TEXTURE_ABSENT 0xFFFFFFFFu
// Textures whose head is loaded when the resolve asks for it: an info word names one from bit 20, plus one
#define SCENE_TEXTURE_STREAMS 4096u
#define SCENE_TEXTURE_STREAM_SHIFT 20u
#define SCENE_TEXTURE_TAIL 512.0f

// SceneMaterial.flags: which maps the main view's shader of the material reads, and what kind it is
#define SCENE_MATERIAL_BUMP 1u
#define SCENE_MATERIAL_DETAIL 2u
#define SCENE_MATERIAL_DETAIL_BUMP 4u
#define SCENE_MATERIAL_LM_HEMI 8u
#define SCENE_MATERIAL_AREF 16u
#define SCENE_MATERIAL_TREE 32u
#define SCENE_MATERIAL_TERRAIN 64u
#define SCENE_MATERIAL_BASE_BC1 128u
// Every map it reads can go into buckets; the maps are not loaded yet, so their slices are absent
#define SCENE_MATERIAL_COMPLETE 256u
#define SCENE_MATERIAL_PBR 512u
// Terrain with a mask blending four layers (SceneMaterial.terrain)
#define SCENE_MATERIAL_LAYERS 1024u
// The bump map's height shifts what the eye sees of the surface
#define SCENE_MATERIAL_STEEP 2048u
// What kind of vertex and baked light the forward frame's material is: lit by its vertices, a tree placed by the
// scene's table, a tree that does not sway. None of them: lightmapped
#define SCENE_MATERIAL_VERTEX_LIT 4096u
#define SCENE_MATERIAL_PLACED 8192u
#define SCENE_MATERIAL_STILL 16384u
// The maps of a material, in the order of SceneMaterial
#define SCENE_MATERIAL_MAPS 26u
#define SCENE_MATERIAL_PLAIN_MAPS 9u
// SceneMaterial.terrain: the mask, then per layer its colour, bump, second bump and specular maps
#define SCENE_TERRAIN_COLOUR 1u
#define SCENE_TERRAIN_BUMP 5u
#define SCENE_TERRAIN_BUMP_X 9u
#define SCENE_TERRAIN_SPECULAR 13u

// Meshes of moving objects are not split ahead: every run of SCENE_CLUSTER_TRIANGLES triangles of
// their own index data is drawn as a cluster. An entry of the visible list with
// SCENE_VISIBLE_DYNAMIC names such a run: the record in the low bits, the run from SCENE_VISIBLE_MODEL_SHIFT, of a view
// drawn in tiles its tile from SCENE_VISIBLE_TILE_SHIFT.
#define SCENE_DYNAMIC_CLUSTERS 65536u
#define SCENE_DYNAMIC_RUNS 2047u
#define SCENE_VISIBLE_DYNAMIC 0x80000000u
#define SCENE_VISIBLE_TILE_SHIFT 27u
#define SCENE_DYNAMIC_PAGE_MASK 0xFFu
#define SCENE_DYNAMIC_SKINNED 0x100u

// g_Counters holds the arguments of two instanced indirect draws, one instance per visible cluster:
// the opaque clusters, then from SCENE_COUNTER_ALPHA the alpha-tested ones
#define SCENE_COUNTER_VERTICES 0u
#define SCENE_COUNTER_CLUSTERS 1u
#define SCENE_COUNTER_ALPHA 4u
#define SCENE_COUNTER_WORDS 8u

struct SceneMesh
{
	uint index_start;
	uint index_count;
	uint vertex_base;
	uint source; // heap page and draw source, as CBDraw.draw_flags
	uint next_lod;
	float lod_distance;
	uint page;
	uint mesh_reserved;
};

struct SceneInstance
{
	float4 bounds; // world-space sphere
	uint mesh;
	uint mesh_depth; // the simplified mesh depth-only passes draw; equals mesh when there is none
	uint material;
	uint transform; // slot 0 is identity
	uint light; // of a tree: the bits of the float its vertex hemi is scaled by
	uint sector;
	uint flags;
	uint extra; // of a tree: the bits of the float added to it
};

struct SceneMaterial
{
	// A texture is a tail slice (its mips from SCENE_TEXTURE_TAIL down) and, when it is larger, a
	// head slice with the mips above; both are bucket << 16 | slice, SCENE_TEXTURE_ABSENT for none
	uint base;
	uint base_head;
	uint base_info; // repeat shift along x, << 4 along y, << 8 the mip count of the head, << 12 log2 of the tail's size, << 16 its mip count
	uint alpha; // slice of the alpha array (scene_depth) holding the base texture's alpha
	uint bump;
	uint bump_head;
	uint bump_info;
	uint flags; // SCENE_MATERIAL_
	uint bump_x;
	uint bump_x_head;
	uint bump_x_info;
	float gloss; // the base texture's material value, what L_material.w is made from
	uint detail;
	uint detail_head;
	uint detail_info;
	float detail_scale;
	uint detail_bump;
	uint detail_bump_head;
	uint detail_bump_info;
	uint pipeline; // of the forward frame: which draw its clusters belong to, SCENE_MATERIAL_NO_PIPELINE for none
	uint detail_bump_x;
	uint detail_bump_x_head;
	uint detail_bump_x_info;
	uint pipeline_start; // where that draw's part of the visible list starts
	uint hemi; // the lightmap with sun and hemi of a lightmapped material
	uint hemi_head;
	uint hemi_info;
	uint env; // of a material that mirrors a cube: its cube of the scene's environment array
	uint specular; // the specular value of a PBR surface, where the texture has such a map
	uint specular_head;
	uint specular_info;
	uint material_reserved3;
	uint lightmap; // the baked light of a lightmapped material, what the forward frame reads
	uint lightmap_head;
	uint lightmap_info;
	uint material_reserved4;
	uint4 terrain[SCENE_MATERIAL_MAPS - SCENE_MATERIAL_PLAIN_MAPS]; // xyz: tail, head and info as for the maps above
};

// The frame's table of moving objects, rebuilt every frame: a mesh part a record, the main view's opaque parts first, then
// the HUD's, the sorted lists' and those only the shadow views draw. The runs of SCENE_CLUSTER_TRIANGLES triangles of an
// opaque one go to the draw of its material's passes: an entry of that draw's visible list is SCENE_VISIBLE_DYNAMIC, the
// run from SCENE_VISIBLE_MODEL_SHIFT, the record below it. A sun cascade culls its casters out of the whole table
#define SCENE_MODELS 4096u
#define SCENE_VISIBLE_MODEL_SHIFT 12u
#define SCENE_MODEL_RUNS 0x7FFFFu
// SceneModel.source past its page and SCENE_DYNAMIC_SKINNED: a part of the HUD, drawn in its own space
#define SCENE_MODEL_HUD 0x200u
// SceneModel.flags
#define SCENE_OBJECT_MAIN 1u
#define SCENE_OBJECT_CASTER 2u // a shadow view of the frame casts its object, and the batch draws its shadow pass as it is
#define SCENE_OBJECT_TRANSLUCENT 4u // its shadow pass is of a later priority: only a view that draws that priority casts it
#define SCENE_OBJECT_SHADOW_ONLY 8u
#define SCENE_OBJECT_HUD 16u
#define SCENE_OBJECT_SORTED 32u
#define SCENE_OBJECT_DISTORT 64u // it has a distortion or a scope mask element
// The objects of the frame's table, SceneModel.object; SCENE_OBJECT_NONE for a part of none
#define SCENE_OBJECTS 4096u
#define SCENE_OBJECT_NONE 0xFFFFFFFFu
// Every shadow view casts its moving objects out of the table, its runs into the view's lists of the opaque and of what
// is cut out. The sun cascades have lists of their own
#define SCENE_CASTER_VIEWS 3u

struct SceneModel
{
	float3x4 world;
	float3x4 world_old; // where it stood the frame before
	float4 bounds; // world-space sphere
	float4 hemi_cube_pos; // the sky its light tracker measured by the faces of a cube: +x +y +z; w: its sun, as L_dynamic_props.x
	float4 hemi_cube_neg; // -x -y -z; w: its hemi, as L_dynamic_props.w
	uint index_start; // into the index data of its heap page
	uint triangle_count;
	uint vertices; // first vertex: in its heap page, or in the skinned output
	uint source; // heap page, SCENE_DYNAMIC_SKINNED when the vertices are skinned output, SCENE_MODEL_HUD
	uint material;
	uint bin; // the forward draw it belongs to: its draw arguments in the main view's first part
	uint visible_start; // where that draw's entries start in the visible list this frame
	uint alpha; // of a caster: the slice of the alpha array its shadow is cut out by, SCENE_TEXTURE_ABSENT for none
	uint flags; // SCENE_OBJECT_
	uint sector; // of its object, SCENE_VIEW_SECTORS for none
	uint object; // its object in the frame's table, by which a lamp leaves out the object it ignores
	uint model_reserved;
};

// Grass: the instances of the frame (CDetail::SlotItem), one bin per model and wind variant. A view's cull turns
// them into entries of a visible list, one per cluster of the instance's mesh, which one indirect draw of
// scene_depth takes. An entry is two words: the instance with its size from bit 24 (127: as it is), then the
// cluster, from bit 24 which of the model's clusters it is and from bit 28 the wind variant.
#define SCENE_GRASS_BINS 128u
// A blade of a frame that draws the slots' stored blades: where it is stored, and its bin above these bits
#define SCENE_GRASS_STORE_BITS 19u
// A model's clusters: its far mesh first, then the whole mesh
#define SCENE_GRASS_RUNS 16u
#define SCENE_GRASS_ITEMS 0x100000u
// The id of a grass pixel: SCENE_ID_GRASS, the cluster of the model from bit 20, the instance; then as any id
// shifted by 7 for the triangle, plus one. Level clusters stay below SCENE_ID_GRASS
#define SCENE_ID_GRASS 0x1000000u
// g_Args of the grass cull: the arguments of the draw, then what the cull counted
#define SCENE_GRASS_KEPT 4u
#define SCENE_GRASS_FAR 5u
#define SCENE_GRASS_TRIANGLES 6u
#define SCENE_GRASS_WORDS 8u
// The tiles of the screen the main view's blades are counted in, after the arguments of the draws
#define SCENE_GRASS_TILES 16384u
// A far blade draws its far mesh once for every blade the thinning took for it, up to this many, as a clump around
// its place: the forward list of a bin's far mesh has room for that many entries a blade, and bits 20-23 of an entry
// say which
#define SCENE_GRASS_COPIES 8u
#define SCENE_GRASS_TILE_BASE (SCENE_GRASS_WORDS + SCENE_GRASS_BINS * 10u)

struct SceneGrassBin
{
	uint first; // of its instances in the frame's list
	uint count;
	float radius; // of the model around its root, for a scale of 1
	uint cluster; // the model's first cluster
	uint far_clusters; // how many clusters its far mesh takes, 0 without one
	uint clusters; // and the whole mesh, which follows
	uint variant; // 0: stands still, 1 and 2: sways with waves[variant - 1]
	uint triangles; // of the far mesh, << 16 of the whole one
	// For the draws a model: where its indices start (the far mesh is the first of them), its vertices as heap
	// page << 24 | first vertex, its material
	uint index_start;
	uint vertices;
	uint material;
	float own_radius; // of the sphere that holds the model, radius - own_radius up from the root along the model's own up
};

// The first ten of CBDetail
struct SceneGrassWind
{
	float4 trample_params;
	float4 wind_global;
	float4 wind_xz1;
	float4 wind_xz1_dir;
	float4 wind_xz2;
	float4 wind_xz2_dir;
	float4 wind_xz3;
	float4 wind_xz3_dir;
	float4 wind_swirl;
	float4 wind_swirl_dir;
};

struct SceneGrassView
{
	float4x4 view_projection;
	float4 planes[SCENE_VIEW_PLANES]; // a sphere is outside when dot(xyz, centre) + w > radius
	uint4 counts; // x: instances, y: bins, z: planes, w: entries the visible list holds
	float4 thin; // xyz: the main view's eye, w: the distance from it where grass starts to thin
	float4 lod; // x: the distance from that eye around which the far mesh takes over, y: alpha below which a pixel is dropped, zw: the main view's jitter
	uint4 limits; // x: the vertices an entry is drawn with: the most a cluster of the frame's models has, y: 1 when the list comes thinned and sized already, 2 when the blades are stored ones a list names (g_Runs), z: 1 for a draw a model, see scene_grass_cull, w: 1 when a blade is tested on the main view's depth pyramid
	float4 budget; // x: the layers of grass a tile of the screen keeps, 0: all of them, y: 1 while the tiles are counted, zw: the tiles across and down
	SceneGrassWind wind;
	float4 waves[8]; // per swaying variant: wave, wind, and both a frame ago
	SceneGrassBin bins[SCENE_GRASS_BINS];
};

#ifndef __cplusplus
// CDetail::SlotItem
struct SceneGrassItem
{
	float3 quat;
	float scale;
	float3 pos;
	float hemi;
	float trample_strength;
	float trample_visual;
	float2 trample_dir;
};

// Far grass is thinned by the square of the distance past where thinning starts, so the blades to a pixel stop
// growing, down to one blade in sixteen. Where a blade stands decides whether it stays (Lot against the kept
// share); those that stay grow to keep the ground covered, and one about to go shrinks first. 0: dropped
float GrassSize(float3 At, float4 Thin, out float Lot, out float Far)
{
	Far = distance(At, Thin.xyz);
	float Kept = clamp(Thin.w * Thin.w / max(Far * Far, 1e-4f), 1.0f / 16.0f, 1.0f);
	Lot = frac(sin(dot(At.xz, float2(12.9898f, 78.233f))) * 43758.5453f);
	return Lot >= Kept ? 0.0f : saturate((Kept - Lot) / (0.25f * Kept)) * rsqrt(sqrt(Kept));
}

// The same for blades the details task put in the order of its own lot (GrassLot of GrassThin.h, bit for bit): what
// it keeps of a slot are the first of them, and this has to drop the same ones
float GrassSizeStored(float3 At, float4 Thin, out float Lot, out float Far)
{
	Far = distance(At, Thin.xyz);
	float Kept = clamp(Thin.w * Thin.w / max(Far * Far, 1e-4f), 1.0f / 16.0f, 1.0f);
	uint Bits = (asuint(At.x) * 0x9E3779B1u) ^ (asuint(At.z) * 0x85EBCA77u);
	Bits ^= Bits >> 15;
	Bits *= 0x2C1B3C6Du;
	Bits ^= Bits >> 12;
	Lot = float(Bits >> 8) * (1.0f / 16777216.0f);
	return Lot >= Kept ? 0.0f : saturate((Kept - Lot) / (0.25f * Kept)) * rsqrt(sqrt(Kept));
}

// The size as an entry of the visible list holds it
uint GrassSizeBits(float Size)
{
	return uint(Size * 127.0f + 0.5f);
}
#endif

// The local lights of the frame, as the shading reads them. A point light with shadows is one light: its six
// shadow entries follow each other in the order of the cube's faces (+x -x +y -y +z -z in the world), and
// faces tells which of them the frame has; a pixel in another face is not lit by it.
#define SCENE_LIGHTS 256u
#define SCENE_LIGHT_SHADOWS 64u
// The lights are in the order of their depth in the view. A pixel's lights: those its depth bin names (the first
// and the last light that reach the bin), of them the ones whose bit is set for its tile of the screen
#define SCENE_LIGHT_BINS 64u
#define SCENE_LIGHT_TILE 16u
#define SCENE_LIGHT_SHADOWED 1u
// The textures spot lights project, bound one by one
#define SCENE_LIGHT_PROJECTORS 8u
#define SCENE_LIGHT_FACES 2u
// A light of the HUD: it lights the HUD by the HUD's own point and casts no shadow map
#define SCENE_LIGHT_HUD 4u

struct SceneLight
{
	float4 position; // xyz: in view space, w: 1 / range^2
	float4 colour; // w: what the legacy model scales its specular by
	float4 direction; // xyz: in view space, w: the cosine of half the cone, below -1 without one
	uint flags; // SCENE_LIGHT_
	uint shadow; // its first shadow entry
	uint faces; // a bit per cube face
	uint projector; // the slot of the texture it projects plus one, << 8 the shadow entry with the texture's matrix; 0: none
};

// The decals a frame draws at most, and the words a tile of the screen holds: its lights, then its decals
#define SCENE_DECALS 256u
#define SCENE_TILE_WORDS ((SCENE_LIGHTS + SCENE_DECALS) / 32u)

// A decal: a box in the world a texture is projected through, along the box's z
struct SceneDecal
{
	float4 sphere; // xyz: the centre in view space, w: the radius around the box; of record 0: x - the number of decals
	float4 to_box[3]; // the rows that take a point of view space into the box: inside it every axis is within -1 .. 1
	float opacity;
	uint layer; // of the decal textures
	uint mode; // how it is blended, SCENE_DECAL_
	uint unused;
};

struct SceneLightShadow
{
	float4x4 to_atlas; // from view space to the light's place in the shadow atlas
	uint4 page; // x: the page of the atlas, y: not 0 when the entry has no shadow map and lets all light through
};

struct SceneLightBlock
{
	uint4 light_counts; // x: lights, y: shadow entries
	float4 light_zones; // x: depth bins to a metre of view depth
	uint4 light_bins[SCENE_LIGHT_BINS / 4u]; // per bin: its first light, << 16 its last; first above last: none
	SceneLight lights[SCENE_LIGHTS];
	SceneLightShadow shadows[SCENE_LIGHT_SHADOWS];
};

struct SceneTransform
{
	float3x4 world;
	float3x4 world_old;
};

// Clusters of one instance: one run of the cluster table, a sphere around all of them and the bounds of their detail
struct SceneRange
{
	float4 bounds;
	uint first;
	uint count;
	uint flags; // SCENE_CHUNK_ALPHA: what its chunks are told
	uint unused;
	// x: the least own error, y: the most a cluster's own sphere reaches nearer than the run's centre, z: the largest
	// error of a coarser group, w: the most such a group's sphere reaches farther (SceneClusterLod)
	float4 detail;
};

// Beside a cluster: which detail of its mesh it stands for. The mesh is a hierarchy of clusters, each coarser group
// made by simplifying a group of finer ones; a cluster is drawn while the error of its own simplification does not
// show and the error of the group made from it would
struct SceneClusterLod
{
	float4 self; // the sphere its own error is measured from
	float4 parent; // and the sphere of the group made from it
	float4 errors; // x: its own, 0 for the mesh as it was built, y: the group's, huge for the coarsest
};

// Beside a transform, for a tree: what the forward frame scales and offsets its baked light by, and the group that
// shows its meshes or a card by distance. A static mesh of a group has a transform of its own, the identity, for the
// group alone
struct SceneTreeLight
{
	float4 scale; // rgb, hemi
	float4 bias; // rgb without the frame's ambient, hemi
	float4 sun; // x: scale, y: bias
	float4 lod; // xyz: the group's centre, w: its radius times its factor, 0 without a group
};

// An LOD card of the level: the sphere of its group, the eight views its facets were made from and its material. The
// main view keeps it where the group is small enough on screen for the card to stand in, and writes SCENE_CARD_WORDS
// words of the visible list a kept card: its shift towards the eye (three floats), its vertices as heap page << 24 |
// first vertex, its two views and fade as the graph's card record has them (TF), its material
#define SCENE_CARD_WORDS 6u

struct SceneCard
{
	float4 sphere;
	float4 facets[8]; // xyz: the normal of a view, as the facet's vertices hold it
	uint vertices; // heap page << 24 | first of its 32 vertices, four a view
	uint material;
	uint sector;
	float lod_factor;
};

// Up to SCENE_CLUSTER_TRIANGLES triangles of one instance's depth mesh. Bounds and cone
// are in world space; the vertices are not when there is a transform.
struct SceneCluster
{
	float4 bounds; // sphere
	float4 extent; // xyz: half the size of its box in the world, around the sphere's centre; 0: it has none; w: not 0: its vertices are a page (scene_pages.hlsli)
	uint instance;
	uint index_start; // into the cluster indices, three per triangle; of a page the word its corners start at
	uint triangles;
	uint vertices; // heap page << 24 | first vertex of the mesh; of a page the word of its header
};

struct SceneView
{
	float4x4 view_projection;
	float4 planes[SCENE_VIEW_PLANES]; // a sphere is outside when dot(xyz, centre) + w > radius
	float4 camera; // w: fog distance
	float4 thresholds; // x: radius / distance^2 at or below which an instance is dropped, y: the main view: what a cluster's density times its distance is multiplied by, under 1 its textures need their heads, z: the bits of the frame, w: the band of distance this pass of the cull takes, under 0 all of them
	uint4 counts; // x: clusters, y: SCENE_VIEW_ flags, then the dynamic mesh count, z: planes, w: size steps
	float4 steps[SCENE_VIEW_STEPS]; // dropped when the sphere volume is below x and the distance above y
	uint4 sectors[SCENE_VIEW_SECTORS / 128u]; // one bit per visited sector
	float4 wave; // tree sway: xyz phase per metre, w phase now
	float4 wind; // xz: sway per metre of height, w: scale of a vertex's sway weight
	float4 material; // x: alpha below which an alpha-tested pixel is dropped, y: unused, zw: the main view's jitter, in clip space over w
	float4 wave_old; // wave and wind of the frame before
	float4 wind_old;
	float4 lod; // x: the main view drops the meshes of a group whose radius / distance^2 is at or below it: its card stands in; y: below it the card is shown
	float4x4 view; // of the main view, for the depth pyramid
	float4 hiz; // xy: the projection's scale of x and y, z: the near plane
	float4 hiz_depth; // depth = x + y / view z
	float4 hiz_size; // xy: size of the pyramid's first level, z: its last level
	float4 detail; // xyz: the main view's eye, every view takes its detail by it, w: pixels an error of one unit takes at one unit from it, over the error allowed; 0: the meshes as built
	uint4 second; // SCENE_VIEW_UNSEEN: x added to a pipeline's index for its draw arguments, y to its place in the visible list; z, w: of the cards' cull the cards, where their part of the visible list starts; of a shadow view the first of its moving casters among the models, the object of the table it leaves out; SCENE_VIEW_TILED: y its tiles
	float4x4 tile_views[SCENE_VIEW_TILES];
	float4 tile_rects[SCENE_VIEW_TILES]; // where a tile sits in the target's clip space: xy scale, zw offset
	uint4 tile_objects[SCENE_VIEW_TILES]; // x: the object of the table the tile leaves out; y 1: the one object it casts
	uint4 tile_sectors[SCENE_VIEW_TILES * 2u]; // the sectors its portals reached, a bit each
};

#endif
