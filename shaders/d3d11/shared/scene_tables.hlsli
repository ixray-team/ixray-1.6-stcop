#ifndef SCENE_TABLES_HLSLI
#define SCENE_TABLES_HLSLI

// Records of the GPU scene tables. Shared with the renderer (GpuScene.h): keep both in step.

#define SCENE_INSTANCE_TREE 1u
#define SCENE_INSTANCE_PROGRESSIVE 2u
#define SCENE_INSTANCE_DEPTH_BATCH 4u // opaque caster drawn by the depth-only batch
#define SCENE_INSTANCE_KEEP 8u // never dropped by the size steps

#define SCENE_VIEW_PLANES 6u
// SceneView.counts.y. Clusters facing away are dropped: thresholds.yzw is the way an orthographic
// view looks (negated when it culls the other facing), or with SCENE_VIEW_PERSPECTIVE the eye.
#define SCENE_VIEW_FACING 1u
#define SCENE_VIEW_PERSPECTIVE 2u
// The target is kept between frames: no trees, and nothing is dropped by its distance or size
#define SCENE_VIEW_KEPT 4u
#define SCENE_VIEW_DYNAMIC_SHIFT 3u
#define SCENE_VIEW_STEPS 13u
#define SCENE_VIEW_SECTORS 256u
#define SCENE_CLUSTER_TRIANGLES 64u
// SceneCluster.triangles: triangle count, the flags of geometry that sways with the wind and of
// geometry cut out by its texture's alpha, then the transform slot
#define SCENE_CLUSTER_COUNT_MASK 0x7Fu
#define SCENE_CLUSTER_WAVE 0x80u
#define SCENE_CLUSTER_ALPHA 0x100u
#define SCENE_CLUSTER_TRANSFORM_SHIFT 9u
#define SCENE_TEXTURE_BUCKETS 16u
#define SCENE_TEXTURE_ABSENT 0xFFFFFFFFu
#define SCENE_TEXTURE_TAIL 512.0f

// Meshes of moving objects are not split ahead: every run of SCENE_CLUSTER_TRIANGLES triangles of
// their own index data is drawn as a cluster. An entry of the visible list with
// SCENE_VISIBLE_DYNAMIC names such a run: the mesh in the low bits, the run above them.
#define SCENE_DYNAMIC_MESHES 512u
#define SCENE_DYNAMIC_CLUSTERS 65536u
#define SCENE_DYNAMIC_RUNS 2047u
#define SCENE_VISIBLE_DYNAMIC 0x80000000u
#define SCENE_VISIBLE_RUN_SHIFT 9u
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
	uint light;
	uint sector;
	uint flags;
	uint extra;
};

struct SceneMaterial
{
	// A texture is a tail slice (its mips from SCENE_TEXTURE_TAIL down) and, when it is larger, a
	// head slice with the mips above; both are bucket << 16 | slice, SCENE_TEXTURE_ABSENT for none
	uint base;
	uint base_head;
	uint base_info; // repeat shift along x, << 4 along y, << 8 the mip count of the head
	uint material_reserved;
};

// A mesh of a moving object, written for one view.
struct SceneDynamic
{
	float3x4 world;
	uint index_start; // into the index data of its heap page
	uint triangle_count;
	uint vertices; // first vertex: in its heap page, or in the skinned output
	uint source; // heap page, SCENE_DYNAMIC_SKINNED when the vertices are skinned output
};

// Grass: the instances of the frame, one bin per model, culled per view into the instance lists of
// one indexed instanced indirect draw per bin
#define SCENE_GRASS_BINS 64u
#define SCENE_GRASS_ARGS 5u

struct SceneGrassBin
{
	uint first; // of its instances in the frame's list, and of its entries in the visible list
	uint count;
	float radius; // of the model, for a scale of 1
	uint index_count;
	uint index_start; // in the index data of its heap page
	uint vertex_base;
	uint page;
	uint grass_reserved;
};

struct SceneGrassView
{
	float4 planes[SCENE_VIEW_PLANES]; // a sphere is outside when dot(xyz, centre) + w > radius
	uint4 counts; // x: instances, y: bins, z: planes
	SceneGrassBin bins[SCENE_GRASS_BINS];
};

struct SceneTransform
{
	float3x4 world;
	float3x4 world_old;
};

// Up to SCENE_CLUSTER_TRIANGLES triangles of one instance's depth mesh. Bounds and cone
// are in world space; the vertices are not when there is a transform.
struct SceneCluster
{
	float4 bounds; // sphere
	float4 cone; // xyz: mean facing, w: a view along the facing by this much or more sees only back faces
	uint instance;
	uint index_start; // into the cluster indices, three per triangle
	uint triangles;
	uint vertices; // heap page << 24 | first vertex of the mesh
};

struct SceneView
{
	float4x4 view_projection;
	float4 planes[SCENE_VIEW_PLANES]; // a sphere is outside when dot(xyz, centre) + w > radius
	float4 camera; // w: fog distance
	float4 thresholds; // x: radius / distance^2 at or below which an instance is dropped, yzw: see SCENE_VIEW_FACING
	uint4 counts; // x: clusters, y: SCENE_VIEW_ flags, then the dynamic mesh count, z: planes, w: size steps
	float4 steps[SCENE_VIEW_STEPS]; // dropped when the sphere volume is below x and the distance above y
	uint4 sectors[SCENE_VIEW_SECTORS / 128u]; // one bit per visited sector
	float4 wave; // tree sway: xyz phase per metre, w phase now
	float4 wind; // xz: sway per metre of height, w: scale of a vertex's sway weight
	float4 material; // x: alpha below which an alpha-tested pixel is dropped, y: +1 or -1, which facing a perspective view culls
};

#endif
