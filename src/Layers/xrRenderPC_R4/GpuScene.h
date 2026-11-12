#pragma once

namespace nri
{
struct Buffer;
} // namespace nri

namespace Hlsl
{
struct alignas(16) uint4
{
	u32 x, y, z, w;
};

#include "../../../shaders/d3d11/shared/scene_tables.hlsli"
}

static_assert(sizeof(Hlsl::SceneMesh) == 32);
static_assert(sizeof(Hlsl::SceneInstance) == 48);
static_assert(sizeof(Hlsl::SceneTransform) == 96);
static_assert(sizeof(Hlsl::SceneCluster) == 48);
static_assert(sizeof(Hlsl::SceneMaterial) == 16);
static_assert(sizeof(Hlsl::SceneDynamic) == 64);
static_assert(sizeof(Hlsl::SceneGrassBin) == 32);
static_assert(sizeof(Hlsl::SceneGrassView) == 2160);
static_assert(sizeof(Hlsl::SceneView) == 496);

struct GpuDepthDraw
{
	IRHIDepthStencilView* Target;
	u32 Slice = 0;
	const RHIViewport* Area = nullptr;	// the part of the target drawn to, all of it when null
	Fvector Look{};						// the way an orthographic view looks
	const Fvector* Eye = nullptr;		// the eye of a perspective view, which has no Look
	bool IsSpot = false;				// the fixed state of the spot shadow maps instead of the shadow pass state
	bool IsDynamicOnly = false;			// the level geometry is in the target already
	bool IsKept = false;				// the target outlives the frame: nothing that moves or depends on the camera
};

class dxRender_Visual;
struct SPass;
class IRender_Sector;
class R_dsgraph_structure;
class CFrustum;
class GfxCmdList;
class IRHIDepthStencilView;

// Everything that is drawn, as persistent records on the GPU. So far: level geometry, written once
// at level load, and shadow casters that a view culls and draws in one batch: level geometry as
// clusters, and the opaque meshes of the moving objects the view's graph collected.
class GpuScene
{
public:
	// Needs the level's vertex and index chunks, so it runs before they are released
	void Load(const xr_vector<dxRender_Visual*>& Visuals, const xr_vector<IRender_Sector*>& Sectors);
	void Unload();
	using DepthDraw = GpuDepthDraw;

	// The instances of one grass model this frame (CDetail::SlotItem records), its geometry and
	// its shadow pass
	struct GrassBin
	{
		const void* Items;
		u32 Count;
		float Radius;
		u32 IndexStart;
		u32 IndexCount;
		u32 VertexBase;
		u32 Page;
		SPass* Pass;
	};

	bool HasDepthBatch() const { return ClusterCount != 0; }
	// Draws one wind variant of the frame's grass into a depth target, culled against the view on
	// the GPU. The instances are uploaded once per frame and variant. Cmd holds the constants the
	// grass shaders read. False when it cannot: the caller then draws the grass itself.
	bool DrawGrass(GfxCmdList& Cmd, u32 Variant, u32 Frame, const GrassBin* Bins, u32 BinCount, const CFrustum& Frustum, const DepthDraw& Draw);
	// Draws the casters the graph's view sees into a depth target; false when there is no batch.
	// The dynamic meshes it draws are taken out of the graph.
	bool DrawDepth(GfxCmdList& Cmd, R_dsgraph_structure& Graph, const DepthDraw& Draw);
	void EndFrame();
	// The visuals of a sector the batch does not draw, or null when a view has to walk the sector
	const xr_vector<dxRender_Visual*>* GetLeftover(u32 Sector) const { return Sector < Leftover.size() ? &Leftover[Sector] : nullptr; }

private:
	void CreatePasses(const xr_vector<Hlsl::SceneCluster>& Records, const xr_vector<u16>& Indices);
	void Cull(R_dsgraph_structure& Graph, const DepthDraw& Draw, const Hlsl::SceneDynamic* Dynamic, u32 DynamicCount);
	void CullGrass(u32 Variant, u32 Frame, const GrassBin* Bins, u32 BinCount, const CFrustum& Frustum);

	nri::Buffer* Meshes = nullptr;
	nri::Buffer* Instances = nullptr;
	nri::Buffer* Transforms = nullptr;
	xr_vector<xr_vector<dxRender_Visual*>> Leftover;
	u32 SectorCount = 0;
	u32 ClusterCount = 0;
};
