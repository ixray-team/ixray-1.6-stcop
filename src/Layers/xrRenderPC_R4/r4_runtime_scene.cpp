// r4_runtime_scene.cpp: runtime-injected terrain for levels without compiled
// geometry (the level viewer's procedural sandbox). A single synthetic
// "outdoor" sector whose root is the terrain visual makes the engine treat the
// level as all-outdoor (SectorsCount()==1 in r__sector_detect.cpp), so the
// stock deferred pass draws the terrain like level static geometry and dynamic
// renderables get a valid sector instead of being culled.

#include "stdafx.h"
#include "FVisual.h"
#include "r__sector.h"
#include "ViewerFallbackTextures.h"
#include "../../xrEngine/Fmesh.h"
#include "../../xrRHI/RHIUtils.h"

namespace RuntimeTerrainSceneDetail
{
	// Level-static vertex layout ("v_static_color" in common_iostructs.hlsli),
	// 32-byte stride, one UV set, no lightmap set.
	struct SStaticVertex
	{
		Fvector	Pos;
		u32		Norm;
		u32		Tang;
		u32		Binorm;
		u32		Color;
		s16		U;
		s16		V;
	};
	static_assert(sizeof(SStaticVertex) == 32);

	// Signed normalized component into a UNORM byte (same packing as q_N in
	// SkeletonX.cpp; the shader recovers it with unpack_bx4).
	u8 PackSigned(float v)
	{
		return u8(clampr(iFloor((v + 1.f) * 127.5f), 0, 255));
	}

	// Primary UV quantization of the static format: unpack_tc_base divides the
	// integer texcoord by 1024, so valid UVs span [-32, 32).
	s16 PackTexcoord(float uv)
	{
		return s16(clampr(iFloor(uv * 1024.f), -32768, 32767));
	}
}

IRenderVisual* CRender::CreateRuntimeTerrainScene(const Fvector* positions, u32 vertCount, const u32* indices, u32 indexCount, float uvWorldScale, const char* textureName, IRenderRuntimeBufferAllocator* bufferAllocator)
{
	// Never clobber a compiled level's scene graph: the runtime scene exists only
	// for levels that carry no baked geometry at all.
	if (b_loaded || !Sectors.empty())
	{
		Msg("! CRender::CreateRuntimeTerrainScene: a level scene is already loaded, request ignored");
		return nullptr;
	}
	if (g_dedicated_server || positions == nullptr || indices == nullptr || vertCount == 0 || indexCount == 0 || (indexCount % 3) != 0)
		return nullptr;
	// The geometry path binds every index buffer as 16-bit (R_Backend.cpp).
	if (vertCount > 65535)
	{
		Msg("! CRender::CreateRuntimeTerrainScene: %u vertices exceed the 16-bit index path", vertCount);
		return nullptr;
	}

	// Idempotent: a previous runtime scene is replaced.
	DestroyRuntimeTerrainScene();

	Fbox bounds;
	bounds.invalidate();
	for (u32 v = 0; v < vertCount; ++v)
		bounds.modify(positions[v]);

	// Smooth per-vertex normals accumulated from triangle normals (as in
	// CDetail::LoadGeom).
	xr_vector<Fvector> normals(vertCount, Fidentity.c);
	for (u32 i = 0; i < indexCount; i += 3)
	{
		const u32 i0 = indices[i + 0], i1 = indices[i + 1], i2 = indices[i + 2];
		if (i0 >= vertCount || i1 >= vertCount || i2 >= vertCount)
		{
			Msg("! CRender::CreateRuntimeTerrainScene: index %u out of range (%u vertices)", std::max(i0, std::max(i1, i2)), vertCount);
			return nullptr;
		}
		Fvector normal;
		normal.mknormal(positions[i0], positions[i1], positions[i2]);
		normals[i0].add(normal);
		normals[i1].add(normal);
		normals[i2].add(normal);
	}

	xr_vector<RuntimeTerrainSceneDetail::SStaticVertex> vertices(vertCount);
	for (u32 v = 0; v < vertCount; ++v)
	{
		RuntimeTerrainSceneDetail::SStaticVertex& dst = vertices[v];
		dst.Pos = positions[v];

		Fvector n = normals[v].normalize_safe();
		// The normal's w feeds the G-buffer hemi (sky ambient) term. Drive it from
		// the up-facingness of the normal so open ground gets full sky light; a
		// height-based value would be zero for a perfectly flat terrain.
		const u32 hemi = u32(clampr(iFloor((n.y * 0.5f + 0.5f) * 255.f), 0, 255));
		dst.Norm = color_rgba(RuntimeTerrainSceneDetail::PackSigned(n.x), RuntimeTerrainSceneDetail::PackSigned(n.y), RuntimeTerrainSceneDetail::PackSigned(n.z), hemi);
		// The terrain texture has no _bump companion: a flat +X/+Z basis is enough
		// (the w bytes are the format's detail-uv offsets - keep them zero).
		dst.Tang = color_rgba(RuntimeTerrainSceneDetail::PackSigned(1.f), RuntimeTerrainSceneDetail::PackSigned(0.f), RuntimeTerrainSceneDetail::PackSigned(0.f), 0);
		dst.Binorm = color_rgba(RuntimeTerrainSceneDetail::PackSigned(0.f), RuntimeTerrainSceneDetail::PackSigned(0.f), RuntimeTerrainSceneDetail::PackSigned(1.f), 0);
		// White albedo multiplier, full sun occlusion in w.
		dst.Color = color_rgba(255, 255, 255, 255);
		dst.U = RuntimeTerrainSceneDetail::PackTexcoord(positions[v].x * uvWorldScale);
		dst.V = RuntimeTerrainSceneDetail::PackTexcoord(positions[v].z * uvWorldScale);
	}

	xr_vector<u16> indices16(indexCount);
	for (u32 i = 0; i < indexCount; ++i)
		indices16[i] = u16(indices[i]);

	RuntimeTerrainBufferAllocator = bufferAllocator;
	RuntimeTerrainVisual = new Fvisual();
	Fvisual* visual = RuntimeTerrainVisual;
	visual->Type = MT_NORMAL;
	visual->dbg_name = "runtime_terrain";

	const u32 vertexBytes = vertCount * sizeof(RuntimeTerrainSceneDetail::SStaticVertex);
	const u32 indexBytes = indexCount * sizeof(u16);
	if (bufferAllocator != nullptr)
	{
		RuntimeTerrainVertexSlice = bufferAllocator->Allocate(vertices.data(), vertexBytes, false);
		RuntimeTerrainIndexSlice = bufferAllocator->Allocate(indices16.data(), indexBytes, true);
		if (RuntimeTerrainVertexSlice.Buffer == nullptr || RuntimeTerrainVertexSlice.Offset == u32(-1) ||
			RuntimeTerrainIndexSlice.Buffer == nullptr || RuntimeTerrainIndexSlice.Offset == u32(-1))
		{
			Msg("! CRender::CreateRuntimeTerrainScene: the viewer GPU pool is exhausted");
			DestroyRuntimeTerrainScene();
			return nullptr;
		}
		visual->p_rm_Vertices = RuntimeTerrainVertexSlice.Buffer;
		visual->p_rm_Vertices->AddRef();
		visual->p_rm_Indices = RuntimeTerrainIndexSlice.Buffer;
		visual->p_rm_Indices->AddRef();
		visual->vBase = RuntimeTerrainVertexSlice.Offset / sizeof(RuntimeTerrainSceneDetail::SStaticVertex);
		visual->iBase = RuntimeTerrainIndexSlice.Offset / sizeof(u16);
	}
	else
	{
		if (!RHIUtils::CreateVertexBuffer(&visual->p_rm_Vertices, vertices.data(), vertexBytes) ||
			!RHIUtils::CreateIndexBuffer(&visual->p_rm_Indices, indices16.data(), indexBytes))
		{
			Msg("! CRender::CreateRuntimeTerrainScene: GPU buffer creation failed");
			DestroyRuntimeTerrainScene();
			return nullptr;
		}
		visual->vBase = 0;
		visual->iBase = 0;
	}

	visual->vCount = vertCount;
	visual->iCount = indexCount;
	visual->dwPrimitives = indexCount / 3;

	RHIInputElementDesc decl[] =
	{
		{ "POSITION", 0, ERHI_FORMAT::R32G32B32_FLOAT, 0, 0,  ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
		{ "NORMAL",   0, ERHI_FORMAT::R8G8B8A8_UNORM,  0, 12, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
		{ "TANGENT",  0, ERHI_FORMAT::R8G8B8A8_UNORM,  0, 16, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
		{ "BINORMAL", 0, ERHI_FORMAT::R8G8B8A8_UNORM,  0, 20, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
		{ "COLOR",    0, ERHI_FORMAT::R8G8B8A8_UNORM,  0, 24, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
		{ "TEXCOORD", 0, ERHI_FORMAT::R16G16_SINT,     0, 28, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	};
	visual->rm_geom.create(decl, std::size(decl), visual->p_rm_Vertices, visual->p_rm_Indices);

	// Exact bounds: the static pass frustum- and HOM-culls by vis_data.
	visual->vis.box = bounds;
	visual->vis.box.getsphere(visual->vis.sphere.P, visual->vis.sphere.R);

	// One base texture and no lightmap pair: USE_LM_HEMI stays undefined and the
	// deffer_base shader reads exactly this vertex layout.
	const char* texture = (textureName != nullptr && textureName[0] != 0) ? textureName : "$memory$viewer_ground";
	visual->shader.create("def_shaders\\def_vertex", texture);
	visual->shader_name = "def_shaders\\def_vertex";
	visual->orig_shader_name = visual->shader_name;
	visual->texture_name = texture;
	visual->orig_texture_name = visual->texture_name;
	if (visual->shader._get() == nullptr)
	{
		Msg("! CRender::CreateRuntimeTerrainScene: shader creation failed (texture '%s')", texture);
		DestroyRuntimeTerrainScene();
		return nullptr;
	}
	if (strncmp(texture, "$memory$", 8) == 0 && ViewerFallbackTextures::EnsureGroundTexture() == nullptr)
	{
		Msg("! CRender::CreateRuntimeTerrainScene: embedded terrain texture creation failed");
		DestroyRuntimeTerrainScene();
		return nullptr;
	}

	// The single synthetic outdoor sector: Sectors.size()==1 turns off portal
	// traversal everywhere, render_main draws root() directly.
	CSector* sector = new CSector();
	sector->SetRoot(visual);
	Sectors.push_back(sector);
	pOutdoorSector = sector;
	pLastSector = sector;
	vLastCameraPos.set(0.f, 0.f, 0.f);

	RuntimeTerrainSector = sector;
	if (Wallmarks == nullptr)
		Wallmarks = new CWallmarksEngine();
	Msg("* CRender: runtime terrain scene created (%u verts, %u tris, texture '%s')", vertCount, indexCount / 3, texture);
	return visual;
}

void CRender::DestroyRuntimeTerrainScene()
{
	if (RuntimeTerrainSector != nullptr)
	{
		// Detach from the traversal state before freeing anything.
		if (pOutdoorSector == RuntimeTerrainSector)
			pOutdoorSector = nullptr;
		if (pLastSector == RuntimeTerrainSector)
			pLastSector = nullptr;

		const auto it = std::find(Sectors.begin(), Sectors.end(), RuntimeTerrainSector);
		if (it != Sectors.end())
			Sectors.erase(it);
	}

	// ~CSector does not own the root; the visual is ours alone (never registered
	// in Visuals or the ModelPool) and its dtor releases its buffer references.
	if (RuntimeTerrainVisual != nullptr)
		xr_delete(Wallmarks);
	xr_delete(RuntimeTerrainSector);
	xr_delete(RuntimeTerrainVisual);

	if (RuntimeTerrainBufferAllocator != nullptr)
	{
		RuntimeTerrainBufferAllocator->Release(RuntimeTerrainVertexSlice);
		RuntimeTerrainBufferAllocator->Release(RuntimeTerrainIndexSlice);
	}
	RuntimeTerrainBufferAllocator = nullptr;
	RuntimeTerrainVertexSlice = {};
	RuntimeTerrainIndexSlice = {};
}
