#include "stdafx.h"
#include "HeightMap.h"

using XRay::Editor::HeightmapUtils::SHeightMap;
using XRay::Editor::HeightmapUtils::SHeightMapChunk;
using XRay::Editor::HeightmapUtils::SHeightMapRenderData;
using XRay::Editor::HeightmapUtils::SHeightMapVertex;


// GPU side of the heightmap preview: chunked vertex buffers, lighting, drawing.
// Kept apart from HeightMap.cpp so the data / math part has no render dependencies.

namespace XRay::Editor::HeightmapUtils
{
	struct SGpuCache
	{
		ref_geom Geom;
	};

	namespace
	{
		void ReleaseGpu(SGpuCache*& Gpu)
		{
			if (!Gpu)
			{
				return;
			}

			if (Gpu->Geom)
			{
				_RELEASE(Gpu->Geom->vb);
				_RELEASE(Gpu->Geom->ib);
				Gpu->Geom.destroy();
			}
			xr_delete(Gpu);
		}

		u32 LerpColor(const Fvector& A, const Fvector& B, float T, float Shade)
		{
			Fvector C;
			C.lerp(A, B, clampr(T, 0.f, 1.f));
			C.mul(clampr(Shade, 0.f, 1.f));
			return color_rgba(u8(C.x), u8(C.y), u8(C.z), 255);
		}
	}
}

void SHeightMapRenderData::InvalidateGpu()
{
	for (SHeightMapChunk& Chunk : Chunks)
	{
		XRay::Editor::HeightmapUtils::ReleaseGpu(Chunk.Gpu);
		Chunk.IsDirty = true;
	}
}

void SHeightMap::RebuildLayout(float scaleY)
{
	SHeightMapRenderData& RD = RenderData;
	RD.Clear();

	const u32 CS = SHeightMapRenderData::CHUNK_SIZE;
	RD.ChunksX = (Width - 2) / CS + 1;
	RD.ChunksZ = (Height - 2) / CS + 1;
	RD.Chunks.resize(RD.ChunksX * RD.ChunksZ);

	for (u32 cz = 0; cz < RD.ChunksZ; ++cz)
	{
		for (u32 cx = 0; cx < RD.ChunksX; ++cx)
		{
			SHeightMapChunk& Chunk = RD.Chunks[cz * RD.ChunksX + cx];
			Chunk.X0 = cx * CS;
			Chunk.Z0 = cz * CS;
			Chunk.X1 = std::min(Chunk.X0 + CS, Width - 1);
			Chunk.Z1 = std::min(Chunk.Z0 + CS, Height - 1);
			Chunk.IsDirty = true;
		}
	}

	RD.LayoutScaleY = scaleY;
	RD.LayoutSize = Size;
	RD.IsLayoutDirty = false;
}

void SHeightMap::BuildChunk(SHeightMapChunk& Chunk, float scaleY)
{
	XRay::Editor::HeightmapUtils::ReleaseGpu(Chunk.Gpu);
	Chunk.IsDirty = false;
	Chunk.VertexCount = 0;
	Chunk.BBox.invalidate();

	static const Fvector LowColor = {72.f, 92.f, 58.f};
	static const Fvector HighColor = {228.f, 222.f, 206.f};
	Fvector LightDir = {0.45f, 1.f, 0.3f};
	LightDir.normalize();

	const float Sx = StepX();
	const float Sz = StepZ();

	auto LocalAt = [&](int x, int z) -> Fvector
	{
		Fvector P;
		P.x = -(x * Sx - HalfX());
		P.z = z * Sz - HalfZ();
		P.y = ToLocalY(GetHeightClamped(x, z), scaleY);
		return P;
	};

	auto MakeVertex = [&](int x, int z) -> SHeightMapVertex
	{
		const Fvector P = LocalAt(x, z);

		// Central-difference normal in local space
		Fvector Tx, Tz, N;
		Tx.sub(LocalAt(x + 1, z), LocalAt(x - 1, z));
		Tz.sub(LocalAt(x, z + 1), LocalAt(x, z - 1));
		N.crossproduct(Tz, Tx);
		if (N.y < 0.f)
		{
			N.invert();
		}
		N.normalize_safe();

		const float Shade = 0.3f + 0.7f * std::max(0.f, N.dotproduct(LightDir));
		u32 Color;
		if (PreviewMode == pmColors && VertexColors.size() == size_t(Width) * Height)
		{
			const u32 T = VertexColors[size_t(z) * Width + x];
			const float S = clampr(0.45f + 0.55f * Shade, 0.f, 1.f);
			Color = color_rgba(u8(color_get_R(T) * S), u8(color_get_G(T) * S), u8(color_get_B(T) * S), 255);
		}
		else if (PreviewMode == pmTexture && (!PreviewTex.empty() || OverlayCoverage))
		{
			// Same UV as the generated terrain mesh
			const float U = 1.f - float(x) / float(Width - 1);
			const float V = 1.f - float(z) / float(Height - 1);
			u32 T = SamplePreviewTexture(U, V);
			const size_t Idx = size_t(z) * Width + x;
			if (OverlayColor && OverlayCoverage && Idx < OverlayCoverage->size() && (*OverlayCoverage)[Idx])
			{
				const float C = (*OverlayCoverage)[Idx] / 255.f;
				const u32 O = (*OverlayColor)[Idx];
				auto L = [&](u32 A, u32 B) { return u8(A + (float(B) - float(A)) * C); };
				T = color_rgba(L(color_get_R(T), color_get_R(O)), L(color_get_G(T), color_get_G(O)), L(color_get_B(T), color_get_B(O)), 255);
			}
			const float S = clampr(0.45f + 0.55f * Shade, 0.f, 1.f);
			Color = color_rgba(u8(color_get_R(T) * S), u8(color_get_G(T) * S), u8(color_get_B(T) * S), 255);
		}
		else
		{
			Color = LerpColor(LowColor, HighColor, GetHeight(x, z), Shade);
		}
		return {P, Color};
	};

	xr_vector<SHeightMapVertex> Vertices;
	Vertices.reserve(size_t(Chunk.X1 - Chunk.X0) * (Chunk.Z1 - Chunk.Z0) * 6);

	for (u32 z = Chunk.Z0; z < Chunk.Z1; ++z)
	{
		for (u32 x = Chunk.X0; x < Chunk.X1; ++x)
		{
			if (GetHeight(x, z) <= 0.f || GetHeight(x + 1, z) <= 0.f || GetHeight(x, z + 1) <= 0.f || GetHeight(x + 1, z + 1) <= 0.f)
			{
				continue;
			}

			const SHeightMapVertex V0 = MakeVertex(x, z);
			const SHeightMapVertex V1 = MakeVertex(x + 1, z);
			const SHeightMapVertex V2 = MakeVertex(x, z + 1);
			const SHeightMapVertex V3 = MakeVertex(x + 1, z + 1);

			// Same triangulation as the generated mesh
			Vertices.insert(Vertices.end(), {V0, V1, V2, V1, V3, V2});
			Chunk.BBox.modify(V0.P);
			Chunk.BBox.modify(V1.P);
			Chunk.BBox.modify(V2.P);
			Chunk.BBox.modify(V3.P);
		}
	}

	if (Vertices.empty())
	{
		return;
	}

	IRHIBuffer* pVB = nullptr;
	if (!RHIUtils::CreateVertexBuffer(&pVB, Vertices.data(), u32(Vertices.size() * sizeof(SHeightMapVertex)), false))
	{
		return;
	}

	Chunk.Gpu = new SGpuCache();
	Chunk.Gpu->Geom.create(FVF::F_L, pVB, nullptr);
	Chunk.VertexCount = u32(Vertices.size());
}

void SHeightMap::Draw(const Fmatrix& xform, float scaleY)
{
	if (!IsValid())
	{
		return;
	}

	SHeightMapRenderData& RD = RenderData;
	if (RD.IsLayoutDirty || !fsimilar(RD.LayoutScaleY, scaleY) || !RD.LayoutSize.similar(Size))
	{
		RebuildLayout(scaleY);
	}

	EDevice->SetShader(GEditorDraw.WireShader);
	RCache.set_xform_world(xform);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);

	CFrustum& Frustum = ::Render->ViewBase;

	for (SHeightMapChunk& Chunk : RD.Chunks)
	{
		if (Chunk.IsDirty)
		{
			BuildChunk(Chunk, scaleY);
		}

		if (!Chunk.Gpu || Chunk.VertexCount == 0)
		{
			continue;
		}

		Fbox WorldBox = Chunk.BBox;
		WorldBox.xform(xform);
		float AABB[6] = {WorldBox.min.x, WorldBox.min.y, WorldBox.min.z, WorldBox.max.x, WorldBox.max.y, WorldBox.max.z};

		u32 Mask = 0xFF;
		if (Frustum.testAABB(AABB, Mask) == fcvNone)
		{
			continue;
		}

		EDevice->DP(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, Chunk.Gpu->Geom, 0, Chunk.VertexCount / 3);
	}

	RCache.set_xform_world(Fidentity);
}
