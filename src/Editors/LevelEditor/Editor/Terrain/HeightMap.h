#pragma once

class CEditableObject;

namespace XRay::Editor::HeightmapUtils
{
	struct SHeightMapVertex
	{
		Fvector P;
		u32 Color;
	};

	struct SGpuCache; // Defined in HeightMap.cpp (owns ref_geom)

	// One render chunk: CHUNK_SIZE x CHUNK_SIZE quads with its own vertex buffer.
	struct SHeightMapChunk
	{
		u32 X0 = 0, Z0 = 0, X1 = 0, Z1 = 0; // Covered vertex range [X0, X1] x [Z0, Z1]
		Fbox BBox;                           // Local-space bounds
		u32 VertexCount = 0;
		bool IsDirty = true;
		SGpuCache* Gpu = nullptr;
	};

	struct SHeightMapRenderData
	{
		static constexpr u32 CHUNK_SIZE = 32;

		xr_vector<SHeightMapChunk> Chunks;
		u32 ChunksX = 0;
		u32 ChunksZ = 0;
		bool IsLayoutDirty = true; // Whole chunk layout must be rebuilt

		float LayoutScaleY = -1.f;
		Fvector LayoutSize = {0, 0, 0};

		void InvalidateGpu();
		void Clear();
	};

	// Heightmap in normalized units [0, 1]. A value <= 0 marks a hole.
	// Local space of the grid matches the generated terrain mesh:
	//   x = -(ix * Size.x - HalfX), z = iz * Size.z - HalfZ, y = (h - 0.5) * ScaleY * Size.y
	struct SHeightMap
	{
		static constexpr float MIN_SOLID = 1.f / 65535.f;

		u32 Width = 0;
		u32 Height = 0;
		xr_vector<float> Data;
		float MinH = 0.f;
		float MaxH = 0.f;

		Fvector Pos = {0, 0, 0};
		Fvector Size = {1, 1, 1};

		SHeightMapRenderData RenderData;

		// Optional color texture for the preview (ARGB, row 0 = v 0), stretched over the whole map like the mesh UV
		xr_vector<u32> PreviewTex;
		u32 PreviewTexW = 0;
		u32 PreviewTexH = 0;
		enum EPreviewMode : u8
		{
			pmHeight,  // Height tint
			pmTexture, // Surface (color map) texture stretched over the map
			pmColors,  // Per-vertex colors supplied by the owner (terrain mask preview)
		};
		EPreviewMode PreviewMode = pmTexture;
		xr_vector<u32> VertexColors; // Width * Height, used by pmColors

		// Optional painted overlay (Width * Height) drawn over the surface texture in pmTexture
		const xr_vector<u32>* OverlayColor = nullptr;
		const xr_vector<u8>* OverlayCoverage = nullptr;

		void SetPreviewTexture(xr_vector<u32>&& Pixels, u32 W, u32 H);
		u32 SamplePreviewTexture(float u, float v) const;

		bool IsValid() const { return !Data.empty() && Width > 1 && Height > 1; }

		IC float GetHeight(u32 x, u32 z) const
		{
			if (x >= Width || z >= Height)
			{
				return 0.f;
			}
			return Data[z * Width + x];
		}

		IC float GetHeightClamped(int x, int z) const
		{
			x = clampr(x, 0, int(Width) - 1);
			z = clampr(z, 0, int(Height) - 1);
			return Data[z * Width + x];
		}

		float StepX() const { return Size.x > 0.f ? Size.x : 1.f; }
		float StepZ() const { return Size.z > 0.f ? Size.z : 1.f; }
		float HalfX() const { return (Width - 1) * StepX() * 0.5f; }
		float HalfZ() const { return (Height - 1) * StepZ() * 0.5f; }

		// Height in local units of a normalized sample
		IC float ToLocalY(float h, float scaleY) const { return (h - 0.5f) * scaleY * Size.y; }
		IC float FromLocalY(float y, float scaleY) const { return y / std::max(scaleY * Size.y, EPS_S) + 0.5f; }

		Fvector GridToLocal(float gx, float gz, float scaleY) const;
		void LocalToGrid(const Fvector& local, float& gx, float& gz) const;

		// Bilinear sample in grid coordinates (holes are treated as their value)
		float Sample(float gx, float gz) const;

		void Create(u32 w, u32 h, float fill);
		void Resample(u32 w, u32 h);
		void UpdateMinMax();

		bool SaveSteam(IWriter* Writer) const;
		bool LoadSteam(IReader* Reader);
		bool LoadRAW(const char* filename);

		void Draw(const Fmatrix& xform, float scaleY);
		void MarkDirty();
		void MarkDirtyRect(int x0, int z0, int x1, int z1);

		// Ray pick in the local space of the grid
		bool RayPickLocal(float& distance, const Fvector& start, const Fvector& direction, float scaleY, Fvector* outPoint = nullptr) const;

	private:
		void RebuildLayout(float scaleY);
		void BuildChunk(SHeightMapChunk& chunk, float scaleY);
	};
};
