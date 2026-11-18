#pragma once
#include "HeightMap.h"

// Terrain material mask (X-Ray terrain: base texture + <base>_mask RGBA, one detail layer per channel).
// Final weights = auto rules (height / slope) -> hand painting -> roads on top.
namespace XRay::Editor::TerrainMask
{
	constexpr u32 LAYER_COUNT = 4;
	constexpr u32 PREVIEW_TEX_SIZE = 32; // Downsampled layer texture used for preview and color bake

	struct SLayer
	{
		shared_str Texture;          // Detail texture of this channel (preview / color bake)
		float TileSize = 4.f;        // Meters per texture repeat

		// Auto rule (layer 0 is the base and fills what is left)
		bool AutoEnabled = false;
		float HeightMin = -1000.f;   // World meters
		float HeightMax = 1000.f;
		float HeightBlend = 2.f;
		float SlopeMin = 0.f;        // Degrees
		float SlopeMax = 90.f;
		float SlopeBlend = 5.f;
		float Noise = 0.f;           // 0..1, breaks up the rule borders
		float NoiseScale = 16.f;     // Meters

		// Runtime
		xr_vector<u32> Small;        // PREVIEW_TEX_SIZE^2, box-filtered
		u32 AvgColor = 0xFF808080;
		shared_str Loaded;
		bool Tried = false;

		void EnsureLoaded();
		void Reload() { Tried = false; Loaded = nullptr; EnsureLoaded(); }
		u32 SampleTiled(float WorldX, float WorldZ) const;
	};

	struct SCellWeights
	{
		float W[LAYER_COUNT];
	};

	// Normalized weights -> 4 x u8 with an exact sum of 255
	void PackWeights(const SCellWeights& C, u8* Out);

	class CTerrainMask
	{
	public:
		SLayer Layers[LAYER_COUNT];

		u32 Width = 0;
		u32 Height = 0;

		// Hand painting: normalized weights (4 x u8) + coverage
		xr_vector<u8> PaintWeights;
		xr_vector<u8> PaintCoverage;

		// Roads: per cell, shoulder and road layers with their weights (255 = no layer)
		xr_vector<u8> RoadLayer;
		xr_vector<u8> RoadWeight;
		xr_vector<u8> ShoulderLayer;
		xr_vector<u8> ShoulderWeight;

		// Surface (color map) painting: RGB color + coverage per cell, over the color derived from the layers
		xr_vector<u32> SurfaceColor;
		xr_vector<u8> SurfaceCoverage;

		// Combined result, 4 x u8 per cell (sum 255)
		xr_vector<u8> Final;

		bool Dirty = true;      // Final needs a full rebuild
		bool RoadsDirty = true; // Road buffers need a rebuild

		void Resize(u32 W, u32 H);
		bool HasPaint() const;
		bool HasSurfacePaint() const;
		void ClearPaint();
		void ClearSurfacePaint();

		void RebuildFinal(const HeightmapUtils::SHeightMap& HM, const Fmatrix& Xform, float ScaleY, int X0, int Z0, int X1, int Z1);
		void RebuildFinal(const HeightmapUtils::SHeightMap& HM, const Fmatrix& Xform, float ScaleY);

		SCellWeights GetFinal(u32 X, u32 Z) const;
		SCellWeights SampleFinal(float Gx, float Gz) const;

		// Brush: add layer Layer with weight W in [0,1], or erase painting
		void PaintCell(u32 X, u32 Z, u32 Layer, float W, bool Erase);
		void PaintSurfaceCell(u32 X, u32 Z, u32 Color, float W, bool Erase);

		// Color map value of a cell: layer average colors (or tiled textures) blended by the weights, then surface painting
		u32 SurfaceAt(u32 X, u32 Z, u32 Base) const;
		u32 SampleSurface(float Gx, float Gz, u32 Base, float& OutCoverage) const;

		// Preview color of a cell: layer textures blended by the final weights
		u32 PreviewColor(u32 X, u32 Z, float WorldX, float WorldZ) const;

		void Save(IWriter& F) const;
		bool Load(IReader& F);

	private:
		SCellWeights ComputeAuto(const HeightmapUtils::SHeightMap& HM, const Fmatrix& Xform, float ScaleY, int X, int Z) const;
	};
}
