#pragma once
#include "HeightMap.h"

// Multi-material X-Ray terrain.
//
// Every polygon has a material: a level shader (BmmD blender with four details R2-R/G/B/A) and a game material.
// The terrain mask <texture>_mask selects, per pixel, which channel of the polygon's shader is rendered.
// A material renders through its own channel; the other three channels of its shader are used for
// transitions into the neighbouring materials, so two touching materials A and B must agree:
//   A.Details[B.Channel] == B.Details[B.Channel] and B.Details[A.Channel] == A.Details[A.Channel].
//
// Per terrain vertex we keep a material id: auto rules (height / slope) -> hand painting -> roads.
// The mask is the one-hot channel of that material, blurred to give the shaders room for transitions.
namespace XRay::Editor::TerrainMask
{
	constexpr u32 CHANNEL_COUNT = 4;
	constexpr u32 PREVIEW_TEX_SIZE = 32; // Downsampled detail texture used for preview and color bake
	constexpr u8 NO_MATERIAL = 255;
	constexpr u32 MAX_MATERIALS = 64;

	// Downsampled texture for previews
	struct SPreviewTexture
	{
		shared_str Texture;
		xr_vector<u32> Small;        // PREVIEW_TEX_SIZE^2, box-filtered
		u32 AvgColor = 0xFF808080;
		shared_str Loaded;
		bool Tried = false;

		void EnsureLoaded();
		void Reload() { Tried = false; Loaded = nullptr; EnsureLoaded(); }
		u32 SampleTiled(float WorldX, float WorldZ, float TileSize) const;
	};

	struct SMaterial
	{
		shared_str Name;
		shared_str Shader;           // Level shader (BmmD)
		shared_str GameMtl;
		u32 Channel = 0;             // Mask channel this material renders through (0..3 = R, G, B, A)
		float TileSize = 4.f;        // Detail tiling for the preview (meters)

		// Auto rule (material 0 is the base and fills what is left)
		bool AutoEnabled = false;
		float HeightMin = -1000.f;   // World meters
		float HeightMax = 1000.f;
		float HeightBlend = 2.f;
		float SlopeMin = 0.f;        // Degrees
		float SlopeMax = 90.f;
		float SlopeBlend = 5.f;
		float Noise = 0.f;           // 0..1, breaks up the rule borders
		float NoiseScale = 16.f;     // Meters

		// Runtime: details of the shader channels
		shared_str DetailsFor;
		bool DetailsValid = false;   // The shader was found and is a BmmD shader
		SPreviewTexture Details[CHANNEL_COUNT];

		void EnsureDetails();
		void ReloadDetails() { DetailsFor = nullptr; EnsureDetails(); }
	};

	// Reads R2-R / R2-G / R2-B / R2-A of a BmmD level shader
	bool ReadShaderDetails(const char* ShaderName, shared_str Out[CHANNEL_COUNT]);

	struct SCellWeights
	{
		float W[CHANNEL_COUNT];
	};

	// Normalized weights -> 4 x u8 with an exact sum of 255
	void PackWeights(const SCellWeights& C, u8* Out);

	struct SSeam
	{
		u8 A, B;
		u32 Count;      // Grid edges where A touches B
		xr_string Why;
	};

	class CTerrainMask
	{
	public:
		xr_vector<SMaterial> Materials;
		u32 BlendRadius = 2;         // Mask blur radius in cells: width of the transitions

		u32 Width = 0;
		u32 Height = 0;

		xr_vector<u8> PaintId;       // Hand painted material per vertex (NO_MATERIAL = not painted)
		xr_vector<u8> RoadId;        // Material written by roads (NO_MATERIAL = none)
		xr_vector<u8> MaterialId;    // Resolved material per vertex
		xr_vector<u8> Final;         // Mask, 4 x u8 per vertex (sum 255)
		xr_vector<u8> SeamFlag;      // 1 where the vertex touches an incompatible material

		// Surface (color map) painting: RGB color + coverage per vertex, over the color derived from the materials
		xr_vector<u32> SurfaceColor;
		xr_vector<u8> SurfaceCoverage;

		bool Dirty = true;      // Needs a full rebuild
		bool RoadsDirty = true; // Road footprint needs a rebuild
		bool ShowSeams = true;

		void EnsureDefaultMaterial(const char* Shader, const char* GameMtl);
		void Resize(u32 W, u32 H);
		bool HasPaint() const;
		bool HasSurfacePaint() const;
		void ClearPaint();
		void ClearSurfacePaint();
		void EnsureDetails();

		// Rebuild ids, mask and seams in the rect (expanded by the blend radius internally)
		void Rebuild(const HeightmapUtils::SHeightMap& HM, const Fmatrix& Xform, float ScaleY, int X0, int Z0, int X1, int Z1);
		void Rebuild(const HeightmapUtils::SHeightMap& HM, const Fmatrix& Xform, float ScaleY);

		SCellWeights GetFinal(u32 X, u32 Z) const;
		SCellWeights SampleFinal(float Gx, float Gz) const;
		u8 MaterialAt(u32 X, u32 Z) const;
		u8 QuadMaterial(u32 X, u32 Z) const; // Majority of the four corners
		void BuildQuadMaterials(xr_vector<u8>& Out) const;

		void PaintCell(u32 X, u32 Z, u8 Material, bool Erase);
		void PaintSurfaceCell(u32 X, u32 Z, u32 Color, float W, bool Erase);

		// Preview: the cell's shader details blended by the mask, as the game renders it
		u32 PreviewColor(u32 X, u32 Z, float WorldX, float WorldZ) const;
		// Color map value: average colors of the details blended by the mask (without surface painting)
		u32 BaseColor(u32 X, u32 Z) const;
		u32 SurfaceAt(u32 X, u32 Z, u32 Base) const;
		u32 SampleSurface(float Gx, float Gz, u32 Base, float& OutCoverage) const;

		// Material pairs that touch but don't have matching transition details
		static bool Compatible(const SMaterial& A, const SMaterial& B, xr_string* Why);
		void CollectSeams(xr_vector<SSeam>& Out) const;

		void Save(IWriter& F) const;
		bool Load(IReader& F);

	private:
		u8 ComputeAuto(const HeightmapUtils::SHeightMap& HM, const Fmatrix& Xform, float ScaleY, int X, int Z) const;
	};
}
