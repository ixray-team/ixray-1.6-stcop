#pragma once
#include "../xrRaysDefines.h"
#include "../../Base/base_color.h"

namespace GpuLightKind
{
	constexpr u16 Sun = 0;
	constexpr u16 Hemi = 1;
	constexpr u16 RGB = 2;
}

// Light source in the layout every GPU tracer expects
struct GpuLight
{
	u16 Type;			// LT_DIRECT, LT_POINT, LT_SECONDARY
	u16 LightType;		// GpuLightKind
	Fvector Diffuse;
	Fvector Position;
	Fvector Direction;
	float Range;
	float Range2;
	float Falloff;
	float Attenuation0;
	float Attenuation1;
	float Attenuation2;
	float Energy;
};

// Per-triangle data needed by the alpha test
struct GpuFace
{
	Fvector2 TC[3];
	u16 SurfaceID;
	bool Opaque;
};

// Alpha channel of a texture; empty when the texture never cuts rays
struct GpuTexture
{
	u32 Width = 0;
	u32 Height = 0;
	xr_vector<u8> Alpha;
};

// Everything a tracer uploads for one compilation: triangle i uses Faces[i]
struct GpuScene
{
	xr_vector<Fvector> Vertices;
	xr_vector<u32> Indices;		// 3 per triangle
	xr_vector<GpuFace> Faces;
	xr_vector<GpuLight> Lights;
	xr_vector<GpuTexture> Textures;
};

// GPU implementation of LightPoint for a batch of sample points
class IGpuRayTracer
{
public:
	virtual ~IGpuRayTracer() = default;

	virtual const char* GetName() const = 0;

	// Create the device once per process
	virtual void Initialize() = 0;

	virtual void LoadScene(const GpuScene& Scene) = 0;
	virtual void UnloadScene() = 0;

	// Thread safe. Colors[i] receives the lighting of Rays[i]; Rays.size() <= MaxRaysPerBatch
	virtual void Trace(u8 Flags, const xr_vector<RayRecvestIndex>& Rays, xr_vector<base_color_c>& Colors) = 0;

	// Free the batch buffers owned by the calling thread
	virtual void ReleaseThreadData() = 0;

	static constexpr u32 MaxRaysPerBatch = 64 * 1024;
};

namespace XRay::RayTrace::GPU
{
	// Collect level (xrLC) or detail (xrDO) geometry, lights and alpha textures
	void BuildScene(GpuScene& Scene);

	// Lighting preview in the compiler window
	void ForgetPreviewFaces();
	void CapturePreviewBakedColors(bool IncludeVertex);
	void CapturePreviewMU();
}
