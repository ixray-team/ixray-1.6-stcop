#pragma once

#include "SH_RT.h"
#include "Shader.h"

class CRender;

extern bool g_GrassTerrainTintEnabled;
extern bool g_GrassTerrainTintGreenOnly;
extern float g_GrassTerrainTintStrength;
extern float g_GrassTerrainTintBrightness;
extern float g_GrassTerrainTintSaturation;
extern float g_GrassTerrainTintContrast;
extern float g_GrassTerrainTintTips;
extern float g_GrassTerrainTintResolutionScale;
extern float g_GrassTerrainTintRadius;
extern float g_GrassTerrainTintHeightLimit;
extern int g_GrassTerrainTintResolution;
extern int g_GrassTerrainTintMode;
extern int g_GrassTerrainTintDebug;

class GrassTerrainTintMap
{
public:
	void Destroy();
	void RequestRebuild();
	void Update(CRender& Renderer);
	void DrawDebug(CRender& Renderer);
	void PrintInfo() const;
	bool IsReady() const { return Ready; }
	const Fvector4& GetBounds() const { return Bounds; }
	const Fvector4& GetHeightRange() const { return HeightRange; }
	Fvector4 GetParameters() const;
	Fvector4 GetOptions() const;
	Fvector4 GetAdjustment() const;

private:
	ref_rt Color;
	ref_rt Height;
	ref_shader DebugShader;
	ref_texture_list EmptyTextures;
	Fvector4 Bounds{};
	Fvector4 HeightRange{};
	xr_atomic_bool RebuildPending = true;
	u32 Resolution = 0;
	u32 SourceResolution = 0;
	int RequestedResolution = -1;
	float RequestedScale = 0.0f;
	bool Ready = false;
};
