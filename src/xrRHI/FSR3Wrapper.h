#pragma once

#include "RHI.h"

struct RHIExtent2D { u32 width; u32 height; };

class RHI_API Fsr3Wrapper
{
public:
	struct ContextParameters
	{
		uint32_t flags = 0;
		RHIExtent2D maxRenderSize = { 0, 0 };
		RHIExtent2D displaySize = { 0, 0 };

	};

	struct DrawParameters
	{

		// Inputs
		IRHISurface* unresolvedColorResource = nullptr;
		IRHISurface* motionvectorResource = nullptr;
		IRHISurface* depthbufferResource = nullptr;
		IRHISurface* reactiveMapResource = nullptr;
		IRHISurface* transparencyAndCompositionResource = nullptr;

		// Output
		IRHISurface* resolvedColorResource = nullptr;

		// Arguments
		uint32_t renderWidth = 0;
		uint32_t renderHeight = 0;
		uint32_t displayWidth = 0;
		uint32_t displayHeight = 0;

		bool cameraReset = false;
		float cameraJitterX = 0.f;
		float cameraJitterY = 0.f;

		bool enableSharpening = true;
		float sharpness = 0.f;

		float frameTimeDelta = 0.f;

		float nearPlane = 1.f;
		float farPlane = 10.f;
		float fovH = 90.f;
	};

public:
	bool GetRenderScale(float& RenderScale, u32 preset, float scale, u32 width, u32 height);
	s32 GetJitterPhaseCount(u32 render_width, u32 display_width) const;
	void GetJitterOffset(float& out_x, float& out_y, u32 frame, s32 phase_count) const;
	bool Create(ContextParameters params);
	void Destroy();

	bool Draw(const DrawParameters& params);

	bool IsCreated() const;
	RHIExtent2D GetDisplaySize() const;

	~Fsr3Wrapper();

private:
	u32 GetOptimalPresetForScale(float scale, u32 preset);
};

extern RHI_API Fsr3Wrapper g_Fsr3Wrapper;
