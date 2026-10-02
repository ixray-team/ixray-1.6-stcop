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

#if defined(IXR_APPLE_SERIES)
inline bool Fsr3Wrapper::GetRenderScale(float&, u32, float, u32, u32) { return false; }
inline s32 Fsr3Wrapper::GetJitterPhaseCount(u32, u32) const { return 1; }
inline void Fsr3Wrapper::GetJitterOffset(float& out_x, float& out_y, u32, s32) const { out_x = 0.f; out_y = 0.f; }
inline bool Fsr3Wrapper::Create(ContextParameters) { return false; }
inline void Fsr3Wrapper::Destroy() {}
inline bool Fsr3Wrapper::Draw(const DrawParameters&) { return false; }
inline bool Fsr3Wrapper::IsCreated() const { return false; }
inline RHIExtent2D Fsr3Wrapper::GetDisplaySize() const { return {}; }
inline Fsr3Wrapper::~Fsr3Wrapper() {}
inline u32 Fsr3Wrapper::GetOptimalPresetForScale(float, u32) { return 0; }

inline Fsr3Wrapper g_Fsr3Wrapper;
#endif
