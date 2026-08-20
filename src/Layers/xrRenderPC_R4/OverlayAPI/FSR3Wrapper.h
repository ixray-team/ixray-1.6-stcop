#pragma once

#include <FidelityFX/host/ffx_fsr3upscaler.h>
#include <FidelityFX/host/backends/dx11/ffx_dx11.h>

class Fsr3Wrapper
{
public:
	struct ContextParameters
	{
		uint32_t flags = 0;
		FfxDimensions2D maxRenderSize = { 0, 0 };
		FfxDimensions2D displaySize = { 0, 0 };
		ID3D11Device* device = nullptr;
	};

	struct DrawParameters
	{
		ID3D11DeviceContext* deviceContext = nullptr;

		// Inputs
		ID3D11Resource* unresolvedColorResource = nullptr;
		ID3D11Resource* motionvectorResource = nullptr;
		ID3D11Resource* depthbufferResource = nullptr;
		ID3D11Resource* reactiveMapResource = nullptr;
		ID3D11Resource* transparencyAndCompositionResource = nullptr;

		// Output
		ID3D11Resource* resolvedColorResource = nullptr;

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
	bool Create(ContextParameters params);
	void Destroy();

	bool Draw(const DrawParameters& params);

	bool IsCreated() const { return Created; }
	FfxDimensions2D GetDisplaySize() const { return ContextDesc.maxUpscaleSize; }

	~Fsr3Wrapper();

private:
	bool Created = false;

	FfxFsr3UpscalerContext Context = {};
	FfxFsr3UpscalerContextDescription ContextDesc = {};
	ContextParameters ContextParams;

	// FSR3 shared resources (see ffxFsr3UpscalerGetSharedResourceDescriptions in the component
	// source - getting the formats wrong is not caught at creation, only later as corrupt output).
	ID3D11Texture2D* DilatedDepth = nullptr;
	ID3D11Texture2D* DilatedMotion = nullptr;
	ID3D11Texture2D* ReconstructedPrevDepth = nullptr;

	xr_vector<char> ScratchBuffer;
};

extern Fsr3Wrapper g_Fsr3Wrapper;
