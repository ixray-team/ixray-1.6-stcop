#pragma once

#include <xess/xess.h>
#include <xess/xess_d3d11.h>

// Intel XeSS Super Resolution (D3D11). The D3D11 path is implemented by the Intel graphics
// driver, so context creation fails with XESS_RESULT_ERROR_UNSUPPORTED_DEVICE on other vendors.
class XeSSWrapper
{
public:
	struct ContextParameters
	{
		ID3D11Device* D3DDevice = nullptr;
		Ivector2 RenderSize = { 0, 0 };
		Ivector2 DisplaySize = { 0, 0 };
	};

	struct DrawParameters
	{
		ID3D11DeviceContext* DeviceContext = nullptr;

		// Inputs (render resolution)
		ID3D11Resource* ColorResource = nullptr;
		ID3D11Resource* VelocityResource = nullptr;
		ID3D11Resource* DepthResource = nullptr;

		// Output (display resolution)
		ID3D11Resource* OutputResource = nullptr;

		u32 RenderWidth = 0;
		u32 RenderHeight = 0;

		// Sub-pixel jitter applied to the projection, in render-resolution pixels
		float JitterX = 0.0f;
		float JitterY = 0.0f;

		bool CameraReset = false;
	};

public:
	~XeSSWrapper();

	bool Create(const ContextParameters& Parameters);
	void Destroy();
	bool Draw(const DrawParameters& Parameters);

	bool GetRenderScale(float& RenderScale);

	bool IsCreated() const { return Context != nullptr; }

private:
	static const char* ResultToString(xess_result_t Result);
	static xess_quality_settings_t GetQualityForPreset(u32 PresetID);
	static float GetUpscaleRatioForPreset(u32 PresetID);
	u32 GetOptimalPresetForScale(float Scale);

	xess_context_handle_t Context = nullptr;
	xess_2d_t MinInput = { 0, 0 };
	xess_2d_t MaxInput = { 0, 0 };
	Ivector2 RenderSize = { 0, 0 };

	bool Unsupported = false;
	bool HistoryValid = false;
	u32 LastDrawFrame = 0;
};

extern XeSSWrapper g_XESSWrapper;
