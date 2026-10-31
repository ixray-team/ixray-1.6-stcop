#pragma once

#include "RenderAPI.h"
#include "Shader.h"

// How the editor scene code is being run in the current frame.
enum class EEditorDrawPass
{
	// Draw everything right away (previews and tools outside the scene pipeline)
	Immediate,
	// Scene geometry goes to the renderer scene graph; helper draws are skipped
	Submit,
	// Helpers are drawn over the lit scene; scene geometry was already submitted
	Overlay,
};

// Draw state used by the editor device and the editor draw utilities: helper shaders,
// fill mode, and draw calls that apply every pass of the current shader.
class RENDER_API CEditorDrawContext
{
public:
	EEditorDrawPass Pass = EEditorDrawPass::Immediate;

	bool IsSubmitPass() const
	{
		return Pass == EEditorDrawPass::Submit;
	}

	bool IsOverlayPass() const
	{
		return Pass == EEditorDrawPass::Overlay;
	}

	ref_shader WireShader;
	ref_shader SelectionShader;
	ref_shader ShaderTL;

	u32 FillMode = D3DFILL_SOLID;
	u32 ShadeMode = D3DSHADE_GOURAUD;
	float ScreenQuality = 1.f;
	u32 Upscaler = 1;
	bool UpscalerReset = false;

	void OnDeviceCreate();
	void OnDeviceDestroy();

	void SetShader(const ref_shader& Shader);
	const ref_shader& GetShader() const
	{
		return CurrentShader;
	}

	void SetRS(D3DRENDERSTATETYPE State, u32 Value);

	void DP(ERHI_PRIMITIVE_TOPOLOGY Topology, ref_geom Geom, u32 BaseVertex, u32 PrimitiveCount);
	void DIP(ERHI_PRIMITIVE_TOPOLOGY Topology, ref_geom Geom, u32 BaseVertex, u32 StartVertex, u32 VertexCount, u32 StartIndex, u32 PrimitiveCount);

private:
	void BindDetailInstance(ref_shader& Shader, u32 Pass);

	ref_shader CurrentShader;

	IRHIBuffer* DetailInstanceBuffer = nullptr;
	IRHIShaderResourceView* DetailInstanceSRV = nullptr;
};

extern RENDER_API CEditorDrawContext GEditorDraw;

class CEditorFillModeScope
{
public:
	explicit CEditorFillModeScope(bool Enable) :
		Active(Enable && GEditorDraw.FillMode != D3DFILL_SOLID)
	{
		if (Active)
		{
			RCache.set_FillModeOverride(true, GEditorDraw.FillMode);
		}
	}

	~CEditorFillModeScope()
	{
		if (Active)
		{
			RCache.set_FillModeOverride(false, D3DFILL_SOLID);
		}
	}

	CEditorFillModeScope(const CEditorFillModeScope&) = delete;
	CEditorFillModeScope& operator=(const CEditorFillModeScope&) = delete;

private:
	bool Active;
};
