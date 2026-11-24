/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
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
	ref_shader TerrainShader;

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
	// DP of records written to the dynamic geometry stream
	void DrawStream(ERHI_PRIMITIVE_TOPOLOGY Topology, ref_geom Geom, u32 BaseVertex, u32 PrimitiveCount);
	void DIP(ERHI_PRIMITIVE_TOPOLOGY Topology, ref_geom Geom, u32 BaseVertex, u32 StartVertex, u32 VertexCount, u32 StartIndex, u32 PrimitiveCount);

private:
	// IsStream: a draw of the dynamic geometry stream, which reads it through NRI
	void BindDetailInstance(ref_shader& Shader, u32 Pass, bool IsStream);
	void Draw(ERHI_PRIMITIVE_TOPOLOGY Topology, ref_geom Geom, u32 BaseVertex, u32 PrimitiveCount, bool IsStream);

	ref_shader CurrentShader;

	IRHIBuffer* DetailInstanceBuffer = nullptr;
	IRHIShaderResourceView* DetailInstanceSRV = nullptr;
};

extern RENDER_API CEditorDrawContext GEditorDraw;

class CEditorFillModeScope
{
public:
	explicit CEditorFillModeScope(GfxCmdList& Cmd, bool Enable) :
		Cmd(Cmd), Active(Enable && GEditorDraw.FillMode != D3DFILL_SOLID)
	{
		if (Active)
		{
			Cmd.SetFillModeOverride(true, GEditorDraw.FillMode);
		}
	}

	~CEditorFillModeScope()
	{
		if (Active)
		{
			Cmd.SetFillModeOverride(false, D3DFILL_SOLID);
		}
	}

	CEditorFillModeScope(const CEditorFillModeScope&) = delete;
	CEditorFillModeScope& operator=(const CEditorFillModeScope&) = delete;

private:
	GfxCmdList& Cmd;
	bool Active;
};
