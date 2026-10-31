#include "stdafx.h"

#include "EditorDrawContext.h"

RENDER_API CEditorDrawContext GEditorDraw;

void CEditorDrawContext::OnDeviceCreate()
{
	WireShader.create("editor\\wire");
	SelectionShader.create("editor\\selection");
	ShaderTL.create("editor\\default");
}

void CEditorDrawContext::OnDeviceDestroy()
{
	CurrentShader = nullptr;

	WireShader.destroy();
	SelectionShader.destroy();
	ShaderTL.destroy();

	_RELEASE(DetailInstanceSRV);
	_RELEASE(DetailInstanceBuffer);
}

void CEditorDrawContext::SetShader(const ref_shader& Shader)
{
	CurrentShader = Shader;
	SetRS(D3DRS_FILLMODE, FillMode);
}

void CEditorDrawContext::SetRS(D3DRENDERSTATETYPE State, u32 Value)
{
	switch (State)
	{
		case D3DRS_TEXTUREFACTOR:
		{
			Fcolor Color;
			Color.set(Value);
			RCache.Constants.Write<CBMaterial>().tfactor.set(Color.r, Color.g, Color.b, Color.a);
		}
		break;
		default:
			RCache.set_RenderState(State, Value);
		break;
	}
}

// HACK: detail VS reads per-instance data from a StructuredBuffer (filled by CDetailManager in game/LE);
// single objects are drawn as instance 0 with one identity instance.
void CEditorDrawContext::BindDetailInstance(ref_shader& Shader, u32 Pass)
{
	SPass& P = *Shader->E[0]->passes[Pass];
	if (!P.vs || P.vs->instance_buffer_slot == u32(-1))
		return;

	if (!DetailInstanceSRV)
	{
		struct alignas(16) SInstance
		{
			Fvector quat;
			float scale;
			Fvector pos;
			float hemi;
			float trample_strength;
			float trample_visual;
			float trample_dir[2];
		};

		SInstance Inst{};
		Inst.scale = 1.f;
		Inst.hemi = 1.f;

		RHIBufferDesc Desc{};
		Desc.Usage = ERHI_USAGE::USAGE_IMMUTABLE;
		Desc.Type = ERHI_BUFFER_TYPE::STRUCTURED;
		Desc.CPUAccessFlags = ERHI_CPU_ACCESS_FLAG::ERHI_CPU_ACCESS_FLAG_NONE;
		Desc.StructureByteStride = sizeof(SInstance);
		Desc.Size = sizeof(SInstance);

		RHIBufferSubresource Init{};
		Init.pSysMem = &Inst;
		DetailInstanceBuffer = GRHI->CreateBuffer(Desc, &Init);
		if (!DetailInstanceBuffer)
			return;

		RHIShaderResourceViewDesc SrvDesc{};
		SrvDesc.Format = ERHI_FORMAT::UNKNOWN;
		SrvDesc.ElementWidth = 1;
		DetailInstanceSRV = GRHI->CreateShaderResourceView(DetailInstanceBuffer, &SrvDesc);
	}

	RCache.set_VSResource(P.vs->instance_buffer_slot, DetailInstanceSRV);
}

void CEditorDrawContext::DP(ERHI_PRIMITIVE_TOPOLOGY Topology, ref_geom Geom, u32 BaseVertex, u32 PrimitiveCount)
{
	if (IsSubmitPass())
	{
		return;
	}

	ref_shader S = CurrentShader ? CurrentShader : WireShader;
	u32 PassCount = S->E[0]->passes.size();

	for (u32 Pass = 0; Pass < PassCount; Pass++)
	{
		RCache.set_Shader(S, Pass);
		BindDetailInstance(S, Pass);
		SetRS(D3DRS_FILLMODE, FillMode);
		RCache.set_Geometry(Geom);
		RCache.Render(Topology, BaseVertex, PrimitiveCount);
	}
}

void CEditorDrawContext::DIP(ERHI_PRIMITIVE_TOPOLOGY Topology, ref_geom Geom, u32 BaseVertex, u32 StartVertex, u32 VertexCount, u32 StartIndex, u32 PrimitiveCount)
{
	if (IsSubmitPass())
	{
		return;
	}

	ref_shader S = CurrentShader ? CurrentShader : WireShader;
	u32 PassCount = S->E[0]->passes.size();
	RCache.set_Geometry(Geom);

	for (u32 Pass = 0; Pass < PassCount; Pass++)
	{
		RCache.set_Shader(S, Pass);
		BindDetailInstance(S, Pass);
		SetRS(D3DRS_FILLMODE, FillMode);
		RCache.Render(Topology, BaseVertex, StartVertex, VertexCount, StartIndex, PrimitiveCount);
	}
}
