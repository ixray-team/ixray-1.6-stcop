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
#include "stdafx.h"

#include "EditorDrawContext.h"
#include "D3DUtils.h"

RENDER_API CEditorDrawContext GEditorDraw;

void CEditorDrawContext::OnDeviceCreate()
{
	WireShader.create("editor\\wire");
	SelectionShader.create("editor\\selection");
	ShaderTL.create("editor\\default");
	TerrainShader.create("editor\\terrain");
}

void CEditorDrawContext::OnDeviceDestroy()
{
	CurrentShader = nullptr;

	WireShader.destroy();
	SelectionShader.destroy();
	ShaderTL.destroy();
	TerrainShader.destroy();

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
	GfxCmdList& Cmd = RImplementation.Imm();
	const auto Targets = [&Cmd](auto Set)
	{
		for (RHIRenderTargetBlendDesc& Target : Cmd.EditBlend().RenderTarget)
		{
			Set(Target);
		}
	};
	const auto Faces = [&Cmd](auto Set, bool Front)
	{
		RHIDepthStencilDesc& Desc = Cmd.EditDepth();
		Set(Desc.BackFace);
		if (Front)
		{
			Set(Desc.FrontFace);
		}
	};
	switch (State)
	{
		case D3DRS_TEXTUREFACTOR:
		{
			Fcolor Color;
			Color.set(Value);
			Cmd.Constants.Write<CBMaterial>().tfactor.set(Color.r, Color.g, Color.b, Color.a);
		}
		break;
		case D3DRS_ZENABLE: Cmd.SetDepthEnable(Value); break;
		case D3DRS_ZWRITEENABLE: Cmd.SetDepthWrite(Value != 0); break;
		case D3DRS_ZFUNC: Cmd.SetDepthFunc(Value); break;
		case D3DRS_FILLMODE: Cmd.EditRaster().FillMode = Value == RHI_FILL_SOLID ? RHI_FILL_SOLID : RHI_FILL_WIREFRAME; break;
		case D3DRS_CULLMODE: Cmd.SetCullMode(ERHI_CULLMODE(Value)); break;
		case D3DRS_SCISSORTESTENABLE: Cmd.EditRaster().ScissorEnable = Value != 0; break;
		case D3DRS_ALPHABLENDENABLE: Targets([Value](RHIRenderTargetBlendDesc& Target) { Target.BlendEnable = Value != 0; }); break;
		case D3DRS_SRCBLEND: Targets([Value](RHIRenderTargetBlendDesc& Target) { Target.SrcBlend = ERHI_BLEND(Value); }); break;
		case D3DRS_DESTBLEND: Targets([Value](RHIRenderTargetBlendDesc& Target) { Target.DestBlend = ERHI_BLEND(Value); }); break;
		case D3DRS_BLENDOP: Targets([Value](RHIRenderTargetBlendDesc& Target) { Target.BlendOp = ERHI_BLEND_OP(Value); }); break;
		case D3DRS_SRCBLENDALPHA: Targets([Value](RHIRenderTargetBlendDesc& Target) { Target.SrcBlendAlpha = ERHI_BLEND(Value); }); break;
		case D3DRS_DESTBLENDALPHA: Targets([Value](RHIRenderTargetBlendDesc& Target) { Target.DestBlendAlpha = ERHI_BLEND(Value); }); break;
		case D3DRS_BLENDOPALPHA: Targets([Value](RHIRenderTargetBlendDesc& Target) { Target.BlendOpAlpha = ERHI_BLEND_OP(Value); }); break;
		case D3DRS_COLORWRITEENABLE: Cmd.SetColorWriteEnable(Value); break;
		case D3DRS_COLORWRITEENABLE1:
		case D3DRS_COLORWRITEENABLE2:
		case D3DRS_COLORWRITEENABLE3: Cmd.EditBlend().RenderTarget[State - D3DRS_COLORWRITEENABLE1].RenderTargetWriteMask = u8(Value); break;
		case D3DRS_STENCILENABLE: Cmd.EditDepth().StencilEnable = Value != 0; break;
		case D3DRS_STENCILMASK: Cmd.EditDepth().StencilReadMask = u8(Value); break;
		case D3DRS_STENCILWRITEMASK: Cmd.EditDepth().StencilWriteMask = u8(Value); break;
		case D3DRS_STENCILREF: Cmd.SetStencilRef(Value); break;
		case D3DRS_STENCILFAIL: Faces([Value](RHIStencilOpDesc& Face) { Face.StencilFailOp = ERHI_STENCIL_OP(Value); }, true); break;
		case D3DRS_STENCILZFAIL: Faces([Value](RHIStencilOpDesc& Face) { Face.StencilDepthFailOp = ERHI_STENCIL_OP(Value); }, true); break;
		case D3DRS_STENCILPASS: Faces([Value](RHIStencilOpDesc& Face) { Face.StencilPassOp = ERHI_STENCIL_OP(Value); }, true); break;
		case D3DRS_STENCILFUNC: Faces([Value](RHIStencilOpDesc& Face) { Face.StencilFunc = ERHI_COMPARISON(Value); }, true); break;
		case D3DRS_CCW_STENCILFAIL: Faces([Value](RHIStencilOpDesc& Face) { Face.StencilFailOp = ERHI_STENCIL_OP(Value); }, false); break;
		case D3DRS_CCW_STENCILZFAIL: Faces([Value](RHIStencilOpDesc& Face) { Face.StencilDepthFailOp = ERHI_STENCIL_OP(Value); }, false); break;
		case D3DRS_CCW_STENCILPASS: Faces([Value](RHIStencilOpDesc& Face) { Face.StencilPassOp = ERHI_STENCIL_OP(Value); }, false); break;
		case D3DRS_CCW_STENCILFUNC: Faces([Value](RHIStencilOpDesc& Face) { Face.StencilFunc = ERHI_COMPARISON(Value); }, false); break;
		default: break;
	}
}

// HACK: detail VS reads per-instance data from a StructuredBuffer (filled by CDetailManager in game/LE);
// single objects are drawn as instance 0 with one identity instance.
void CEditorDrawContext::BindDetailInstance(ref_shader& Shader, u32 Pass, bool IsStream)
{
	SPass& P = *Shader->E[0]->passes[Pass];
	if (!P.vs || P.vs->instance_buffer_slot == u32(-1))
		return;

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

	static const SInstance Inst = []
	{
		SInstance Item{};
		Item.scale = 1.f;
		Item.hemi = 1.f;
		return Item;
	}();

	if (!DetailInstanceSRV)
	{
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

	GfxCmdList& Cmd = RImplementation.Imm();

	if (IsStream)
	{
		Cmd.SetResource(ERHI_SHADER_TYPE::VS, P.vs->instance_buffer_slot, DetailInstanceSRV, RImplementation.NriPasses.StreamRecords(&Inst, sizeof(Inst), sizeof(Inst)));
	}
	else
	{
		Cmd.SetResource(ERHI_SHADER_TYPE::VS, P.vs->instance_buffer_slot, DetailInstanceSRV);
	}
}

void CEditorDrawContext::DP(ERHI_PRIMITIVE_TOPOLOGY Topology, ref_geom Geom, u32 BaseVertex, u32 PrimitiveCount)
{
	Draw(Topology, Geom, BaseVertex, PrimitiveCount, false);
}

void CEditorDrawContext::DrawStream(ERHI_PRIMITIVE_TOPOLOGY Topology, ref_geom Geom, u32 BaseVertex, u32 PrimitiveCount)
{
	Draw(Topology, Geom, BaseVertex, PrimitiveCount, true);
}

void CEditorDrawContext::Draw(ERHI_PRIMITIVE_TOPOLOGY Topology, ref_geom Geom, u32 BaseVertex, u32 PrimitiveCount, bool IsStream)
{
	if (IsSubmitPass())
	{
		return;
	}

	GfxCmdList& Cmd = RImplementation.Imm();
	ref_shader S = CurrentShader ? CurrentShader : WireShader;
	u32 PassCount = S->E[0]->passes.size();

	for (u32 Pass = 0; Pass < PassCount; Pass++)
	{
		Cmd.SetPass(S, Pass);
		BindDetailInstance(S, Pass, IsStream);
		SetRS(D3DRS_FILLMODE, FillMode);
		DebugDraw::Primitive(Cmd, Topology, Geom, BaseVertex, PrimitiveCount, IsStream);
	}
}

void CEditorDrawContext::DIP(ERHI_PRIMITIVE_TOPOLOGY Topology, ref_geom Geom, u32 BaseVertex, u32 StartVertex, u32 VertexCount, u32 StartIndex, u32 PrimitiveCount)
{
	if (IsSubmitPass())
	{
		return;
	}

	GfxCmdList& Cmd = RImplementation.Imm();
	ref_shader S = CurrentShader ? CurrentShader : WireShader;
	u32 PassCount = S->E[0]->passes.size();
	Cmd.SetGeometry(Geom);

	for (u32 Pass = 0; Pass < PassCount; Pass++)
	{
		Cmd.SetPass(S, Pass);
		BindDetailInstance(S, Pass, false);
		SetRS(D3DRS_FILLMODE, FillMode);
		Cmd.DrawIndexed(Topology, RHITopologyUtils::GetIndexCount(PrimitiveCount, Topology), StartIndex, BaseVertex);
	}
}
