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
#include "ImGuiNri.h"
#include "dxRenderDeviceRender.h"
#include "PackedVertex.h"

namespace
{
// ImGui's colours are 0xAABBGGRR, the stream's 0xAARRGGBB
u32 StreamColor(ImU32 Color)
{
	return (Color & 0xFF00FF00u) | ((Color & 0xFFu) << 16) | ((Color >> 16) & 0xFFu);
}

// ImGui's own: straight alpha into the colour, the target's alpha kept as the platform backends keep it
void SetBlend(GfxCmdList& Cmd, bool IsOpaque)
{
	RHIBlendDesc& Blend = Cmd.EditBlend();
	Blend.AlphaToCoverageEnable = false;
	Blend.IndependentBlendEnable = false;
	Blend.RenderTarget[0] = { !IsOpaque, RHI_BLEND_SRC_ALPHA, RHI_BLEND_INV_SRC_ALPHA, RHI_BLEND_OP_ADD, RHI_BLEND_ONE, RHI_BLEND_INV_SRC_ALPHA, RHI_BLEND_OP_ADD, u8(RHI_COLOR_WRITE_ENABLE_ALL) };
}

// Indexed triangles become records of their corners: pixels of screen_res, the size of the targets, which the vertex shader
// stretches over the viewport. Most of what ImGui draws is rectangles of two triangles on four vertices (text, images,
// plain fills): a run of those goes out as quads, four records each where the triangles took six
void DrawTriangles(GfxCmdList& Cmd, const ImDrawList& List, const ImDrawCmd& Command, const ImVec2& Origin, const ImVec2& Scale)
{
	DynamicGeometry& Stream = RImplementation.Dynamic;
	const ImDrawVert* Vertices = List.VtxBuffer.Data + Command.VtxOffset;
	const ImDrawIdx* Indices = List.IdxBuffer.Data + Command.IdxOffset;
	const u32 Count = Command.ElemCount;

	const auto Record = [&](u32 Index)
	{
		const ImDrawVert& Vertex = Vertices[Index];
		PackedVertex Made = { { (Vertex.pos.x - Origin.x) * Scale.x - 0.5f, (Vertex.pos.y - Origin.y) * Scale.y - 0.5f, 0.0f }, 0, 0, 0, StreamColor(Vertex.col), std::bit_cast<u32>(1.0f) };
		VertexPacking::EncodeFitUV(Made, Fvector2().set(Vertex.uv.x, Vertex.uv.y), Fvector2().set(0.0f, 0.0f));
		return Made;
	};

	// ImGui's rectangle: a, a + 1, a + 2, then a, a + 2, a + 3
	const auto IsQuad = [&](u32 At)
	{
		const ImDrawIdx* I = Indices + At;
		return At + 6 <= Count && I[1] == I[0] + 1 && I[2] == I[0] + 2 && I[3] == I[0] && I[4] == I[2] && I[5] == I[0] + 3;
	};

	// A draw costs more than the records a few rectangles spare: only a run of this many is drawn as quads
	constexpr u32 Least = 8;
	const auto QuadsAt = [&](u32 At)
	{
		u32 Found = 0;

		while (Found < Least && IsQuad(At + Found * 6))
		{
			++Found;
		}

		return Found;
	};

	for (u32 Done = 0; Done < Count;)
	{
		const bool IsQuads = QuadsAt(Done) == Least;
		const u32 Step = IsQuads ? 6 : 3;
		const u32 Each = IsQuads ? 4 : 3;
		const u32 Most = Stream.GetCapacity() / Each;
		u32 Run = 0;

		while (Run < Most && Done + (Run + 1) * Step <= Count && (IsQuads ? IsQuad(Done + Run * Step) : QuadsAt(Done + Run * Step) < Least))
		{
			++Run;
		}

		if (!Run)
		{
			break;
		}

		u32 First = 0;
		PackedVertex* Records = static_cast<PackedVertex*>(Stream.Lock(Run * Each, EVertexFormat::Packed, First));

		for (u32 Item = 0; Item < Run; ++Item)
		{
			const ImDrawIdx* I = Indices + Done + Item * Step;
			PackedVertex* Out = Records + Item * Each;

			if (IsQuads)
			{
				// The stream's quad is the triangles 0 1 2 and 3 2 1: its shared edge is ImGui's a to a + 2
				Out[0] = Record(I[1]);
				Out[1] = Record(I[0]);
				Out[2] = Record(I[2]);
				Out[3] = Record(I[5]);
			}
			else
			{
				Out[0] = Record(I[0]);
				Out[1] = Record(I[1]);
				Out[2] = Record(I[2]);
			}
		}

		Stream.Unlock(Run * Each);

		if (IsQuads)
		{
			Stream.DrawQuads(Cmd, Run, First);
		}
		else
		{
			Stream.Draw(Cmd, ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, Run * 3, First);
		}

		Done += Run * Step;
	}
}
} // namespace

void ImGuiNri::Draw(GfxCmdList& Cmd, ref_shader& Pass, ImDrawData& Data)
{
	PROF_EVENT("ImGui: NRI");

	// D3D11 takes the texture uploads on its context
	Cmd.FlushRun();
	RHIUtils::ImGui::UpdateTextures();

	IRHIRenderTargetView* Target = Cmd.GetRenderTarget();

	if (!Pass || !Target || Data.DisplaySize.x <= 0.0f || Data.DisplaySize.y <= 0.0f || !Data.TotalIdxCount)
	{
		return;
	}

	const float Width = float(Target->GetSurface()->GetWidth());
	const float Height = float(Target->GetSurface()->GetHeight());
	RHIViewport Area(0.0f, 0.0f, Width, Height);
	Cmd.SetDepthStencil(nullptr);
	Cmd.SetPass(Pass);
	Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	Cmd.SetDepthEnable(false);
	Cmd.SetScissorOverride(true);
	Cmd.SetViewport(Area);
	Cmd.Constants.UpdateScreen();
	SetBlend(Cmd, false);

	const ImVec2 Origin = Data.DisplayPos;
	const ImVec2 Clipped = Data.FramebufferScale;
	const ImVec2 Scale(GfxCmdList::GetTargetWidth() / Data.DisplaySize.x, GfxCmdList::GetTargetHeight() / Data.DisplaySize.y);
	const u32 Slot = ShaderBindings::Texture("s_base");
	IRHISurface* Bound = nullptr;

	for (const ImDrawList* List : Data.CmdLists)
	{
		for (const ImDrawCmd& Command : List->CmdBuffer)
		{
			if (Command.UserCallback == BlendCallback || Command.UserCallback == ImDrawCallback_ResetRenderState)
			{
				SetBlend(Cmd, Command.UserCallback == BlendCallback && Command.UserCallbackData);
				continue;
			}

			if (Command.UserCallback)
			{
				Command.UserCallback(List, &Command);
				continue;
			}

			Irect Clip;
			Clip.set(std::max(s32((Command.ClipRect.x - Origin.x) * Clipped.x), 0), std::max(s32((Command.ClipRect.y - Origin.y) * Clipped.y), 0),
				std::min(s32((Command.ClipRect.z - Origin.x) * Clipped.x), s32(Width)), std::min(s32((Command.ClipRect.w - Origin.y) * Clipped.y), s32(Height)));
			IRHISurface* Image = RHIUtils::ImGui::ImageSurface(Command.GetTexID());
			IRHIShaderResourceView* View = Image ? RHIUtils::ImGui::ImageView(Image) : nullptr;

			if (Clip.x2 <= Clip.x1 || Clip.y2 <= Clip.y1 || !View)
			{
				continue;
			}

			if (Image != Bound)
			{
				Cmd.SetResource(ERHI_SHADER_TYPE::PS, Slot, View, Image);
				Bound = Image;
			}

			Cmd.SetScissor(&Clip);
			DrawTriangles(Cmd, *List, Command, Origin, Scale);
		}
	}

	// The image views stay bound in the old backend's cache past their life otherwise
	Cmd.SetResource(ERHI_SHADER_TYPE::PS, Slot, nullptr);
	Cmd.SetScissor(nullptr);
	Cmd.SetScissorOverride(false);
	Cmd.FlushRun();
	Cmd.RestoreInput();
}

void ImGuiNri::BlendCallback(const ImDrawList*, const ImDrawCmd* Command)
{
	// Run by the platform windows' backend, which draws with the old backend's state
	const float Factor[4] = {};

	if (Command->UserCallbackData)
	{
		GRHI->SetBlendState(static_cast<RHIObject*>(Command->UserCallbackData), Factor, 0xffffffff);
	}
	else if (void* Own = RHIUtils::ImGui::GetBlenderState())
	{
		GRHI->SetRawBlendState(Own, Factor, 0xffffffff);
	}
}

void* ImGuiNri::Image(CTexture* Texture)
{
	return Texture && Texture->GetView() ? RHIUtils::ImGui::Image(Texture->pSurface) : nullptr;
}

void dxRenderDeviceRender::DrawImGui(ImDrawData* Data)
{
	if (Data)
	{
		ImGuiNri::Draw(RImplementation.Imm(), m_ImGuiShader, *Data);
	}
}
