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
#include "ixr.h"
#include "ixr_fullscreen_pass.h"
#include "EditorDrawContext.h"

// The blended surfaces the forward level frame weighs order-independently (WBOIT, ShaderElement::OrderIndependent),
// first of the sorted ones: drawn in any order into the sum of their weighed colours and the share of the scene they
// leave, then laid over the scene. What stays sorted, the particles among it, is drawn over them as before. A frame
// with no such surface clears and lays nothing. Scene: the target they are laid over
void CRender::RenderForwardOit(GfxCmdList& Cmd, const ref_rt& Scene)
{
	const auto IsWeighed = [](const R_dsgraph::DynamicPacket& Node) { return Node.Element->flags.OrderIndependent != 0; };

	if (!ForwardWboit() || std::none_of(GraphMain.mapSorted.begin(), GraphMain.mapSorted.end(), IsWeighed))
	{
		return;
	}

	GPU_EVENT(ForwardOit);

	if (!Target->rt_Forward || !Target->s_oit_composite)
	{
		const u32 Width = Target->rt_Generic_0->dwWidth;
		const u32 Height = Target->rt_Generic_0->dwHeight;
		// The sum's alpha is the sum of the weights, which the mean is divided by: the scene's own format has none
		Target->rt_Forward.create(r2_RT_wboit, Width, Height, ERHI_FORMAT::R16G16B16A16_FLOAT);
		Target->rt_Revealage.create(r2_RT_wboit_revealage, Width, Height, ERHI_FORMAT::R16_UNORM);
		Target->rt_Forward->IsViewSized = true;
		Target->rt_Revealage->IsViewSized = true;
		Target->s_oit_composite.create("forward_oit_composite");
	}

	Target->u_setrt(Cmd, Target->rt_Forward, Target->rt_Revealage, RDepth);
	Cmd.ClearTarget(Target->rt_Forward->pRT);
	// Black clears to one: the whole scene shows yet
	Cmd.ClearTarget(Target->rt_Revealage->pRT, ERTColor::Black);

	{
		CEditorFillModeScope FillModeScope(Cmd, Device.IsEditorMode());
		IRHIRenderTargetView* const Weighed[2] = { Target->rt_Forward->pRT, Target->rt_Revealage->pRT };
		this->Scene.DrawSorted(Cmd, GraphMain, Weighed, 2, RDepth, true);
	}

	{
		GPU_EVENT(ForwardOitComposite);
		Target->u_setrt(Cmd, Scene, nullptr, nullptr);
		Target->DrawPassSQ(Cmd, Target->s_oit_composite, 0);
	}

	Target->u_setrt(Cmd, Scene, nullptr, RDepth);
	rmNormal();
}
