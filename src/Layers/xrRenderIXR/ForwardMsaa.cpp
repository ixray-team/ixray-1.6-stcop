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
#include "dxRenderDeviceRender.h"
#include "xrRender_console.h"
#include "ShaderBindings.h"
#include "NriPass.h"

// MSAA at once: the multisampled targets follow the setting, also after the targets were made anew. The materials
// their alpha-tested passes depend on are compiled again by SyncShadingQuality
void CRender::SyncForwardMsaa()
{
	const u32 Wanted = ps_r_msaa;

	if (ForwardReflections() && !Target->rt_sslr)
	{
		Target->CreateSslrTargets();
	}

	// The weighed targets of the blended surfaces are made by the first frame that weighs one (RenderForwardOit)
	if (!ForwardWboit())
	{
		Target->rt_Forward = nullptr;
		Target->rt_Revealage = nullptr;
	}

	if (!Target->s_hud_contact)
	{
		Target->s_hud_contact.create("hud_contact_shadows");
		Target->s_hud_contact_filter.create("hud_contact_filter");
		Target->rt_HudContact.create("$user$hud_contact", Target->rt_Generic_0->dwWidth, Target->rt_Generic_0->dwHeight, ERHI_FORMAT::R8_UNORM);
		Target->rt_HudContactRaw.create("$user$hud_contact_raw", Target->rt_Generic_0->dwWidth, Target->rt_Generic_0->dwHeight, ERHI_FORMAT::R8_UNORM);
		// Made for the screen, as what the constructor makes: the frame draws into the view's part of them
		Target->rt_HudContact->IsViewSized = true;
		Target->rt_HudContactRaw->IsViewSized = true;
	}

	if (ForwardMsaaApplied == Wanted && bool(Target->rt_ForwardMs) == (Wanted != 0))
	{
		return;
	}

	Target->rt_ForwardMs = nullptr;
	Target->rt_DistortMs = nullptr;
	Target->rt_ThinMs = nullptr;
	Target->rt_VelocityMs = nullptr;
	Target->rt_ForwardDepthMs = nullptr;

	if (Wanted)
	{
		const u32 Width = Target->rt_Generic_0->dwWidth;
		const u32 Height = Target->rt_Generic_0->dwHeight;

		Target->rt_ForwardMs.create("$user$forward_ms", Width, Height, Target->rt_Generic_0->fmt, 1u << Wanted);
		Target->rt_DistortMs.create("$user$distort_ms", Width, Height, Target->rt_Generic_1->fmt, 1u << Wanted);
		Target->rt_ThinMs.create("$user$thin_ms", Width, Height, ERHI_FORMAT::R32_UINT, 1u << Wanted);
		Target->rt_VelocityMs.create("$user$velocity_ms", Width, Height, Target->rt_Velocity->fmt, 1u << Wanted);
		Target->rt_ForwardDepthMs.create("$user$forward_depth_ms", Width, Height, RDepth->GetSurface()->GetFormat(), 1u << Wanted);

		for (const ref_rt* Screen : { &Target->rt_ForwardMs, &Target->rt_DistortMs, &Target->rt_ThinMs, &Target->rt_VelocityMs, &Target->rt_ForwardDepthMs })
		{
			(*Screen)->IsViewSized = true;
		}

		if (!Target->s_depth_resolve_ms)
		{
			Target->s_depth_resolve_ms.create("depth_resolve_ms");
			Target->s_thin_resolve_ms.create("thin_resolve_ms");
		}
	}

	ForwardMsaaApplied = Wanted;
}

void CRender::BeginForwardMsaa()
{
	if (!Target->rt_ForwardMs)
	{
		return;
	}

	Target->rt_Generic_0.swap(Target->rt_ForwardMs);
	Target->rt_Generic_1.swap(Target->rt_DistortMs);
	Target->ForwardWindowDepth = RDepth;
	RDepth = Target->rt_ForwardDepthMs->pZRT;
}

// The sky and the clouds with nothing in front of them: what is far fades into them by the fog on it, as the frame
// that shades a pixel once blends its picture over the sky. Drawn with or without the occlusion pass: the colour
// pass reads it either way
void CRender::RenderSkyBehind(GfxCmdList& Cmd)
{
	{
		GPU_EVENT(SkyBehind);
		Target->u_setrt(Cmd, Target->rt_SkyBehind, nullptr, nullptr);
		DrawSky();
	}

	Target->u_setrt(Cmd, Target->rt_Generic_0, nullptr, RDepth);
	Cmd.SetResource(ERHI_SHADER_TYPE::PS, ShaderBindings::Texture("s_sky_behind"), Target->rt_SkyBehind->pTexture->get_SRView(), Target->rt_SkyBehind->pSurface);
}

// The occlusion of the frame from the depth its opaque surfaces have just written, before their colour is drawn
void CRender::RenderForwardOcclusion(GfxCmdList& Cmd)
{
	GPU_EVENT(ForwardOcclusion);
	IRHIDepthStencilView* SceneDepth = RDepth;

	// The passes read a single-sampled depth: the window's, which gets the nearest sample of the scene's
	if (Target->ForwardWindowDepth)
	{
		const bool HasThin = ForwardOcclusionNormals();
		RDepth = Target->ForwardWindowDepth;
		Target->u_setrt(Cmd, u32(Cmd.GetWidth()), u32(Cmd.GetHeight()), HasThin ? Target->rt_Thin->pRT : nullptr, nullptr, nullptr, RDepth);
		Cmd.SetPass((HasThin ? Target->s_thin_resolve_ms : Target->s_depth_resolve_ms)->E[0]);
		Cmd.SetStencil(false);
		DrawFullscreen(Cmd, 3);
	}

	Target->copy_position();

	// The level drawn as ids gets its surfaces here: the passes below read what they need of them in the thin target
	if (Scene.IsVisibilityFrame())
	{
		GPU_EVENT(SceneResolve);
		// A surface is read or written, not both: nothing of the pass stays bound as a target
		Target->u_setrt(Cmd, u32(Cmd.GetWidth()), u32(Cmd.GetHeight()), nullptr, nullptr, nullptr, nullptr);
		Cmd.SetView(RFrame.mView, RFrame.mProject);
		Scene.Resolve(Cmd, Target->rt_Ids->pSurface, Target->rt_Position->pSurface, Target->rt_Surface->pSurface, Target->rt_Thin->pSurface);
	}

	Target->PhaseDepthPyramid(Cmd);
	Target->phase_gtao(Cmd);

	// How the frame has moved: what stands still by how the camera has moved over the depth; a model keeps the
	// velocity the depth pass gave it, which the pass over the depth leaves where the thin target names a model
	if (ForwardVelocityPass())
	{
		// Through NRI, as the colour is: the old D3D11 backend copies, which is not defined for a multisampled surface
		if (Target->rt_VelocityMs)
		{
			IRHIRenderTargetView* const Resolved[1] = { Target->rt_Velocity->pRT };
			IRHISurface* const Sampled[1] = { Target->rt_VelocityMs->pSurface };
			ResolveNri(Resolved, Sampled, 1);
			Cmd.RestoreInput();
		}

		Target->pharse_velocity(Cmd);
	}

	// Reflections of the frame before, moved by it
	if (ForwardReflections())
	{
		Target->phase_sslr(Cmd);
	}

	if (ForwardHudShadows())
	{
		GPU_EVENT(HudContactShadows);
		Target->u_setrt(Cmd, Target->rt_HudContactRaw, nullptr, nullptr);
		Cmd.SetPass(Target->s_hud_contact->E[0]);
		DrawFullscreen(Cmd, 3);
		Target->u_setrt(Cmd, Target->rt_HudContact, nullptr, nullptr);
		Cmd.SetPass(Target->s_hud_contact_filter->E[0]);
		DrawFullscreen(Cmd, 3);
	}

	RDepth = SceneDepth;
	RenderSkyBehind(Cmd);

	if (ForwardReflections())
	{
		Cmd.SetResource(ERHI_SHADER_TYPE::PS, ShaderBindings::Texture("s_refl"), Target->SslrOutput()->pTexture->get_SRView(), Target->SslrOutput()->pSurface);
	}

	rmNormal();
	Cmd.SetStencil(false);
	Cmd.SetColorWriteEnable();
}

void CRender::EndForwardMsaa(GfxCmdList& Cmd)
{
	if (!Target->ForwardWindowDepth)
	{
		return;
	}

	GPU_EVENT(ForwardMsaaResolve);
	Target->rt_Generic_0.swap(Target->rt_ForwardMs);
	Target->rt_Generic_1.swap(Target->rt_DistortMs);
	RDepth = Target->ForwardWindowDepth;
	Target->ForwardWindowDepth = nullptr;

	Target->u_setrt(Cmd, u32(Cmd.GetWidth()), u32(Cmd.GetHeight()), nullptr, nullptr, nullptr, RDepth);
	Cmd.ClearDepthStencil(RDepth, ERHI_CLEAR_TARGET::DEPTH | ERHI_CLEAR_TARGET::STENCIL, 1.f, 0);
	Cmd.SetPass(Target->s_depth_resolve_ms->E[0]);
	Cmd.SetStencil(false);
	DrawFullscreen(Cmd, 3);

	// Through NRI: the old D3D11 backend copies, and a copy of a multisampled surface is not defined there; it came
	// out black after a reset
	IRHIRenderTargetView* const Plain[2] = { Target->rt_Generic_0->pRT, Target->rt_Generic_1->pRT };
	IRHISurface* const Sampled[2] = { Target->rt_ForwardMs->pSurface, Target->rt_DistortMs->pSurface };
	ResolveNri(Plain, Sampled, 2);
	Cmd.RestoreInput();
}

// What the lens of a scope shows: the frame before the HUD's sorted surfaces, as the frame that fills a G-buffer copies
// it for them, through the scope's own post-process when every sorted surface of the HUD asks for it
void CRender::CopyForwardLensScene(GfxCmdList& Cmd)
{
	if (!GraphMain.mapHUDSorted.empty())
	{
		GPU_EVENT(HudLensScene);

		for (const auto& Node : GraphMain.mapHUDSorted)
		{
			if (!Node.Element->flags.ScopePostprocess)
			{
				Device.ScopePostprocessActive = false;
				break;
			}
		}

		// Under r_msaa the frame is resolved into the plain target, which the post-process reads by its name
		const ref_rt& Plain = Target->ForwardWindowDepth ? Target->rt_ForwardMs : Target->rt_Generic_0;

		if (Target->ForwardWindowDepth)
		{
			Target->u_setrt(Cmd, u32(Cmd.GetWidth()), u32(Cmd.GetHeight()), nullptr, nullptr, nullptr, nullptr);
			IRHIRenderTargetView* const Resolved[1] = { Plain->pRT };
			IRHISurface* const Sampled[1] = { Target->rt_Generic_0->pSurface };
			ResolveNri(Resolved, Sampled, 1);
			Cmd.RestoreInput();
		}

		if (Device.ScopePostprocessActive)
		{
			Target->phase_pp(Cmd, &Device.ScopePostprocess);
		}
		else
		{
			Cmd.CopySurface(Target->rt_Accumulator->pSurface, Plain->pSurface);
		}
	}

	// The rain leaves the velocity target bound, a plain one under r_msaa
	Target->u_setrt(Cmd, Target->rt_Generic_0, nullptr, RDepth);
	Cmd.Constants.Write<CBVisor>().ScopePostprocessActive = Device.ScopePostprocessActive ? 1.0f : 0.0f;
}
