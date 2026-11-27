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
#include "ixr_rendertarget.h"
#include "ShaderBindings.h"
#include "xrRender_console.h"

// Level 0 of the SSLR min-Z pyramid and the GTAO view-z pyramid, from one depth read before both
// The frame traces reflections: the one that lights by itself where they are on, the forward level frame at
// high shading quality
bool CRenderTarget::SslrActive() const
{
	return ForwardReflections();
}

void CRenderTarget::CreateSslrTargets()
{
	const u32 Width = rt_Generic_0->dwWidth;
	const u32 Height = rt_Generic_0->dwHeight;

	// Screen targets also when the forward frame asks for them after the constructor
	const bool WasViewSized = CRT::MarkViewSized;
	CRT::MarkViewSized = true;
	rt_sslr_old.create(r2_RT_sslr_old, Width, Height, ERHI_FORMAT::R16G16B16A16_FLOAT, 1, CRT::USE_UAV_FLAG);
	rt_sslr.create(r2_RT_sslr, Width, Height, ERHI_FORMAT::R16G16B16A16_FLOAT, 1, CRT::USE_UAV_FLAG);

	rt_sslr_data.create(r2_RT_sslr_data, Width, Height, ERHI_FORMAT::R16G16B16A16_FLOAT, 1, CRT::USE_UAV_FLAG);
	rt_sslr_temp.create(r2_RT_sslr_temp, Width, Height, ERHI_FORMAT::R16G16B16A16_FLOAT, 1, CRT::USE_UAV_FLAG);

	rt_sslr_hiz.create(r2_RT_sslr_hiz, Width, Height, ERHI_FORMAT::R32_FLOAT, 1, CRT::CRTCreationFlags(CRT::USE_UAV_FLAG | CRT::MIPPED_RT_FLAG));
	CRT::MarkViewSized = WasViewSized;
}

void CRenderTarget::PhaseDepthPyramid(GfxCmdList& Cmd)
{
	const bool Sslr = SslrActive();
	const bool Gtao = ForwardOcclusion();

	if (!Sslr && !Gtao)
	{
		return;
	}

	GPU_EVENT(depth_pyramid);

	// Writes to a level not given are dropped
	NriBinding Levels[6];
	u32 LevelCount = 0;

	if (Sslr)
	{
		Levels[LevelCount++] = { 0, rt_sslr_hiz->pSurface };
	}

	for (u32 MipLevel = 0; Gtao && MipLevel < 5; ++MipLevel)
	{
		Levels[LevelCount++] = { MipLevel + 1, rt_ao_depth->pSurface, MipLevel };
	}

	SPass& P = *(s_sslr->E[3]->passes[0]);
	Cmd.SetStates(P.state);
	Cmd.SetConstantTable(P.constants);
	Cmd.SetTextures(P.T);

	const u32 GroupsX = ((u32)Cmd.GetWidth() + 15u) / 16u;
	const u32 GroupsY = ((u32)Cmd.GetHeight() + 15u) / 16u;

	RImplementation.NriPasses.Dispatch(Cmd, P.cs._get(), nullptr, 0, Levels, LevelCount, GroupsX, GroupsY, 1);
}

void CRenderTarget::phase_sslr(GfxCmdList& Cmd)
{
	if (!SslrActive())
		return;

	GPU_EVENT(phase_sslr);

	// sslr_render reads the AO target, SSAO leaves it bound as a render target
	u_setrt(Cmd, (u32)Cmd.GetWidth(), (u32)Cmd.GetHeight(), nullptr, nullptr, nullptr, nullptr);

	//groups
	const u32 tgroupsX = (Cmd.GetWidth() + 7u) / 8u;
	const u32 tgroupsY = (Cmd.GetHeight() + 7u) / 8u;

	// Min-Z pyramid for the hierarchical trace (Stachowiak, Stochastic SSR), level 0 comes from PhaseDepthPyramid
	{
		GPU_EVENT(sslr_hiz);

		ShaderElement* S = &*(s_sslr->E[4]);
		SPass& P = *(S->passes[0]);
		Cmd.SetStates(P.state);
		Cmd.SetConstantTable(P.constants);
		Cmd.SetTextures(P.T);

		for (u32 MipLevel = 1; MipLevel < rt_sslr_hiz->pMippedUAV.size(); ++MipLevel)
		{
			// The view's part of the two levels: what lies beside it in the pyramid is of other frames
			const u32 MipWidth = std::max(u32(Cmd.GetWidth()) >> MipLevel, 1u);
			const u32 MipHeight = std::max(u32(Cmd.GetHeight()) >> MipLevel, 1u);
			const u32 SrcWidth = std::max(u32(Cmd.GetWidth()) >> (MipLevel - 1), 1u);
			const u32 SrcHeight = std::max(u32(Cmd.GetHeight()) >> (MipLevel - 1), 1u);
			Cmd.Constants.Write<CBPost>().HiZSizes.set(float(SrcWidth), float(SrcHeight), float(MipWidth), float(MipHeight));

			const NriBinding Levels[2] = { { 0, rt_sslr_hiz->pSurface, MipLevel - 1 }, { 1, rt_sslr_hiz->pSurface, MipLevel } };

			RImplementation.NriPasses.Dispatch(Cmd, P.cs._get(), nullptr, 0, Levels, 2, (MipWidth + 7u) / 8u, (MipHeight + 7u) / 8u, 1);
		}
	}

	{
		GPU_EVENT(sslr_render);

		//Shader setup... can't use set_element because of set_PS bullshit
	    ShaderElement* S;
        S = (&*(s_sslr->E[0]));
        SPass& P = *(S->passes[0]);
        Cmd.SetStates(P.state);
        Cmd.SetConstantTable(P.constants);
        Cmd.SetTextures(P.T);

		//Dispatch, half resolution traces one pixel of every 2x2 quad
		const u32 TraceShift = RImplementation.o.SslrHq ? 0 : 1;
		const u32 GroupsX = (((u32)Cmd.GetWidth() >> TraceShift) + 7u) / 8u;
		const u32 GroupsY = (((u32)Cmd.GetHeight() >> TraceShift) + 7u) / 8u;
		const NriBinding Traced[2] = { { 0, rt_sslr_temp->pSurface }, { 1, rt_sslr_data->pSurface } };

		RImplementation.NriPasses.Dispatch(Cmd, P.cs._get(), nullptr, 0, Traced, 2, GroupsX, GroupsY, 1);
	}


	// Ray reuse and history in one pass: last frame's output is the history, the two targets swap instead of a copy
	{
		GPU_EVENT(sslr_filter);

		SslrFlip = !SslrFlip;
		const ref_rt& History = SslrFlip ? rt_sslr : rt_sslr_old;

	    ShaderElement* S;
        S = (&*(s_sslr->E[1]));
        SPass& P = *(S->passes[0]);
        Cmd.SetStates(P.state);
        Cmd.SetConstantTable(P.constants);
        Cmd.SetTextures(P.T);

		// The output may still be combine's s_refl from two frames ago: D3D11 unbinds it for the UAV behind the resource cache
		Cmd.SetResource(ERHI_SHADER_TYPE::PS, ShaderBindings::Texture("s_refl"), nullptr);

		const NriBinding Earlier = { ShaderBindings::Texture("s_sslr_history"), History->pSurface };
		const NriBinding Output = { 0, SslrOutput()->pSurface };

		RImplementation.NriPasses.Dispatch(Cmd, P.cs._get(), &Earlier, 1, &Output, 1, tgroupsX, tgroupsY, 1);
	}
}

void CRender::begin_reflection_collect()
{
	// The cube stands along the axes of the world, so a face drawn in an earlier frame still fits the ones drawn now. It
	// stays where it stood until the eye is 10 cm away: moved with the eye's sway, its faces drawn in turn from points a
	// few millimetres apart disagreed, and every edge the mirrors marched flickered
	if (reflection_cam_pos.distance_to(RFrame.vCameraPosition) > 0.1f)
	{
		reflection_cam_pos = RFrame.vCameraPosition;
	}
	reflection_cam_dir.set(0.f, 0.f, 1.f);
	reflection_cam_top.set(0.f, 1.f, 0.f);
	reflection_cam_right.set(1.f, 0.f, 0.f);
	ReflectionFaces = 0;
	reflection_near = RFrame.fViewportNear;
	reflection_sector = pLastSector;
	reflection_far = 1000.f;
	if (RFrame.HasEnv())
	{
		reflection_far = RFrame.Env().far_plane;
		ReflectionFog = RFrame.Env().fog_far;
	}

	if (!HasReflectionCube() || !Target || !Target->rt_Reflection || !reflection_sector)
	{
		for (R_dsgraph_structure& Graph : GraphReflection)
		{
			Graph.r_dsgraph_clear_passes();
		}
		return;
	}

	reflection_view.build_camera_dir(reflection_cam_pos, reflection_cam_dir, reflection_cam_top);

	// r_reflections_update_rate: two, three or all six faces a frame, in turn; a new cube is drawn whole
	static const u32 Rates[] = { 2, 3, 6 };
	const void* Cube = Target->rt_Reflection._get();
	const u32 Count = Cube == ReflectionCube ? Rates[std::min(ps_r_reflections_update_rate, 2u)] : 6;
	ReflectionCube = Cube;

	for (u32 Face = 0; Face < Count; ++Face)
	{
		ReflectionFaces |= 1u << ((ReflectionCursor + Face) % 6);
	}

	ReflectionCursor = (ReflectionCursor + Count) % 6;

	const u32 dwSize = Target->rt_Reflection->dwSize;
	const float screen = _sqr((float)dwSize) * (EPS_S + ps_r__LOD);

	GraphView ReflectionView = MakeGraphView(PHASE_REFLECT);
	ReflectionView.SsaDiscard = _sqr(ps_r__ssaDISCARD) / screen;
	ReflectionView.SsaLodA = _sqr(ps_r2_ssaLOD_A / 3) / screen;
	ReflectionView.SsaLodB = _sqr(ps_r2_ssaLOD_B / 3) / screen;

	Fmatrix env_project;
	env_project.build_projection(PI_DIV_2, 1.0f, reflection_near, reflection_far * 0.4f);

	Fvector cm_norm[6];
	Fvector cm_dir[6];
	cm_dir[2].mul(reflection_cam_top, +1.0f);
	cm_dir[3].mul(reflection_cam_top, -1.0f);
	cm_norm[2].mul(reflection_cam_dir, -1.0f);
	cm_norm[3].mul(reflection_cam_dir, +1.0f);
	cm_dir[0].mul(reflection_cam_right, +1.0f);
	cm_dir[1].mul(reflection_cam_right, -1.0f);
	cm_norm[0].mul(reflection_cam_top, +1.0f);
	cm_norm[1].mul(reflection_cam_top, +1.0f);
	cm_dir[4].mul(reflection_cam_dir, +1.0f);
	cm_dir[5].mul(reflection_cam_dir, -1.0f);
	cm_norm[4].mul(reflection_cam_top, +1.0f);
	cm_norm[5].mul(reflection_cam_top, +1.0f);

	for (u32 i = 0; i < (u32)GraphReflection.size(); ++i)
	{
		if (!(ReflectionFaces & (1u << i)))
		{
			continue;
		}

		Fmatrix env_view;
		Fmatrix env_full;
		env_view.build_camera_dir(reflection_cam_pos, cm_dir[i], cm_norm[i]);
		env_full.mul(env_project, env_view);

		ReflectionView.SetSubspace(reflection_sector, env_full, reflection_cam_pos);
		GraphJobs.push_back({ &GraphReflection[i], ReflectionView, Scene.IsStaticBinned() });
	}
}

void CRender::render_reflections(GfxCmdList& Cmd)
{
	if (!HasReflectionCube())
	{
		return;
	}

	WaitGraphJobs(ReflectionJobsEnd);

	GPU_EVENT(RENDER_REFLECTIONS);

	if (!RImplementation.pLastSector)
	{
		return;
	}

	Device.Statistic->TEST2.Begin();
	GPU_EVENT(FORWARD_REFLECTIONS);

	u32 DwSize = Target->rt_Reflection->dwSize;

	static Fmatrix EnvProject;
	static Fmatrix EnvView;
	static Fvector CmNorm[6];
	static Fvector CmDir[6];

	CmDir[2].mul(reflection_cam_top, +1.0f);
	CmDir[3].mul(reflection_cam_top, -1.0f);
	CmNorm[2].mul(reflection_cam_dir, -1.0f);
	CmNorm[3].mul(reflection_cam_dir, +1.0f);

	CmDir[0].mul(reflection_cam_right, +1.0f);
	CmDir[1].mul(reflection_cam_right, -1.0f);
	CmNorm[0].mul(reflection_cam_top, +1.0f);
	CmNorm[1].mul(reflection_cam_top, +1.0f);

	CmDir[4].mul(reflection_cam_dir, +1.0f);
	CmDir[5].mul(reflection_cam_dir, -1.0f);
	CmNorm[4].mul(reflection_cam_top, +1.0f);
	CmNorm[5].mul(reflection_cam_top, +1.0f);

	EnvProject.build_projection(PI_DIV_2, 1.0f, reflection_near, reflection_far * 0.4f);

	// The share of the map's history kept: 0.9 with the eye still, none once it moves 10 cm a frame. The cube stands at
	// the eye and is not reprojected; with the eye bobbing a centimetre a frame the history was dropped every frame
	static Fvector HistoryEye = { 0.0f, 0.0f, 0.0f };
	static const void* HistoryTarget = nullptr;
	const float Moved = HistoryTarget == Target->rt_Reflection_history._get() ? HistoryEye.distance_to(reflection_cam_pos) : 1.0f;
	const float Keep = 0.9f * std::clamp(1.0f - Moved / 0.1f, 0.0f, 1.0f);
	HistoryTarget = Target->rt_Reflection_history._get();
	HistoryEye = reflection_cam_pos;

	const Fvector4 fog_color4 = { ReflectionFog, 0.0f, 0.0f, 0.0f };

	is_render_cubemap = true;
	phase = PHASE_REFLECT;

	for (u32 i = 0; i < GraphReflection.size(); ++i)
	{
		// A face that waits its turn keeps what it was drawn with
		if (!(ReflectionFaces & (1u << i)))
		{
			continue;
		}

		EnvView.build_camera_dir(reflection_cam_pos, CmDir[i], CmNorm[i]);

		Cmd.ClearTarget(Target->rt_Reflection_temp->pRT[i], (const float*)&fog_color4);

		const bool IsScene = Scene.IsForwardBinned();
		bool NeedRender = IsScene || GraphReflection[i].HasPasses(0) ||
						GraphReflection[i].HasPasses(1) || GraphReflection[i].mapSorted.size();

		if (!NeedRender)
		{
			continue;
		}

		Cmd.ClearDepthStencil(Target->rt_Depth->pZRT, ERHI_CLEAR_TARGET::DEPTH, 1.0f, 0L);
		Target->u_setrt(Cmd, DwSize, DwSize, Target->rt_Reflection->pRT[i], Target->rt_Reflection_temp->pRT[i], NULL, Target->rt_Depth->pZRT);

		RImplementation.rmNormal();
		Cmd.SetStencil(FALSE);
		Cmd.SetColorWriteEnable();
		Cmd.SetView(EnvView, EnvProject);

		// The level's clusters from the scene, culled for the face on the GPU; what is left comes from the graph
		if (IsScene)
		{
			Fmatrix EnvFull;
			EnvFull.mul(EnvProject, EnvView);

			if (Scene.CullFace(GraphReflection[i], reflection_cam_pos, EnvFull, i))
			{
				IRHIRenderTargetView* Views[2] = { Target->rt_Reflection->pRT[i], Target->rt_Reflection_temp->pRT[i] };
				Scene.DrawForward(Cmd, Views, 2, Target->rt_Depth->pZRT, true, 1, &i);
				Target->u_setrt(Cmd, DwSize, DwSize, Target->rt_Reflection->pRT[i], Target->rt_Reflection_temp->pRT[i], NULL, Target->rt_Depth->pZRT);
				RImplementation.rmNormal();
				Cmd.SetStencil(FALSE);
				Cmd.SetColorWriteEnable();
				Cmd.SetView(EnvView, EnvProject);
			}
		}

		GraphReflection[i].r_dsgraph_render_graph(0);
	}

	Cmd.SetView(RFrame.mView, RFrame.mProject);
	is_render_cubemap = false;
	phase = PHASE_NORMAL;

	Target->u_setrt(Cmd, DwSize, DwSize, nullptr, nullptr, nullptr, nullptr);

	// Every level of the map in one dispatch: boxed down a tile of 64 texels to a group at low, prefiltered by GGX a
	// level to a z of the dispatch from medium, its sharpest level settling over the jittered faces while the eye stays
	{
		GPU_EVENT(mirror_ibl);

		const bool IsBoxed = !Target->rt_Reflection_history;
		SPass& P = *(Target->s_sslr->E[IsBoxed ? 5 : 2]->passes[0]);
		Cmd.SetStates(P.state);
		Cmd.SetConstantTable(P.constants);
		Cmd.SetTextures(P.T);

		Cmd.Constants.Write<CBPost>().mirror_history.set(Keep, 0.0f, 0.0f, 0.0f);

		NriBinding Levels[8];
		const u32 LevelCount = std::min(u32(Target->rt_Reflection_forward->pMippedUAV.size()), 7u);

		for (u32 MipLevel = 0; MipLevel < LevelCount; ++MipLevel)
		{
			Levels[MipLevel] = { MipLevel, Target->rt_Reflection_forward->pSurface, MipLevel };
		}

		const u32 Size = Target->rt_Reflection_forward->dwWidth;

		if (IsBoxed)
		{
			NriPasses.Dispatch(Cmd, P.cs._get(), nullptr, 0, Levels, LevelCount, Size / 64u, Size / 64u, 1);
		}
		else
		{
			Levels[LevelCount] = { 7, Target->rt_Reflection_history->pSurface };
			NriPasses.Dispatch(Cmd, P.cs._get(), nullptr, 0, Levels, LevelCount + 1, (Size + 7u) / 8u, (Size + 7u) / 8u, LevelCount);
		}
	}

	Device.Statistic->TEST2.End();
}
