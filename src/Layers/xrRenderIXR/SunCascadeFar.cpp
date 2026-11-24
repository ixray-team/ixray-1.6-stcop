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

namespace
{
	// How far the sun may have turned since a cascade was drawn before it is drawn anew out of its turn
	constexpr float SunTurnLimit = 0.99999f;
}

// Which cascades the forward frame draws, and with what matrix, is known when it starts: the portals of each one (and the
// statics the batch leaves, if any) are walked on the task pool meanwhile, a task a graph, and RenderFarSunCascade waits
void CRender::BeginFarSunCascades()
{
	SunCascadeDrawnCount = 0;
	IsSunCascadeQueried = false;
	light* Sun = (light*)Lights.sun_adapted._get();
	const bool EditorMode = Device.IsEditorMode();

	// The editors' shadow setting takes their sun's shadows with the lamps'
	if (!g_RenderFeatures.SunShadows || !Sun || (EditorMode && !EditorLightShadows))
	{
		return;
	}

	Fvector Direction = Sun->direction;
	Direction.normalize_safe();
	Fvector Right = { 1.f, 0.f, 0.f };

	if (std::abs(Right.dotproduct(Direction)) > .99f)
	{
		Right.set(0.f, 0.f, 1.f);
	}

	Fvector Up;
	Up.crossproduct(Direction, Right).normalize();
	Right.crossproduct(Up, Direction).normalize();

	// Below high lighting quality the widest cascade alone is drawn
	const u32 Last = u32(m_sun_cascades.size()) - 1;
	const bool IsSingle = ps_r_lighting_quality < 2;

	for (u32 Index = 0; Index < m_sun_cascades.size() && Index < GraphSun.size(); ++Index)
	{
		const sun::cascade& Cascade = m_sun_cascades[Index];

		if (!IsSunCascadeCollected(Index) || (IsSingle && Index != Last))
		{
			continue;
		}

		// The nearest cascade is drawn every frame. The other two take turns, a frame each, and are sampled with the
		// matrix they were drawn with meanwhile. The editors draw all of them: several viewports render within a frame
		const bool IsKept = Cascade.IsValid && Cascade.CaptureMapSize == o.smapsize && Cascade.CaptureSize == Cascade.size && RFrame.dwFrame - Cascade.CaptureFrame <= 2 &&
			Cascade.CaptureDirection.dotproduct(Direction) >= SunTurnLimit;

		if (IsKept && Index != 0 && !IsSingle && !EditorMode && (RFrame.dwFrame & 1) != (Index & 1))
		{
			continue;
		}

		// The cascades of the advanced frame follow the view frustum, each one further along the view. These sit
		// around the eye, one inside the other, so the ground at one's feet is never at an edge; they move by whole texels
		Fvector Origin;
		{
			const float Size = Cascade.size;
			const float Texel = Size / float(o.smapsize);
			const float Height = 500.f;

			// The centre is put together from whole steps along the three axes of the light, so the matrix is the same
			// bit for bit while the eye stays inside one step: a matrix that differs in its last digits rasterises the
			// casters differently, and a texel that opens or closes shows as a patch under a grazing sun
			const float DepthStep = 4.f;
			const Fvector& Eye = RFrame.vCameraPosition;
			Fvector Centre;
			Centre.mul(Right, std::floor(Eye.dotproduct(Right) / Texel) * Texel);
			Centre.mad(Up, std::floor(Eye.dotproduct(Up) / Texel) * Texel);
			Centre.mad(Direction, std::floor(Eye.dotproduct(Direction) / DepthStep) * DepthStep);

			Fmatrix View, Project;
			View.build_camera_dir(Fvector().mad(Centre, Direction, -Height), Direction, Up);
			Project.OrthographicOffCenterLH(-Size * 0.5f, Size * 0.5f, -Size * 0.5f, Size * 0.5f, 0.1f, Height + 1.41421f * Size);
			sun_cascade_xforms[Index].mul(Project, View);
			PreparedSunCascades[Index].DepthRange = Height + 1.41421f * Size - 0.1f;
			Origin.mad(Centre, Direction, -1200.f);
		}

		// What casts must not depend on how large it looks from the eye: a mesh that comes and goes, or changes its
		// detail, as the eye sways makes its shadow flicker
		GraphView& View = SunCascadeViews[Index];
		View = SunView;
		View.SetSubspace(pOutdoorSector, sun_cascade_xforms[Index], Origin);
		View.SsaDiscard = 0.f;
		View.SsaLodA = 0.f;
		View.FullDetail = true;
		SunCascadeDrawn[SunCascadeDrawnCount++] = Index;

		// The editors' casters are submitted on this thread when the cascade is drawn
		if (EditorMode)
		{
			continue;
		}

		SunCascadeTasks.run([this, Index]()
		{
			PROF_EVENT("SunCascadeBuild");
			GraphSun[Index].Begin(SunCascadeViews[Index]);
			TraverseShadowView(GraphSun[Index]);
		});
	}
}

// The moving casters of the frame's shadow views at once (GpuScene::CollectCasters), once a frame: by the lamps' shadows
// when the frame has shadowed lamps, else by the cascades
void CRender::CollectShadowCasters()
{
	if (Device.IsEditorMode() || ShadowCastersFrame == RFrame.dwFrame)
	{
		return;
	}

	{
		PROF_EVENT("SunCascadeWait");
		SunCascadeTasks.wait();
	}

	PROF_EVENT("ShadowCollect");
	ShadowCastersFrame = RFrame.dwFrame;
	Scene.CollectCasters(GraphSun.data(), SunCascadeDrawn, SunCascadeDrawnCount, IsSunCascadeQueried ? SunCascadeQueries : nullptr);

	// Held, the queries would keep what they found alive past its level
	for (xr_vector<ISpatialShared>& Query : SunCascadeQueries)
	{
		Query.clear();
	}

	IsSunCascadeQueried = false;
}

// The sun's cascades of the forward frame, which surface_sun_shadow of shaders/common/surface_forward.hlsli samples
void CRender::RenderFarSunCascade(GfxCmdList& Cmd)
{
	{
		PROF_EVENT("SunCascadeWait");
		SunCascadeTasks.wait();
	}

	light* Sun = (light*)Lights.sun_adapted._get();
	const bool EditorMode = Device.IsEditorMode();

	if (!g_RenderFeatures.SunShadows || !Sun || (EditorMode && !EditorLightShadows))
	{
		for (sun::cascade& Cascade : m_sun_cascades)
		{
			Cascade.IsValid = false;
		}

		Cmd.Constants.UpdateSunShadows();
		return;
	}

	GPU_EVENT(RenderSunCascade);
	phase = PHASE_SMAP;

	Fvector Direction = Sun->direction;
	Direction.normalize_safe();
	RHIViewport Viewport = { 0.f, 0.f, float(o.smapsize), float(o.smapsize), 0.f, 1.f };
	const u32 Last = u32(m_sun_cascades.size()) - 1;
	const bool IsSingle = ps_r_lighting_quality < 2;

	const auto BindCascade = [&](u32 Index)
	{
		Cmd.SetViewport(Viewport);
		Cmd.SetTextures(&SunShadowEmptyTextures);
		GRHI->ShaderResourceCache->Apply();
		Target->u_setrt(Cmd, nullptr, nullptr, nullptr, Target->rt_smap_depth_sun_dsv[Index]);
		GRHI->ApplyRenderTargetChange();
	};

	const auto MakeDraw = [&](u32 Index)
	{
		GpuScene::DepthDraw Draw = { Target->rt_smap_depth_sun_dsv[Index], Index };
		Draw.Look = Sun->direction;
		return Draw;
	};

	for (u32 Index = 0; Index < m_sun_cascades.size() && Index < GraphSun.size(); ++Index)
	{
		if (!IsSunCascadeCollected(Index) || (IsSingle && Index != Last))
		{
			m_sun_cascades[Index].IsValid = false;
		}
	}

	// The cascades drawn this frame (BeginFarSunCascades), in order, and those of them whose batch the scene records: their
	// moving casters are the frame's table, which the objects only they see join first (their graphs get what the batch
	// cannot draw); all of them are cleared and prepared, the batches are recorded (on the workers with D3D12), and only
	// then does each cascade draw what its graph kept. Every draw is of depth alone, so their order does not change the
	// picture
	static_assert(std::tuple_size_v<decltype(GraphSun)> == SCENE_CASTER_VIEWS);
	const u32* Drawn = SunCascadeDrawn;
	const u32 DrawnCount = SunCascadeDrawnCount;
	CollectShadowCasters();
	Scene.UploadModels(Cmd);

	for (u32 Item = 0; Item < DrawnCount; ++Item)
	{
		const u32 Index = Drawn[Item];
		R_dsgraph_structure& Graph = GraphSun[Index];

		if (EditorMode)
		{
			PROF_EVENT("SunCascadeCollect");
			Graph.Begin(SunCascadeViews[Index]);
			EditorRenderSubspace(Cmd, Graph);
		}

		BindCascade(Index);
		Cmd.ClearDepthStencil(Target->rt_smap_depth_sun_dsv[Index], ERHI_CLEAR_TARGET::DEPTH, 1.f, 0);
		Scene.PrepareView(Cmd, Graph, MakeDraw(Index), Index);
	}

	Scene.PrepareViews(Cmd);

	{
		PROF_EVENT("SunCascadeRecordViews");

		if (ViewRecorder::IsParallel())
		{
			u32 Views[GpuScene::OwnViewCount];
			ViewRecorder::Record(Scene.PreparedViews(Views), "SunCascade", [this, &Views](nri::CommandBuffer& Commands, u32 Item)
			{
				PROF_EVENT("SunCascadeRecord");
				Scene.RecordView(Commands, Views[Item]);
			});
		}
		else
		{
			Scene.RecordViews(Cmd);
		}

		Scene.FinishViews();
	}

	for (u32 Item = 0; Item < DrawnCount; ++Item)
	{
		const u32 Index = Drawn[Item];
		sun::cascade& Cascade = m_sun_cascades[Index];
		R_dsgraph_structure& Graph = GraphSun[Index];

		Sun->X.D.minX = Sun->X.D.minY = 0;
		Sun->X.D.maxX = Sun->X.D.maxY = o.smapsize - 1;
		Sun->X.D.combine = sun_cascade_xforms[Index];
		BindCascade(Index);

		// The grass is not in the graph
		if (CDetailManager* ActiveDetails = GetDetails(); ActiveDetails && ActiveDetails->dtSlots && ps_r2_ls_flags.test(R2FLAG_SUN_DETAILS) && (IsSingle || int(Index) < ps_r2_sun_details_cascades))
		{
			PROF_EVENT("SunCascadeGrass");
			const GpuScene::DepthDraw Draw = MakeDraw(Index);
			Cmd.SetObjectTransform(Fidentity);
			Cmd.SetView(Fidentity, Sun->X.D.combine);
			ActiveDetails->hw_Render(Cmd, &Sun->X.D.combine, &Draw);
		}

		if (Graph.HasPasses(0))
		{
			GPU_EVENT(SunCascadeDraw);
			Cmd.SetObjectTransform(Fidentity);
			Cmd.SetView(Fidentity, Sun->X.D.combine);
			Graph.r_dsgraph_render_graph(0);
		}

		Graph.r_pmask(true, false);

		Cascade.xform = sun_cascade_xforms[Index];
		Cascade.CaptureDirection = Direction;
		Cascade.CaptureDepthRange = PreparedSunCascades[Index].DepthRange;
		Cascade.CaptureBias = Cascade.bias;
		Cascade.CaptureSize = Cascade.size;
		Cascade.CaptureMapSize = o.smapsize;
		Cascade.CaptureFrame = RFrame.dwFrame;
		Cascade.IsValid = true;
	}

	// The slices leave the output before a surface binds them as a texture
	Target->u_setrt(Cmd, Target->rt_Generic_0, nullptr, RDepth);
	GRHI->ApplyRenderTargetChange();

	phase = PHASE_NORMAL;
	Cmd.Constants.UpdateSunShadows();
	Cmd.SetObjectTransform(Fidentity);
	Cmd.SetView(RFrame.mView, RFrame.mProject);
	Viewport.Width = float(Cmd.GetWidth());
	Viewport.Height = float(Cmd.GetHeight());
	Cmd.SetViewport(Viewport);
	RestoreSunCascadeChain();
}
