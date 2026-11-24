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
#include "../../xrEngine/xr_object.h"

// The shadow maps of the local lights of a frame whose level is drawn forward. The frame that shades a pixel once
// reads a page of the atlas right after drawing it and draws over it again; here every map stays where it was drawn
// until the surfaces have read it, so the pages of the atlas are all there is: what does not fit casts no shadow.
// What it reads of the game comes first, with the frame's collect: the parts, their places, and what casts in them
void CRender::CollectForwardLampShadows()
{
	// Several editor viewports render within one frame: a map an earlier one drew is not this one's
	if (Device.IsEditorMode())
	{
		for (light* Light : SurfaceLights)
		{
			Light->ShadowFrame = u32(-1);

			for (light* Part : Light->omnipart)
			{
				if (Part)
				{
					Part->ShadowFrame = u32(-1);
				}
			}
		}
	}

	LampParts.clear();
	LampLights.clear();

	if (!g_RenderFeatures.LampShadows || o.noshadows || !ps_r_lamp_shadows)
	{
		return;
	}

	Target->BeginSpotShadowCacheFrame();

	for (light* Light : SurfaceLights)
	{
		if (!Light->flags.bActive || Light->range <= EPS_L || !Light->flags.bShadow || Light->flags.bHudMode)
		{
			continue;
		}

		if (Light->flags.type == IRender_Light::SPOT)
		{
			Light->xform_calc();
			LampParts.emplace_back().Light = Light;
			LampParts.back().Lamp = u32(LampLights.size());
		}
		else if (Light->flags.type == IRender_Light::POINT && Light->omnipart[0])
		{
			Light->UpdateOmniparts();

			for (light* Part : Light->omnipart)
			{
				LampParts.emplace_back().Light = Part;
				LampParts.back().Lamp = u32(LampLights.size());
			}
		}
		else
		{
			continue;
		}

		LampLights.push_back(Light);
	}

	if (LampParts.empty())
	{
		return;
	}

	for (CRenderTarget::SpotShadowPart& Part : LampParts)
	{
		Part.Light->optimize_smap_size();
	}

	std::stable_sort(LampParts.begin(), LampParts.end(), [](const CRenderTarget::SpotShadowPart& Left, const CRenderTarget::SpotShadowPart& Right) { return Left.Light->X.S.size > Right.Light->X.S.size; });

	// Every part's place first, the largest first, a page after another
	for (u32 Page = 0, Placed = 0; Page < CRenderTarget::SmapPages && Placed < LampParts.size(); ++Page)
	{
		LP_smap_pool.initialize(o.smapsize);

		for (CRenderTarget::SpotShadowPart& Part : LampParts)
		{
			SMAP_Rect Rect;

			if (Part.Page == u32(-1) && LP_smap_pool.push(Rect, Part.Light->X.S.size))
			{
				Part.Light->X.S.posX = Rect.min.x;
				Part.Light->X.S.posY = Rect.min.y;
				Part.Light->vis.smap_ID = u16(Page);
				Part.Page = Page;
				++Placed;
			}
		}
	}

	std::erase_if(LampParts, [](const CRenderTarget::SpotShadowPart& Part) { return Part.Page == u32(-1); });
	std::stable_sort(LampParts.begin(), LampParts.end(), [](const CRenderTarget::SpotShadowPart& Left, const CRenderTarget::SpotShadowPart& Right) { return Left.Page < Right.Page; });

	// The editors submit their scene when a part is drawn
	if (LampParts.empty() || Device.IsEditorMode())
	{
		return;
	}

	phase = PHASE_SMAP;

	// What each part draws, before any of it is
	{
		PROF_EVENT("LampPrepare");

		for (CRenderTarget::SpotShadowPart& Part : LampParts)
		{
			Target->PrepareSpotShadow(Part);
		}
	}

	// A lamp that lights nothing the eye sees keeps the map it has: nothing that moves is cast into it, nor its grass.
	// One drawn whole has no map to keep, and casts as before
	LampSeen.Read(LampLights);
	LampWanted.assign(LampLights.size(), false);

	for (CRenderTarget::SpotShadowPart& Part : LampParts)
	{
		Part.IsUnseen = !Part.IsDecor && LampSeen.IsHidden(Part.Lamp);

		if (Part.IsUnseen)
		{
			Target->CountUnseenSpotShadow();
		}

		Part.HasCasters = Part.HasCasters && !(Part.IsUnseen && Part.UseCache && Part.HasStatic);
		LampWanted[Part.Lamp] = LampWanted[Part.Lamp] || Part.HasCasters;
	}

	// What moves: a sphere query a lamp and the drawn cascades' queries on the workers, each casting part's portals and
	// the objects it would call, then the frame's table with the cascades' at once
	const bool IsDynamic = psDeviceFlags.test(rsDrawDynamic);
	LampQueries.resize(std::max(LampQueries.size(), LampLights.size()));

	if (IsDynamic)
	{
		PROF_EVENT("LampQueries");
		IsSunCascadeQueried = true;
		xr_parallel_for(size_t(0), LampLights.size() + SunCascadeDrawnCount, [this](size_t Index)
		{
			if (Index >= LampLights.size())
			{
				const u32 Cascade = SunCascadeDrawn[Index - LampLights.size()];
				g_SpatialSpace->q_frustum(SunCascadeQueries[Cascade], ISpatial_DB::O_ORDERED, ESPATIAL_TYPE::RENDERABLE | ESPATIAL_TYPE::RENDERABLESHADOW, SunCascadeViews[Cascade].Frustum);
				return;
			}

			const light* Light = LampLights[Index];
			LampQueries[Index].clear();

			if (!LampWanted[Index])
			{
				return;
			}

			g_SpatialSpace->q_sphere(LampQueries[Index], ISpatial_DB::O_ORDERED, ESPATIAL_TYPE::RENDERABLE | ESPATIAL_TYPE::RENDERABLESHADOW, Light->position, Light->range);
		});
	}

	LampTiles.resize(LampParts.size());
	Scene.BeginLamps();

	{
		PROF_EVENT("LampCollect");

		for (u32 Index = 0; Index < LampParts.size(); ++Index)
		{
			CRenderTarget::SpotShadowPart& Part = LampParts[Index];
			light& L = *Part.Light;
			GpuTileView& Tile = LampTiles[Index];
			Tile = {};
			Tile.Transform = L.X.S.combine;
			Part.HasCasters = Part.HasCasters && IsDynamic && !Part.IsEmpty && (Part.IsDecor || L.SpatialComponent->sector);
			// The level's batch of a part drawn whole, or of a cached one that waits for its depth, is culled by the
			// sectors this traversal reaches: the part's only one this frame
			const bool HasLevel = !Part.IsDecor && !Part.IsEmpty && L.SpatialComponent->sector && (Part.IsDirect || (Part.HasStatic && !Part.UseCache));

			if (!Part.HasCasters && !HasLevel)
			{
				continue;
			}

			// A decor object casts wherever it stands
			if (Part.IsDecor)
			{
				std::fill(std::begin(Tile.Sectors), std::end(Tile.Sectors), u32(-1));
				Tile.IsOnly = true;

				for (CObject* Object : L.decor_object)
				{
					if (Object && !Object->getDestroy())
					{
						Scene.HitDecor(Object->dcast_Renderable(), Index);
					}
				}

				continue;
			}

			GraphLampCollect.Begin(MakeLampView(L));
			GraphLampCollect.Traverse();

			if (Part.HasCasters)
			{
				Scene.HitLamp(GraphLampCollect, LampQueries[Part.Lamp], L.ignore_object ? L.ignore_object->dcast_Renderable() : nullptr, Index, Tile);
				continue;
			}

			for (u32 Sector = 0; Sector < std::min(u32(Sectors.size()), SCENE_VIEW_SECTORS); ++Sector)
			{
				Tile.Sectors[Sector >> 5] |= GraphLampCollect.PortalTraverser.IsVisited(Sector) ? 1u << (Sector & 31) : 0u;
			}
		}
	}

	CollectShadowCasters();

	for (u32 Index = 0; Index < LampParts.size(); ++Index)
	{
		const CRenderTarget::SpotShadowPart& Part = LampParts[Index];
		CObject* Ignore = Part.HasCasters && !Part.IsDecor ? Part.Light->ignore_object : nullptr;
		LampTiles[Index].Object = Ignore ? Scene.ObjectOf(Ignore->dcast_Renderable()) : SCENE_OBJECT_NONE;
	}

	// Held, the queries would keep what they found alive past its level
	for (xr_vector<ISpatialShared>& Query : LampQueries)
	{
		Query.clear();
	}

	phase = PHASE_NORMAL;
}

void CRender::RenderForwardLampShadows(GfxCmdList& Cmd)
{
	if (LampParts.empty())
	{
		return;
	}

	GPU_EVENT(ForwardLampShadows);

	if (!Device.IsEditorMode())
	{
		LampSeen.Mark(Cmd, LampLights);
	}

	// The surfaces of the last frame left the atlas bound as a texture
	Cmd.SetTextures(&SunShadowEmptyTextures);
	GRHI->ShaderResourceCache->Apply();
	phase = PHASE_SMAP;

	if (Device.IsEditorMode())
	{
		for (u32 Index = 0; Index < LampParts.size(); ++Index)
		{
			if (!Index || LampParts[Index - 1].Page != LampParts[Index].Page)
			{
				Target->phase_smap_spot_clear(LampParts[Index].Page);
			}

			if (RenderEditorLampShadow(Cmd, *LampParts[Index].Light))
			{
				LampParts[Index].Light->ShadowFrame = Device.dwFrame;
			}
		}
	}
	else
	{
		// A cache build draws into its entry's own depth; then what moves of a cached part against what its depth holds
		{
			PROF_EVENT("LampBake");

			for (CRenderTarget::SpotShadowPart& Part : LampParts)
			{
				Target->BuildSpotShadow(Part);
			}

			Scene.UploadModels(Cmd);

			for (u32 Index = 0; Index < LampParts.size(); ++Index)
			{
				if (LampParts[Index].Entry)
				{
					Target->BakeSpotShadow(Cmd, LampParts[Index], LampTiles[Index]);
				}
			}
		}

		// A page: cleared, its cached depths restored, then what each part's graph and grass draw, while the scene keeps the
		// page's batches (each direct part's level, then what moves of all of them) for the page's view. The batches are
		// recorded after every page, on the workers with D3D12: depth alone over the restored depths, so the order changes
		// no texel
		static_assert(CRenderTarget::SmapPages <= GpuScene::OwnViewCount && GpuScene::OwnViewCount <= ViewRecorder::MaxViews);

		for (u32 First = 0, End = 0; First < LampParts.size(); First = End)
		{
			const u32 Page = LampParts[First].Page;

			while (End < LampParts.size() && LampParts[End].Page == Page)
			{
				++End;
			}

			Target->phase_smap_spot_clear(Page);

			{
				PROF_EVENT("LampRestore");
				GPU_EVENT(SpotShadowCacheRestore);
				Target->RestoreSpotShadows(Cmd, LampParts.data() + First, End - First);
			}

			Scene.KeepDepth(Page);

			for (u32 Index = First; Index < End; ++Index)
			{
				if (RenderLampShadow(Cmd, LampParts[Index], Index))
				{
					LampParts[Index].Light->ShadowFrame = RFrame.dwFrame;
				}
			}

			PROF_EVENT("LampTiles");
			Scene.DrawTiles(Cmd);
		}

		Scene.PrepareViews(Cmd);

		{
			PROF_EVENT("LampRecordViews");

			if (ViewRecorder::IsParallel())
			{
				u32 Views[GpuScene::OwnViewCount];
				ViewRecorder::Record(Scene.PreparedViews(Views), "LampPage", [this, &Views](nri::CommandBuffer& Commands, u32 Item)
				{
					PROF_EVENT("LampPageRecord");
					Scene.RecordView(Commands, Views[Item]);
				});
			}
			else
			{
				Scene.RecordViews(Cmd);
			}

			Scene.FinishViews();
		}
	}

	// The pages leave the output before a surface binds them as a texture
	Target->u_setrt(Cmd, Target->rt_Generic_0, nullptr, RDepth);
	GRHI->ApplyRenderTargetChange();

	phase = PHASE_NORMAL;
	Cmd.SetColorWriteEnable();
	Cmd.SetObjectTransform(Fidentity);
	Cmd.SetView(RFrame.mView, RFrame.mProject);
	RHIViewport Viewport = { 0.f, 0.f, float(Cmd.GetWidth()), float(Cmd.GetHeight()), 0.f, 1.f };
	Cmd.SetViewport(Viewport);
}
