#include "stdafx.h"

#include "FHierrarhyVisual.h"
#include "SkeletonCustom.h"
#include "../../xrEngine/Fmesh.h"
#include "../../xrEngine/IRenderable.h"

#include "FLOD.h"
#include "FVisual.h"
#include "ParticleGroup.h"
#include "FTreeVisual.h"
#include "ParticleEffect.h"
#include "newproject_dsgraph_constants.h"
#include "dx10StateUtils.h"
#include "StaticTraversalStats.h"

using namespace R_dsgraph;

namespace SpotShadowCasters
{
static bool IsShadowBaseShader(const shared_str& Name)
{
	constexpr size_t Length = sizeof("shadow_base") - 1;
	if (!Name || strncmp(*Name, "shadow_base", Length) != 0)
	{
		return false;
	}
	const char* Suffix = *Name + Length;
	while (*Suffix)
	{
		if (*Suffix++ != '_')
		{
			return false;
		}
		if (*Suffix >= '0' && *Suffix <= '9')
		{
			do
			{
				++Suffix;
			} while (*Suffix >= '0' && *Suffix <= '9');
		}
		else if (strncmp(Suffix, "puddles", 7) == 0)
		{
			Suffix += 7;
		}
		else
		{
			return false;
		}
	}
	return true;
}

static bool IsDepthOnlyShadowPass(const SPass& Pass)
{
	if (!Pass.state)
	{
		return false;
	}
	D3D_BLEND_DESC Blend;
	D3D_DEPTH_STENCIL_DESC Depth;
	dx10StateUtils::ResetDescription(Blend);
	dx10StateUtils::ResetDescription(Depth);
	Pass.state->state_code.UpdateDesc(Blend);
	Pass.state->state_code.UpdateDesc(Depth);
	const auto HarmlessStencil = [](const auto& Face)
	{
		return Face.StencilFunc == D3D11_COMPARISON_ALWAYS && Face.StencilFailOp == D3D11_STENCIL_OP_KEEP &&
			   Face.StencilDepthFailOp == D3D11_STENCIL_OP_KEEP && Face.StencilPassOp == D3D11_STENCIL_OP_KEEP;
	};
	return !Blend.RenderTarget[0].BlendEnable && !Blend.AlphaToCoverageEnable && Depth.DepthEnable && Depth.DepthWriteMask == D3D11_DEPTH_WRITE_MASK_ALL &&
		   (!Depth.StencilEnable || (HarmlessStencil(Depth.FrontFace) && HarmlessStencil(Depth.BackFace))) &&
		   (Depth.DepthFunc == D3D11_COMPARISON_LESS || Depth.DepthFunc == D3D11_COMPARISON_LESS_EQUAL);
}

static bool IsSpotShadowCacheCaster(const ShaderElement& Element, R_dsgraph_structure::SpotShadowBuildContext& Context)
{
	const auto RejectLambda = [&](const char* Reason)
	{
		Context.RejectionReason = Reason;
		return false;
	};
	if (Element.flags.iPriority / 2 != 0 || Element.flags.bStrictB2F || Element.flags.bEmissive || Element.flags.bDistort || Element.flags.bWmark || Element.passes.size() != 1)
	{
		return RejectLambda("element_flags_or_pass_count");
	}

	const ref_pass& Reference = Element.passes[0];
	if (!Reference)
	{
		return RejectLambda("missing_pass");
	}
	const SPass& Pass = *Reference;
	if (!Pass.vs || !Pass.ps || !Pass.state)
	{
		return RejectLambda("missing_shader_or_state");
	}
	if (!IsShadowBaseShader(Pass.vs->cName) || !IsShadowBaseShader(Pass.ps->cName))
	{
		return RejectLambda("shader_program");
	}
	if ((Pass.gs && Pass.gs->gs) || (Pass.hs && Pass.hs->sh) || (Pass.ds && Pass.ds->sh) || (Pass.cs && Pass.cs->sh))
	{
		return RejectLambda("additional_shader_stages");
	}
	if ((Pass.M && !Pass.M->empty()) || (Pass.C && !Pass.C->empty()))
	{
		return RejectLambda("animated_matrix_or_constant");
	}
	if (Pass.iPriority != u8(-1) && Pass.iPriority / 2 != 0)
	{
		return RejectLambda("pass_priority");
	}

	if (!IsDepthOnlyShadowPass(Pass))
	{
		return RejectLambda("depth_blend_or_stencil_state");
	}

	if (Pass.T)
	{
		xr_vector<std::pair<ref_texture, u64>> Textures;
		Textures.reserve(Pass.T->size());
		for (const auto& Texture : *Pass.T)
		{
			bool Pending = false;
			u64 Revision = 0;
			if (!Texture.second || !Texture.second->CanCacheShadow(Pending, Revision))
			{
				if (Pending)
				{
					Context.PendingTextures.push_back(Texture.second);
				}
				Context.RejectedTexture = Texture.second ? Texture.second->cName : shared_str();
				return RejectLambda(Pending ? "texture_pending" : "texture_not_cacheable");
			}
			Textures.emplace_back(Texture.second, Revision);
		}
		Context.Textures.insert(Context.Textures.end(), Textures.begin(), Textures.end());
	}
	return true;
}
} // namespace SpotShadowCasters

////////////////////////////////////////////////////////////////////////////////////////////////////
// Scene graph actual insertion and sorting ////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
RENDER_API float r_ssaDISCARD;
float r_ssaDONTSORT;
float r_ssaLOD_A;
float r_ssaLOD_B;
float r_ssaLOD_MU0, r_ssaLOD_MU1, r_ssaLOD_MU2, r_ssaLOD_MU3, r_ssaLOD_MU4;
float r_ssaGLOD_start;
float r_ssaGLOD_end;
float r_ssaHZBvsTEX;

ICF bool FullDetailRejectStatic(dxRender_Visual* pVisual)
{
	if (ps_r1_full_detail_distance_scale >= 1.f ||
		RImplementation.phase != CRender::PHASE_NORMAL ||
		pVisual->IsIgnoreOptimize || !g_pGamePersistent ||
		!g_pGamePersistent->Environment().CurrentEnv)
		return false;

	switch (pVisual->Type)
	{
	case MT_LOD:
	case MT_LOD1:
	case MT_LOD2:
	case MT_LOD3:
	case MT_LOD4:
	case MT_MESH_LODS:
		return false;
	}

	const float far_plane = g_pGamePersistent->Environment().CurrentEnv->far_plane;
	const float full_detail_distance = std::max(100.f, far_plane * ps_r1_full_detail_distance_scale);
	const float dist_sq = Device.vCameraPosition.distance_to_sqr(pVisual->vis.sphere.P) + EPS;
	const float nearest_distance = _sqrt(dist_sq) - pVisual->vis.sphere.R;
	if (nearest_distance <= full_detail_distance)
		return false;

	const float transition_range = std::max(1.f, far_plane - full_detail_distance);
	const float transition = clampr((nearest_distance - full_detail_distance) / transition_range, 0.f, 1.f);
	const float ssa = pVisual->vis.sphere.R / dist_sq;
	return ssa <= r_ssaDISCARD * (1.f + transition * 11.f);
}

// Aproximate, adjusted by fov, distance from camera to position (For right work when looking though binoculars and scopes)
ICF float GetDistFromCamera(const Fvector& from_position)
{
	float distance = Device.vCameraPosition.distance_to(from_position);
	float fov_K = BASE_FOV / Device.fFOV;
	float adjusted_distane = distance / fov_K;

	return adjusted_distane;
}

ICF bool IsValuableToRender(dxRender_Visual* pVisual, bool isStatic, bool sm, Fmatrix& transform_matrix, bool ignore_optimize = false)
{
	if (ignore_optimize)
		return true;

	int opt_level = isStatic ? opt_static : opt_dynamic;
	if (opt_level < 1)
	{
		return true;
	}

	if (!isStatic && RImplementation.val_bHUD)
	{
		return true;
	}

	float sphere_volume = pVisual->getVisData().sphere.volume();
	Fvector pos = pVisual->vis.sphere.P;

	if (!isStatic)
	{
		transform_matrix.transform_tiny(pos, pVisual->vis.sphere.P);
	}

	float adjusted_distance = GetDistFromCamera(pos);

	// Настройки для статических и динамических объектов
	static Fvector4 static_sizes[12] =
	{
		o_optimize_static_l1_size, o_optimize_static_l2_size, o_optimize_static_l3_size,
		o_optimize_static_l4_size, o_optimize_static_l5_size, o_optimize_static_l6_size,
		o_optimize_static_l7_size, o_optimize_static_l8_size, o_optimize_static_l9_size,
		o_optimize_static_l10_size, o_optimize_static_l11_size, o_optimize_static_l12_size
	};
	static Fvector4 static_dists[12] =
	{
		o_optimize_static_l1_dist, o_optimize_static_l2_dist, o_optimize_static_l3_dist,
		o_optimize_static_l4_dist, o_optimize_static_l5_dist, o_optimize_static_l6_dist,
		o_optimize_static_l7_dist, o_optimize_static_l8_dist, o_optimize_static_l9_dist,
		o_optimize_static_l10_dist, o_optimize_static_l11_dist, o_optimize_static_l12_dist
	};
	static Fvector4 dynamic_sizes[5] =
	{
		o_optimize_dynamic_l1_size, o_optimize_dynamic_l2_size, o_optimize_dynamic_l3_size,
		o_optimize_dynamic_l4_size, o_optimize_dynamic_l5_size
	};
	static Fvector4 dynamic_dists[5] =
	{
		o_optimize_dynamic_l1_dist, o_optimize_dynamic_l2_dist, o_optimize_dynamic_l3_dist,
		o_optimize_dynamic_l4_dist, o_optimize_dynamic_l5_dist
	};

	auto CheckLevelLabmda = [](Fvector4* sizes, Fvector4* dists, int count, int opt_level, float sphere_volume, float adjusted_distance) -> bool
	{
		for (int i = 0; i < count; ++i)
		{
			Fvector4 sz = sizes[i];
			Fvector4 ds = dists[i];

			float level_size = sz.x;
			float level_dist = ds.x;

			// Определяем компоненту по opt_level
			switch (opt_level)
			{
				case 2: level_size = sz.y; level_dist = ds.y; break;
				case 3: level_size = sz.z; level_dist = ds.z; break;
				case 4: level_size = sz.w; level_dist = ds.w; break;
			}

			if (sphere_volume < level_size && adjusted_distance > level_dist)
			{
				return false;
			}
		}
		return true;
	};

	if (sm && ps_r__common_flags.test(RFLAG_OPT_SHAD_GEOM))
	{
		if (sphere_volume < 50000.f && adjusted_distance > 160.f)
		{
			return false;
		}

		if (!CheckLevelLabmda(static_sizes, static_dists, std::size(static_sizes), opt_level, sphere_volume, adjusted_distance))
		{
			return false;
		}
	}

	if (isStatic)
	{
		return CheckLevelLabmda(static_sizes, static_dists, std::size(static_sizes), opt_level, sphere_volume, adjusted_distance);
	}

	return CheckLevelLabmda(dynamic_sizes, dynamic_dists, std::size(dynamic_sizes), opt_level, sphere_volume, adjusted_distance);
}

ICF R_dsgraph::_NormalItem MakeNormalItem(float ssa, dxRender_Visual* pVisual)
{
	R_dsgraph::_NormalItem I = { ssa, pVisual->vis.sphere.R, pVisual, nullptr, 0, 0, 0, 0 };

	if (MT_NORMAL != pVisual->Type)
		return I;

	Fvisual* V = (Fvisual*)pVisual;
	IRender_Mesh* M = V;

	if (V->m_fast && RImplementation.phase == CRender::PHASE_SMAP)
		M = V->m_fast;

	I.geom		= M->rm_geom._get();
	I.vBase		= M->vBase;
	I.vCount	= M->vCount;
	I.iBase		= M->iBase;
	I.pCount	= M->dwPrimitives;

	return I;
}

ICF float CalcSSADistanceSQ(const Fvector& C)
{
	if (RImplementation.val_bHUD)
	{
		return C.square_magnitude() + EPS;
	}

	return Device.vCameraPosition.distance_to_sqr(C) + EPS;
}

ICF	float CalcSSA(float& distSQ, Fvector& C, dxRender_Visual* V)
{
	float R = V->vis.sphere.R + 0;
	distSQ = CalcSSADistanceSQ(C);
	return	R / distSQ;
}

ICF	float CalcSSA(float& distSQ, Fvector& C, float R)
{
	distSQ = CalcSSADistanceSQ(C);
	return	R / distSQ;
}

ICF bool IsHudVisual(dxRender_Visual* pVisual)
{
	if (RImplementation.val_bHUD)
	{
		return true;
	}

	IParticleCustom* pParticle = pVisual->dcast_ParticleCustom();
	return pParticle && pParticle->GetHudMode();
}

void R_dsgraph_structure::r_dsgraph_insert_dynamic(dxRender_Visual* pVisual, Fvector& Center, bool Force)
{
	CRender& RI = RImplementation;
	if (SpotShadowCachedCasters && SpotShadowCachedCasters->contains(pVisual))
	{
		++SpotShadowCachedSkips;
		return;
	}

	if (pVisual->vis.marker == RI.marker)	
	{
		if (SpotShadowPass && !SpotShadowBuild)
		{
			++SpotShadowDuplicateSkips;
		}
		return;
	}

	pVisual->vis.marker = RI.marker;

	if (LightingModeIsStaticRender())
	{
		if (RI.vis_intersect && pVisual->vis.accept_frame != Device.dwFrame) return;
		pVisual->vis.accept_frame = Device.dwFrame;
	}

	float distSQ;
	float SSA = CalcSSA(distSQ, Center, pVisual);
	if (SSA <= r_ssaDISCARD && !Force) return;

	bool bHUD = IsHudVisual(pVisual);

	// Distortive geometry should be marked and R2 special-cases it
	// a) Allow to optimize RT order
	// b) Should be rendered to special distort buffer in another pass

	if (!pVisual->shader._get()) return;

	ShaderElement* sh_d = pVisual->shader->E[4] ? &*pVisual->shader->E[4] : nullptr;

	if (SpotShadowPass && (bHUD || RI.val_bUI))
	{
		return;
	}

	if (!SpotShadowPass && !RI.val_bUI && RImplementation.o.distortion && sh_d && sh_d->flags.bDistort && pmask[sh_d->flags.iPriority / 2] && !psDeviceFlags.test(rsClearBB))
	{
		mapSorted_T& test = bHUD ? mapHUDDistort : mapDistort;
		mapSorted_Node* N = test.insertInAnyWay(distSQ);

		N->val.ssa = SSA;
		N->val.pObject = RI.val_pObject;
		N->val.pVisual = pVisual;
		N->val.Matrix = *RI.val_pTransform;
		N->val.se = sh_d;		// 4=L_special
	}

	if (!SpotShadowPass && sh_d && sh_d->flags.bScopeMask && pmask[0])
	{
		mapHUDScopeMask.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh_d });
	}

	ShaderElement* sh = RImplementation.rimp_select_sh_dynamic(pVisual, distSQ, bHUD);

	if (!sh || RI.val_bInvisible)
	{
		return;
	}

	// UI rendering
	if (RI.val_bUI)
	{
		if (sh->flags.bStrictB2F || sh->flags.iPriority > 1)
		{
			mapUISorted.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh });
		}
		else
		{
			mapUI.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh });
			if (LightingModeIsDynamicRender() && sh->flags.bEmissive && sh != sh_d)
			{
				mapUIEmissive.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh_d });
			}
		}

		return;
	}

	if (pmask[sh->flags.iPriority / 2])
	{
		// HUD rendering
		if (bHUD)
		{
			if (sh->flags.bStrictB2F || sh->flags.iPriority > 1)
			{
				mapHUDSorted.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh });
			}
			else
			{
				mapHUD.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh });
				if (LightingModeIsDynamicRender() && sh->flags.bEmissive)
				{
					mapHUDEmissive.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh_d });
				}
			}
			return;
		}

		// Shadows registering
		if (RI.L_Shadows && RI.phase == CRender::PHASE_NORMAL)
		{
			_MatrixItem item = { SSA, RI.val_pObject, pVisual, *RI.val_pTransform };
			RI.L_Shadows->add_element(item);
		}

		// strict-sorting selection
		if (sh->flags.bStrictB2F || pVisual->dcast_ParticleCustom())
		{
			mapSorted.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh });
			return;
		}

		if (!SpotShadowPass && LightingModeIsDynamicRender() && sh->flags.bEmissive)
		{
			mapEmissive.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh_d });
		}

		if (!SpotShadowPass && LightingModeIsDynamicRender() && sh->flags.bWmark && pmask[2])
		{
			mapWmark.insertInAnyWay(distSQ, { SSA, RI.val_pObject, pVisual, *RI.val_pTransform, sh });
			return;
		}
	}

	for (u32 iPass = 0; iPass < sh->passes.size(); ++iPass)
	{
		// the most common node
		if (sh->passes[iPass] == nullptr)
		{
			continue;
		}

		SPass& pass = *sh->passes[iPass];
		u32 iPriority = pass.iPriority != u8(-1) ? pass.iPriority : sh->flags.iPriority;

		if (!pmask[iPriority / 2])
		{
			continue;
		}

		mapMatrix_T& map = mapMatrixPasses[iPriority / 2][iPass];
		
#ifdef USE_RESOURCE_DEBUGGER
		mapMatrixVS::TNode*			Nvs		= map.insert		(pass.vs);
		mapMatrixGS::TNode*			Ngs		= Nvs->val.insert	(pass.gs);
		mapMatrixPS::TNode*			Nps		= Ngs->val.insert	(pass.ps);
#else
		mapMatrixVS::TNode*			Nvs		= map.insert		(&*pass.vs);
		mapMatrixGS::TNode*			Ngs		= Nvs->val.insert	(pass.gs->gs);
		mapMatrixPS::TNode*			Nps		= Ngs->val.insert	(pass.ps->ps);
#endif

#	ifdef USE_RESOURCE_DEBUGGER
		Nps->val.hs = pass.hs;
		Nps->val.ds = pass.ds;
		mapMatrixCS::TNode*			Ncs		= Nps->val.mapCS.insert	(pass.constants._get());
#	else
		Nps->val.hs = pass.hs->sh;
		Nps->val.ds = pass.ds->sh;
		mapMatrixCS::TNode*			Ncs		= Nps->val.mapCS.insert	(pass.constants._get());
#	endif
		mapMatrixStates::TNode*		Nstate	= Ncs->val.insert	(pass.state->state);
		mapMatrixTextures::TNode*	Ntex	= Nstate->val.insert(pass.T._get());

		Ntex->val.visuals.push_back({ SSA, RI.val_pObject, pVisual, *RI.val_pTransform });
	}
}

void R_dsgraph_structure::r_dsgraph_insert_static(dxRender_Visual* pVisual)
{
	CRender& RI = RImplementation;
	if (SpotShadowCachedCasters && SpotShadowCachedCasters->contains(pVisual))
	{
		++SpotShadowCachedSkips;
		return;
	}

	CStaticTraversalStats::SectorCounters* Stats = g_StaticTraversalStats.Current;
	if (Stats)
	{
		++Stats->InsertCalls;
	}

	if (pVisual->vis.marker == RI.marker)
	{
		if (SpotShadowPass && !SpotShadowBuild)
		{
			++SpotShadowDuplicateSkips;
		}
		if (Stats)
		{
			++Stats->InsertDuplicates;
		}
		return;
	}

	pVisual->vis.marker = RI.marker;

	if (LightingModeIsStaticRender())
	{
		if (RI.vis_intersect && pVisual->vis.accept_frame != Device.dwFrame) return;
		pVisual->vis.accept_frame = Device.dwFrame;
	}

	float distSQ = 0.f;
	float SSA = SpotShadowBuild ? 1.f : CalcSSA(distSQ, pVisual->vis.sphere.P, pVisual);
	if (!SpotShadowBuild && SSA <= r_ssaDISCARD)
	{
		if (Stats)
		{
			++Stats->InsertDiscarded;
		}
		return;
	}

	// Distortive geometry should be marked and R2 special-cases it
	// a) Allow to optimize RT order
	// b) Should be rendered to special distort buffer in another pass
	if (!pVisual->shader._get()) return;
	ShaderElement* sh_d = pVisual->shader->E[4] ? &*pVisual->shader->E[4] : nullptr;

	if (!SpotShadowPass && !SpotShadowBuild && RImplementation.o.distortion && sh_d && sh_d->flags.bDistort && pmask[sh_d->flags.iPriority/2] && !psDeviceFlags.test(rsClearBB))
	{
		mapDistort.insertInAnyWay(distSQ, { SSA, nullptr, pVisual, Fidentity, sh_d });
	}

	// Select shader
	ShaderElement* sh = RImplementation.rimp_select_sh_static(pVisual, distSQ);
	if (!sh)
	{
		return;
	}
	if (Stats)
	{
		++Stats->Inserted;
	}
	if (SpotShadowBuild && SpotShadowBuild->CollectOnly)
	{
		SpotShadowBuild->RejectionReason = nullptr;
		SpotShadowBuild->RejectedTexture = nullptr;
		const bool FixedGeometry = pVisual->Type == MT_NORMAL || pVisual->Type == MT_PROGRESSIVE;
		if (FixedGeometry && SpotShadowCasters::IsSpotShadowCacheCaster(*sh, *SpotShadowBuild))
		{
			SpotShadowBuild->CachedCasters.push_back(pVisual);
		}
		else
		{
			SpotShadowBuild->PerFrameCasters.push_back(pVisual);
			if (!SpotShadowBuild->PerFrameReason.empty())
			{
				return;
			}
			const SPass* Pass = !sh->passes.empty() && sh->passes[0] ? &*sh->passes[0] : nullptr;
			string2048 Diagnostic;
			xr_sprintf(Diagnostic, "%s visual_type=%u shader=%s vs=%s ps=%s texture=%s", FixedGeometry ? SpotShadowBuild->RejectionReason : "animated_or_unsupported_visual", pVisual->Type, pVisual->shader_name ? *pVisual->shader_name : "<unnamed>", Pass && Pass->vs && Pass->vs->cName ? *Pass->vs->cName : "<none>", Pass && Pass->ps && Pass->ps->cName ? *Pass->ps->cName : "<none>", SpotShadowBuild->RejectedTexture ? *SpotShadowBuild->RejectedTexture : "<none>");
			SpotShadowBuild->PerFrameReason = Diagnostic;
		}
		return;
	}

	if (pmask[sh->flags.iPriority / 2])
	{
		// strict-sorting selection
		if (sh->flags.bStrictB2F)
		{
			mapSorted.insertInAnyWay(distSQ, {SSA, nullptr, pVisual, Fidentity, sh});
			return;
		}

		if (!SpotShadowPass && LightingModeIsDynamicRender() && sh->flags.bEmissive)
		{
			mapEmissive.insertInAnyWay(distSQ, { SSA, nullptr, pVisual, Fidentity, sh_d });
		}

		if (!SpotShadowPass && LightingModeIsDynamicRender() && sh->flags.bWmark && pmask[2])
		{
			mapWmark.insertInAnyWay(distSQ, { SSA, nullptr, pVisual, Fidentity, sh });
			return;
		}

		if (val_feedback && counter_S == val_feedback_breakp)
		{
			val_feedback->rfeedback_static(pVisual);
		}

		counter_S++;
	}

	for (u32 iPass = 0; iPass < sh->passes.size(); ++iPass)
	{
		if (sh->passes[iPass] == nullptr)
		{
			continue;
		}

		SPass& pass = *sh->passes[iPass];
		u32 iPriority = pass.iPriority != u8(-1) ? pass.iPriority : sh->flags.iPriority;

		if (!pmask[iPriority / 2])
		{
			continue;
		}

		mapNormal_T& map = mapNormalPasses[iPriority / 2][iPass];

		mapNormalVS::TNode*			Nvs		= map.insert		(&*pass.vs);
		mapNormalGS::TNode*			Ngs		= Nvs->val.insert	(pass.gs->gs);
		mapNormalPS::TNode*			Nps		= Ngs->val.insert	(pass.ps->ps);

		Nps->val.hs = pass.hs->sh;
		Nps->val.ds = pass.ds->sh;
		mapNormalCS::TNode*			Ncs		= Nps->val.mapCS.insert	(pass.constants._get());
		mapNormalStates::TNode*		Nstate	= Ncs->val.insert	(pass.state->state);
		mapNormalTextures::TNode*	Ntex	= Nstate->val.insert(pass.T._get());

		Ntex->val.push_back(MakeNormalItem(SSA, pVisual));
	}
}

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
void R_dsgraph_structure::add_leafs_Dynamic(dxRender_Visual *pVisual, bool IgnoreObject, bool Force)
{
	if (0 == pVisual) return;

	if (!IsValuableToRender(pVisual, false, phase == CRender::PHASE_SMAP, *val_pTransform, IgnoreObject))
	{
		return;
	}
	
	// Visual is 100% visible - simply add it
	switch (pVisual->Type)
	{
	case MT_PARTICLE_GROUP:
		{
			// Add all children, doesn't perform any tests
			PS::CParticleGroup* pG = (PS::CParticleGroup*)pVisual;
			xrCriticalSectionGuard guard(pG->onframe_lock);
			for (PS::CParticleGroup::SItem& item : pG->items)
			{
				if (item.root_effect)
					add_leafs_Dynamic(item.root_effect);

				xrCriticalSectionGuard guard(item.childs_cs);
				for (dxRender_Visual* pEffect : item.children_related)
					add_leafs_Dynamic(pEffect);

				for (dxRender_Visual* pEffect : item.children_free)
					add_leafs_Dynamic(pEffect);
			}
		}
		return;
	case MT_HIERRARHY:
		{
			// Add all children, doesn't perform any tests
			FHierrarhyVisual* pV = (FHierrarhyVisual*)pVisual;
			for (dxRender_Visual* V : pV->children)
				add_leafs_Dynamic(V, IgnoreObject);
		}
		return;
	case MT_SKELETON_ANIM:
	case MT_SKELETON_RIGID:
		{
			// Add all children, doesn't perform any tests
			CKinematics * pV = (CKinematics*)pVisual;
			bool _use_lod = false;
			if (pV->m_lod)				
			{
				Fvector Tpos; float D;
				val_pTransform->transform_tiny(Tpos, pV->vis.sphere.P);
				float ssa =	CalcSSA(D,Tpos,pV->vis.sphere.R/2.f);// assume dynamics never consume full sphere
				if (ssa<r_ssaLOD_A)
					_use_lod= true;
			}
			if (_use_lod)				
				add_leafs_Dynamic(pV->m_lod, IgnoreObject);
			else
			{
				for (dxRender_Visual* V : pV->children)
					add_leafs_Dynamic(V, IgnoreObject);
			}
		}
		return;
	default:
		{
			// General type of visual
			// Calculate distance to it's center
			Fvector Tpos;
			val_pTransform->transform_tiny(Tpos, pVisual->vis.sphere.P);
			r_dsgraph_insert_dynamic(pVisual,Tpos, Force);
		}
		return;
	}
}

void add_leafs_Static(xr_vector<dxRender_Visual*>& children);
ICF void r_dsgraph_insert_static_lod(dxRender_Visual* pVisual)
{
	CRender& RI = RImplementation;
	FLOD* pV = (FLOD*)pVisual;
	float D; float ssa = CalcSSA(D, pV->vis.sphere.P, pV);
	ssa *= pV->lod_factor;
	if (ssa < r_ssaLOD_A && !RI.SpotShadowPass)
	{
		if (ssa < r_ssaDISCARD)	return;
		mapLOD_Node* N = RI.mapLOD.insertInAnyWay(D);
		N->val.ssa = ssa;
		N->val.pVisual = pVisual;
	}
	if (ssa > r_ssaLOD_B || RI.phase == CRender::PHASE_SMAP)
	{
		// Add all children, doesn't perform any tests
		add_leafs_Static(pV->children);
	}
}

//Seakad: лучше работает
IC float distance_to_aabb(const Fvector& point,const Fbox& box)
{
    float dx = 0.0f;
    float dy = 0.0f;
    float dz = 0.0f;

    // X
    if (point.x < box.min.x)
        dx = box.min.x - point.x;
    else if (point.x > box.max.x)
        dx = point.x - box.max.x;

    // Y
    if (point.y < box.min.y)
        dy = box.min.y - point.y;
    else if (point.y > box.max.y)
        dy = point.y - box.max.y;

    // Z
    if (point.z < box.min.z)
        dz = box.min.z - point.z;
    else if (point.z > box.max.z)
        dz = point.z - box.max.z;

    return _sqrt(dx * dx + dy * dy + dz * dz);
}

IC FMUMeshLOD* SelectLOD(float D, const xr_vector<dxRender_Visual*>& V)
{
	VERIFY(!V.empty());
	switch (V.size())
	{
		case 5:
		{
			if (D >= r_ssaLOD_MU3 && D < (r_ssaLOD_MU4/(ps_r__LOD_MU4_discard * 0.001f)))
			{
				return (FMUMeshLOD*)V[4];
			}
		}
		case 4:
		{
			if (D >= r_ssaLOD_MU2 && D < r_ssaLOD_MU3)
			{
				return (FMUMeshLOD*)V[3];
			}
		}
		case 3:
		{
			if (D >= r_ssaLOD_MU1 && D < r_ssaLOD_MU2)
			{
				return (FMUMeshLOD*)V[2];
			}
		}
		case 2:
		{
			if (D >= r_ssaLOD_MU0 && D < r_ssaLOD_MU1)
			{
				return (FMUMeshLOD*)V[1];
			}
		}
		case 1:
		{
			if (D < r_ssaLOD_MU0)
			{
				return (FMUMeshLOD*)V.at(0);
			}
		}
	}
	return nullptr;
}

void add_leafs_Static(xr_vector<dxRender_Visual*>& children)
{
	//PROF_EVENT("add_leafs_Static")
	CRender& RI = RImplementation;
	float fog_distance = g_pGamePersistent->Environment().CurrentEnv->fog_distance;
	CStaticTraversalStats::SectorCounters* Stats = g_StaticTraversalStats.Current;
	for(dxRender_Visual* pVisual : children)
	{
		vis_data& vis = pVisual->vis;
		if (Stats)
		{
			++Stats->Visited;
		}
		if (FullDetailRejectStatic(pVisual))
		{
			if (Stats)
			{
				++Stats->RejectFullDetail;
			}
			continue;
		}

		if (RI.phase == CRender::PHASE_NORMAL)
		if (!RI.HOM.visible(vis))
		{
			if (Stats)
			{
				++Stats->RejectHOM;
			}
			continue;
		}

		if (!pVisual->IsIgnoreOptimize && !IsValuableToRender(pVisual, true, RI.phase == CRender::PHASE_SMAP, *RI.val_pTransform))
		{
			if (Stats)
			{
				++Stats->RejectValuable;
			}
			continue;
		}

		if (Device.vCameraPosition.distance_to_sqr(vis.sphere.P) > _sqr(fog_distance + vis.sphere.R))
		{
			if (Stats)
			{
				++Stats->RejectFog;
			}
			continue;
		}

		float D = distance_to_aabb(Device.vCameraPosition,pVisual->vis.box);

		// Visual is 100% visible - simply add it
		switch (pVisual->Type)
		{
			case MT_HIERRARHY:
			{
				if (Stats)
				{
					++Stats->FullHierarchies;
				}
				// Add all children, doesn't perform any tests
				add_leafs_Static(static_cast<FHierrarhyVisual*>(pVisual)->children);
			}continue;
			case MT_LOD:
			{
				r_dsgraph_insert_static_lod(pVisual);
			}continue;
			case MT_MESH_LODS:
			{
				FMUMeshLOD* SelectedLOD = SelectLOD(D, static_cast<FMUMeshLODs*>(pVisual)->children);
				if(SelectedLOD)
				{
					add_leafs_Static(SelectedLOD->children);
				}
				continue;
			}
			case MT_TREE_PM:
			case MT_TREE_ST:
			{
				//PROF_EVENT("CRender::add_leafs_Static: MT_TREE")
				// General type of visual
				RI.r_dsgraph_insert_static(pVisual);
			}continue;
			default:
			{
				//PROF_EVENT("CRender::add_leafs_Static: Default")
				// General type of visual
				RI.r_dsgraph_insert_static(pVisual);
			}continue;
		}
	}
}

IC float fog_cull_distance()
{
	static u32   frame = u32(-1);
	static float cached = 0.f;
	if (frame != Device.dwFrame)
	{
		frame = Device.dwFrame;
		cached = g_pGamePersistent->Environment().CurrentEnv->fog_distance;
	}
	return cached;
}

void R_dsgraph_structure::add_Static(dxRender_Visual *pVisual, u32 planes)
{
	if (SpotShadowBuild)
	{
		CollectSpotShadowStatic(pVisual, planes);
		return;
	}
	//PROF_EVENT("add_Static")
	CStaticTraversalStats::SectorCounters* Stats = g_StaticTraversalStats.Current;
	if (Stats)
	{
		++Stats->Visited;
	}
	if (!pVisual->IsIgnoreOptimize && !IsValuableToRender(pVisual, true, phase == CRender::PHASE_SMAP, *val_pTransform))
	{
		if (Stats)
		{
			++Stats->RejectValuable;
		}
		return;
	}

	// Check frustum visibility and calculate distance to visual's center
	EFC_Visible	VIS;
	vis_data& vis = pVisual->vis;
	if (pVisual->Type == MT_LOD || pVisual->Type == MT_LOD1 || pVisual->Type == MT_LOD2 || pVisual->Type == MT_LOD3 || pVisual->Type == MT_LOD4)
	{
		VIS = View->testAABB	(vis.box.data(),planes);
	}
	else
	{
		VIS = View->testSAABB	(vis.sphere.P,vis.sphere.R,vis.box.data(),planes);
	}

	if (fcvNone==VIS)
	{
		if (Stats)
		{
			++Stats->RejectFrustum;
		}
		return;
	}

	if (FullDetailRejectStatic(pVisual))
	{
		if (Stats)
		{
			++Stats->RejectFullDetail;
		}
		return;
	}
	if(phase==CRender::PHASE_NORMAL)
	if (!RImplementation.HOM.visible(vis))
	{
		if (Stats)
		{
			++Stats->RejectHOM;
		}
		return;
	}

	if (Device.vCameraPosition.distance_to_sqr(vis.sphere.P) > _sqr(fog_cull_distance() + vis.sphere.R))
	{
		if (Stats)
		{
			++Stats->RejectFog;
		}
		return;
	}

	float D = distance_to_aabb(Device.vCameraPosition,pVisual->vis.box);

	PROF_EVENT("add_Static switch")
	// If we get here visual is visible or partially visible
	switch (pVisual->Type)
	{
	case MT_HIERRARHY:
	{
		// Add all children
		FHierrarhyVisual* pV = (FHierrarhyVisual*)pVisual;
		if (fcvPartial == VIS)
		{
			if (Stats)
			{
				++Stats->PartialHierarchies;
			}
			for (dxRender_Visual* V : pV->children)
				add_Static(V, planes);
		}
		else
		{
			if (Stats)
			{
				++Stats->FullHierarchies;
			}
			add_leafs_Static(pV->children);
		}
		}return;
		case MT_LOD:
		{
			r_dsgraph_insert_static_lod(pVisual);
		}return;
		case MT_MESH_LODS:
		{
			auto Casted = SelectLOD(D, static_cast<FMUMeshLODs*>(pVisual)->children);
			if (Casted)
			{
				add_leafs_Static(Casted->children);
			}
			return;
		}
	case MT_TREE_ST:
	case MT_TREE_PM:
	{
		// General type of visual
		r_dsgraph_insert_static(pVisual);
	}return;
	default:
	{
		// General type of visual
		r_dsgraph_insert_static(pVisual);
	}return;
	}
}

void R_dsgraph_structure::CollectSpotShadowStatic(dxRender_Visual* Visual, u32 Planes)
{
	if (!Visual)
	{
		return;
	}
	if (View->testAABB(Visual->vis.box.data(), Planes) == fcvNone)
	{
		return;
	}
	// Even a visual without a shadow shader can become a caster after a material reload.
	Visual->IsSpotShadowCacheReferenced.store(true, std::memory_order_relaxed);
	switch (Visual->Type)
	{
		case MT_MESH_LODS:
		case MT_LOD:
		{
			// Keep the parent so per-frame collection can select the camera-dependent LOD.
			if (Visual->vis.marker != RImplementation.marker)
			{
				Visual->vis.marker = RImplementation.marker;
				SpotShadowBuild->PerFrameCasters.push_back(Visual);
				if (SpotShadowBuild->PerFrameReason.empty())
				{
					string128 Reason;
					xr_sprintf(Reason, "camera_dependent_lod_group visual_type=%u", Visual->Type);
					SpotShadowBuild->PerFrameReason = Reason;
				}
			}
			return;
		}
		case MT_HIERRARHY:
		case MT_LOD0:
		case MT_LOD1:
		case MT_LOD2:
		case MT_LOD3:
		case MT_LOD4:
		{
			// A billboard parent contributes its actual shadow geometry, never a camera-facing quad.
			for (dxRender_Visual* Child : static_cast<FHierrarhyVisual*>(Visual)->children)
			{
				CollectSpotShadowStatic(Child, Planes);
			}
			return;
		}
		default:
		{
			r_dsgraph_insert_static(Visual);
			return;
		}
	}
}

CDB::MODEL* R_dsgraph_structure::GetHOMModel()
{
	return RImplementation.HOM.GetHOMModel();
}
xr_vector<u32>* R_dsgraph_structure::GetHOMInvaltids()
{
	return RImplementation.HOM.get_invaltids();
};
