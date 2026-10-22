#include "stdafx.h"
#include "GrassTerrainTintMap.h"
#include "GrassTerrainTintMath.h"
#include "FBasicVisual.h"
#include "ResourceManager.h"
#include "blenders/Blender.h"
#include "dxRenderDeviceRender.h"
#include "../../xrEngine/EngineAPI.h"

bool g_GrassTerrainTintEnabled = true;
bool g_GrassTerrainTintGreenOnly = true;
float g_GrassTerrainTintStrength = 0.2f;
float g_GrassTerrainTintRadius = 2.0f;
float g_GrassTerrainTintHeightLimit = 1.5f;
int g_GrassTerrainTintResolution = 1024;
int g_GrassTerrainTintDebug = 0;

namespace GrassTerrainTint
{
class DebugBlender final : public IBlender
{
public:
	const char* getComment() override { return "Terrain color map preview"; }
	bool canBeLMAPped() override { return false; }
	void Compile(CBlender_Compile& C) override
	{
		IBlender::Compile(C);
		if (C.iElement != 0)
		{
			return;
		}
		C.r_Pass("stub_fullscreen_triangle", "grass_terrain_tint_debug", false, false, false);
		C.r_CullMode(D3DCULL_NONE);
		C.r_dx10Texture("s_grass_tint_color", "$user$grass_terrain_tint_color");
		C.r_dx10Sampler("smp_rtlinear");
		C.r_End();
	}
};

ShaderElement* CaptureElement(dxRender_Visual* Visual)
{
	if (!Visual || (Visual->Type != MT_NORMAL && Visual->Type != MT_PROGRESSIVE) || !Visual->shader)
	{
		return nullptr;
	}
	const auto& Element = Visual->shader->E[SE_R2_TERRAIN_TINT];
	return Element && Element->flags.bLandscape && !Element->passes.empty() ? &*Element : nullptr;
}
} // namespace GrassTerrainTint

void GrassTerrainTintMap::Destroy()
{
	Ready = false;
	DebugShader.destroy();
	EmptyTextures._set(nullptr);
	Color.destroy();
	Height.destroy();
	Bounds.set(0.0f, 0.0f, 0.0f, 0.0f);
	HeightRange.set(0.0f, 0.0f, 0.0f, 0.0f);
	Resolution = 0;
	RebuildPending.store(true);
}

void GrassTerrainTintMap::RequestRebuild()
{
	RebuildPending.store(true);
}

Fvector4 GrassTerrainTintMap::GetParameters() const
{
	const float Width = Bounds.z > 0.0f ? 1.0f / Bounds.z : 0.0f;
	const float Depth = Bounds.w > 0.0f ? 1.0f / Bounds.w : 0.0f;
	Fvector4 Result;
	Result.set(Ready && g_GrassTerrainTintEnabled ? g_GrassTerrainTintStrength : 0.0f, GrassTerrainTint::SmoothingMip(Width, Depth, Resolution, g_GrassTerrainTintRadius), g_GrassTerrainTintHeightLimit, float(g_GrassTerrainTintDebug));
	return Result;
}

Fvector4 GrassTerrainTintMap::GetOptions() const
{
	Fvector4 Result;
	Result.set(g_GrassTerrainTintGreenOnly ? 1.0f : 0.0f, Ready && g_GrassTerrainTintEnabled ? 1.0f : 0.0f, 0.0f, 0.0f);
	return Result;
}

void GrassTerrainTintMap::Update(CRender& Renderer)
{
	if (!g_GrassTerrainTintEnabled || !Renderer.b_loaded || Device.IsEditorMode() || !LightingModeIsDynamicRender())
	{
		return;
	}
	const u32 RequestedResolution = GrassTerrainTint::MapResolution(g_GrassTerrainTintResolution);
	if (!RebuildPending.exchange(false) && Resolution == RequestedResolution)
	{
		return;
	}
	CTimer Timer;
	Timer.Start();
	Ready = false;
	Resolution = RequestedResolution;

	Fbox Box;
	Box.invalidate();
	xr_vector<dxRender_Visual*> Terrain;
	for (dxRender_Visual* Visual : Renderer.Visuals)
	{
		if (GrassTerrainTint::CaptureElement(Visual))
		{
			Terrain.push_back(Visual);
			Box.merge(Visual->vis.box);
		}
	}
	if (Terrain.empty())
	{
		Msg("! Grass terrain tint: no supported terrain surfaces; keeping original vegetation colors");
		return;
	}
	const float Width = Box.max.x - Box.min.x;
	const float Depth = Box.max.z - Box.min.z;
	const float Range = Box.max.y - Box.min.y + 2.0f;
	if (!std::isfinite(Width) || !std::isfinite(Depth) || !std::isfinite(Range) || Width <= EPS_L || Depth <= EPS_L || Range <= EPS_L)
	{
		Msg("! Grass terrain tint: invalid terrain bounds; keeping original vegetation colors");
		return;
	}
	Bounds.set(Box.min.x, Box.min.z, 1.0f / Width, 1.0f / Depth);
	HeightRange.set(Box.min.y - 1.0f, Range, Box.max.y + 1.0f, 1.0f / Range);

	// Clear bindings before replacing named user textures on an explicit rebuild.
	if (!EmptyTextures)
	{
		STextureList Empty;
		EmptyTextures = dxRenderDeviceRender::Instance().Resources->_CreateTextureList(Empty);
	}
	RCache.set_Textures(EmptyTextures);
	GRHI->ShaderResourceCache->Apply();
	Color.destroy();
	Height.destroy();
	Color.create("$user$grass_terrain_tint_color", Resolution, Resolution, ERHI_FORMAT::R16G16B16A16_FLOAT, 1, CRT::AUTOGEN_MIP_MAPS);
	Height.create("$user$grass_terrain_tint_height", Resolution, Resolution, ERHI_FORMAT::R32_FLOAT);
	ref_rt DepthTarget;
	DepthTarget.create("$user$grass_terrain_tint_depth", Resolution, Resolution, ERHI_FORMAT::D32_FLOAT);
	if (!Color || !Height || !DepthTarget || !Color->valid() || !Height->valid() || !DepthTarget->valid())
	{
		Msg("! Grass terrain tint: could not allocate capture targets; keeping original vegetation colors");
		return;
	}

	{
		auto* Previous0 = RCache.get_RT(0);
		auto* Previous1 = RCache.get_RT(1);
		auto* Previous2 = RCache.get_RT(2);
		auto* Previous3 = RCache.get_RT(3);
		auto* PreviousDepth = GRHI->GetDepthStencilView();
		const u32 PreviousWidth = RCache.get_target_width();
		const u32 PreviousHeight = RCache.get_target_height();
		const u32 PreviousPhase = Renderer.phase;
		auto RestoreTargets = [&]
		{
			Renderer.Target->u_setrt(PreviousWidth, PreviousHeight, Previous0, Previous1, Previous2, Previous3, PreviousDepth);
			Renderer.phase = PreviousPhase;
			RCache.set_Scissor(nullptr);
		};
		xr_scope_exit Restore(std::move(RestoreTargets));
		Renderer.phase = CRender::PHASE_NORMAL;
		Renderer.Target->u_setrt(Color, Height, DepthTarget->pZRT);
		GRHI->ClearTarget(Color->pRT, ERTColor::Transparent);
		GRHI->ClearTarget(Height->pRT, ERTColor::Transparent);
		GRHI->ClearDepthStencil(DepthTarget->pZRT, ERHI_CLEAR_TARGET::DEPTH, 1.0f, 0);
		RCache.set_Scissor(nullptr);

		// Every static terrain leaf is drawn directly, without the player's portal/HOM/LOD visibility.
		for (dxRender_Visual* Visual : Terrain)
		{
			RCache.set_Element(Visual->shader->E[SE_R2_TERRAIN_TINT]);
			Visual->Render(1.0f);
		}
	}
	GRHI->GenerateMips(Color->pTexture->get_SRView());
	Ready = true;
	Msg("* Grass terrain tint: %ux%u, %zu terrain surfaces, XZ=(%.1f,%.1f)..(%.1f,%.1f), %.2f ms",
		Resolution,
		Resolution,
		Terrain.size(),
		Box.min.x,
		Box.min.z,
		Box.max.x,
		Box.max.z,
		Timer.GetElapsed_ms_f());
}

void GrassTerrainTintMap::DrawDebug(CRender& Renderer)
{
	if (!Ready)
	{
		return;
	}
	if (!DebugShader)
	{
		GrassTerrainTint::DebugBlender Blender;
		DebugShader.create(&Blender, "grass_terrain_tint_debug");
	}
	Renderer.Target->u_setrt(RCache.get_width(), RCache.get_height(), Renderer.Target->rt_BackbufferLUT->pRT, nullptr, nullptr, nullptr, nullptr);
	RCache.set_Element(DebugShader->E[0]);
	RCache.set_Geometry(Renderer.Target->FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
}
