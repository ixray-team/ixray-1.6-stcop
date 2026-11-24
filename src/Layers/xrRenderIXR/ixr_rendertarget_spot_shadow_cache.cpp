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
#include "light.h"
#include "FBasicVisual.h"
#include "../../xrEngine/xr_object.h"
#include "ixr_fullscreen_pass.h"

namespace SpotShadowCacheUtils
{
constexpr u32 MaxEntries = 128;
constexpr u32 MaxFreeDepths = 6;
constexpr u32 Lifetime = 1000;
constexpr u32 TextureRetryFrames = 30;

class CopyBlender final : public IBlender
{
public:
	const char* getComment() override
	{
		return "Restore cached spot shadow depth";
	}
	bool canBeLMAPped() override
	{
		return false;
	}
	void Compile(CBlender_Compile& C) override
	{
		IBlender::Compile(C);
		if (C.iElement != 0)
		{
			return;
		}
		C.r_Pass("stub_fullscreen_triangle", "spot_shadow_cache_copy", false, false, true);
		C.r_CullMode(D3DCULL_NONE);
		C.r_Stencil(false);
		C.r_ColorWriteEnable(false, false, false, false);
		C.r_dx10Texture("s_static_shadow", C.L_textures[0]);
		C.r_End();
	}
};

static bool EqualMatrix(const Fmatrix& Left, const Fmatrix& Right)
{
	for (u32 Row = 0; Row < 4; ++Row)
	{
		for (u32 Column = 0; Column < 4; ++Column)
		{
			if (Left.m[Row][Column] != Right.m[Row][Column])
			{
				return false;
			}
		}
	}
	return true;
}

static bool ValidMatrix(const Fmatrix& Matrix)
{
	for (u32 Row = 0; Row < 4; ++Row)
	{
		for (u32 Column = 0; Column < 4; ++Column)
		{
			if (!_valid(Matrix.m[Row][Column]))
			{
				return false;
			}
		}
	}
	return true;
}

static bool HasGeometry(R_dsgraph_structure& Renderer, u32 Priority)
{
	return Renderer.HasPasses(Priority);
}

static bool HasDecorativeCasters(const light& Source)
{
	for (const CObject* Object : Source.decor_object)
	{
		if (Object && !Object->getDestroy())
		{
			return true;
		}
	}
	return false;
}

static bool CanStoreDepth(const light& Source)
{
	return !Device.IsEditorMode() && g_RenderFeatures.LampShadows && RImplementation.GraphMain.b_loaded &&
		   psDeviceFlags.test(rsDrawStatic) && !psDeviceFlags.test(rsClearBB) &&
		   (Source.flags.type == IRender_Light::SPOT || Source.flags.type == IRender_Light::OMNIPART) &&
		   !Source.flags.bHudMode && Source.SpatialComponent && Source.SpatialComponent->sector;
}

// Grass shadows and moving casters only matter near the camera; a lamp bakes its dynamic casters
// after they have stayed the same for this many frames
constexpr float GrassRange = 20.f;
constexpr float DynamicRange = 40.f;
constexpr u32 BakeFrames = 30;
// The cached depth holds the level's translucent casters while r4_translucent_shadows is on
static bool IsTranslucentCached = false;

static bool HasStaticCasters(const xr_vector<dxRender_Visual*>& Cached)
{
	return !Cached.empty() || RImplementation.Scene.HasDepthBatch();
}

// Sectors: those the part's portals reached this frame (LampCollect); without them the portals are traversed here
static bool DrawLevelBatch(GfxCmdList& Cmd, R_dsgraph_structure& Renderer, const light& Source, IRHIDepthStencilView* Target, const RHIViewport* Area, u32 Slice = 0, const u32* Sectors = nullptr)
{
	if (!RImplementation.Scene.HasDepthBatch())
	{
		return true;
	}

	if (!Sectors)
	{
		Renderer.Traverse();
	}

	GpuScene::DepthDraw Draw = { Target, Slice, Area };
	Draw.Eye = &Source.position;
	Draw.Sectors = Sectors;
	Draw.IsSpot = true;
	Draw.IsKept = true;
	Draw.Kind = ShadowKind::SpotCache;
	return RImplementation.Scene.DrawDepth(Cmd, Renderer, Draw);
}

static u32 DepthTexelBytes(ERHI_FORMAT Format)
{
	return Format == ERHI_FORMAT::D16_UNORM || Format == ERHI_FORMAT::R16_TYPELESS ? 2 : 4;
}
} // namespace SpotShadowCacheUtils

bool CRenderTarget::SpotShadowCacheKey::operator==(const SpotShadowCacheKey& Other) const
{
	return Size == Other.Size && Format == Other.Format && MipBias == Other.MipBias && Anisotropy == Other.Anisotropy && HasSameGeometry(Other);
}

bool CRenderTarget::SpotShadowCacheKey::HasSameGeometry(const SpotShadowCacheKey& Other) const
{
	return Sector == Other.Sector && Revision == Other.Revision &&
		   SpotShadowCacheUtils::EqualMatrix(View, Other.View) && SpotShadowCacheUtils::EqualMatrix(Project, Other.Project);
}

CRenderTarget::SpotShadowCacheKey CRenderTarget::MakeSpotShadowCacheKey(const light& Source) const
{
	SpotShadowCacheKey Key;
	Key.View = Source.X.S.view;
	Key.Project = Source.X.S.project;
	Key.Sector = Source.SpatialComponent ? Source.SpatialComponent->sector : nullptr;
	Key.Size = Source.X.S.size;
	Key.Format = static_cast<ERHI_FORMAT>(RImplementation.o.HW_smap_FORMAT);
	Key.Revision = SpotShadowAppliedRevision;
	Key.MipBias = ps_r__tf_Mipbias;
	Key.Anisotropy = ps_r__tf_Anisotropic;
	return Key;
}

void CRenderTarget::InvalidateSpotShadowCache()
{
	// Resource callbacks may run while their containers are being traversed.
	SpotShadowContentRevision.fetch_add(1, std::memory_order_relaxed);
}

void CRenderTarget::ClearSpotShadowCache()
{
	if (!SpotShadowCache.empty() || !SpotShadowFreeDepths.empty())
	{
		RImplementation.Imm().Invalidate();
	}
	SpotShadowCache.clear();
	SpotShadowFreeDepths.clear();
	SpotShadowDepthRequests.clear();
	SpotShadowCacheBytes = 0;
}

void CRenderTarget::CreateSpotShadowDepths()
{
	if (SpotShadowDepthRequests.empty())
	{
		return;
	}

	PROF_EVENT("SpotShadowCache::CreateDepths");

	for (SpotShadowFreeDepth& Request : SpotShadowDepthRequests)
	{
		string128 Name;
		xr_sprintf(Name, "$user$spot_shadow_cache_%llu", static_cast<unsigned long long>(++SpotShadowResourceSerial));
		Request.Depth.create(Name, Request.Size, Request.Size, Request.Format);

		if (Request.Depth && Request.Depth->pSurface && Request.Depth->pZRT && Request.Depth->pTexture && Request.Depth->pTexture->get_SRView())
		{
			CreateEffectSQ<SpotShadowCacheUtils::CopyBlender>(Request.CopyShader, nullptr, Name);
		}

		if (Request.CopyShader && Request.CopyShader->E[0] && !Request.CopyShader->E[0]->passes.empty())
		{
			SpotShadowFreeDepths.push_back(Request);
			continue;
		}

		Msg("! Spot shadow cache: cannot make a %ux%u depth", Request.Size, Request.Size);
	}

	SpotShadowDepthRequests.clear();
}

void CRenderTarget::ReleaseSpotShadowDepth(SpotShadowCacheEntry& Entry)
{
	if (Entry.Depth || Entry.CopyShader)
	{
		RImplementation.Imm().SetTextures(&SpotShadowEmptyTextures);
		GRHI->ShaderResourceCache->Apply();
	}
	SpotShadowCacheBytes -= Entry.Bytes;

	if (Entry.Depth && Entry.CopyShader && Entry.Bytes)
	{
		if (SpotShadowFreeDepths.size() >= SpotShadowCacheUtils::MaxFreeDepths)
		{
			SpotShadowFreeDepths.erase(SpotShadowFreeDepths.begin());
		}

		SpotShadowFreeDepths.push_back({ Entry.Depth, Entry.CopyShader, Entry.Key.Size, Entry.Key.Format });
	}

	Entry.CopyShader.destroy();
	Entry.Depth.destroy();
	Entry.Bytes = 0;
	Entry.State = SpotShadowCacheState::Pending;
}

void CRenderTarget::BeginSpotShadowCacheFrame()
{
	const bool NewFrame = SpotShadowStatisticsFrame != RFrame.dwFrame;
	if (NewFrame)
	{
		SpotShadowLastStatistics = SpotShadowStatisticsFrame + 1 == RFrame.dwFrame ? SpotShadowStatistics : SpotShadowCacheStatistics();
		SpotShadowStatisticsFrame = RFrame.dwFrame;
		SpotShadowStatistics = {};
	}
	const u64 Revision = SpotShadowContentRevision.load(std::memory_order_relaxed);
	const bool IsTranslucent = ps_r2_ls_flags_ext.test(R4FLAG_TSHDOWS);
	if (Revision != SpotShadowAppliedRevision || IsTranslucent != SpotShadowCacheUtils::IsTranslucentCached)
	{
		SpotShadowStatistics.Invalidations += u32(Revision - SpotShadowAppliedRevision);
		ClearSpotShadowCache();
		SpotShadowAppliedRevision = Revision;
		SpotShadowCacheUtils::IsTranslucentCached = IsTranslucent;
	}
	if (!NewFrame)
	{
		return;
	}
	TrimSpotShadowCache();
	for (size_t Index = 0; Index < SpotShadowCache.size();)
	{
		const SpotShadowCacheEntry& Entry = SpotShadowCache[Index];
		if (RFrame.dwFrame - Entry.LastUsedFrame <= SpotShadowCacheUtils::Lifetime)
		{
			++Index;
			continue;
		}
		RImplementation.Imm().SetTextures(&SpotShadowEmptyTextures);
		GRHI->ShaderResourceCache->Apply();
		SpotShadowCacheBytes -= Entry.Bytes;
		SpotShadowCache.erase(SpotShadowCache.begin() + Index);
		++SpotShadowStatistics.Evictions;
	}
}

// Twice the atlas: the most one frame can draw fits twice over, at any shadow map size
u64 CRenderTarget::SpotShadowBudget() const
{
	const u64 Side = RImplementation.o.smapsize;
	return 2 * SmapPages * Side * Side * SpotShadowCacheUtils::DepthTexelBytes(static_cast<ERHI_FORMAT>(RImplementation.o.HW_smap_FORMAT));
}

void CRenderTarget::TrimSpotShadowCache(u64 RequiredBytes, SpotShadowCacheEntry* Keep)
{
	const u64 Budget = SpotShadowBudget();
	while (SpotShadowCacheBytes + RequiredBytes > Budget)
	{
		SpotShadowCacheEntry* Oldest = nullptr;
		for (SpotShadowCacheEntry& Entry : SpotShadowCache)
		{
			if (&Entry != Keep && Entry.Bytes && Entry.LastUsedFrame != RFrame.dwFrame && (!Oldest || Entry.LastUsedFrame < Oldest->LastUsedFrame))
			{
				Oldest = &Entry;
			}
		}
		if (!Oldest)
		{
			break;
		}
		ReleaseSpotShadowDepth(*Oldest);
		++SpotShadowStatistics.Evictions;
	}
}

CRenderTarget::SpotShadowCacheEntry* CRenderTarget::FindSpotShadowCache(const light& Source, const SpotShadowCacheKey& Key)
{
	for (SpotShadowCacheEntry& Entry : SpotShadowCache)
	{
		if (Entry.LightId == Source.SpotShadowCacheId)
		{
			// A depth larger than the tile stays: the restore samples it down
			SpotShadowCacheKey Wanted = Key;
			Wanted.Size = std::max(Key.Size, Entry.Key.Size);

			if (!(Entry.Key == Wanted))
			{
				ReleaseSpotShadowDepth(Entry);
				if (!Entry.Key.HasSameGeometry(Key))
				{
					// The frame it was last seen tells a lamp that moved from one seen anew
					const u32 Seen = Entry.LastSeenFrame;
					Entry = {};
					Entry.LastSeenFrame = Seen;
					++SpotShadowStatistics.GeometryInvalidations;
				}
				else
				{
					// Raster settings invalidate depth, not the light-space caster snapshot.
					++SpotShadowStatistics.DepthInvalidations;
				}
				Entry.Key = Key;
				Entry.LightId = Source.SpotShadowCacheId;
			}
			return &Entry;
		}
	}
	if (SpotShadowCache.size() >= SpotShadowCacheUtils::MaxEntries)
	{
		const auto OlderLambda = [](const auto& Left, const auto& Right)
		{
			return Left.LastUsedFrame < Right.LastUsedFrame;
		};
		auto Oldest = std::min_element(SpotShadowCache.begin(), SpotShadowCache.end(), OlderLambda);
		if (Oldest->LastUsedFrame == RFrame.dwFrame)
		{
			return nullptr;
		}
		ReleaseSpotShadowDepth(*Oldest);
		++SpotShadowStatistics.Evictions;
		// Taken over in place: the frame's lamp parts hold their entries by address
		*Oldest = {};
		Oldest->Key = Key;
		Oldest->LightId = Source.SpotShadowCacheId;
		return &*Oldest;
	}
	SpotShadowCache.reserve(SpotShadowCacheUtils::MaxEntries);
	SpotShadowCache.emplace_back();
	SpotShadowCache.back().Key = Key;
	SpotShadowCache.back().LightId = Source.SpotShadowCacheId;
	return &SpotShadowCache.back();
}

void CRenderTarget::ClassifySpotShadowCasters(light& Source, SpotShadowCacheEntry& Entry)
{
	PROF_EVENT("SpotShadowCache::Classify");
	R_dsgraph_structure& Renderer = RImplementation.GraphLampCollect;
	R_dsgraph_structure::SpotShadowBuildContext Context;
	auto* PreviousContext = Renderer.SpotShadowBuild;
	auto* PreviousFrustum = Renderer.View;
	auto RestoreContextLambda = [&]
	{
		Renderer.SpotShadowBuild = PreviousContext;
		Renderer.View = PreviousFrustum;
	};
	xr_scope_exit RestoreContext(std::move(RestoreContextLambda));
	Renderer.SpotShadowBuild = &Context;

	CFrustum Frustum;
	Frustum.CreateFromMatrix(Source.X.S.combine, FRUSTUM_P_ALL & ~FRUSTUM_P_NEAR);
	if (Entry.Classified)
	{
		// Texture changes only require reclassifying the saved leaves and LOD parents.
		Renderer.private_visuals.clear();
		Renderer.View = &Frustum;
		for (dxRender_Visual* Visual : Entry.CachedCasters)
		{
			Renderer.CollectSpotShadowStatic(Visual, Frustum.getMask());
		}
		for (dxRender_Visual* Visual : Entry.PerFrameCasters)
		{
			Renderer.CollectSpotShadowStatic(Visual, Frustum.getMask());
		}
		++SpotShadowStatistics.CasterReclassifications;
	}
	else
	{
		// The batch casts the whole level when it leaves the lamp's view nothing: no static to sort into the entry
		if (!RImplementation.Scene.IsShadowBinned(Renderer))
		{
			Renderer.private_visuals.clear();
			Renderer.Traverse();
			Renderer.CaptureStatic();
		}

		++SpotShadowStatistics.Classifications;
	}
	Entry.CachedCasters = std::move(Context.CachedCasters);
	Entry.PerFrameCasters = std::move(Context.PerFrameCasters);
	Entry.PendingTextures.clear();
	xr_hash_set<CTexture*> PendingTextures;
	for (const ref_texture& Texture : Context.PendingTextures)
	{
		if (PendingTextures.insert(Texture._get()).second)
		{
			Entry.PendingTextures.push_back(Texture);
		}
	}
	Entry.PerFrameReason = std::move(Context.PerFrameReason);
	Entry.CachedCasterSet.clear();
	Entry.CachedCasterSet.insert(Entry.CachedCasters.begin(), Entry.CachedCasters.end());
	Entry.Textures.clear();
	xr_hash_set<CTexture*> Textures;
	for (const auto& Texture : Context.Textures)
	{
		if (Textures.insert(Texture.first._get()).second)
		{
			Entry.Textures.push_back(Texture);
		}
	}
#ifdef DEBUG
	VERIFY(Entry.CachedCasterSet.size() == Entry.CachedCasters.size());
	for (dxRender_Visual* Visual : Entry.PerFrameCasters)
	{
		VERIFY(!Entry.CachedCasterSet.contains(Visual));
	}
#endif
	Entry.Classified = true;
	Entry.NextTextureCheckFrame = RFrame.dwFrame + SpotShadowCacheUtils::TextureRetryFrames;
	Entry.State = SpotShadowCacheState::Pending;
}

bool CRenderTarget::BuildSpotShadowCache(light& Source, SpotShadowCacheEntry& Entry)
{
	PROF_EVENT("SpotShadowCache::Build");
	R_dsgraph_structure& Renderer = RImplementation.GraphPass;
	R_dsgraph_structure::SpotShadowBuildContext Context;
	auto* PreviousContext = Renderer.SpotShadowBuild;
	auto RestoreContextLambda = [&]
	{
		Renderer.SpotShadowBuild = PreviousContext;
	};
	xr_scope_exit RestoreContext(std::move(RestoreContextLambda));
	Context.IsDepthCacheBuild = true;
	Renderer.SpotShadowBuild = &Context;

	Entry.State = SpotShadowCacheState::Pending;
	// What moves may have been still for long: only what is in the depth starts over
	Entry.BakedCasters = 0;

	if (SpotShadowCacheUtils::HasStaticCasters(Entry.CachedCasters))
	{
		// The smallest depth let go that holds the tile: an entry's depth may be larger than its tile
		auto Free = SpotShadowFreeDepths.end();

		for (auto Item = SpotShadowFreeDepths.begin(); !Entry.Depth && Item != SpotShadowFreeDepths.end(); ++Item)
		{
			if (Item->Format == Entry.Key.Format && Item->Size >= Entry.Key.Size && (Free == SpotShadowFreeDepths.end() || Item->Size < Free->Size))
			{
				Free = Item;
			}
		}

		if (Free != SpotShadowFreeDepths.end())
		{
			Entry.Key.Size = Free->Size;
		}

		const u64 Bytes = u64(Entry.Key.Size) * Entry.Key.Size * SpotShadowCacheUtils::DepthTexelBytes(Entry.Key.Format);

		if (Free != SpotShadowFreeDepths.end())
		{
			// Taken out first: the trim lets depths go into the same list
			Entry.Depth = Free->Depth;
			Entry.CopyShader = Free->CopyShader;
			SpotShadowFreeDepths.erase(Free);
			TrimSpotShadowCache(Bytes, &Entry);
			Entry.Bytes = Bytes;
			SpotShadowCacheBytes += Bytes;
		}

		if (!Entry.Depth)
		{
			TrimSpotShadowCache(Bytes, &Entry);
			if (SpotShadowCacheBytes + Bytes > SpotShadowBudget())
			{
				++SpotShadowStatistics.BudgetLimited;
				return false;
			}
			// Making a depth stops the frame for a millisecond or two: it is made after the present, and the entry
			// is built from it at its next sight. Until then the lamp draws its static casters directly
			SpotShadowDepthRequests.push_back({ {}, {}, Entry.Key.Size, Entry.Key.Format });
			++SpotShadowStatistics.DepthWaits;
			return false;
		}
		if (!Entry.CopyShader || !Entry.CopyShader->E[0] || Entry.CopyShader->E[0]->passes.empty())
		{
			ReleaseSpotShadowDepth(Entry);
			++SpotShadowStatistics.ResourceFailures;
			return false;
		}
		GPU_EVENT(SpotShadowCacheBuild);
		GfxCmdList& Cmd = RImplementation.Imm();
		Cmd.SetTextures(&SpotShadowEmptyTextures);
		GRHI->ShaderResourceCache->Apply();
		RHIViewport Viewport = {0.f, 0.f, float(Entry.Key.Size), float(Entry.Key.Size), 0.f, 1.f};
		SetupSpotShadowTarget(Cmd, Entry.Depth->pZRT, Entry.Key.Size, Entry.Key.Size, Viewport);
		Cmd.ClearDepthStencil(Entry.Depth->pZRT, ERHI_CLEAR_TARGET::DEPTH, 1.f, 0);

		if (!Entry.CachedCasters.empty())
		{
			Context.CollectOnly = false;
			Renderer.private_visuals.clear();

			for (dxRender_Visual* Visual : Entry.CachedCasters)
			{
				Renderer.r_dsgraph_insert_static(Visual);
			}

			Cmd.SetObjectTransform(Fidentity);
			Cmd.SetView(Source.X.S.view, Source.X.S.project);
			RImplementation.Scene.CountGraph(ShadowKind::SpotCache, Renderer);
			Renderer.r_dsgraph_render_graph(0);
		}

		const bool HasLevel = SpotShadowCacheUtils::DrawLevelBatch(Cmd, Renderer, Source, Entry.Depth->pZRT, nullptr);
		// Detach the cache DSV before the atlas restore reads its SRV.
		phase_smap_spot(Cmd, &Source);
		Cmd.ApplyRenderTargets();
		if (!HasLevel)
		{
			return false;
		}
	}
	else if (Entry.Depth)
	{
		ReleaseSpotShadowDepth(Entry);
	}
	Entry.State = SpotShadowCacheState::Ready;
	++SpotShadowStatistics.Builds;
	SpotShadowStatistics.StaticCasters += u32(Entry.CachedCasters.size());
	return true;
}

// One pass a page, a scissored triangle each part, each reading its entry's depth
void CRenderTarget::RestoreSpotShadows(GfxCmdList& Cmd, const SpotShadowPart* Parts, u32 Count)
{
	constexpr u32 Most = 64;
	const SpotShadowPart* Restored[Most];
	IRHISurface* Reads[Most];

	for (u32 Index = 0; Index < Count;)
	{
		u32 Taken = 0;

		for (; Index < Count && Taken < Most; ++Index)
		{
			const SpotShadowPart& Part = Parts[Index];

			if (Part.Entry && Part.UseCache && Part.HasStatic && Part.Entry->Depth)
			{
				Restored[Taken] = &Part;
				Reads[Taken++] = Part.Entry->Depth->pSurface;
			}
		}

		if (!Taken)
		{
			continue;
		}

		const float Size = float(RImplementation.o.smapsize);
		Cmd.SetTextures(&SpotShadowEmptyTextures);
		GRHI->ShaderResourceCache->Apply();
		SetupSpotShadowTarget(Cmd, SmapPage(Restored[0]->Light->vis.smap_ID), RImplementation.o.smapsize, RImplementation.o.smapsize, { 0.f, 0.f, Size, Size, 0.f, 1.f });
		Cmd.ApplyRenderTargets();
		Cmd.SetPass(Restored[0]->Entry->CopyShader->E[0]);
		Cmd.SetDepthEnable(TRUE);
		Cmd.SetDepthFunc(D3DCMP_ALWAYS);

		if (RImplementation.NriPasses.BeginTiles(Cmd, Reads, Taken))
		{
			for (u32 Tile = 0; Tile < Taken; ++Tile)
			{
				const light& Source = *Restored[Tile]->Light;
				Irect Clip;
				Clip.set(Source.X.S.posX, Source.X.S.posY, Source.X.S.posX + Source.X.S.size, Source.X.S.posY + Source.X.S.size);
				Cmd.Constants.Write<CBPost>().shadow_cache_destination.set(float(Source.X.S.posX), float(Source.X.S.posY), float(Restored[Tile]->Entry->Key.Size) / float(Source.X.S.size), 0.f);
				Cmd.SetScissor(&Clip);
				RImplementation.NriPasses.AddTile(Cmd, Reads[Tile]);
				++SpotShadowStatistics.Restores;
				SpotShadowStatistics.RestoredPixels += u64(Source.X.S.size) * Source.X.S.size;
			}

			RImplementation.NriPasses.EndTiles(Cmd);
		}

		Cmd.SetTextures(&SpotShadowEmptyTextures);
		GRHI->ShaderResourceCache->Apply();
		Cmd.SetScissor(nullptr);
	}
}

bool CRenderTarget::HasSpotShadowTextureChanges(const SpotShadowCacheEntry& Entry) const
{
	for (const auto& Texture : Entry.Textures)
	{
		if (Texture.first->GetShadowCacheRevision() != Texture.second)
		{
			return true;
		}
	}
	return false;
}

bool CRenderTarget::HasSpotShadowPendingTextureChanges(SpotShadowCacheEntry& Entry)
{
	if (Entry.PendingTextures.empty() || RFrame.dwFrame < Entry.NextTextureCheckFrame)
	{
		return false;
	}
	Entry.NextTextureCheckFrame = RFrame.dwFrame + SpotShadowCacheUtils::TextureRetryFrames;
	++SpotShadowStatistics.PendingTextureChecks;
	for (const ref_texture& Texture : Entry.PendingTextures)
	{
		bool Pending = false;
		u64 Revision = 0;
		Texture->CanCacheShadow(Pending, Revision);
		if (!Pending)
		{
			return true;
		}
	}
	return false;
}

bool CRenderTarget::CheckSpotShadowGraph()
{
	if (!SpotShadowCacheUtils::HasGeometry(RImplementation.GraphPass, 0) && !SpotShadowCacheUtils::HasGeometry(RImplementation.GraphPass, 1) && !RImplementation.GraphPass.mapSorted.size())
	{
		return true;
	}
	if (++SpotShadowStatistics.DirtyGraphs == 1)
	{
		Msg("! Spot shadow graph is not empty; skipping shadow lights to preserve queued geometry (frame %u)", RFrame.dwFrame);
	}
	VERIFY2(false, "Spot shadow graph must be empty at the light boundary");
	return false;
}

void CRenderTarget::PrepareSpotShadow(SpotShadowPart& Part)
{
	PROF_EVENT("SpotShadowCache");
	light& Source = *Part.Light;
	++SpotShadowStatistics.Lights;
	Part.IsDecor = SpotShadowCacheUtils::HasDecorativeCasters(Source);
	Part.HasCasters = true;

	if (!SpotShadowCacheUtils::CanStoreDepth(Source) || Part.IsDecor)
	{
		++SpotShadowStatistics.Uncached;
		Part.IsDirect = true;
		return;
	}

	if (!Source.X.S.size || !SpotShadowCacheUtils::ValidMatrix(Source.X.S.view) || !SpotShadowCacheUtils::ValidMatrix(Source.X.S.project))
	{
		Part.IsEmpty = true;
		Part.HasCasters = false;
		return;
	}

	const SpotShadowCacheKey Key = MakeSpotShadowCacheKey(Source);
	SpotShadowCacheEntry* Entry = FindSpotShadowCache(Source, Key);

	if (!Entry)
	{
		++SpotShadowStatistics.CapacityLimited;
		Part.IsDirect = true;
		return;
	}

	const bool IsMoving = !Entry->StableObservations && Entry->LastSeenFrame + 1 == RFrame.dwFrame;

	if (!Entry->StableObservations || Entry->LastSeenFrame != RFrame.dwFrame)
	{
		Entry->StableObservations = Entry->StableObservations && Entry->LastSeenFrame + 1 == RFrame.dwFrame ? 2 : 1;
		Entry->LastSeenFrame = RFrame.dwFrame;
	}

	Entry->LastUsedFrame = RFrame.dwFrame;

	// An entry bakes once its lamp stays put for two frames: one that moves every frame (a torch, a swinging lamp) never does
	if (Entry->State != SpotShadowCacheState::Ready && Entry->StableObservations < 2)
	{
		++(IsMoving ? SpotShadowStatistics.Moving : SpotShadowStatistics.FirstSight);
		Part.IsDirect = true;
		return;
	}

	R_dsgraph_structure& Renderer = RImplementation.GraphLampCollect;
	const bool PreviousPass = Renderer.SpotShadowPass;
	const bool PreviousSkip = Renderer.SkipDepthBatched;
	auto* PreviousCachedCasters = Renderer.SpotShadowCachedCasters;
	auto RestoreStateLambda = [&]
	{
		Renderer.SpotShadowPass = PreviousPass;
		Renderer.SkipDepthBatched = PreviousSkip;
		Renderer.SpotShadowCachedCasters = PreviousCachedCasters;
	};
	xr_scope_exit RestoreState(std::move(RestoreStateLambda));
	Renderer.SpotShadowPass = true;
	Renderer.SkipDepthBatched = RImplementation.Scene.HasDepthBatch();
	Renderer.SpotShadowCachedCasters = nullptr;
	Renderer.Begin(RImplementation.MakeLampView(Source));
	const bool TextureChanged = Entry->Classified && HasSpotShadowTextureChanges(*Entry);
	if (TextureChanged)
	{
		++SpotShadowStatistics.TextureInvalidations;
	}
	const bool Reclassify = !Entry->Classified || TextureChanged || HasSpotShadowPendingTextureChanges(*Entry);
	if (Reclassify)
	{
		ClassifySpotShadowCasters(Source, *Entry);
	}
	const bool NeedsDepth = Entry->State == SpotShadowCacheState::Ready && SpotShadowCacheUtils::HasStaticCasters(Entry->CachedCasters);
	const bool HasDepth = Entry->Depth && Entry->Depth->pSurface && Entry->Depth->pZRT && Entry->Depth->pTexture && Entry->Depth->pTexture->get_SRView();
	const bool HasCopyShader = Entry->CopyShader && Entry->CopyShader->E[0] && !Entry->CopyShader->E[0]->passes.empty();
	if ((NeedsDepth || Entry->Depth) && (!HasDepth || !HasCopyShader))
	{
		ReleaseSpotShadowDepth(*Entry);
		++SpotShadowStatistics.ResourceFailures;
	}
	const bool CacheHit = Entry->State == SpotShadowCacheState::Ready;
	bool UseCache = CacheHit;
	// The depth is drawn with the frame; until then the part collects as one drawn whole
	Part.IsBuilt = !CacheHit && Entry->StableObservations >= 2;
	if (!Entry->PendingTextures.empty())
	{
		++SpotShadowStatistics.TexturePending;
	}
	if (Key.Revision != SpotShadowContentRevision.load(std::memory_order_relaxed))
	{
		Entry->State = SpotShadowCacheState::Pending;
		Entry->Classified = false;
		UseCache = false;
	}
	else if (HasSpotShadowTextureChanges(*Entry))
	{
		// Keep the geometry snapshot for a texture-only retry on the next observation.
		Entry->State = SpotShadowCacheState::Pending;
		UseCache = false;
	}

	Part.Entry = Entry;
	Part.UseCache = UseCache;
	Part.HasStatic = SpotShadowCacheUtils::HasStaticCasters(Entry->CachedCasters);
	SpotShadowStatistics.PerFrameCasters += u32(Entry->PerFrameCasters.size());
	SpotShadowStatistics.Hits += CacheHit && UseCache;
	SpotShadowStatistics.Direct += !UseCache;

	// A cached lamp far from the camera keeps its cached depth as it is
	const float Range = Source.SpatialComponent->sphere.P.distance_to(RFrame.vCameraPosition);
	Part.HasCasters = !(UseCache && Part.HasStatic && Range > SpotShadowCacheUtils::DynamicRange);
}

void CRenderTarget::BuildSpotShadow(SpotShadowPart& Part)
{
	if (!Part.IsBuilt)
	{
		return;
	}

	R_dsgraph_structure& Renderer = RImplementation.GraphPass;
	GfxCmdList& Cmd = RImplementation.Imm();
	const Fmatrix World = Cmd.GetObjectTransform();
	const GfxView View = Cmd.GetView();
	const bool PreviousPass = Renderer.SpotShadowPass;
	const bool PreviousSkip = Renderer.SkipDepthBatched;
	auto* PreviousCachedCasters = Renderer.SpotShadowCachedCasters;
	auto RestoreStateLambda = [&]
	{
		Renderer.SpotShadowPass = PreviousPass;
		Renderer.SkipDepthBatched = PreviousSkip;
		Renderer.SpotShadowCachedCasters = PreviousCachedCasters;
		Cmd.SetScissor(nullptr);
		Cmd.SetObjectTransform(World);
		Cmd.SetView(View);
	};
	xr_scope_exit RestoreState(std::move(RestoreStateLambda));
	Renderer.SpotShadowPass = true;
	Renderer.SkipDepthBatched = RImplementation.Scene.HasDepthBatch();
	Renderer.SpotShadowCachedCasters = nullptr;
	Renderer.Begin(RImplementation.MakeLampView(*Part.Light));
	Part.UseCache = BuildSpotShadowCache(*Part.Light, *Part.Entry);
}

void CRenderTarget::BakeSpotShadow(GfxCmdList& Cmd, SpotShadowPart& Part, const GpuTileView& Tile)
{
	SpotShadowCacheEntry& Entry = *Part.Entry;
	u64 Hash = 0;
	const u32 Count = Part.HasCasters ? RImplementation.Scene.TestTile(Tile, Hash) : 0;
	const bool IsChecked = Part.HasCasters && Part.UseCache && Part.HasStatic && Entry.Depth;
	Part.HasCasters = Count != 0;

	if (!IsChecked)
	{
		return;
	}

	if (Entry.BakedCasters && Count && Hash == Entry.BakedCasters)
	{
		Part.HasCasters = false;
		return;
	}

	// What is baked changed: what moves is drawn over it this once, then the cache is built again
	if (Entry.BakedCasters)
	{
		Entry.State = SpotShadowCacheState::Pending;
		Entry.BakedCasters = 0;
		return;
	}

	Entry.SeenFrames = Hash && Hash == Entry.SeenCasters ? Entry.SeenFrames + 1 : 0;
	Entry.SeenCasters = Hash;

	if (!Count || !Hash || Entry.State != SpotShadowCacheState::Ready || Entry.SeenFrames < SpotShadowCacheUtils::BakeFrames)
	{
		return;
	}

	PROF_EVENT("SpotShadowCache::Bake");
	GPU_EVENT(SpotShadowCacheBake);
	const RHIViewport Whole = { 0.f, 0.f, float(Entry.Key.Size), float(Entry.Key.Size), 0.f, 1.f };

	if (RImplementation.Scene.QueueTile(Cmd, Entry.Depth->pZRT, 0, Whole, Tile))
	{
		RImplementation.Scene.DrawTiles(Cmd);
		// Detach the cache DSV before the atlas restore reads its SRV.
		phase_smap_spot(Cmd, Part.Light);
		Cmd.ApplyRenderTargets();
		Entry.BakedCasters = Hash;
		Part.HasCasters = false;
	}
}

bool CRenderTarget::DrawSpotShadow(GfxCmdList& Cmd, SpotShadowPart& Part, u32 Index)
{
	light& Source = *Part.Light;
	SpotShadowCacheEntry& Entry = *Part.Entry;
	R_dsgraph_structure& Renderer = RImplementation.GraphPass;
	const Fmatrix World = Cmd.GetObjectTransform();
	const GfxView View = Cmd.GetView();
	const bool PreviousPass = Renderer.SpotShadowPass;
	const bool PreviousSkip = Renderer.SkipDepthBatched;
	auto* PreviousContext = Renderer.SpotShadowBuild;
	auto* PreviousCachedCasters = Renderer.SpotShadowCachedCasters;
	const u32 PreviousCachedSkips = Renderer.SpotShadowCachedSkips;
	const u32 PreviousDuplicateSkips = Renderer.SpotShadowDuplicateSkips;
	const u32 PreviousStaticItems = Renderer.SpotShadowStaticItems;
	const u32 PreviousDynamicItems = Renderer.SpotShadowDynamicItems;
	auto* PreviousFrustum = Renderer.View;
	CFrustum ShadowFrustum;
	ShadowFrustum.CreateFromMatrix(Source.X.S.combine, FRUSTUM_P_ALL & ~FRUSTUM_P_NEAR);
	auto RestoreStateLambda = [&]
	{
		Renderer.SpotShadowPass = PreviousPass;
		Renderer.SkipDepthBatched = PreviousSkip;
		Renderer.SpotShadowBuild = PreviousContext;
		Renderer.SpotShadowCachedCasters = PreviousCachedCasters;
		SpotShadowStatistics.CachedSkips += Renderer.SpotShadowCachedSkips;
		SpotShadowStatistics.DuplicateSkips += Renderer.SpotShadowDuplicateSkips;
		SpotShadowStatistics.FrameStaticItems += Renderer.SpotShadowStaticItems;
		SpotShadowStatistics.FrameDynamicItems += Renderer.SpotShadowDynamicItems;
		Renderer.SpotShadowCachedSkips = PreviousCachedSkips;
		Renderer.SpotShadowDuplicateSkips = PreviousDuplicateSkips;
		Renderer.SpotShadowStaticItems = PreviousStaticItems;
		Renderer.SpotShadowDynamicItems = PreviousDynamicItems;
		Renderer.View = PreviousFrustum;
		Cmd.SetScissor(nullptr);
		Cmd.SetObjectTransform(World);
		Cmd.SetView(View);
	};
	xr_scope_exit RestoreState(std::move(RestoreStateLambda));
	Renderer.SpotShadowPass = true;
	Renderer.SkipDepthBatched = RImplementation.Scene.HasDepthBatch();
	Renderer.SpotShadowBuild = nullptr;
	Renderer.SpotShadowCachedCasters = nullptr;
	Renderer.SpotShadowCachedSkips = 0;
	Renderer.SpotShadowDuplicateSkips = 0;
	Renderer.SpotShadowStaticItems = 0;
	Renderer.SpotShadowDynamicItems = 0;
	Renderer.Begin(RImplementation.MakeLampView(Source));

	if (Part.HasStatic && !Part.UseCache)
	{
		PROF_EVENT("SpotShadowCache::DirectStatic");
		GPU_EVENT(SpotShadowCacheDirectStatic);
		phase_smap_spot(Cmd, &Source);

		// Direct and cached depth use identical candidates and fixed detail from the first observation.
		if (!Entry.CachedCasters.empty())
		{
			R_dsgraph_structure::SpotShadowBuildContext DirectContext;
			DirectContext.CollectOnly = false;
			Renderer.SpotShadowBuild = &DirectContext;
			Renderer.private_visuals.clear();

			for (dxRender_Visual* Visual : Entry.CachedCasters)
			{
				Renderer.r_dsgraph_insert_static(Visual);
			}

			Cmd.SetObjectTransform(Fidentity);
			Cmd.SetView(Source.X.S.view, Source.X.S.project);
			RImplementation.Scene.CountGraph(ShadowKind::SpotCache, Renderer);
			Renderer.r_dsgraph_render_graph(0);
			Renderer.SpotShadowBuild = nullptr;
		}

		const RHIViewport Area = { float(Source.X.S.posX), float(Source.X.S.posY), float(Source.X.S.size), float(Source.X.S.size), 0.0f, 1.0f };
		SpotShadowCacheUtils::DrawLevelBatch(Cmd, Renderer, Source, SmapPage(Source.vis.smap_ID), &Area, Source.vis.smap_ID % SmapPages, RImplementation.LampTiles[Index].Sectors);
	}

	Renderer.SpotShadowCachedCasters = &Entry.CachedCasterSet;
	Renderer.private_visuals.clear();

	if (!Entry.PerFrameCasters.empty())
	{
		PROF_EVENT("SpotShadowCache::PerFrameStatic");
		// Share the dynamic collection marker; keep LOD groups intact for normal per-frame selection.
		Renderer.View = &ShadowFrustum;
		for (dxRender_Visual* Visual : Entry.PerFrameCasters)
		{
			Renderer.add_Static(Visual, ShadowFrustum.getMask());
		}
	}

	RImplementation.Scene.InsertLampLeftovers(Renderer, Index);
	const bool HasNormal = Renderer.HasPasses(0);
	const bool HasSpecial = Renderer.HasPasses(1) || Renderer.mapSorted.size();

	if (!Part.HasStatic && !HasNormal && !HasSpecial && !Part.HasCasters)
	{
		return false;
	}

	SpotShadowStatistics.DynamicPasses += HasNormal;
	SpotShadowStatistics.SpecialPasses += HasSpecial;
	phase_smap_spot(Cmd, &Source);
	RImplementation.DrawLampGraph(Cmd, Source, !Part.IsUnseen && Source.SpatialComponent->sphere.P.distance_to(RFrame.vCameraPosition) < SpotShadowCacheUtils::GrassRange, ShadowKind::SpotCache);
	RImplementation.QueueLampTile(Cmd, Part, Index);
	return CheckSpotShadowGraph();
}

void CRenderTarget::DescribeSpotShadowCache(string512& Line) const
{
	const SpotShadowCacheStatistics Empty = {};
	const SpotShadowCacheStatistics& Frame = SpotShadowStatisticsFrame == RFrame.dwFrame ? SpotShadowLastStatistics : SpotShadowStatisticsFrame + 1 == RFrame.dwFrame ? SpotShadowStatistics : Empty;
	const u32 Other = Frame.Direct - std::min(Frame.Direct, Frame.BudgetLimited + Frame.DepthWaits);

	xr_sprintf(Line, "; lamp parts %u: hit %u, baked %u, drawn directly %u (first sight %u, moved since the frame before %u, no entry left %u, over the budget %u, waiting for a depth %u, changed or failed %u), not taken %u (decor objects, no sector); entries %u, %.1f of %.0f MB",
		Frame.Lights, Frame.Hits, Frame.Builds, Frame.FirstSight + Frame.Moving + Frame.CapacityLimited + Frame.Direct, Frame.FirstSight, Frame.Moving, Frame.CapacityLimited, Frame.BudgetLimited,
		Frame.DepthWaits, Other, Frame.Uncached, u32(SpotShadowCache.size()), double(SpotShadowCacheBytes) / (1024.0 * 1024.0), double(SpotShadowBudget()) / (1024.0 * 1024.0));
}

void CRenderTarget::DumpSpotShadowCache() const
{
	Msg("* spot shadow cache: entries=%u memory=%.2f/%.0f MiB", u32(SpotShadowCache.size()), double(SpotShadowCacheBytes) / (1024.0 * 1024.0), double(SpotShadowBudget()) / (1024.0 * 1024.0));
	Msg("* frame %u: lights=%u hits=%u builds=%u restores=%u first_sight=%u moving=%u direct=%u uncached=%u unseen=%u evictions=%u static_casters=%u per_frame_casters=%u dynamic_passes=%u special_passes=%u",
		SpotShadowStatisticsFrame,
		SpotShadowStatistics.Lights,
		SpotShadowStatistics.Hits,
		SpotShadowStatistics.Builds,
		SpotShadowStatistics.Restores,
		SpotShadowStatistics.FirstSight,
		SpotShadowStatistics.Moving,
		SpotShadowStatistics.Direct,
		SpotShadowStatistics.Uncached,
		SpotShadowStatistics.Unseen,
		SpotShadowStatistics.Evictions,
		SpotShadowStatistics.StaticCasters,
		SpotShadowStatistics.PerFrameCasters,
		SpotShadowStatistics.DynamicPasses,
		SpotShadowStatistics.SpecialPasses);
	Msg("* depth cache limits: texture_pending=%u budget=%u depth_waits=%u capacity=%u resource_failures=%u",
		SpotShadowStatistics.TexturePending,
		SpotShadowStatistics.BudgetLimited,
		SpotShadowStatistics.DepthWaits,
		SpotShadowStatistics.CapacityLimited,
		SpotShadowStatistics.ResourceFailures);
	Msg("* cache audit: classifications=%u invalidations=%u cached_skips=%u duplicate_skips=%u dirty_graphs=%u",
		SpotShadowStatistics.Classifications,
		SpotShadowStatistics.Invalidations,
		SpotShadowStatistics.CachedSkips,
		SpotShadowStatistics.DuplicateSkips,
		SpotShadowStatistics.DirtyGraphs);
	Msg("* cache frame graph: static_items=%u dynamic_items=%u copy_mpixels=%.2f",
		SpotShadowStatistics.FrameStaticItems,
		SpotShadowStatistics.FrameDynamicItems,
		double(SpotShadowStatistics.RestoredPixels) / 1000000.0);
	Msg("* cache routing: texture_invalidations=%u", SpotShadowStatistics.TextureInvalidations);
	Msg("* cache geometry reuse: geometry_changes=%u depth_changes=%u caster_reclassifications=%u pending_texture_checks=%u",
		SpotShadowStatistics.GeometryInvalidations,
		SpotShadowStatistics.DepthInvalidations,
		SpotShadowStatistics.CasterReclassifications,
		SpotShadowStatistics.PendingTextureChecks);
	const u32 TexelBytes = SpotShadowCacheUtils::DepthTexelBytes(static_cast<ERHI_FORMAT>(RImplementation.o.HW_smap_FORMAT));
	const u32 TileLimit = SMAP_adapt_max;
	const u64 MaxTileBytes = u64(TileLimit) * TileLimit * TexelBytes;
	const u64 BudgetBytes = SpotShadowBudget();
	Msg("* cache depth cost: texel_bytes=%u atlas_size=%u tile_limit=%u tile_limit_mib=%.2f budget_tile_limit=%llu restore_read_write_mib=%.2f",
		TexelBytes,
		RImplementation.o.smapsize,
		TileLimit,
		double(MaxTileBytes) / (1024.0 * 1024.0),
		static_cast<unsigned long long>(MaxTileBytes ? BudgetBytes / MaxTileBytes : 0),
		double(SpotShadowStatistics.RestoredPixels) * TexelBytes * 2 / (1024.0 * 1024.0));
	u32 Ready = 0;
	u32 Pending = 0;
	u32 Mixed = 0;
	for (const SpotShadowCacheEntry& Entry : SpotShadowCache)
	{
		Ready += Entry.State == SpotShadowCacheState::Ready;
		Pending += Entry.State == SpotShadowCacheState::Pending;
		Mixed += !Entry.PerFrameCasters.empty();
	}
	Msg("* cache entries: ready=%u pending=%u with_per_frame_casters=%u", Ready, Pending, Mixed);
	constexpr u32 MaxDetails = 16;
	u32 Printed = 0;
	for (const SpotShadowCacheEntry& Entry : SpotShadowCache)
	{
		if (Entry.PerFrameReason.empty())
		{
			continue;
		}
		Msg("* cache per-frame [%u] size=%u casters=%u: %s", Printed, Entry.Key.Size, u32(Entry.PerFrameCasters.size()), Entry.PerFrameReason.c_str());
		if (++Printed == MaxDetails)
		{
			break;
		}
	}
	if (Mixed > Printed)
	{
		Msg("* cache per-frame: %u more entries omitted", Mixed - Printed);
	}
}
