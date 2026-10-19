#include "stdafx.h"
#include "light.h"
#include "FBasicVisual.h"
#include "../../xrEngine/xr_object.h"
#include "r4_fullscreen_pass.h"

namespace SpotShadowCacheUtils
{
constexpr u32 MaxEntries = 128;
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

static bool HasGeometry(CRender& Renderer, u32 Priority)
{
	for (u32 Pass = 0; Pass < SHADER_PASSES_MAX; ++Pass)
	{
		if (Renderer.mapNormalPasses[Priority][Pass].size() || Renderer.mapMatrixPasses[Priority][Pass].size())
		{
			return true;
		}
	}
	return false;
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
	return g_SpotShadowCacheMode > 0 && !Device.IsEditorMode() && LightingModeIsDynamicRender() && RImplementation.b_loaded &&
		   psDeviceFlags.test(rsDrawStatic) && !psDeviceFlags.test(rsClearBB) &&
		   (Source.flags.type == IRender_Light::SPOT || Source.flags.type == IRender_Light::OMNIPART) &&
		   !Source.flags.bHudMode && Source.SpatialComponent && Source.SpatialComponent->sector;
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
	if (!SpotShadowCache.empty() || SpotShadowLastMode > 0)
	{
		RCache.Invalidate();
	}
	SpotShadowCache.clear();
	SpotShadowCacheBytes = 0;
}

void CRenderTarget::ReleaseSpotShadowDepth(SpotShadowCacheEntry& Entry)
{
	if (Entry.Depth || Entry.CopyShader)
	{
		RCache.set_Textures(&SpotShadowEmptyTextures);
		GRHI->ShaderResourceCache->Apply();
	}
	SpotShadowCacheBytes -= Entry.Bytes;
	Entry.CopyShader.destroy();
	Entry.Depth.destroy();
	Entry.Bytes = 0;
	Entry.State = SpotShadowCacheState::Pending;
}

void CRenderTarget::BeginSpotShadowCacheFrame()
{
	const bool NewFrame = SpotShadowStatisticsFrame != Device.dwFrame;
	if (NewFrame)
	{
		SpotShadowStatisticsFrame = Device.dwFrame;
		SpotShadowStatistics = {};
	}
	const u64 Revision = SpotShadowContentRevision.load(std::memory_order_relaxed);
	if (Revision != SpotShadowAppliedRevision || SpotShadowLastMode != g_SpotShadowCacheMode)
	{
		SpotShadowStatistics.Invalidations += u32(Revision - SpotShadowAppliedRevision);
		ClearSpotShadowCache();
		SpotShadowAppliedRevision = Revision;
		SpotShadowLastMode = g_SpotShadowCacheMode;
	}
	if (!NewFrame)
	{
		return;
	}
	TrimSpotShadowCache();
	for (size_t Index = 0; Index < SpotShadowCache.size();)
	{
		const SpotShadowCacheEntry& Entry = SpotShadowCache[Index];
		if (Device.dwFrame - Entry.LastUsedFrame <= SpotShadowCacheUtils::Lifetime)
		{
			++Index;
			continue;
		}
		RCache.set_Textures(&SpotShadowEmptyTextures);
		GRHI->ShaderResourceCache->Apply();
		SpotShadowCacheBytes -= Entry.Bytes;
		SpotShadowCache.erase(SpotShadowCache.begin() + Index);
		++SpotShadowStatistics.Evictions;
	}
}

void CRenderTarget::TrimSpotShadowCache(u64 RequiredBytes, SpotShadowCacheEntry* Keep)
{
	const u64 Budget = u64(g_SpotShadowCacheBudgetMb) * 1024 * 1024;
	while (SpotShadowCacheBytes + RequiredBytes > Budget)
	{
		SpotShadowCacheEntry* Oldest = nullptr;
		for (SpotShadowCacheEntry& Entry : SpotShadowCache)
		{
			if (&Entry != Keep && Entry.Bytes && Entry.LastUsedFrame != Device.dwFrame && (!Oldest || Entry.LastUsedFrame < Oldest->LastUsedFrame))
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
			if (!(Entry.Key == Key))
			{
				ReleaseSpotShadowDepth(Entry);
				if (!Entry.Key.HasSameGeometry(Key))
				{
					Entry = {};
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
		if (Oldest->LastUsedFrame == Device.dwFrame)
		{
			return nullptr;
		}
		ReleaseSpotShadowDepth(*Oldest);
		SpotShadowCache.erase(Oldest);
		++SpotShadowStatistics.Evictions;
	}
	SpotShadowCache.emplace_back();
	SpotShadowCache.back().Key = Key;
	SpotShadowCache.back().LightId = Source.SpotShadowCacheId;
	return &SpotShadowCache.back();
}

void CRenderTarget::ClassifySpotShadowCasters(light& Source, SpotShadowCacheEntry& Entry)
{
	PROF_EVENT("SpotShadowCache::Classify");
	CRender& Renderer = RImplementation;
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
		++Renderer.marker;
		Renderer.set_Frustum(&Frustum);
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
		Renderer.r_dsgraph_render_subspace(Entry.Key.Sector, &Frustum, Source.X.S.combine, Source.position, R_dsgraph_structure::SubspaceGeometry::StaticOnly, true);
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
	Entry.NextTextureCheckFrame = Device.dwFrame + SpotShadowCacheUtils::TextureRetryFrames;
	Entry.State = SpotShadowCacheState::Pending;
}

bool CRenderTarget::BuildSpotShadowCache(light& Source, SpotShadowCacheEntry& Entry)
{
	PROF_EVENT("SpotShadowCache::Build");
	CRender& Renderer = RImplementation;
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

	if (!Entry.CachedCasters.empty())
	{
		const u64 Bytes = u64(Entry.Key.Size) * Entry.Key.Size * SpotShadowCacheUtils::DepthTexelBytes(Entry.Key.Format);
		if (!Entry.Depth)
		{
			TrimSpotShadowCache(Bytes, &Entry);
			if (SpotShadowCacheBytes + Bytes > u64(g_SpotShadowCacheBudgetMb) * 1024 * 1024)
			{
				++SpotShadowStatistics.BudgetLimited;
				return false;
			}
			string128 Name;
			xr_sprintf(Name, "$user$spot_shadow_cache_%llu", static_cast<unsigned long long>(++SpotShadowResourceSerial));
			Entry.Depth.create(Name, Entry.Key.Size, Entry.Key.Size, Entry.Key.Format);
			if (!Entry.Depth || !Entry.Depth->pSurface || !Entry.Depth->pZRT || !Entry.Depth->pTexture || !Entry.Depth->pTexture->get_SRView())
			{
				ReleaseSpotShadowDepth(Entry);
				++SpotShadowStatistics.ResourceFailures;
				return false;
			}
			Entry.Bytes = Bytes;
			SpotShadowCacheBytes += Bytes;
			CreateEffectSQ<SpotShadowCacheUtils::CopyBlender>(Entry.CopyShader, nullptr, Name);
		}
		if (!Entry.CopyShader || !Entry.CopyShader->E[0] || Entry.CopyShader->E[0]->passes.empty())
		{
			ReleaseSpotShadowDepth(Entry);
			++SpotShadowStatistics.ResourceFailures;
			return false;
		}
		GPU_EVENT(SpotShadowCacheBuild);
		Context.CollectOnly = false;
		++Renderer.marker;
		for (dxRender_Visual* Visual : Entry.CachedCasters)
		{
			Renderer.r_dsgraph_insert_static(Visual);
		}
		RCache.set_Textures(&SpotShadowEmptyTextures);
		GRHI->ShaderResourceCache->Apply();
		RHIViewport Viewport = {0.f, 0.f, float(Entry.Key.Size), float(Entry.Key.Size), 0.f, 1.f};
		SetupSpotShadowTarget(Entry.Depth->pZRT, Entry.Key.Size, Entry.Key.Size, Viewport);
		GRHI->ClearDepthStencil(Entry.Depth->pZRT, ERHI_CLEAR_TARGET::DEPTH, 1.f, 0);
		RCache.set_xform_world(Fidentity);
		RCache.set_xform_view(Source.X.S.view);
		RCache.set_xform_project(Source.X.S.project);
		Renderer.r_dsgraph_render_graph(0);
		// Detach the cache DSV before the atlas restore reads its SRV.
		phase_smap_spot(&Source);
		GRHI->ApplyRenderTargetChange();
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

void CRenderTarget::RestoreSpotShadowCache(const light& Source, SpotShadowCacheEntry& Entry)
{
	PROF_EVENT("SpotShadowCache::Restore");
	GPU_EVENT(SpotShadowCacheRestore);
	RCache.set_Textures(&SpotShadowEmptyTextures);
	GRHI->ShaderResourceCache->Apply();
	RHIViewport Viewport = {float(Source.X.S.posX), float(Source.X.S.posY), float(Source.X.S.size), float(Source.X.S.size), 0.f, 1.f};
	// Keep the atlas RTV/DSV pair bound across restore and geometry draws.
	// The copy shader only writes depth; detaching the RTV needlessly unbinds the entire OM state.
	SetupSpotShadowTarget(rt_smap_depth->pZRT, rt_smap_depth->dwWidth, rt_smap_depth->dwHeight, Viewport, rt_smap_surf->pRT);
	GRHI->ApplyRenderTargetChange();
	Irect Tile;
	Tile.set(Source.X.S.posX, Source.X.S.posY, Source.X.S.posX + Source.X.S.size, Source.X.S.posY + Source.X.S.size);
	const auto BindLambda = [&]
	{
		RCache.set_c("shadow_cache_destination", float(Source.X.S.posX), float(Source.X.S.posY), 0.f, 0.f);
		RCache.set_Scissor(&Tile);
		GRHI->StateManager->SetDepthEnable(TRUE);
		GRHI->StateManager->SetDepthFunc(D3DCMP_ALWAYS);
	};
	DrawPassSQ(Entry.CopyShader, 0, BindLambda);
	RCache.set_Textures(&SpotShadowEmptyTextures);
	GRHI->ShaderResourceCache->Apply();
	RCache.set_Scissor(nullptr);
	++SpotShadowStatistics.Restores;
	SpotShadowStatistics.RestoredPixels += u64(Source.X.S.size) * Source.X.S.size;
}

bool CRenderTarget::CanCacheSpotShadow(const light& Source) const
{
	return SpotShadowCacheUtils::CanStoreDepth(Source) && !SpotShadowCacheUtils::HasDecorativeCasters(Source);
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
	if (Entry.PendingTextures.empty() || Device.dwFrame < Entry.NextTextureCheckFrame)
	{
		return false;
	}
	Entry.NextTextureCheckFrame = Device.dwFrame + SpotShadowCacheUtils::TextureRetryFrames;
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
	if (!SpotShadowCacheUtils::HasGeometry(RImplementation, 0) && !SpotShadowCacheUtils::HasGeometry(RImplementation, 1) && !RImplementation.mapSorted.size())
	{
		return true;
	}
	if (++SpotShadowStatistics.DirtyGraphs == 1)
	{
		Msg("! Spot shadow graph is not empty; skipping shadow lights to preserve queued geometry (frame %u)", Device.dwFrame);
	}
	VERIFY2(false, "Spot shadow graph must be empty at the light boundary");
	return false;
}

bool CRenderTarget::RenderSpotShadow(light& Source)
{
	PROF_EVENT("SpotShadowCache");
	if (!CanCacheSpotShadow(Source) || !CheckSpotShadowGraph())
	{
		return false;
	}
	++SpotShadowStatistics.Lights;
	Source.X.S.transluent = false;
	if (!Source.X.S.size || !SpotShadowCacheUtils::ValidMatrix(Source.X.S.view) || !SpotShadowCacheUtils::ValidMatrix(Source.X.S.project))
	{
		return false;
	}

	CRender& Renderer = RImplementation;
	const Fmatrix World = RCache.get_xform_world();
	const Fmatrix View = RCache.get_xform_view();
	const Fmatrix Project = RCache.get_xform_project();
	const bool PreviousPass = Renderer.SpotShadowPass;
	auto* PreviousContext = Renderer.SpotShadowBuild;
	auto* PreviousCachedCasters = Renderer.SpotShadowCachedCasters;
	const u32 PreviousCachedSkips = Renderer.SpotShadowCachedSkips;
	const u32 PreviousDuplicateSkips = Renderer.SpotShadowDuplicateSkips;
	const u32 PreviousStaticItems = Renderer.SpotShadowStaticItems;
	const u32 PreviousDynamicItems = Renderer.SpotShadowDynamicItems;
	auto* PreviousFrustum = Renderer.View;
	CFrustum ShadowFrustum;
	ShadowFrustum.CreateFromMatrix(Source.X.S.combine, FRUSTUM_P_ALL & ~FRUSTUM_P_NEAR);
	R_feedback* Feedback = Renderer.val_feedback;
	const u32 FeedbackBreak = Renderer.val_feedback_breakp;
	auto RestoreStateLambda = [&]
	{
		Renderer.SpotShadowPass = PreviousPass;
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
		Renderer.set_Feedback(Feedback, FeedbackBreak);
		RCache.set_Scissor(nullptr);
		RCache.set_xform_world(World);
		RCache.set_xform_view(View);
		RCache.set_xform_project(Project);
	};
	xr_scope_exit RestoreState(std::move(RestoreStateLambda));
	Renderer.SpotShadowPass = true;
	Renderer.SpotShadowBuild = nullptr;
	Renderer.SpotShadowCachedCasters = nullptr;
	Renderer.SpotShadowCachedSkips = 0;
	Renderer.SpotShadowDuplicateSkips = 0;
	Renderer.SpotShadowStaticItems = 0;
	Renderer.SpotShadowDynamicItems = 0;
	Renderer.set_Feedback(nullptr, 0);
	SpotShadowCacheEntry Transient;
	const SpotShadowCacheKey Key = MakeSpotShadowCacheKey(Source);
	SpotShadowCacheEntry* Entry = FindSpotShadowCache(Source, Key);
	if (!Entry)
	{
		Entry = &Transient;
		Entry->Key = Key;
		++SpotShadowStatistics.CapacityLimited;
	}
	if (!Entry->StableObservations || Entry->LastSeenFrame != Device.dwFrame)
	{
		Entry->StableObservations = Entry->StableObservations && Entry->LastSeenFrame + 1 == Device.dwFrame ? 2 : 1;
		Entry->LastSeenFrame = Device.dwFrame;
	}
	Entry->LastUsedFrame = Device.dwFrame;
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
	const bool NeedsDepth = Entry->State == SpotShadowCacheState::Ready && !Entry->CachedCasters.empty();
	const bool HasDepth = Entry->Depth && Entry->Depth->pSurface && Entry->Depth->pZRT && Entry->Depth->pTexture && Entry->Depth->pTexture->get_SRView();
	const bool HasCopyShader = Entry->CopyShader && Entry->CopyShader->E[0] && !Entry->CopyShader->E[0]->passes.empty();
	if ((NeedsDepth || Entry->Depth) && (!HasDepth || !HasCopyShader))
	{
		ReleaseSpotShadowDepth(*Entry);
		++SpotShadowStatistics.ResourceFailures;
	}
	const bool CacheHit = Entry->State == SpotShadowCacheState::Ready && g_SpotShadowCacheMode != 2;
	bool UseCache = CacheHit;
	if (!CacheHit)
	{
		const bool Stable = Entry->StableObservations >= 2 || g_SpotShadowCacheMode == 2;
		if (!Stable)
		{
			++SpotShadowStatistics.Pending;
		}
		if (Entry != &Transient && Stable)
		{
			UseCache = BuildSpotShadowCache(Source, *Entry);
		}
	}
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

	const bool HasStatic = !Entry->CachedCasters.empty();
	if (HasStatic && !UseCache)
	{
		PROF_EVENT("SpotShadowCache::DirectStatic");
		GPU_EVENT(SpotShadowCacheDirectStatic);
		// Direct and cached depth use identical candidates and fixed detail from the first observation.
		R_dsgraph_structure::SpotShadowBuildContext DirectContext;
		DirectContext.CollectOnly = false;
		Renderer.SpotShadowBuild = &DirectContext;
		auto RestoreDirectContextLambda = [&]
		{
			Renderer.SpotShadowBuild = nullptr;
		};
		xr_scope_exit RestoreDirectContext(std::move(RestoreDirectContextLambda));
		++Renderer.marker;
		for (dxRender_Visual* Visual : Entry->CachedCasters)
		{
			Renderer.r_dsgraph_insert_static(Visual);
		}
		phase_smap_spot(&Source);
		RCache.set_xform_world(Fidentity);
		RCache.set_xform_view(Source.X.S.view);
		RCache.set_xform_project(Source.X.S.project);
		Renderer.r_dsgraph_render_graph(0);
	}
	Renderer.SpotShadowCachedCasters = &Entry->CachedCasterSet;
	if (psDeviceFlags.test(rsDrawDynamic))
	{
		Renderer.r_dsgraph_render_subspace(Source.SpatialComponent->sector, Source.X.S.combine, Source.position, R_dsgraph_structure::SubspaceGeometry::DynamicOnly, Source.ignore_object);
	}
	else
	{
		++Renderer.marker;
	}
	// Share the dynamic collection marker; keep LOD groups intact for normal per-frame selection.
	Renderer.set_Frustum(&ShadowFrustum);
	for (dxRender_Visual* Visual : Entry->PerFrameCasters)
	{
		Renderer.add_Static(Visual, ShadowFrustum.getMask());
	}
	SpotShadowStatistics.PerFrameCasters += u32(Entry->PerFrameCasters.size());
	if (CacheHit && UseCache)
	{
		++SpotShadowStatistics.Hits;
	}
	if (!UseCache)
	{
		++SpotShadowStatistics.Direct;
	}

	const bool HasCachedDepth = UseCache && Entry && !Entry->CachedCasters.empty();
	const bool HasNormal = SpotShadowCacheUtils::HasGeometry(Renderer, 0);
	const bool HasSpecial = SpotShadowCacheUtils::HasGeometry(Renderer, 1) || Renderer.mapSorted.size();
	if (!HasStatic && !HasNormal && !HasSpecial)
	{
		return false;
	}
	if (HasCachedDepth)
	{
		RestoreSpotShadowCache(Source, *Entry);
	}
	phase_smap_spot(&Source);
	RCache.set_xform_world(Fidentity);
	RCache.set_xform_view(Source.X.S.view);
	RCache.set_xform_project(Source.X.S.project);
	if (HasNormal)
	{
		GPU_EVENT(SpotShadowCacheDynamic);
		Renderer.r_dsgraph_render_graph(0);
		++SpotShadowStatistics.DynamicPasses;
	}
	if (ps_r2_ls_flags.test(R2FLAG_LIGHTS_DETAILS) && psDeviceFlags.is(rsDetails) &&
		Renderer.GetDetails() && Renderer.GetDetails()->dtSlots && Source.SpatialComponent &&
		Source.SpatialComponent->sphere.P.distance_to_sqr(RDEVICE.vCameraPosition) < _sqr(40.f))
	{
		Renderer.GetDetails()->hw_Render(&Source);
	}
	if (HasSpecial)
	{
		GPU_EVENT(SpotShadowCacheSpecial);
		Source.X.S.transluent = true;
		phase_smap_spot_tsh(&Source);
		Renderer.r_dsgraph_render_graph(1);
		Renderer.r_dsgraph_render_sorted(false);
		++SpotShadowStatistics.SpecialPasses;
	}
	return CheckSpotShadowGraph();
}

void CRenderTarget::DumpSpotShadowCache() const
{
	Msg("* spot shadow cache: mode=%d entries=%u memory=%.2f/%d MiB", g_SpotShadowCacheMode, u32(SpotShadowCache.size()), double(SpotShadowCacheBytes) / (1024.0 * 1024.0), g_SpotShadowCacheBudgetMb);
	Msg("* frame %u: lights=%u hits=%u builds=%u restores=%u pending=%u direct=%u evictions=%u static_casters=%u per_frame_casters=%u dynamic_passes=%u special_passes=%u",
		SpotShadowStatisticsFrame,
		SpotShadowStatistics.Lights,
		SpotShadowStatistics.Hits,
		SpotShadowStatistics.Builds,
		SpotShadowStatistics.Restores,
		SpotShadowStatistics.Pending,
		SpotShadowStatistics.Direct,
		SpotShadowStatistics.Evictions,
		SpotShadowStatistics.StaticCasters,
		SpotShadowStatistics.PerFrameCasters,
		SpotShadowStatistics.DynamicPasses,
		SpotShadowStatistics.SpecialPasses);
	Msg("* depth cache limits: texture_pending=%u budget=%u capacity=%u resource_failures=%u",
		SpotShadowStatistics.TexturePending,
		SpotShadowStatistics.BudgetLimited,
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
	Msg("* cache routing: legacy_lights=%u texture_invalidations=%u", SpotShadowStatistics.LegacyLights, SpotShadowStatistics.TextureInvalidations);
	Msg("* cache geometry reuse: geometry_changes=%u depth_changes=%u caster_reclassifications=%u pending_texture_checks=%u",
		SpotShadowStatistics.GeometryInvalidations,
		SpotShadowStatistics.DepthInvalidations,
		SpotShadowStatistics.CasterReclassifications,
		SpotShadowStatistics.PendingTextureChecks);
	const u32 TexelBytes = SpotShadowCacheUtils::DepthTexelBytes(static_cast<ERHI_FORMAT>(RImplementation.o.HW_smap_FORMAT));
	const u32 TileLimit = SMAP_adapt_max;
	const u64 MaxTileBytes = u64(TileLimit) * TileLimit * TexelBytes;
	const u64 BudgetBytes = u64(g_SpotShadowCacheBudgetMb) * 1024 * 1024;
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
