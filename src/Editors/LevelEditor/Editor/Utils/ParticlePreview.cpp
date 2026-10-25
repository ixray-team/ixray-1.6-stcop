#include "stdafx.h"
#include "ParticlePreview.h"

#include "../xrCore/FS_internal.h"

// Effects without a time limit are looped: take a frame after they have settled
static constexpr float DefaultPreviewTime = 2.0f;
static constexpr float MaxPreviewTime = 10.0f;

// Matches PS::uDT_STEP, which the renderer does not export
static constexpr u32 SimulationStep = 33;

// Primary thread budget per Update
static constexpr u32 MaxCreatePerUpdate = 8;
static constexpr u32 MaxRenderPerUpdate = 4;
static constexpr u32 MaxInFlight = 32;

static constexpr u32 PreviewCacheMagic = 0x31565050; // "PPV1"
// Bump when the bake output changes, so old cache files are rebaked
static constexpr u32 PreviewCacheVersion = 1;

static const float PreviewClearColor[4] = {0.0f, 0.0f, 0.0f, 0.0f};

static void MergeParticleBounds(PS::CParticleEffect* Effect, Fbox& Box)
{
	if (Effect == nullptr)
	{
		return;
	}

	PAPI::Particle* Particles = nullptr;
	u32 Count = 0;
	Effect->Pholder.GetParticles(Particles, Count);

	for (u32 Idx = 0; Idx < Count; ++Idx)
	{
		const PAPI::Particle& Particle = Particles[Idx];
		const float Extent = std::max(Particle.size.x, Particle.size.y) * 0.5f;

		Fvector Pos;
		Effect->m_XFORM.transform_tiny(Pos, Particle.pos);
		Box.modify(Fvector().sub(Pos, Fvector().set(Extent, Extent, Extent)));
		Box.modify(Fvector().add(Pos, Fvector().set(Extent, Extent, Extent)));
	}
}

CParticlePreviewCache::~CParticlePreviewCache()
{
	Tasks.wait();
}

void CParticlePreviewCache::Clear()
{
	Tasks.wait();

	for (auto& [Name, Item] : Entries)
	{
		DestroyVisual(*Item);
	}

	Entries.clear();
	Queue.clear();

	TargetColor.destroy();
	TargetDepth.destroy();
}

ref_texture CParticlePreviewCache::Get(const xr_string& Name, void* Def, bool IsGroup)
{
	auto Found = Entries.find(Name);
	if (Found != Entries.end())
	{
		Entry* Item = Found->second.get();
		return Item->State == EState::Ready ? Item->Texture : ref_texture();
	}

	if (Def == nullptr)
	{
		return {};
	}

	Entry* Item = Entries.emplace(Name, xr_make_unique<Entry>()).first->second.get();
	Item->Def = Def;
	Item->IsGroup = IsGroup;
	Item->Hash = GetDefHash(Def, IsGroup);

	string_path CachePath = {};
	FS.update_path(CachePath, "$app_data_root$", (xr_string("particle_previews\\") + Name + ".ppv").c_str());
	Item->CacheFile = CachePath;

	Tasks.run([Item]()
	{
		Item->State = LoadCache(*Item) ? EState::Loaded : EState::Missed;
	});

	return {};
}

static void HashDef(PS::CPEDef* Def, CMemoryWriter& Writer)
{
	if (Def != nullptr)
	{
		Def->Save(Writer);
	}
}

u32 CParticlePreviewCache::GetDefHash(void* Def, bool IsGroup)
{
	CMemoryWriter Writer;
	Writer.w_u32(PreviewCacheVersion);
	Writer.w_u32(PreviewSize);

	if (IsGroup)
	{
		PS::CPGDef* Group = (PS::CPGDef*)Def;
		Group->Save(Writer);

		// The group stores effect names only: hash the effects it plays too
		for (const PS::CPGDef::SEffect* Effect : Group->m_Effects)
		{
			for (const shared_str& EffectName : {Effect->m_EffectName, Effect->m_OnPlayChildName, Effect->m_OnBirthChildName, Effect->m_OnDeadChildName})
			{
				if (EffectName.size())
				{
					HashDef(RImplementation.PSLibrary.FindPED(EffectName.c_str()), Writer);
				}
			}
		}
	}
	else
	{
		HashDef((PS::CPEDef*)Def, Writer);
	}

	return crc32(Writer.pointer(), Writer.size());
}

bool CParticlePreviewCache::LoadCache(Entry& Item)
{
	std::error_code Error;
	if (!std::filesystem::is_regular_file(Item.CacheFile, Error))
	{
		return false;
	}

	CFileReader Reader(Item.CacheFile.xstring().c_str());

	const intptr_t PixelsSize = intptr_t(PreviewSize * PreviewSize * sizeof(u32));
	if (Reader.length() != intptr_t(sizeof(u32) * 4) + PixelsSize)
	{
		return false;
	}

	if (Reader.r_u32() != PreviewCacheMagic || Reader.r_u32() != PreviewCacheVersion || Reader.r_u32() != Item.Hash || Reader.r_u32() != PreviewSize)
	{
		return false;
	}

	Item.Pixels.resize(PreviewSize * PreviewSize);
	Reader.r(Item.Pixels.data(), PixelsSize);
	return true;
}

void CParticlePreviewCache::SaveCache(const xr_path& CacheFile, u32 Hash, const U32Vec& Pixels)
{
	IWriter* Writer = FS.w_open(CacheFile.xstring().c_str());
	if (Writer == nullptr || !Writer->valid())
	{
		Msg("! Can't write particle preview cache: %s", CacheFile.xstring().c_str());
		FS.w_close(Writer);
		return;
	}

	Writer->w_u32(PreviewCacheMagic);
	Writer->w_u32(PreviewCacheVersion);
	Writer->w_u32(Hash);
	Writer->w_u32(PreviewSize);
	Writer->w(Pixels.data(), u32(Pixels.size() * sizeof(u32)));

	FS.w_close(Writer);
}

float CParticlePreviewCache::GetPreviewTime(const Entry& Item)
{
	float LifeTime = 0.0f;

	if (Item.IsGroup)
	{
		const PS::CPGDef* Def = (const PS::CPGDef*)Item.Def;
		LifeTime = Def->m_fTimeLimit;

		if (LifeTime <= 0.0f)
		{
			for (const PS::CPGDef::SEffect* Effect : Def->m_Effects)
			{
				if (Effect->m_Flags.is(PS::CPGDef::SEffect::flEnabled))
				{
					LifeTime = std::max(LifeTime, Effect->m_Time1);
				}
			}
		}
	}
	else
	{
		const PS::CPEDef* Def = (const PS::CPEDef*)Item.Def;
		if (Def->m_Flags.is(PS::CPEDef::dfTimeLimit))
		{
			LifeTime = Def->m_fTimeLimit;
		}
	}

	if (LifeTime <= 0.0f)
	{
		return DefaultPreviewTime;
	}

	return std::min(LifeTime * 0.5f, MaxPreviewTime);
}

void CParticlePreviewCache::Simulate(Entry& Item)
{
	IParticleCustom* Particle = Item.Visual->dcast_ParticleCustom();
	const u32 StepCount = iCeil(GetPreviewTime(Item) * 1000.0f / float(SimulationStep));

	for (u32 Step = 0; Step < StepCount; Step++)
	{
		Particle->OnFrame(SimulationStep);
	}
}

bool CParticlePreviewCache::CreateVisual(Entry& Item)
{
	CModelPool* Models = ((CRender*)::Render)->Models;

	if (Item.IsGroup)
	{
		PS::CParticleGroup* Group = (PS::CParticleGroup*)Models->CreatePG(0);
		Group->Compile((PS::CPGDef*)Item.Def);
		Group->Play();
		Item.Visual = Group;
	}
	else
	{
		PS::CParticleEffect* Effect = (PS::CParticleEffect*)Models->CreatePE(0);
		Effect->Compile((PS::CPEDef*)Item.Def);
		Effect->Play();
		Item.Visual = Effect;
	}

	return Item.Visual != nullptr;
}

void CParticlePreviewCache::DestroyVisual(Entry& Item)
{
	if (Item.Visual == nullptr)
	{
		return;
	}

	((CRender*)::Render)->Models->Delete(Item.Visual);
	Item.Visual = nullptr;
}

bool CParticlePreviewCache::Render(Entry& Item)
{
	if (!TargetColor)
	{
		TargetColor.create("$user$particle_preview_color", PreviewSize, PreviewSize, ERHI_FORMAT::B8G8R8A8_UNORM);
		TargetDepth.create("$user$particle_preview_depth", PreviewSize, PreviewSize, ERHI_FORMAT::R24G8_TYPELESS);
	}

	if (!TargetColor || !TargetDepth || TargetColor->pRT == nullptr || TargetDepth->pZRT == nullptr)
	{
		return false;
	}

	// Frame the live particles: vis.box also covers the emitter domains and is much larger
	Fbox Box;
	Box.invalidate();

	if (Item.IsGroup)
	{
		PS::CParticleGroup* Group = (PS::CParticleGroup*)Item.Visual;
		for (PS::CParticleGroup::SItem& GroupItem : Group->items)
		{
			MergeParticleBounds(GroupItem.root_effect, Box);
			for (PS::CParticleEffect* Child : GroupItem.children_related)
			{
				MergeParticleBounds(Child, Box);
			}
			for (PS::CParticleEffect* Child : GroupItem.children_free)
			{
				MergeParticleBounds(Child, Box);
			}
		}
	}
	else
	{
		MergeParticleBounds((PS::CParticleEffect*)Item.Visual, Box);
	}

	if (!Box.is_valid())
	{
		Box = Item.Visual->vis.box;
	}

	if (!Box.is_valid())
	{
		Box.set(Fvector().set(0.0f, 0.0f, 0.0f), Fvector().set(0.0f, 0.0f, 0.0f));
	}

	Fvector Center;
	float Radius = 0.0f;
	Box.getsphere(Center, Radius);
	Radius = std::max(Radius, 0.1f);

	const float Fov = deg2rad(45.0f);
	const float Distance = Radius / std::sin(Fov * 0.5f) * 1.05f;

	Fvector Direction;
	Direction.set(-0.4f, -0.35f, 1.0f).normalize();

	Fvector Position;
	Position.mad(Center, Direction, -Distance);

	Fmatrix View;
	View.build_camera_dir(Position, Direction, Fvector().set(0.0f, 1.0f, 0.0f));

	Fmatrix Project;
	Project.build_projection(Fov, 1.0f, std::max(Distance - Radius * 2.0f, 0.05f), Distance + Radius * 2.0f);

	// Swap the device camera: sprites are oriented from it
	const Fmatrix SavedView = Device.mView;
	const Fmatrix SavedProject = Device.mProject;
	const Fmatrix SavedFull = Device.mFullTransform;
	const Fvector SavedPosition = Device.vCameraPosition;
	const Fvector SavedDirection = Device.vCameraDirection;
	const Fvector SavedTop = Device.vCameraTop;
	const Fvector SavedRight = Device.vCameraRight;

	IRHIRenderTargetView* SavedRT[4] = {};
	for (u32 Idx = 0; Idx < 4; ++Idx)
	{
		SavedRT[Idx] = GRHI->GetRenderTargetView(Idx);
	}
	IRHIDepthStencilView* SavedDSV = GRHI->GetDepthStencilView();

	Device.mView = View;
	Device.mProject = Project;
	Device.mFullTransform.mul(Project, View);

	Fmatrix Camera;
	Camera.invert(View);
	Device.vCameraPosition.set(Camera.c);
	Device.vCameraDirection.set(Camera.k);
	Device.vCameraTop.set(Camera.j);
	Device.vCameraRight.set(Camera.i);

	RCache.set_xform_view(View);
	RCache.set_xform_project(Project);

	RCache.set_RT(TargetColor->pRT);
	RCache.set_RT(nullptr, 1);
	RCache.set_RT(nullptr, 2);
	RCache.set_RT(nullptr, 3);
	GRHI->SetDepthStencilView(TargetDepth->pZRT);
	GRHI->ApplyRenderTargetChange();

	GRHI->ClearTarget(TargetColor->pRT, PreviewClearColor);
	GRHI->ClearDepthStencil(TargetDepth->pZRT, ERHI_CLEAR_TARGET::DEPTH | ERHI_CLEAR_TARGET::STENCIL, 1.0f, 0);

	RHIViewport Viewport = {0, 0, (float)PreviewSize, (float)PreviewSize, 0.f, 1.f};
	GRHI->SetViewport(Viewport);

	((CRender*)::Render)->Models->RenderSingle(Item.Visual, Fidentity, 1.0f);
	GRHI->ApplyRenderTargetChange();

	U32Vec& Pixels = Item.Pixels;
	const bool IsValid = EDevice->ReadbackRT(TargetColor, Pixels);

	Device.mView = SavedView;
	Device.mProject = SavedProject;
	Device.mFullTransform = SavedFull;
	Device.vCameraPosition = SavedPosition;
	Device.vCameraDirection = SavedDirection;
	Device.vCameraTop = SavedTop;
	Device.vCameraRight = SavedRight;
	RCache.set_xform_view(SavedView);
	RCache.set_xform_project(SavedProject);

	for (u32 Idx = 0; Idx < 4; ++Idx)
	{
		RCache.set_RT(SavedRT[Idx], Idx);
	}
	GRHI->SetDepthStencilView(SavedDSV);
	GRHI->ApplyRenderTargetChange();

	if (!IsValid)
	{
		return false;
	}

	// Blended sprites leave garbage in alpha: rebuild it from the brightness over the
	// transparent clear, so additive effects lay over the tile background
	for (u32& Pixel : Pixels)
	{
		const u32 B = Pixel & 0xFF;
		const u32 G = (Pixel >> 8) & 0xFF;
		const u32 R = (Pixel >> 16) & 0xFF;
		const u32 A = std::max({R, G, B});

		if (A == 0)
		{
			Pixel = 0;
			continue;
		}

		Pixel = (A << 24) | ((R * 255 / A) << 16) | ((G * 255 / A) << 8) | (B * 255 / A);
	}

	return true;
}

bool CParticlePreviewCache::CreateTexture(Entry& Item)
{
	if (Item.Pixels.size() != PreviewSize * PreviewSize)
	{
		return false;
	}

	RHITextureDesc Desc;
	Desc.Width = PreviewSize;
	Desc.Height = PreviewSize;
	Desc.Format = ERHI_FORMAT::B8G8R8A8_UNORM;
	Desc.MipLevels = 1;
	Desc.ArraySize = 1;
	Desc.Usage = ERHI_USAGE::USAGE_DEFAULT;
	Desc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE;

	RHISubResource SubResource{};
	SubResource.Data = Item.Pixels.data();
	SubResource.DataSize = PreviewSize * 4;

	IRHISurface* Surface = GRHI->CreateTexture2D(Desc, SubResource);
	if (Surface == nullptr)
	{
		return false;
	}

	CTexture* Texture = new CTexture();
	Texture->surface_set(Surface);
	Surface->Release();

	Item.Texture = Texture;
	Item.Pixels.clear();
	Item.Pixels.shrink_to_fit();
	return true;
}

void CParticlePreviewCache::Update()
{
	u32 Rendered = 0;
	u32 InFlight = 0;

	for (auto& [Name, Item] : Entries)
	{
		const EState State = Item->State;
		if (State == EState::Loaded)
		{
			Item->State = CreateTexture(*Item) ? EState::Ready : EState::Failed;
		}
		else if (State == EState::Missed)
		{
			Item->State = EState::Queued;
			Queue.push_back(Item.get());
		}
		else if (State == EState::Simulating)
		{
			InFlight++;
		}
		else if (State == EState::Simulated)
		{
			if (Rendered < MaxRenderPerUpdate)
			{
				Rendered++;
				const bool IsValid = Render(*Item);
				DestroyVisual(*Item);

				if (IsValid)
				{
					Tasks.run([CacheFile = Item->CacheFile, Hash = Item->Hash, Pixels = Item->Pixels]()
					{
						SaveCache(CacheFile, Hash, Pixels);
					});
				}

				Item->State = IsValid && CreateTexture(*Item) ? EState::Ready : EState::Failed;
			}
			else
			{
				InFlight++;
			}
		}
	}

	u32 Created = 0;
	size_t Processed = 0;
	for (; Processed < Queue.size() && Created < MaxCreatePerUpdate && InFlight < MaxInFlight; Processed++)
	{
		Entry* Item = Queue[Processed];
		Created++;

		if (!CreateVisual(*Item))
		{
			Item->State = EState::Failed;
			continue;
		}

		InFlight++;
		Item->State = EState::Simulating;
		Tasks.run([Item]()
		{
			Simulate(*Item);
			Item->State = EState::Simulated;
		});
	}

	Queue.erase(Queue.begin(), Queue.begin() + Processed);
}
