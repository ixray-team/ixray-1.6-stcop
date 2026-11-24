#include "stdafx.h"

#include "../../xrRHI/RHITextureInterfaces.h"
#include "ResourceManager.h"

#include "../../xrEngine/Render.h"

#include "../../xrEngine/tntQAVI.h"
#include "../../xrEngine/xrTheora_Surface.h"

#include "dxRenderDeviceRender.h"

namespace
{
// The view the old backend binds: typed formats for the depth surfaces, and for what NRI made, as its texture is typeless
IRHIShaderResourceView* MakeView(IRHISurface* Surface)
{
	RHIShaderResourceViewDesc Desc = {};
	Desc.Format = Surface->GetFormat();
	Desc.MipLevels = Surface->GetMipLevels();
	Desc.ArraySize = Surface->GetArraySize();
	const ERHI_RESOURCE_DIMENSION Type = Surface->GetTextureType();

	if (Type == ERHI_RESOURCE_DIMENSION::TEXTURE3D)
	{
		Desc.ViewDimension = ERHI_SRV_DIMENSION::TEXTURE3D;
		return GRHI->CreateShaderResourceView(Surface, &Desc);
	}

	if (Type != ERHI_RESOURCE_DIMENSION::TEXTURE2D)
	{
		return GRHI->CreateShaderResourceView(Surface, nullptr);
	}

	if ((ERHI_RESOURCE_MISC_FLAG(Surface->GetMiscFlags()) & ERHI_RESOURCE_MISC_FLAG::TEXTURECUBE) != ERHI_RESOURCE_MISC_FLAG(0))
	{
		Desc.ViewDimension = ERHI_SRV_DIMENSION::TEXTURECUBE;
	}
	else if (Surface->GetSampleDescCount() > 1)
	{
		VERIFY(Surface->GetArraySize() == 1);
		Desc.ViewDimension = ERHI_SRV_DIMENSION::TEXTURE2DMS;
	}
	else
	{
		Desc.ViewDimension = Desc.ArraySize > 1 ? ERHI_SRV_DIMENSION::TEXTURE2DARRAY : ERHI_SRV_DIMENSION::TEXTURE2D;
	}

	switch (Surface->GetFormat())
	{
	case ERHI_FORMAT::R24G8_TYPELESS:
		Desc.Format = ERHI_FORMAT::R24_UNORM_X8_TYPELESS;
		break;
	case ERHI_FORMAT::R32_TYPELESS:
		Desc.Format = ERHI_FORMAT::R32_FLOAT;
		break;
	case ERHI_FORMAT::R16_TYPELESS:
		Desc.Format = ERHI_FORMAT::R16_UNORM;
		break;
	}

	return GRHI->CreateShaderResourceView(Surface, &Desc);
}
} // namespace

void resptrcode_texture::create(const char* _name)
{
	PROF_EVENT("resptrcode_texture::create");
	_set(DEV->_CreateTexture(_name));
}

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
CTexture::CTexture		()
{
	pSurface			= nullptr;
	m_pSRView			= nullptr;
	pAVI				= nullptr;
	pTheora				= nullptr;
	seqMSPF				= 0;
	flags.MemoryUsage	= 0;
	flags.bLoaded		= false;
	flags.bUser			= false;
	flags.seqCycles		= false;
	m_material			= 1.0f;
	can_unload = true;
	bind				= xr_make_delegate(this,&CTexture::apply_load);
}

CTexture::~CTexture()
{
	Unload();
	DEV->_DeleteTexture(this);
}

void CTexture::surface_set(IRHISurface* surf)
{
	xrCriticalSectionGuard Guard(LoadGuard);
	FinishLoading();
	ShadowCacheRevision.fetch_add(1, std::memory_order_relaxed);
	if (surf)			surf->AddRef();
	_RELEASE(pSurface);
	_RELEASE(m_pSRView);
	Gpu.Reset();

	pSurface = surf;
	m_pSRView = pSurface ? MakeView(pSurface) : nullptr;
}

void CTexture::Make(const RHITextureDesc& Desc, const RHISubResource* Levels)
{
	xrCriticalSectionGuard Guard(LoadGuard);
	FinishLoading();
	ShadowCacheRevision.fetch_add(1, std::memory_order_relaxed);
	_RELEASE(pSurface);
	_RELEASE(m_pSRView);

	if (Gpu.Create(Desc, Levels, *cName))
	{
		pSurface = Gpu.Adopt();
		m_pSRView = pSurface ? MakeView(pSurface) : nullptr;
	}
}

IRHISurface* CTexture::surface_get()
{
	WaitForLoad();
	Shown().Ready();

	if (pSurface)
	{
		pSurface->AddRef();
	}
	return pSurface;
}

void CTexture::PostLoad	()
{
	if (pTheora)				bind = xr_make_delegate(this,&CTexture::apply_theora);
	else if (pAVI)				bind = xr_make_delegate(this,&CTexture::apply_avi);
	else if (!seqDATA.empty())	bind = xr_make_delegate(this,&CTexture::apply_seq);
	else						bind = xr_make_delegate(this,&CTexture::apply_normal);
}

void CTexture::apply_load(u32 dwStage)
{
	// A texture bound for the first time in a frame is read by a worker and made by the frame's upload step; the
	// placeholder stands for it until then. Where no frame takes uploads it is loaded here
	const bool IsUploading = TextureDecode::IsUploading();

	if (IsUploading && !flags.bLoaded && !IsLoadPending() && Sampled2D.load(std::memory_order_acquire) && !Device.IsEditorMode() && ps_r__common_flags.test(RFLAG_MT_TEX_LOAD))
	{
		StartLoading(true);
	}

	if (IsUploading && ShowsPlaceholder())
	{
		ApplyView(dwStage, Placeholder->m_pSRView);
		return;
	}

	Load();

	bind(dwStage);
}

bool CTexture::ShowsPlaceholder() const
{
	return IsLoading.load(std::memory_order_acquire) && !LoadDone.load(std::memory_order_acquire) && Placeholder &&
		Sampled2D.load(std::memory_order_acquire);
}

GpuResource& CTexture::Shown()
{
	return SeqGpu.empty() ? Gpu : SeqGpu[SeqFrame];
}

void CTexture::Apply(u32 dwStage)
{
	Shown().Ready();
	IRHIShaderResourceView* View = m_pSRView;
	if (!View && Placeholder && Sampled2D.load(std::memory_order_acquire))
	{
		View = Placeholder->m_pSRView;
	}
	ApplyView(dwStage, View);
}

void CTexture::ApplyView(u32 dwStage, IRHIShaderResourceView* View)
{
	if (dwStage < rstVertex)
	{
		//	Pixel shader stage resources
		GRHI->ShaderResourceCache->SetPSResource(dwStage, View);
	}
	else if (dwStage < rstGeometry)
	{
		//	Vertex shader stage resources
		GRHI->ShaderResourceCache->SetVSResource(dwStage - rstVertex, View);
	}
	else if (dwStage < rstHull)
	{
		//	Geometry shader stage resources
		GRHI->ShaderResourceCache->SetGSResource(dwStage - rstGeometry, View);
	}
	else if (dwStage < rstDomain)
	{
		GRHI->ShaderResourceCache->SetHSResource(dwStage - rstHull, View);
	}
	else if (dwStage < rstCompute)
	{
		GRHI->ShaderResourceCache->SetDSResource(dwStage - rstDomain, View);
	}
	else if (dwStage < rstInvalid)
	{
		GRHI->ShaderResourceCache->SetCSResource(dwStage - rstCompute, View);
	}
	else VERIFY("Invalid stage");
}

void CTexture::apply_theora(u32 dwStage)
{
	if (!pTheora || !pSurface)
		return;

	if (pTheora->Update(m_play_time != 0xFFFFFFFF ? m_play_time : RFrame.dwTimeContinual))
	{
		ERHI_RESOURCE_DIMENSION type = pSurface->GetTextureType();
		R_ASSERT(ERHI_RESOURCE_DIMENSION::TEXTURE2D == type);

		u32 RowPitch = 0;
		void* InputBuffer = pSurface->Lock(0, &RowPitch);


		u32 Right = pTheora->Width(true);
		u32 _w = pTheora->Width(false);

		u32 DeltaOffset = RowPitch / u32(pTheora->Width(false) * 4);
		_w *= DeltaOffset;

		int _pos = 0;
		pTheora->DecompressFrame((u32*)InputBuffer, _w - Right, _pos);

		pSurface->Unlock();
	}
	Apply(dwStage);
}

void CTexture::apply_avi(u32 dwStage)
{
	if (pAVI->NeedUpdate())
	{
		ERHI_RESOURCE_DIMENSION type = pSurface->GetTextureType();
		R_ASSERT(ERHI_RESOURCE_DIMENSION::TEXTURE2D == type);

		u32 RowPitch = 0;
		void* InputBuffer = pSurface->Lock(0, &RowPitch);

		// AVI
		R_ASSERT(RowPitch == int(pAVI->m_dwWidth * 4));
		u8* ptr;
		pAVI->GetFrame(&ptr);
		CopyMemory(InputBuffer, ptr, pAVI->m_dwWidth * pAVI->m_dwHeight * 4);
		pSurface->Unlock();
	}

	Apply(dwStage);
}

void CTexture::apply_seq(u32 dwStage)
{
	// SEQ
	u32	frame = RFrame.dwTimeContinual / seqMSPF;
	u32	frame_data = (u32)seqDATA.size();

	if (flags.seqCycles)
	{
		u32	frame_id = frame % (frame_data * 2);
		if (frame_id >= frame_data)	frame_id = (frame_data - 1) - (frame_id % frame_data);
		SeqFrame = frame_id;
	}
	else
	{
		SeqFrame = frame % frame_data;
	}

	pSurface = seqDATA[SeqFrame];
	m_pSRView = m_seqSRView[SeqFrame];

	Apply(dwStage);
}

void CTexture::apply_normal	(u32 dwStage)
{
	Apply(dwStage);
}

void CTexture::Preload	()
{
	m_bumpmap = DEV->m_textures_description.GetBumpName(cName);
	m_material = DEV->m_textures_description.GetMaterial(cName);
}

void CTexture::Load()
{
	PROF_EVENT("CTexture::Load");
	xrCriticalSectionGuard Guard(LoadGuard);
	FinishLoading();
	if (!flags.bLoaded)
	{
		Preload();
		LoadImpl();
	}
	PostLoad();
}

bool CTexture::StartLoading(bool IsDecodeOnly)
{
	xrCriticalSectionGuard Guard(LoadGuard);
	if (LoadTask || flags.bLoaded)
	{
		return true;
	}

	string_path FileName;
	if (strchr(*cName, '$') || FS.exist(FileName, _game_textures_, *cName, ".ogm") ||
		FS.exist(FileName, _game_textures_, *cName, ".avi"))
	{
		return false;
	}

	Preload();
	if (!Placeholder)
	{
		CTexture* Shared = DEV->_GetTexturePlaceholder();
		if (Shared != this)
		{
			Placeholder._set(Shared);
		}
	}
	LoadDone.store(false, std::memory_order_relaxed);
	LoadCancelled.store(false, std::memory_order_relaxed);
	LoadTask = xr_make_unique<xr_task_group>(ETaskPriority::Background);
	auto LoadLambda = [this, IsDecodeOnly]()
	{
		if (!LoadCancelled.load(std::memory_order_acquire))
		{
			PROF_EVENT("CTexture::LoadAsync");
			string_path Sequence;

			if (!IsDecodeOnly)
			{
				LoadImpl();
			}
			else if (!FS.exist(Sequence, _game_textures_, *cName, ".seq") && TextureDecode::Read(*cName, Decoded))
			{
				// The frame says when it is made
				TextureDecode::Queue(Decoded, LoadDone);
				return;
			}
		}
		LoadDone.store(true, std::memory_order_release);
	};
	IsLoading.store(true, std::memory_order_release);
	LoadTask->run(LoadLambda);
	return true;
}

bool CTexture::CanCacheShadow(bool& Pending, u64& Revision)
{
	xrCriticalSectionGuard Guard(LoadGuard);
	Revision = ShadowCacheRevision.load(std::memory_order_relaxed);
	Pending = false;
	if (!can_unload || !cName || strchr(*cName, '$'))
	{
		return false;
	}
	Pending = LoadTask != nullptr || !flags.bLoaded;
	if (Pending)
	{
		return false;
	}
	return pSurface && m_pSRView && !flags.bUser && !pAVI && !pTheora && seqDATA.empty();
}

void CTexture::FinishLoading(bool Discard)
{
	if (LoadTask)
	{
		if (Discard)
		{
			LoadCancelled.store(true, std::memory_order_release);
		}
		LoadTask->wait();
		LoadTask.reset();

		if (Discard)
		{
			TextureDecode::Drop(Decoded);
		}

		if (!flags.bLoaded && !Discard)
		{
			// A cancelled parent task may prevent the queued load from starting.
			LoadImpl();
		}
		IsLoading.store(false, std::memory_order_release);
	}

	Gpu.Ready();
}

void CTexture::WaitForLoad()
{
	if (IsLoading.load(std::memory_order_acquire))
	{
		xrCriticalSectionGuard Guard(LoadGuard);
		FinishLoading();
	}
}

void CTexture::EnsureLoaded()
{
	if (flags.bLoaded)
	{
		WaitForLoad();
		return;
	}

	Load();
}

IRHISurface* CTexture::BoundSurface() const
{
	IRHISurface* Surface = ShowsPlaceholder() ? Placeholder->pSurface : pSurface;
	if (!Surface && !m_pSRView && Placeholder && Sampled2D.load(std::memory_order_acquire))
	{
		Surface = Placeholder->pSurface;
	}
	return Surface;
}

nri::Descriptor* CTexture::BoundView()
{
	CTexture* Bound = ShowsPlaceholder() || (!pSurface && !m_pSRView && Placeholder && Sampled2D.load(std::memory_order_acquire)) ? Placeholder._get() : this;
	GpuResource& Item = Bound->Shown();
	Item.Ready();
	return Item.View;
}

bool CTexture::BindsBC1() const
{
	const IRHISurface* Surface = BoundSurface();
	if (!Surface)
	{
		return false;
	}

	const ERHI_FORMAT Format = Surface->GetFormat();
	return Format >= ERHI_FORMAT::BC1_TYPELESS && Format < ERHI_FORMAT::BC2_TYPELESS;
}

void CTexture::LoadImpl()
{
	flags.bLoaded = true;
	if (pSurface)
		return;

	flags.bUser = false;
	flags.MemoryUsage = 0;

	if (0 == _stricmp(*cName, "$null"))
		return;

	if (nullptr != strstr(*cName, "$user$"))
	{
		flags.bUser = true;
		return;
	}

	// Check for OGM
	string_path fn;
	if (FS.exist(fn, _game_textures_, *cName, ".ogm"))
	{
		// AVI
		pTheora = new CTheoraSurface();
		m_play_time = 0xFFFFFFFF;

		if (!pTheora->Load(fn))
		{
			xr_delete(pTheora);
			FATAL("Can't open video stream");
		}
		else
		{
			flags.MemoryUsage = pTheora->Width(true) * pTheora->Height(true) * 4;
			pTheora->Play(true, Device.dwTimeContinual);

			u32 _w = pTheora->Width(false);
			u32 _h = pTheora->Height(false);

			// Create RHITextureDesc for the texture
			RHITextureDesc rhiDesc;
			rhiDesc.Width = _w;
			rhiDesc.Height = _h;
			rhiDesc.MipLevels = 1;
			rhiDesc.Format = ERHI_FORMAT::R8G8B8A8_UNORM;
			rhiDesc.Usage = ERHI_USAGE::USAGE_DYNAMIC;
			rhiDesc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE;
			rhiDesc.CPUAccessFlags = ERHI_CPU_ACCESS_FLAG::ERHI_CPU_ACCESS_FLAG_WRITE;
			rhiDesc.MiscFlags = 0;

			pSurface = GRHI->CreateTextureFromMemory(nullptr, 0, rhiDesc);
			if (pSurface == nullptr)
			{
				FATAL("Invalid video stream");
				xr_delete(pTheora);
				pSurface = nullptr;
				m_pSRView = nullptr;
			}
			else
			{
				m_pSRView = GRHI->CreateShaderResourceView(pSurface, nullptr);
			}

		}
	}
	else if (FS.exist(fn, _game_textures_, *cName, ".avi"))
	{
		// AVI
		pAVI = new CAviPlayerCustom();

		if (!pAVI->Load(fn))
		{
			xr_delete(pAVI);
			FATAL("Can't open video stream");
		}
		else
		{
			flags.MemoryUsage = pAVI->m_dwWidth * pAVI->m_dwHeight * 4;

			RHITextureDesc rhiDesc;
			rhiDesc.Width = pAVI->m_dwWidth;
			rhiDesc.Height = pAVI->m_dwHeight;
			rhiDesc.MipLevels = 1;
			rhiDesc.Format = ERHI_FORMAT::R8G8B8A8_UNORM;
			rhiDesc.Usage = ERHI_USAGE::USAGE_DYNAMIC;
			rhiDesc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE;
			rhiDesc.CPUAccessFlags = ERHI_CPU_ACCESS_FLAG::ERHI_CPU_ACCESS_FLAG_WRITE;
			rhiDesc.MiscFlags = 0;

			// Use GRHI to create the surface
			pSurface = GRHI->CreateTextureFromMemory(nullptr, 0, rhiDesc);
			if (pSurface == nullptr)
			{
				FATAL("Invalid video stream");
				xr_delete(pAVI);
				pSurface = nullptr;
				m_pSRView = nullptr;
			}
			else
			{
				m_pSRView = GRHI->CreateShaderResourceView(pSurface, nullptr);
			}

		}
	}
	else if (FS.exist(fn, _game_textures_, *cName, ".seq"))
	{
		// Sequence
		string256 buffer;
		IReader* _fs = FS.r_open(fn);

		flags.seqCycles = false;
		_fs->r_string(buffer, sizeof(buffer));
		if (0 == _stricmp(buffer, "cycled"))
		{
			flags.seqCycles = true;
			_fs->r_string(buffer, sizeof(buffer));
		}
		u32 fps = atoi(buffer);
		seqMSPF = 1000 / fps;

		while (!_fs->eof())
		{
			_fs->r_string(buffer, sizeof(buffer));
			_Trim(buffer);
			if (buffer[0])
			{
				u32	mem = 0;
				GpuResource Frame;

				if (Frame.Load(buffer, mem))
				{
					seqDATA.push_back(Frame.Adopt());
					m_seqSRView.push_back(MakeView(seqDATA.back()));
					SeqGpu.push_back(std::move(Frame));
					flags.MemoryUsage += mem;
				}
			}
		}
		pSurface = nullptr;
		SeqFrame = 0;
		FS.r_close(_fs);
	}
	else
	{
		u32	mem = 0;

		if (Decoded.Texture)
		{
			TextureDecode::Create(Decoded, Gpu, mem);
		}
		else
		{
			// Read but not made, or not a file the frame can make: it is loaded here
			TextureDecode::Drop(Decoded);
			Gpu.Load(*cName, mem);
		}

		pSurface = Gpu.Adopt();

		if (pSurface)
		{
			flags.MemoryUsage = mem;
			m_pSRView = MakeView(pSurface);
		}
	}
}

void CTexture::Unload()
{
	xrCriticalSectionGuard Guard(LoadGuard);
	FinishLoading(true);
	if (!can_unload) return;
	ShadowCacheRevision.fetch_add(1, std::memory_order_relaxed);
	flags.bLoaded = false;

	if (!seqDATA.empty())
	{
		for (u32 I = 0; I < seqDATA.size(); I++)
		{
			_RELEASE(seqDATA[I]);
			_RELEASE(m_seqSRView[I]);
		}
		seqDATA.clear();
		m_seqSRView.clear();
		SeqGpu.clear();
		pSurface = nullptr;
		m_pSRView = nullptr;
	}

	_RELEASE(pSurface);
	_RELEASE(m_pSRView);
	Gpu.Reset();

	xr_delete(pAVI);
	xr_delete(pTheora);

	bind = xr_make_delegate(this, &CTexture::apply_load);
}

IRHIShaderResourceView* CTexture::GetView()
{
	WaitForLoad();
	Shown().Ready();
	return m_pSRView;
}

void CTexture::video_Play(bool looped, u32 _time)
{
	if (pTheora)
	{
		pTheora->Play(looped, (_time != 0xFFFFFFFF) ? (m_play_time = _time) : Device.dwTimeContinual);
	}
}

void CTexture::video_Pause(bool state)
{
	if (pTheora)
	{
		pTheora->Pause(state);
	}
}

void CTexture::video_Stop()
{
	if (pTheora)
	{
		pTheora->Stop();
	}
}

bool CTexture::video_IsPlaying()
{
	return (pTheora) ? pTheora->IsPlaying() : false;
}

void CTexture::CreateEmpty(u32 w, u32 h)
{
	xrCriticalSectionGuard Guard(LoadGuard);
	FinishLoading();
	R_ASSERT(GRHI->DevicePtr && "must be valid");

	flags.bLoaded = true;
	if (pSurface)
		return;

	flags.bUser = false;
	flags.MemoryUsage = 0;
	if (0 == _stricmp(*cName, "$null"))	return;
	if (nullptr != strstr(*cName, "$user$")) {
		flags.bUser = true;
		return;
	}

	Preload();

	RHITextureDesc desc = {};
	desc.Width = w;
	desc.Height = h;
	desc.MipLevels = 1;
	desc.ArraySize = 1;
	desc.Format = ERHI_FORMAT::R8G8B8A8_UNORM;
	desc.Usage = ERHI_USAGE::USAGE_DEFAULT;
	desc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE;

	RHISubResource Subres = {};
	pSurface = GRHI->CreateTexture2D(desc, Subres);

	if (pSurface)
	{
		flags.MemoryUsage = w * h * 4;
		m_pSRView = GRHI->CreateShaderResourceView(pSurface, nullptr);
	}

	PostLoad();

	can_unload = false;
}
