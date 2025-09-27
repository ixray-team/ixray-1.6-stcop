#include "stdafx.h"

#include "../../xrRHI/RHITextureInterfaces.h"
#include "../xrRender/ResourceManager.h"

#ifndef _EDITOR
#	include "../../xrEngine/Render.h"
#endif

#include "../../xrEngine/tntQAVI.h"
#include "../../xrEngine/xrTheora_Surface.h"

#include "../xrRender/dxRenderDeviceRender.h"

#include "StateManager/dx10ShaderResourceStateCache.h"

#define PRIORITY_HIGH	12
#define PRIORITY_NORMAL	8
#define PRIORITY_LOW	4

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
	flags.seqCycles		= FALSE;
	flags.bLoadedAsStaging = FALSE;
	m_material			= 1.0f;
	bind				= xr_make_delegate(this,&CTexture::apply_load);
}

CTexture::~CTexture()
{
	Unload();
	DEV->_DeleteTexture(this);
}

void CTexture::surface_set(IRHISurface* surf)
{
	if (surf)			surf->AddRef();
	_RELEASE(pSurface);
	_RELEASE(m_pSRView);

	pSurface = surf;
	m_pSRView = 0;

	if (pSurface)
	{
		RHIShaderResourceViewDesc ViewDesc = {};
		ViewDesc.MostDetailedMip = 0;

		u32 type = pSurface->GetTextureType();
		if (D3D_RESOURCE_DIMENSION_TEXTURE2D == type)
		{
			if (pSurface->GetMiscFlags() & D3D_RESOURCE_MISC_TEXTURECUBE)
			{
				ViewDesc.ViewDimension = D3D_SRV_DIMENSION_TEXTURECUBE;
				ViewDesc.MipLevels = pSurface->GetMipLevels();
			}
			else
			{
				if (pSurface->GetSampleDescCount() <= 1)
				{
					ViewDesc.ViewDimension = (pSurface->GetArraySize() > 1) ? D3D_SRV_DIMENSION_TEXTURE2DARRAY : D3D_SRV_DIMENSION_TEXTURE2D;
					ViewDesc.MipLevels = pSurface->GetMipLevels();
				}
				else
				{
					VERIFY(pSurface->GetArraySize() == 1);
					ViewDesc.ViewDimension = D3D_SRV_DIMENSION_TEXTURE2DMS;
					ViewDesc.MipLevels = pSurface->GetMipLevels();
				}
			}

			ViewDesc.Format = ERHI_FORMAT::UNKNOWN;

			switch (pSurface->GetFormat())
			{
			case ERHI_FORMAT::R24G8_TYPELESS:
				ViewDesc.Format = ERHI_FORMAT::R24_UNORM_X8_TYPELESS;
				break;
			case ERHI_FORMAT::R32_TYPELESS:
				ViewDesc.Format = ERHI_FORMAT::R32_FLOAT;
				break;
			}

			ViewDesc.ArraySize = pSurface->GetArraySize();

			if ((pSurface->GetSampleDescCount() <= 1) || (ViewDesc.Format != ERHI_FORMAT::R24_UNORM_X8_TYPELESS))
			{
				ViewDesc.FirstArraySlice = 0;
				m_pSRView = GRHI->CreateShaderResourceView(pSurface, &ViewDesc);
			}
		}
		else
		{
			m_pSRView = GRHI->CreateShaderResourceView(pSurface, nullptr);
		}
	}
}

IRHISurface* CTexture::surface_get()
{
	if (flags.bLoadedAsStaging)
		ProcessStaging();

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
	if (!flags.bLoaded)		Load();
	else					PostLoad();

	bind(dwStage);
}

void CTexture::ProcessStaging()
{
	VERIFY(pSurface);
	VERIFY(flags.bLoadedAsStaging);

	IRHISurface* pTargetSurface = 0;

	u32 type = pSurface->GetTextureType();

	switch (type)
	{
		case D3D_RESOURCE_DIMENSION_TEXTURE2D:
		{
			RHITextureDesc desc;
			desc.Width = pSurface->GetWidth();
			desc.Height = pSurface->GetHeight();
			desc.Depth = 1;
			desc.MipLevels = pSurface->GetMipLevels();
			desc.Format = pSurface->GetFormat();
			desc.Usage = D3D_USAGE_DEFAULT;
			desc.BindFlags = D3D_BIND_SHADER_RESOURCE;
			desc.CPUAccessFlags = 0;
			desc.MiscFlags = pSurface->GetMiscFlags();
			
			pTargetSurface = GRHI->CreateTextureFromMemory(nullptr, 0, desc);
		}
		break;
		case D3D_RESOURCE_DIMENSION_TEXTURE3D:
		{
			RHITextureDesc desc;
			desc.Width = pSurface->GetWidth();
			desc.Height = pSurface->GetHeight();
			desc.Depth = pSurface->GetDepth();
			desc.MipLevels = pSurface->GetMipLevels();
			desc.Format = pSurface->GetFormat();
			desc.Usage = D3D_USAGE_DEFAULT;
			desc.BindFlags = D3D_BIND_SHADER_RESOURCE;
			desc.CPUAccessFlags = 0;
			desc.MiscFlags = pSurface->GetMiscFlags();
			
			pTargetSurface = GRHI->CreateTextureFromMemory(nullptr, 0, desc);
		}
		break;
	default:
		VERIFY(!"CTexture::ProcessStaging unsupported dimensions.");
	}

	GRHI->CopySurface(pTargetSurface, pSurface);

	flags.bLoadedAsStaging = FALSE;

	//	Check if texture was not copied _before_ it was converted.
	pSurface->Release();
	pSurface = 0;

	surface_set(pTargetSurface);

	_RELEASE(pTargetSurface);
}

void CTexture::Apply(u32 dwStage)
{
	if (flags.bLoadedAsStaging)
		ProcessStaging();

	if (dwStage < rstVertex)
	{
		//	Pixel shader stage resources
		SRVSManager.SetPSResource(dwStage, m_pSRView ? (ID3D11ShaderResourceView*)m_pSRView->GetRawSRV() : nullptr);
	}
	else if (dwStage < rstGeometry)
	{
		//	Vertex shader stage resources
		SRVSManager.SetVSResource(dwStage - rstVertex, m_pSRView ? (ID3D11ShaderResourceView*)m_pSRView->GetRawSRV() : nullptr);
	}
	else if (dwStage < rstHull)
	{
		//	Geometry shader stage resources
		SRVSManager.SetGSResource(dwStage - rstGeometry, m_pSRView ? (ID3D11ShaderResourceView*)m_pSRView->GetRawSRV() : nullptr);
	}
	else if (dwStage < rstDomain)
	{
		//	Geometry shader stage resources
		SRVSManager.SetHSResource(dwStage - rstHull, m_pSRView ? (ID3D11ShaderResourceView*)m_pSRView->GetRawSRV() : nullptr);
	}
	else if (dwStage < rstCompute)
	{
		//	Geometry shader stage resources
		SRVSManager.SetDSResource(dwStage - rstDomain, m_pSRView ? (ID3D11ShaderResourceView*)m_pSRView->GetRawSRV() : nullptr);
	}
	else if (dwStage < rstInvalid)
	{
		//	Geometry shader stage resources
		SRVSManager.SetCSResource(dwStage - rstCompute, m_pSRView ? (ID3D11ShaderResourceView*)m_pSRView->GetRawSRV() : nullptr);
	}
	else VERIFY("Invalid stage");
}

void CTexture::apply_theora(u32 dwStage)
{
	if (pTheora->Update(m_play_time != 0xFFFFFFFF ? m_play_time : Device.dwTimeContinual))
	{
		u32 type = pSurface->GetTextureType();
		R_ASSERT(D3D_RESOURCE_DIMENSION_TEXTURE2D == type);
		ID3DTexture2D* T2D = (ID3DTexture2D*)pSurface->GetRawTexture();
		D3D_MAPPED_TEXTURE2D	mapData{};
		RECT rect;
		rect.left = 0;
		rect.top = 0;
		rect.right = pTheora->Width(true);
		rect.bottom = pTheora->Height(true);

		u32 _w = pTheora->Width(false);

		R_CHK(RContext->Map(T2D, 0, D3D_MAP_WRITE_DISCARD, 0, &mapData));

		int DeltaOffset = mapData.RowPitch / int(pTheora->Width(false) * 4);
		_w *= DeltaOffset;

		int _pos = 0;
		pTheora->DecompressFrame((u32*)mapData.pData, _w - rect.right, _pos);
		VERIFY(u32(_pos) == rect.bottom * _w);

		RContext->Unmap(T2D, 0);
	}

	Apply(dwStage);
}

void CTexture::apply_avi(u32 dwStage)
{
	if (pAVI->NeedUpdate())
	{
		D3D_RESOURCE_DIMENSION type = (D3D_RESOURCE_DIMENSION)pSurface->GetTextureType();
		R_ASSERT(D3D_RESOURCE_DIMENSION_TEXTURE2D == type);
		ID3DTexture2D* T2D = (ID3DTexture2D*)pSurface->GetRawTexture();
		D3D_MAPPED_TEXTURE2D mapData{};

		// AVI
		R_CHK(RContext->Map(T2D, 0, D3D_MAP_WRITE_DISCARD, 0, &mapData));
		R_ASSERT(mapData.RowPitch == int(pAVI->m_dwWidth*4));
		BYTE* ptr; pAVI->GetFrame(&ptr);
		CopyMemory(mapData.pData,ptr,pAVI->m_dwWidth*pAVI->m_dwHeight*4);
		RContext->Unmap(T2D, 0);
	}

	Apply(dwStage);
}

void CTexture::apply_seq(u32 dwStage)
{
	// SEQ
	u32	frame = Device.dwTimeContinual / seqMSPF;
	u32	frame_data = (u32)seqDATA.size();

	if (flags.seqCycles)
	{
		u32	frame_id = frame % (frame_data * 2);
		if (frame_id >= frame_data)	frame_id = (frame_data - 1) - (frame_id % frame_data);
		pSurface = seqDATA[frame_id];
		m_pSRView = m_seqSRView[frame_id];
	}
	else
	{
		u32	frame_id = frame % frame_data;
		pSurface = seqDATA[frame_id];
		m_pSRView = m_seqSRView[frame_id];
	}

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

	flags.bLoaded = true;
	if (pSurface)
		return;

	flags.bUser = false;
	flags.MemoryUsage = 0;

	if (0 == _stricmp(*cName, "$null"))
		return;

	if (0 != strstr(*cName, "$user$"))
	{
		flags.bUser = true;
		return;
	}

	Preload();

	bool bCreateView = true;

	// Check for OGM
	string_path			fn;
	if (FS.exist(fn, "$game_textures$", *cName, ".ogm"))
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
			pTheora->Play(TRUE, Device.dwTimeContinual);

			ID3DTexture2D* pTexture = 0;
			u32 _w = pTheora->Width(false);
			u32 _h = pTheora->Height(false);

			// Create RHITextureDesc for the texture
			RHITextureDesc rhiDesc;
			rhiDesc.Width = _w;
			rhiDesc.Height = _h;
			rhiDesc.MipLevels = 1;
			rhiDesc.Format = ERHI_FORMAT::R8G8B8A8_UNORM;
			rhiDesc.Usage = D3D_USAGE_DYNAMIC;
			rhiDesc.BindFlags = D3D_BIND_SHADER_RESOURCE;
			rhiDesc.CPUAccessFlags = D3D_CPU_ACCESS_WRITE;
			rhiDesc.MiscFlags = 0;

			pSurface = GRHI->CreateTextureFromMemory(nullptr, 0, rhiDesc);
			if (pSurface == nullptr)
			{
				FATAL("Invalid video stream");
				xr_delete(pTheora);
				pSurface = 0;
				m_pSRView = 0;
			}
			else
			{
				m_pSRView = GRHI->CreateShaderResourceView(pSurface, nullptr);
			}

		}
	}
	else if (FS.exist(fn, "$game_textures$", *cName, ".avi"))
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
			rhiDesc.Usage = D3D_USAGE_DYNAMIC;
			rhiDesc.BindFlags = D3D_BIND_SHADER_RESOURCE;
			rhiDesc.CPUAccessFlags = D3D_CPU_ACCESS_WRITE;
			rhiDesc.MiscFlags = 0;

			// Use GRHI to create the surface
			pSurface = GRHI->CreateTextureFromMemory(nullptr, 0, rhiDesc);
			if (pSurface == nullptr)
			{
				FATAL("Invalid video stream");
				xr_delete(pAVI);
				pSurface = 0;
				m_pSRView = 0;
			}
			else
			{
				m_pSRView = GRHI->CreateShaderResourceView(pSurface, nullptr);
			}

		}
	}
	else if (FS.exist(fn, "$game_textures$", *cName, ".seq"))
	{
		// Sequence
		string256 buffer;
		IReader* _fs = FS.r_open(fn);

		flags.seqCycles = FALSE;
		_fs->r_string(buffer, sizeof(buffer));
		if (0 == _stricmp(buffer, "cycled"))
		{
			flags.seqCycles = TRUE;
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
				// Load another texture
				u32	mem = 0;
				pSurface = ::RImplementation.texture_load(buffer, mem);
				if (pSurface)
				{
					seqDATA.push_back(pSurface);
					m_seqSRView.push_back(0);
					m_seqSRView.back() = GRHI->CreateShaderResourceView(seqDATA.back(), nullptr);
					flags.MemoryUsage += mem;
				}
			}
		}
		pSurface = 0;
		FS.r_close(_fs);
	}
	else
	{
		// Normal texture
		u32	mem = 0;
		//pSurface = ::RImplementation.texture_load	(*cName,mem);
		pSurface = ::RImplementation.texture_load(*cName, mem, true);

		if (GetUsage() == ERHI_USAGE::USAGE_STAGING)
		{
			flags.bLoadedAsStaging = TRUE;
			bCreateView = false;
		}

		// Calc memory usage and preload into vid-mem
		if (pSurface)
		{
			flags.MemoryUsage = mem;

			if (bCreateView)
			{
				m_pSRView = GRHI->CreateShaderResourceView(pSurface, nullptr);
			}
		}
	}

	PostLoad();
}

void CTexture::Unload()
{
	flags.bLoaded = false;
	flags.bLoadedAsStaging = false;

	if (!seqDATA.empty())
	{
		for (u32 I = 0; I < seqDATA.size(); I++)
		{
			_RELEASE(seqDATA[I]);
			_RELEASE(m_seqSRView[I]);
		}
		seqDATA.clear();
		m_seqSRView.clear();
		pSurface = 0;
		m_pSRView = 0;
	}

	_RELEASE(pSurface);
	_RELEASE(m_pSRView);

	xr_delete(pAVI);
	xr_delete(pTheora);

	bind = xr_make_delegate(this, &CTexture::apply_load);
}

ERHI_USAGE CTexture::GetUsage()
{
	ERHI_USAGE res = ERHI_USAGE::USAGE_DEFAULT;

	if (pSurface)
	{
		res = pSurface->GetUsage();
	}

	return res;
}

IRHIShaderResourceView* CTexture::GetView()
{
	return m_pSRView;
}

void CTexture::video_Play(BOOL looped, u32 _time)
{
	if (pTheora)
	{
		pTheora->Play(looped, (_time != 0xFFFFFFFF) ? (m_play_time = _time) : Device.dwTimeContinual);
	}
}

void CTexture::video_Pause(BOOL state)
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