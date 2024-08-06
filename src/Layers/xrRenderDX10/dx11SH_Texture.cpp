#include "stdafx.h"
#pragma hdrstop

#include "../xrRender/ResourceManager.h"

#ifndef _EDITOR
#include "../../xrEngine/render.h"
#endif

#include "../../xrEngine/tntQAVI.h"
#include "../../xrEngine/xrTheora_Surface.h"

#include "../xrRender/dxRenderDeviceRender.h"

#include "StateManager/dx10ShaderResourceStateCache.h"

#define		PRIORITY_HIGH	12
#define		PRIORITY_NORMAL	8
#define		PRIORITY_LOW	4

void resptrcode_texture::create(LPCSTR _name)
{
	_set(DEV->_CreateTexture(_name));
}


//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
CTexture::CTexture		()
{
	pSurface			= nullptr;
	pAVI				= nullptr;
	pTheora				= nullptr;
	desc_cache			= 0;
	seqMSPF				= 0;
	flags.MemoryUsage	= 0;
	flags.bLoaded		= false;
	flags.bUser			= false;
	flags.seqCycles		= FALSE;
	flags.bLoadedAsStaging = FALSE;
	m_material			= 1.0f;
	bind				= fastdelegate::FastDelegate1<u32>(this,&CTexture::apply_load);
}

CTexture::~CTexture()
{
	Unload				();

	// release external reference
	DEV->_DeleteTexture	(this);
}

void					CTexture::surface_set	(IRHIResource* surf )
{
	if (surf)			surf->AddRef		();
	_RELEASE			(pSurface);

	pSurface			= surf;

	if (pSurface)
	{
		desc_update();

		eResourceDimension	type;
		pSurface->GetType(&type);
		if (RESOURCE_DIMENSION_TEXTURE2D == type )
		{
			D3D_SHADER_RESOURCE_VIEW_DESC ViewDesc { };
			
			if (desc.IsTextureCube)
			{
				ViewDesc.ViewDimension = D3D_SRV_DIMENSION_TEXTURECUBE;
				ViewDesc.TextureCube.MostDetailedMip = 0;
				ViewDesc.TextureCube.MipLevels = desc.MipLevels;
			}
			else
			{
				// NO SAMPLING #TODO: !!!
				//if(desc.SampleDesc.Count <= 1)
				//{
				//	ViewDesc.ViewDimension = (desc.ArraySize > 1) ? D3D_SRV_DIMENSION_TEXTURE2DARRAY : D3D_SRV_DIMENSION_TEXTURE2D;
				//	ViewDesc.Texture2D.MostDetailedMip = 0;
				//	ViewDesc.Texture2D.MipLevels = desc.MipLevels;
				//}
				//else
				{
					VERIFY(desc.ArraySize == 1);
					ViewDesc.ViewDimension = D3D_SRV_DIMENSION_TEXTURE2DMS;
					ViewDesc.Texture2D.MostDetailedMip = 0;
					ViewDesc.Texture2D.MipLevels = desc.MipLevels;
				}
			}			

			ViewDesc.Format = DXGI_FORMAT_UNKNOWN;

			switch(desc.Format)
			{
			case DXGI_FORMAT_R24G8_TYPELESS:
				ViewDesc.Format = DXGI_FORMAT_R24_UNORM_X8_TYPELESS;
				break;
			case DXGI_FORMAT_R32_TYPELESS:
				ViewDesc.Format = DXGI_FORMAT_R32_FLOAT;
				break;
			}
			
			if (desc.ArraySize > 1)
                ViewDesc.Texture2DArray.ArraySize = desc.ArraySize;
		}
		
	}	
}

IRHIResource*	CTexture::surface_get	()
{
	if (flags.bLoadedAsStaging)
		ProcessStaging();

	if (pSurface)
		pSurface->AddRef();

	return pSurface;
}

ID3D11ShaderResourceView* CTexture::get_SRView()
{
	ID3DShaderResourceView* pSRView = nullptr;
	reinterpret_cast<ITexture2D*>(pSurface)->GetShaderResourceView((IShaderResourceView**)&pSRView);
	return pSRView;
}

void CTexture::PostLoad	()
{
	if (pTheora)				bind		= fastdelegate::FastDelegate1<u32>(this,&CTexture::apply_theora);
	else if (pAVI)				bind		= fastdelegate::FastDelegate1<u32>(this,&CTexture::apply_avi);
	else if (!seqDATA.empty())	bind		= fastdelegate::FastDelegate1<u32>(this,&CTexture::apply_seq);
	else						bind		= fastdelegate::FastDelegate1<u32>(this,&CTexture::apply_normal);
}

void CTexture::apply_load	(u32 dwStage)	{
	if (!flags.bLoaded)		Load			()	;
	else					PostLoad		()	;
	bind					(dwStage)			;
};

void CTexture::ProcessStaging()
{
#if 0
	VERIFY(pSurface);
	VERIFY(flags.bLoadedAsStaging);

	IRHIResource* pTargetSurface = 0;

	eResourceDimension type;
	pSurface->GetType(&type);

	switch (type)
	{
	case RESOURCE_DIMENSION_TEXTURE2D:
		{
			ID3DTexture2D*	T	= (ID3DTexture2D*)pSurface;
			D3D_TEXTURE2D_DESC TexDesc;
			T->GetDesc(&TexDesc);
			TexDesc.Usage = D3D_USAGE_DEFAULT;
			TexDesc.BindFlags = D3D_BIND_SHADER_RESOURCE;
			TexDesc.CPUAccessFlags = 0;

			T = 0;

			CHK_DX(RDevice->CreateTexture2D( &TexDesc,       // Texture desc
				nullptr,                  // Initial data
				&T )); // [out] Texture

			pTargetSurface = T;
		}
		break;
	case RESOURCE_DIMENSION_TEXTURE3D:
		{
			ID3DTexture3D*	T	= (ID3DTexture3D*)pSurface;
			D3D_TEXTURE3D_DESC TexDesc;
			T->GetDesc(&TexDesc);
			TexDesc.Usage = D3D_USAGE_DEFAULT;
			TexDesc.BindFlags = D3D_BIND_SHADER_RESOURCE;
			TexDesc.CPUAccessFlags = 0;

			T = 0;

			CHK_DX(RDevice->CreateTexture3D( &TexDesc,       // Texture desc
				nullptr,                  // Initial data
				&T )); // [out] Texture

			pTargetSurface = T;
		}
		break;
	default:
		VERIFY(!"CTexture::ProcessStaging unsupported dimensions.");
	}

	RContext->CopyResource(pTargetSurface, pSurface);
	/*
	for( int i=0; i<iNumSubresources; ++i)
	{
		RDevice->CopySubresourceRegion(
			pTargetSurface,
			i,
			0,
			0,
			0,
			pSurface,
			i,
			0
			);
	}
	*/
	

	flags.bLoadedAsStaging = FALSE;

	//	Check if texture was not copied _before_ it was converted.
	ULONG RefCnt = pSurface->Release();
	pSurface = 0;

	VERIFY(!RefCnt);

	surface_set(pTargetSurface);

	_RELEASE(pTargetSurface);
#endif
}

void CTexture::Apply(u32 dwStage)
{
	flags.bLoadedAsStaging = false;
	if (flags.bLoadedAsStaging)
		ProcessStaging();

	ID3DShaderResourceView* pSRView = nullptr;
	reinterpret_cast<ITexture2D*>(pSurface)->GetShaderResourceView((IShaderResourceView**)&pSRView);

	if (dwStage<rstVertex)	//	Pixel shader stage resources
	{
		SRVSManager.SetPSResource(dwStage, pSRView);
	}
	else if (dwStage<rstGeometry)	//	Vertex shader stage resources
	{
		SRVSManager.SetVSResource(dwStage-rstVertex, pSRView);
	}
	else if (dwStage<rstHull)	//	Geometry shader stage resources
	{
		SRVSManager.SetGSResource(dwStage-rstGeometry, pSRView);
	}
	else if (dwStage<rstDomain)	//	Geometry shader stage resources
	{
		SRVSManager.SetHSResource(dwStage-rstHull, pSRView);
	}
	else if (dwStage<rstCompute)	//	Geometry shader stage resources
	{
		SRVSManager.SetDSResource(dwStage-rstDomain, pSRView);
	}
	else if (dwStage<rstInvalid)	//	Geometry shader stage resources
	{
		SRVSManager.SetCSResource(dwStage-rstCompute, pSRView);
	}
	else
	{
		VERIFY("Invalid stage");
	}
}

void CTexture::apply_theora(u32 dwStage)
{
	if (pTheora->Update(m_play_time!=0xFFFFFFFF?m_play_time:Device.dwTimeContinual))
	{
		eResourceDimension	type;
		pSurface->GetType(&type);
		R_ASSERT(D3D_RESOURCE_DIMENSION_TEXTURE2D == type);
		ITexture2D*	T2D		= (ITexture2D*)pSurface;
		SMappedSubresource	mapData{};
		RECT rect;
		rect.left			= 0;
		rect.top			= 0;
		rect.right			= pTheora->Width(true);
		rect.bottom			= pTheora->Height(true);

		u32 _w				= pTheora->Width(false);

		//R_CHK				(T2D->LockRect(0,&R,&rect,0));
		//R_CHK				(RContext->Map(T2D, 0, D3D_MAP_WRITE_DISCARD, 0, &mapData));
		T2D->Map(0, MAPPING_WRITE_DISCARD, 0, &mapData);

		//R_ASSERT			(R.Pitch == int(pTheora->Width(false)*4));
		//R_ASSERT			(mapData.RowPitch == int(pTheora->Width(false)*4));

		int DeltaOffset = mapData.RowPitch / int(pTheora->Width(false) * 4);
		_w *= DeltaOffset;

		int _pos = 0;
		pTheora->DecompressFrame((u32*)mapData.pData, _w - rect.right, _pos);
		VERIFY(u32(_pos) == rect.bottom*_w);

		T2D->Unmap(0);
	}
	Apply(dwStage);
	//CHK_DX(RDevice->SetTexture(dwStage,pSurface));
};
void CTexture::apply_avi	(u32 dwStage)	
{
	if (pAVI->NeedUpdate()){
		eResourceDimension	type;
		pSurface->GetType(&type);
		R_ASSERT(RESOURCE_DIMENSION_TEXTURE2D == type);
		ITexture2D*	T2D	= (ITexture2D*)pSurface;
		SMappedSubresource mapData{};

		// AVI
		T2D->Map(0, MAPPING_WRITE_DISCARD, 0, &mapData);
		R_ASSERT(mapData.RowPitch == int(pAVI->m_dwWidth*4));
		BYTE* ptr; pAVI->GetFrame(&ptr);
		CopyMemory(mapData.pData,ptr,pAVI->m_dwWidth*pAVI->m_dwHeight*4);
		T2D->Unmap(0);
	}

	Apply(dwStage);
};
void CTexture::apply_seq	(u32 dwStage)	{
	// SEQ
	u32	frame		= Device.dwTimeContinual/seqMSPF; //Device.dwTimeGlobal
	u32	frame_data	= (u32)seqDATA.size();
	if (flags.seqCycles)		{
		u32	frame_id	= frame%(frame_data*2);
		if (frame_id>=frame_data)	frame_id = (frame_data-1) - (frame_id%frame_data);
		pSurface 			= seqDATA[frame_id];
	} else {
		u32	frame_id	= frame%frame_data;
		pSurface 			= seqDATA[frame_id];
	}

	Apply(dwStage);
};
void CTexture::apply_normal	(u32 dwStage)	{
	Apply(dwStage);
};

void CTexture::Preload	()
{
	m_bumpmap = DEV->m_textures_description.GetBumpName(cName);
	m_material = DEV->m_textures_description.GetMaterial(cName);
}

void CTexture::Load		()
{
	flags.bLoaded					= true;
	desc_cache						= 0;
	if (pSurface)					return;

	flags.bUser						= false;
	flags.MemoryUsage				= 0;
	if (0==_stricmp(*cName,"$null"))	return;
	if (0!=strstr(*cName,"$user$"))	{
		flags.bUser	= true;
		return;
	}

	Preload							();

	bool	bCreateView = true;

	// Check for OGM
	string_path			fn;
	if (FS.exist(fn,"$game_textures$",*cName,".ogm")){
		// AVI
		pTheora		= new CTheoraSurface();
		m_play_time	= 0xFFFFFFFF;

		if (!pTheora->Load(fn)) {
			xr_delete(pTheora);
			FATAL				("Can't open video stream");
		} else {
			flags.MemoryUsage	= pTheora->Width(true)*pTheora->Height(true)*4;
			pTheora->Play		(TRUE,Device.dwTimeContinual);

			// Now create texture
			ITexture2D*	pTexture = 0;
			u32 _w = pTheora->Width(false);
			u32 _h = pTheora->Height(false);

//			HRESULT hrr = RDevice->CreateTexture(
//				_w, _h, 1, 0, D3DFMT_A8R8G8B8, D3DPOOL_MANAGED, &pTexture, nullptr );
			STexture2DDesc	desc_;
			desc_.Width = _w;
			desc_.Height = _h;
			desc_.MipLevels = 1;
			desc_.ArraySize = 1;
			desc_.Format = FMT_B8G8R8A8; //DXGI_FORMAT_R8G8B8A8_UNORM;
			desc_.Usage = USAGE_DYNAMIC;
			pTexture = g_RenderRHI->CreateTexture2D(desc_, 0);

			pSurface = pTexture;
			if (!pTexture)
			{
				FATAL		("Invalid video stream");
				xr_delete	(pTheora);
				pSurface	= 0;

			}
		}
	} else
		if (FS.exist(fn,"$game_textures$",*cName,".avi")){
			// AVI
			pAVI = new CAviPlayerCustom();

			if (!pAVI->Load(fn)) {
				xr_delete(pAVI);
				FATAL				("Can't open video stream");
			} else {
				flags.MemoryUsage	= pAVI->m_dwWidth*pAVI->m_dwHeight*4;

				// Now create texture
				ITexture2D*	pTexture = 0;
				//HRESULT hrr = RDevice->CreateTexture(
				//pAVI->m_dwWidth,pAVI->m_dwHeight,1,0,D3DFMT_A8R8G8B8,D3DPOOL_MANAGED,
				//	&pTexture,nullptr
				//	);
				STexture2DDesc	desc_;
				desc_.Width = pAVI->m_dwWidth;
				desc_.Height = pAVI->m_dwHeight;
				desc_.MipLevels = 1;
				desc_.ArraySize = 1;
				desc_.Format = FMT_B8G8R8A8;//DXGI_FORMAT_R8G8B8A8_UNORM;
				desc_.Usage = USAGE_DYNAMIC;
	
				pTexture = g_RenderRHI->CreateTexture2D(desc_, 0);

				pSurface	= pTexture;
				if (!pTexture)
				{
					FATAL		("Invalid video stream");
					xr_delete	(pAVI);
					pSurface = 0;
				}
			}
		} else
			if (FS.exist(fn,"$game_textures$",*cName,".seq"))
			{
				// Sequence
				string256 buffer;
				IReader* _fs		= FS.r_open(fn);

				flags.seqCycles	= FALSE;
				_fs->r_string	(buffer,sizeof(buffer));
				if (0==_stricmp	(buffer,"cycled"))
				{
					flags.seqCycles	= TRUE;
					_fs->r_string	(buffer,sizeof(buffer));
				}
				u32 fps	= atoi(buffer);
				seqMSPF		= 1000/fps;

				while (!_fs->eof())
				{
					_fs->r_string(buffer,sizeof(buffer));
					_Trim		(buffer);
					if (buffer[0])	
					{
						// Load another texture
						u32	mem  = 0;
						pSurface = ::RImplementation.texture_load	(buffer,mem);
						if (pSurface)	
						{
							// pSurface->SetPriority	(PRIORITY_LOW);
							seqDATA.push_back(pSurface);
							flags.MemoryUsage		+= mem;
						}
					}
				}
				pSurface	= 0;
				FS.r_close	(_fs);
			} 
			else
			{
				// Normal texture
				u32	mem  = 0;

				pSurface = ::RImplementation.texture_load	(*cName,mem, true);
				pSurface->SetDebugName(*cName);

				if (GetUsage() == D3D_USAGE_STAGING)
				{
					flags.bLoadedAsStaging = TRUE;
					bCreateView = false;
				}

				// Calc memory usage and preload into vid-mem
				if (pSurface) 
				{
					flags.MemoryUsage		=	mem;
				}
		
			}

			PostLoad	()		;
}

void CTexture::Unload	()
{
#ifdef DEBUG
	string_path				msg_buff;
	xr_sprintf				(msg_buff,sizeof(msg_buff),"* Unloading texture [%s] pSurface RefCount=",cName.c_str());
#endif // DEBUG

	//.	if (flags.bLoaded)		Msg		("* Unloaded: %s",cName.c_str());

	flags.bLoaded			= FALSE;
	flags.bLoadedAsStaging	= FALSE;
	if (!seqDATA.empty())	{
		for (u32 I=0; I<seqDATA.size(); I++)
		{
			_RELEASE	(seqDATA[I]);
		}
		seqDATA.clear();
		pSurface	= 0;
	}

#ifdef DEBUG
	_SHOW_REF		(msg_buff, pSurface);
#endif // DEBUG
	_RELEASE		(pSurface);

	xr_delete		(pAVI);
	xr_delete		(pTheora);

	bind			= fastdelegate::FastDelegate1<u32>(this,&CTexture::apply_load);
}

void CTexture::desc_update	()
{
	desc_cache	= pSurface;
	if (pSurface)
	{
		eResourceDimension	type;
		pSurface->GetType(&type);
		if (RESOURCE_DIMENSION_TEXTURE2D == type)
		{
			ITexture2D*	T = (ITexture2D*)pSurface;
			T->GetDesc(&desc);
		}
	}
}

eResourceUsage CTexture::GetUsage()
{
	eResourceUsage res = USAGE_DEFAULT;

	if (pSurface)
	{
		eResourceDimension	type;
		pSurface->GetType(&type);
		switch(type)
		{
#if 0
		case RESOURCE_DIMENSION_TEXTURE1D:

			{
				ID3DTexture1D*	T	= (ID3DTexture1D*)pSurface;
				D3D_TEXTURE1D_DESC	descr;
				T->GetDesc(&descr);
				res = descr.Usage;
			}
#endif
			break;

		case RESOURCE_DIMENSION_TEXTURE2D:
			{
				ITexture2D*	T	= (ITexture2D*)pSurface;
				STexture2DDesc	descr;
				T->GetDesc(&descr);
				res = descr.Usage;
			}
			break;
#if 0
		case RESOURCE_DIMENSION_TEXTURE3D:

			{
				ID3DTexture3D*	T	= (ID3DTexture3D*)pSurface;
				D3D_TEXTURE3D_DESC	descr;
				T->GetDesc(&descr);
				res = descr.Usage;
			}
#endif
			break;

		default:
			VERIFY(!"Unknown texture format???");
		}
	}

	return res;
}

void CTexture::video_Play		(BOOL looped, u32 _time)	
{ 
	if (pTheora) pTheora->Play	(looped,(_time!=0xFFFFFFFF)?(m_play_time=_time):Device.dwTimeContinual); 
}

void CTexture::video_Pause		(BOOL state)
{
	if (pTheora) pTheora->Pause	(state); 
}

void CTexture::video_Stop			()				
{ 
	if (pTheora) pTheora->Stop(); 
}

BOOL CTexture::video_IsPlaying	()				
{ 
	return (pTheora)?pTheora->IsPlaying():FALSE; 
}
