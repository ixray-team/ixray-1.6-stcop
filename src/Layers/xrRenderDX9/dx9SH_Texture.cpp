#include "stdafx.h"


#include "../xrRender/ResourceManager.h"

#ifndef _EDITOR
#include "../../xrEngine/Render.h"
#endif

#include "../../xrEngine/tntQAVI.h"
#include "../../xrEngine/xrTheora_Surface.h"

#include "../xrRender/dxRenderDeviceRender.h"

#define		PRIORITY_HIGH	12
#define		PRIORITY_NORMAL	8
#define		PRIORITY_LOW	4



void ECORE_API resptrcode_texture::create(LPCSTR _name)
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
	seqMSPF				= 0;
	flags.MemoryUsage	= 0;
	flags.bLoaded		= false;
	flags.bUser			= false;
	flags.seqCycles		= FALSE;
	m_material			= 1.0f;
	bind				= xr_make_delegate(this,&CTexture::apply_load);
}

CTexture::~CTexture()
{
	Unload				();

	// release external reference
	DEV->_DeleteTexture	(this);
}

// RHI interface methods
void CTexture::surface_set(IRHISurface* surf)
{
	if (surf) surf->AddRef();
	_RELEASE(pSurface);
	pSurface = surf;
}

IRHISurface* CTexture::surface_get()
{
	if (pSurface) pSurface->AddRef();
	return pSurface;
}

void CTexture::PostLoad()
{
	if (pTheora)				bind = xr_make_delegate(this, &CTexture::apply_theora);
	else if (pAVI)				bind = xr_make_delegate(this, &CTexture::apply_avi);
	else if (!seqDATA.empty())	bind = xr_make_delegate(this, &CTexture::apply_seq);
	else						bind = xr_make_delegate(this, &CTexture::apply_normal);
}

void CTexture::apply_load	(u32 dwStage)	{
	if (!flags.bLoaded)		Load			()	;
	else					PostLoad		()	;
	bind					(dwStage)			;
};

void CTexture::apply_theora	(u32 dwStage)
{
	if (pTheora->Update(m_play_time!=0xFFFFFFFF?m_play_time:RDEVICE.dwTimeContinual))
    {
		R_ASSERT(D3DRTYPE_TEXTURE == pSurface->GetTextureType());

		u32 RowPitch = 0;
		void* lockedPtr = pSurface->Lock(0, &RowPitch);
		RECT rect{};
		rect.left = 0;
		rect.top = 0;
		rect.right = pTheora->Width(true);
		rect.bottom = pTheora->Height(true);

		u32 _w = pTheora->Width(false);

		R_ASSERT(int(RowPitch) == int(pTheora->Width(false) * 4));
		int _pos = 0;
		pTheora->DecompressFrame((u32*)lockedPtr, _w - rect.right, _pos);
		VERIFY(u32(_pos) == rect.bottom * _w);
		pSurface->Unlock();
	}
	CHK_DX(RDevice->SetTexture(dwStage, (IDirect3DBaseTexture9*)pSurface->GetRawTexture()));
}

void CTexture::apply_avi(u32 dwStage)	
{
	if (pAVI->NeedUpdate())
	{
		R_ASSERT(D3DRTYPE_TEXTURE == pSurface->GetTextureType());
		u32 RowPitch = 0;
		void* lockedPtr = pSurface->Lock(0, &RowPitch);
		R_ASSERT(int(RowPitch) == int(pAVI->m_dwWidth * 4));
		BYTE* ptr; pAVI->GetFrame(&ptr);
		CopyMemory(lockedPtr, ptr, pAVI->m_dwWidth * pAVI->m_dwHeight * 4);
		pSurface->Unlock();
	}
	CHK_DX(RDevice->SetTexture(dwStage, (IDirect3DBaseTexture9*)pSurface->GetRawTexture()));
}

void CTexture::apply_seq(u32 dwStage)
{
	// SEQ
	u32	frame		=RDEVICE.dwTimeContinual/seqMSPF; //RDEVICE.dwTimeGlobal
	u32	frame_data	= (u32)seqDATA.size();
	if (flags.seqCycles)
	{
		u32	frame_id	= frame%(frame_data*2);
		if (frame_id>=frame_data)	frame_id = (frame_data-1) - (frame_id%frame_data);
		// Create RHITextureDesc for the sequence texture
		RHITextureDesc rhiDesc;
		rhiDesc.Width = 1;  // Will be set properly by the texture
		rhiDesc.Height = 1;
		rhiDesc.Depth = 1;
		rhiDesc.MipLevels = 1;
		rhiDesc.Format = ERHI_FORMAT::B8G8R8A8_UNORM;
		rhiDesc.CPUAccessFlags = 0;
		rhiDesc.MiscFlags = 0;
		
		// Use GRHI to create the surface from sequence data
		pSurface = GRHI->CreateTextureFromMemory(seqDATA[frame_id]->GetRawTexture(), 0, rhiDesc);
	}
	else
	{
		u32	frame_id	= frame%frame_data;
		// Create RHITextureDesc for the sequence texture
		RHITextureDesc rhiDesc;
		rhiDesc.Width = 1;  // Will be set properly by the texture
		rhiDesc.Height = 1;
		rhiDesc.Depth = 1;
		rhiDesc.MipLevels = 1;
		rhiDesc.Format = ERHI_FORMAT::B8G8R8A8_UNORM;
		rhiDesc.CPUAccessFlags = 0;
		rhiDesc.MiscFlags = 0;
		
		// Use GRHI to create the surface from sequence data
		pSurface = GRHI->CreateTextureFromMemory(seqDATA[frame_id]->GetRawTexture(), 0, rhiDesc);
	}

	CHK_DX(RDevice->SetTexture(dwStage, pSurface ? (IDirect3DBaseTexture9*)pSurface->GetRawTexture() : nullptr));
};

void CTexture::apply_normal	(u32 dwStage)
{
	CHK_DX(RDevice->SetTexture(dwStage, pSurface ? (IDirect3DBaseTexture9*)pSurface->GetRawTexture() : nullptr));
};

void CTexture::Preload	()
{
	m_bumpmap = DEV->m_textures_description.GetBumpName(cName);
	m_material = DEV->m_textures_description.GetMaterial(cName);
}

void CTexture::Load		()
{
	PROF_EVENT("CTexture::Load");
	flags.bLoaded					= true;
	if (pSurface)					return;

	flags.bUser						= false;
	flags.MemoryUsage				= 0;
	if (0==_stricmp(*cName,"$null"))	return;
	if (0!=strstr(*cName,"$user$"))	
	{
		flags.bUser	= true;
		return;
	}

	Preload							();
	if (!g_dedicated_server)
	{
		// Check for OGM
		string_path			fn;
		if (FS.exist(fn,"$game_textures$",*cName,".ogm"))
		{
			// AVI
			pTheora		= new CTheoraSurface();
			m_play_time	= 0xFFFFFFFF;

			if (!pTheora->Load(fn)) 
			{
				xr_delete(pTheora);
				FATAL				("Can't open video stream");
			} 
			else 
			{
				flags.MemoryUsage	= pTheora->Width(true)*pTheora->Height(true)*4;
				BOOL bstop_at_end	= (0!=strstr(cName.c_str(), "intro\\")) || (0!=strstr(cName.c_str(), "outro\\"));
				pTheora->Play		(!bstop_at_end, RDEVICE.dwTimeContinual);

				// Now create texture
				ID3DTexture2D*	pTexture = 0;
				u32 _w = pTheora->Width(false);
				u32 _h = pTheora->Height(false);

				HRESULT hrr = RDevice->CreateTexture(
					_w, _h, 1, 0, D3DFMT_A8R8G8B8, D3DPOOL_MANAGED, &pTexture, nullptr );

				// Create RHITextureDesc for the texture
				RHITextureDesc rhiDesc;
				rhiDesc.Width = _w;
				rhiDesc.Height = _h;
				rhiDesc.Depth = 1;
				rhiDesc.MipLevels = 1;
				rhiDesc.Format = ERHI_FORMAT::B8G8R8A8_UNORM;
				rhiDesc.CPUAccessFlags = 0;
				rhiDesc.MiscFlags = 0;
				
				// Use GRHI to create the surface
				pSurface = GRHI->CreateTextureFromMemory(pTexture, 0, rhiDesc);
				if (FAILED(hrr))
				{
					FATAL		("Invalid video stream");
					R_CHK		(hrr);
					xr_delete	(pTheora);
					pSurface	= 0;
				}

			}
		} 
		else if (FS.exist(fn,"$game_textures$",*cName,".avi"))
		{
			// AVI
			pAVI = new CAviPlayerCustom();

			if (!pAVI->Load(fn)) 
			{
				xr_delete(pAVI);
				FATAL				("Can't open video stream");
			} 
			else 
			{
				flags.MemoryUsage	= pAVI->m_dwWidth*pAVI->m_dwHeight*4;

				// Now create texture
				ID3DTexture2D*	pTexture = 0;
				HRESULT hrr = RDevice->CreateTexture
				(
					pAVI->m_dwWidth,pAVI->m_dwHeight,1,0,D3DFMT_A8R8G8B8,D3DPOOL_MANAGED,
					&pTexture,nullptr
				);

				// Use GRHI to create the surface
				pSurface = GRHI->CreateTextureFromMemory(pTexture, 0, {});
				if (FAILED(hrr))
				{
					FATAL		("Invalid video stream");
					R_CHK		(hrr);
					xr_delete	(pAVI);
					pSurface = 0;
				}

			}
		} 
		else if (FS.exist(fn,"$game_textures$",*cName,".seq"))
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
					IDirect3DBaseTexture9* baseTexture = ::RImplementation.texture_load(buffer,mem);
					if (baseTexture) {
						// Create RHITextureDesc for the loaded texture
						RHITextureDesc rhiDesc;
						rhiDesc.Width = 1;  // Will be set properly by the texture
						rhiDesc.Height = 1;
						rhiDesc.Depth = 1;
						rhiDesc.MipLevels = 1;
						rhiDesc.Format = ERHI_FORMAT::B8G8R8A8_UNORM;
						rhiDesc.CPUAccessFlags = 0;
						rhiDesc.MiscFlags = 0;
						
						// Use GRHI to create the surface from loaded texture
						pSurface = GRHI->CreateTextureFromMemory(baseTexture, 0, rhiDesc);
					}
					if (pSurface)	
					{
						// pSurface->SetPriority	(PRIORITY_LOW);
						seqDATA.push_back		(pSurface);
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
			IDirect3DBaseTexture9* baseTexture = ::RImplementation.texture_load(*cName,mem);
			if (baseTexture) {
				// Create RHITextureDesc for the loaded texture
				RHITextureDesc rhiDesc;
				rhiDesc.Width = 1;  // Will be set properly by the texture
				rhiDesc.Height = 1;
				rhiDesc.Depth = 1;
				rhiDesc.MipLevels = 1;
				rhiDesc.Format = ERHI_FORMAT::B8G8R8A8_UNORM;
				rhiDesc.CPUAccessFlags = 0;
				rhiDesc.MiscFlags = 0;
				
				// Use GRHI to create the surface from loaded texture
				pSurface = GRHI->CreateTextureFromMemory(baseTexture, 0, rhiDesc);
			}

			// Calc memory usage and preload into vid-mem
			if (pSurface) {
				// pSurface->SetPriority	(PRIORITY_NORMAL);
				flags.MemoryUsage		=	mem;
			}
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
	if (!seqDATA.empty())	{
		for (u32 I=0; I<seqDATA.size(); I++)
		{
			_RELEASE	(seqDATA[I]);
		}
		seqDATA.clear();
		pSurface	= 0;
	}
	flags.MemoryUsage = 0;

#ifdef DEBUG
	_SHOW_REF		(msg_buff, pSurface);
#endif // DEBUG

	_RELEASE								(pSurface);

	xr_delete		(pAVI);
	xr_delete		(pTheora);

	bind			= xr_make_delegate(this,&CTexture::apply_load);
}

void CTexture::video_Play		(BOOL looped, u32 _time)	
{ 
	if (pTheora) pTheora->Play	(looped,(_time!=0xFFFFFFFF)?(m_play_time=_time):RDEVICE.dwTimeContinual);
}

void CTexture::video_Pause		(BOOL state)
{
	if (pTheora) pTheora->Pause	(state); 
}

void CTexture::video_Stop			()				
{ 
	if (pTheora) pTheora->Stop(); 
}

bool CTexture::video_IsPlaying()
{
	return (pTheora) ? pTheora->IsPlaying() : false;
}

IRHIShaderResourceView* CTexture::GetView()
{
	return m_pSRView;
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
