#pragma once

//////////////////////////////////////////////////////////////////////////
class		CRT		:	public xr_resource_named	{
public:
	CRT();
	~CRT();
#ifdef USE_DX11
	void	create(LPCSTR Name, u32 w, u32 h, ERHITextureFormat f, u32 SampleCount = 1, bool useUAV = false );
#else
	void	create(LPCSTR Name, u32 w, u32 h, ERHITextureFormat f, u32 SampleCount = 1 );
#endif
	void	destroy();
	void	reset_begin();
	void	reset_end();
	IC BOOL	valid()	{ return !!pTexture; }

public:
	IRHITexture*			pSurface;
	IRHISurface*			pRT;
	IRHIDepthStencilView*	pZRT;
#ifdef USE_DX11
	ID3D11UnorderedAccessView*	pUAView;
#endif //USE_DX11
	ref_texture				pTexture;

	u32						dwWidth;
	u32						dwHeight;

	ERHITextureFormat fmt;

	u64						_order;
};
struct 		resptrcode_crt	: public resptr_base<CRT>
{
#ifdef USE_DX11
	void				create			(LPCSTR Name, u32 w, u32 h, ERHITextureFormat f, u32 SampleCount = 1, bool useUAV = false );
#else
	void				create			(LPCSTR Name, u32 w, u32 h, ERHITextureFormat f, u32 SampleCount = 1);
#endif
	void				destroy			()	{ _set(NULL);		}
};
typedef	resptr_core<CRT,resptrcode_crt>		ref_rt;
