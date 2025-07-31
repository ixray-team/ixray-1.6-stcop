#pragma once

//////////////////////////////////////////////////////////////////////////
class		CRT		:	public xr_resource_named	{
public:
	CRT();
	~CRT();
#ifdef USE_DX11
	enum CRTCreationFlags
	{
		USE_UAV_FLAG = u32(1 << 0),
		MIPPED_RT_FLAG = u32(1 << 1)
	};
	void	create(LPCSTR Name, u32 w, u32 h, DxgiFormat f, u32 SampleCount = 1, CRT::CRTCreationFlags CreationFlags = (CRT::CRTCreationFlags)NULL);
#else
	void	create(LPCSTR Name, u32 w, u32 h, D3DFORMAT f, u32 SampleCount = 1);
#endif
	void	destroy();
	void	reset_begin();
	void	reset_end();

	IC BOOL	valid() {
		return !!pTexture;
	}

public:
	ID3DTexture2D*			pSurface;
	ID3DRenderTargetView*	pRT;

	xr_vector<ID3DRenderTargetView*> pMippedRT;
#ifdef USE_DX11
	ID3DDepthStencilView*	pZRT;
	ID3D11UnorderedAccessView*	pUAView;
#endif //USE_DX11
	ref_texture				pTexture;

	u32						dwWidth;
	u32						dwHeight;

#ifdef USE_DX11
	DxgiFormat				fmt;
#else
	D3DFORMAT				fmt;
#endif

	u64						_order;
};

struct resptrcode_crt : public resptr_base<CRT> {
#ifdef USE_DX11
	void				create(LPCSTR Name, u32 w, u32 h, DxgiFormat f, u32 SampleCount = 1, CRT::CRTCreationFlags CreationFlags = (CRT::CRTCreationFlags)NULL);
#else
	void				create(LPCSTR Name, u32 w, u32 h, D3DFORMAT f, u32 SampleCount = 1);
#endif
	void				destroy() {
		_set(NULL);
	}
};
typedef	resptr_core<CRT, resptrcode_crt> ref_rt;

#ifdef USE_DX11
//////////////////////////////////////////////////////////////////////////
class		CRTC	:	public xr_resource_named	{
public:
	ID3DTexture2D*			pSurface;
	ID3DRenderTargetView*	pRT[6];
	ref_texture				pTexture;

	u32						dwSize;

	DxgiFormat				fmt;

	u64						_order;

	CRTC					();
	~CRTC					();

	void				create(LPCSTR name, u32 size, DxgiFormat f, CRT::CRTCreationFlags CreationFlags = (CRT::CRTCreationFlags)NULL);
	void				destroy			();
	void				reset_begin		();
	void				reset_end		();
	IC BOOL				valid			()	{ return !pTexture; }
};

struct 		resptrcode_crtc	: public resptr_base<CRTC>
{
	void				create(LPCSTR Name, u32 size, DxgiFormat f, CRT::CRTCreationFlags CreationFlags = (CRT::CRTCreationFlags)NULL);
	void				destroy			()	{ _set(NULL);		}
};

typedef	resptr_core<CRTC,resptrcode_crtc> ref_rtc;
#endif