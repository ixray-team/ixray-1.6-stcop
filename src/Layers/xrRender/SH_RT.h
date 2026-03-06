#pragma once

//////////////////////////////////////////////////////////////////////////
class ECORE_API CRT :
	public xr_resource_named
{
public:
	CRT();
	~CRT();

	enum CRTCreationFlags
	{
		USE_UAV_FLAG = u32(1 << 0),
		MIPPED_RT_FLAG = u32(1 << 1),
		AUTOGEN_MIP_MAPS = u32(1 << 2),
	};

	void create(const char* Name, u32 w, u32 h, ERHI_FORMAT f, u32 SampleCount = 1, CRT::CRTCreationFlags CreationFlags = (CRT::CRTCreationFlags)0);
	void destroy();
	void reset_begin();
	void reset_end();

	IC bool	valid()
	{
		return !!pSurface;
	}

public:
	IRHISurface* pSurface;
	IRHIRenderTargetView* pRT;

	xr_vector<IRHIRenderTargetView*> pMippedRT;
	IRHIDepthStencilView* pZRT;
	IRHIUnorderedAccessView* pUAView;

	ref_texture pTexture;

	u32 dwWidth;
	u32 dwHeight;

	ERHI_FORMAT fmt;

	u64 _order;
};

struct resptrcode_crt :
	public resptr_base<CRT>
{
	void create(const char* Name, u32 w, u32 h, ERHI_FORMAT f, u32 SampleCount = 1, CRT::CRTCreationFlags CreationFlags = (CRT::CRTCreationFlags)0);
	void destroy()
	{
		_set(nullptr);
	}
};
typedef	resptr_core<CRT, resptrcode_crt> ref_rt;

#ifdef USE_DX11
//////////////////////////////////////////////////////////////////////////
class CRTC:
	public xr_resource_named
{
public:
	IRHISurface*			pSurface;
	IRHIRenderTargetView*	pRT[6];
	ref_texture				pTexture;

	u32						dwSize;

	ERHI_FORMAT				fmt;

	u64						_order;

	CRTC					();
	~CRTC					();

	void				create(const char* name, u32 size, ERHI_FORMAT f, CRT::CRTCreationFlags CreationFlags = (CRT::CRTCreationFlags)0);
	void				destroy			();
	void				reset_begin		();
	void				reset_end		();
	IC bool				valid			()	{ return !pTexture; }
};

struct resptrcode_crtc:
	public resptr_base<CRTC>
{
	void				create(const char* Name, u32 size, ERHI_FORMAT f, CRT::CRTCreationFlags CreationFlags = (CRT::CRTCreationFlags)0);
	void				destroy			()	{ _set(NULL);		}
};

typedef	resptr_core<CRTC,resptrcode_crtc> ref_rtc;
#endif