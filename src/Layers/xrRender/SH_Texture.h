#pragma once

#include "../../xrCore/xr_resource.h"

class ENGINE_API CAviPlayerCustom;
class CTheoraSurface;

class ECORE_API CTexture : public xr_resource_named
{
public:
	//	Since DX10 allows up to 128 unique textures, 
	//	distance between enum values should be at leas 128
	enum ResourceShaderType	//	Don't change this since it's hardware-dependent
	{
		rstPixel = 0,	//	Default texture offset
		rstVertex = RHI_VERTEX_TEXTURESAMPLER,
		rstGeometry = rstVertex+256,
		rstHull = rstGeometry+256,
		rstDomain = rstHull+256,
		rstCompute = rstDomain+256,
        rstInvalid = rstCompute+256
	};

public:
	void								apply_load		(u32	stage);
	void								apply_theora	(u32	stage);
	void								apply_avi		(u32	stage);
	void								apply_seq		(u32	stage);
	void								apply_normal	(u32	stage);

	void								Preload			();
	void								Load			();
	void								PostLoad		();
	void								Unload			(void);

	void								surface_set		(IRHISurface* surf );
	IRHISurface*						surface_get 	();

	IC BOOL								isUser			()		{ return flags.bUser;					}
	IC u32								get_Width		()		{ return pSurface->GetWidth();	}
	IC u32								get_Height		()		{ return pSurface->GetHeight();	}
	IC ERHI_FORMAT						get_Format		()		{ return pSurface ? pSurface->GetFormat() : ERHI_FORMAT::UNKNOWN; }

	void								video_Sync		(u32 _time){m_play_time=_time;}
	void								video_Play		(BOOL looped, u32 _time=0xFFFFFFFF);
	void								video_Pause		(BOOL state);
	void								video_Stop		();
	bool								video_IsPlaying	();

	CTexture							();
	virtual ~CTexture					();

	IRHIShaderResourceView* get_SRView() { return m_pSRView; }

private:
	void								Apply(u32 dwStage);
	void								ProcessStaging();

	ERHI_USAGE							GetUsage();
public:
	struct 
	{
		u32 bLoaded		: 1;
		u32 bUser		: 1;
		u32 seqCycles	: 1;
		u32 MemoryUsage	: 28;
		u32 bLoadedAsStaging: 1;
	} flags;
	xr_delegate<void(u32)> bind;

	CAviPlayerCustom*		pAVI;
	CTheoraSurface*			pTheora;
	float					m_material;
	shared_str				m_bumpmap;

	union
	{
		u32 m_play_time;		// sync theora time
		u32 seqMSPF;			// Sequence data milliseconds per frame
	};

	IRHIShaderResourceView* GetView();

	IRHISurface* pSurface;

private:
	IRHIShaderResourceView* m_pSRView;

	// Sequence data
	xr_vector<IRHISurface*> seqDATA;

	// Sequence view data
	xr_vector<IRHIShaderResourceView*> m_seqSRView;
};

struct resptrcode_texture :
	public resptr_base<CTexture>
{
	ECORE_API void create(const char* _name);
	IC void destroy() { _set(nullptr); }
	IC bool bump_exist() { return 0 != bump_get().size(); }

	IC shared_str bump_get() { return _get()->m_bumpmap; }
};

using ref_texture = resptr_core<CTexture, resptrcode_texture>;