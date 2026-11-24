#pragma once
#include "RenderAPI.h"
#include "GpuResource.h"
#include "TextureDecode.h"

#include "../../xrCore/xr_resource.h"

class ENGINE_API CAviPlayerCustom;
class CTheoraSurface;

class RENDER_API CTexture : public xr_resource_named
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
	// IsDecodeOnly: the worker only reads the file; the frame makes the texture of it through NRI, and the thread
	// that binds it next takes it (TextureDecode)
	bool StartLoading(bool IsDecodeOnly = false);
	bool CanCacheShadow(bool& Pending, u64& Revision);
	u64 GetShadowCacheRevision() const { return ShadowCacheRevision.load(std::memory_order_relaxed); }
	void WaitForLoad();
	void EnsureLoaded();
	bool IsLoadPending() const { return IsLoading.load(std::memory_order_acquire); }
	void AllowPlaceholder() { Sampled2D.store(true, std::memory_order_release); }
	void								PostLoad		();
	void								Unload			(void);
	void								CreateEmpty(u32 w, u32 h);

	void								surface_set		(IRHISurface* surf );
	// The renderer's own textures, made through NRI from Levels: a font atlas, the noise, the material table
	void Make(const RHITextureDesc& Desc, const RHISubResource* Levels);
	IRHISurface*						surface_get 	();

	IC bool isUser()
	{
		WaitForLoad();
		return flags.bUser;
	}
	IC u32 get_Width()
	{
		EnsureLoaded();
		return pSurface ? pSurface->GetWidth() : 0;
	}
	IC u32 get_Height()
	{
		EnsureLoaded();
		return pSurface ? pSurface->GetHeight() : 0;
	}
	IC ERHI_FORMAT get_Format()
	{
		WaitForLoad();
		return pSurface ? pSurface->GetFormat() : ERHI_FORMAT::UNKNOWN;
	}
	bool BindsBC1() const;
	// The surface its view is of when bound: the placeholder's while it loads
	IRHISurface* BoundSurface() const;
	// The same as a view NRI made, null where the old backend made the texture (a target, a video, an empty texture)
	nri::Descriptor* BoundView();

	void								video_Sync		(u32 _time){m_play_time=_time;}
	void								video_Play		(bool looped, u32 _time=0xFFFFFFFF);
	void								video_Pause		(bool state);
	void								video_Stop		();
	bool								video_IsPlaying	();

	CTexture							();
	virtual ~CTexture					() override;

	IRHIShaderResourceView* get_SRView() { return GetView(); }

private:
	xrCriticalSection LoadGuard;
	xr_unique_ptr<xr_task_group> LoadTask;
	xr_atomic_bool IsLoading = false;
	xr_atomic_bool LoadDone = false;
	xr_atomic_bool LoadCancelled = false;
	TextureDecode::Decoded Decoded;
	xr_atomic_bool Sampled2D = false;
	xr_atomic_u64 ShadowCacheRevision = 0;
	resptr_core<CTexture, resptr_base<CTexture>> Placeholder;
	void FinishLoading(bool Discard = false);
	bool ShowsPlaceholder() const;
	void LoadImpl();
	GpuResource& Shown();

	static void							ApplyView(u32 dwStage, IRHIShaderResourceView* View);
	void								Apply(u32 dwStage);

public:
	struct 
	{
		u32 bLoaded		: 1;
		u32 bUser		: 1;
		u32 seqCycles	: 1;
		u32 MemoryUsage	: 29;
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
	bool can_unload;

private:
	IRHIShaderResourceView* m_pSRView;

	GpuResource Gpu;

	// Sequence data
	xr_vector<IRHISurface*> seqDATA;
	xr_vector<GpuResource> SeqGpu;
	u32 SeqFrame = 0;

	// Sequence view data
	xr_vector<IRHIShaderResourceView*> m_seqSRView;
};

struct resptrcode_texture :
	public resptr_base<CTexture>
{
	RENDER_API void create(const char* _name);
	IC void destroy() { _set(nullptr); }
	IC bool bump_exist() { return 0 != bump_get().size(); }

	IC shared_str bump_get() { return _get()->m_bumpmap; }
};

using ref_texture = resptr_core<CTexture, resptrcode_texture>;
#include "TextureAtlas.h"
