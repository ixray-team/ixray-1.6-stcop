#pragma once

class XRayRenderDeviceRender: public IRenderDeviceRender
{
public:
	XRayRenderDeviceRender();
	virtual ~XRayRenderDeviceRender();
	virtual void	Copy(IRenderDeviceRender &_in) ;

	virtual void	setGamma(float fGamma);
	virtual void	setBrightness(float fGamma) ;
	virtual void	setContrast(float fGamma) ;
	virtual void	updateGamma() ;

	//	Destroy
	virtual void	OnDeviceDestroy(BOOL bKeepTextures) ;
	virtual void	ValidateHW() ;
	virtual void	DestroyHW() ;
	virtual void	Reset(HWND hWnd, u32& dwWidth, u32& dwHeight, float& fWidth_2, float& fHeight_2) ;
	//	Init
	virtual void	SetupStates() ;
	virtual void	OnDeviceCreate(LPCSTR shName) ;
	virtual void	Create(HWND hWnd, u32& dwWidth, u32& dwHeight, float& fWidth_2, float& fHeight_2, bool) ;
	virtual void	SetupGPU(BOOL bForceGPU_SW, BOOL bForceGPU_NonPure, BOOL bForceGPU_REF) ;
	//	Overdraw
	virtual void	overdrawBegin() ;
	virtual void	overdrawEnd() ;

	//	Resources control
	virtual void	DeferredLoad(BOOL E) ;
	virtual void	ResourcesDeferredUpload() ;
	virtual void	ResourcesDestroyNecessaryTextures() ;
	virtual void	ResourcesStoreNecessaryTextures() ;

	//	HWSupport
	virtual bool	HWSupportsShaderYUV2RGB() ;

	// HW stats

	//	Device state
	virtual DeviceState GetDeviceState() ;
	virtual BOOL	GetForceGPU_REF() ;
	virtual u32		GetCacheStatPolys() ;
	virtual void	Begin() ;
	virtual void	Clear() ;
	virtual void	End() ;
	virtual void	ClearTarget() ;
	virtual void	SetCacheXform( Fmatrix& mView,  Fmatrix& mProject) ;
	virtual void	OnAssetsChanged() ;

	virtual void	ResourcesDumpMemoryUsage();
	virtual void	ResourcesGetMemoryUsage(u32& m_base, u32& c_base, u32& m_lmaps, u32& c_lmaps);
	void Reset(SDL_Window* window, u32& dwWidth, u32& dwHeight, float& fWidth_2, float& fHeight_2) override;
	void Create(SDL_Window* window, u32& dwWidth, u32& dwHeight, float& fWidth_2, float& fHeight_2, bool) override;
	void ResourcesDeferredUnload() override;
	void SetupDefaultTarget() override;
	void SetCacheXformOld(Fmatrix& mView, Fmatrix& mProject) override;

private:

};
