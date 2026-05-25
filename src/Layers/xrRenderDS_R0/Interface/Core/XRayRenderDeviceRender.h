#pragma once

class CDS0_RenderDeviceRender:
	public IRenderDeviceRender
{
public:
	CDS0_RenderDeviceRender();
	virtual ~CDS0_RenderDeviceRender();
	virtual void	Copy(IRenderDeviceRender &_in) ;

	virtual void	setGamma(float fGamma);
	virtual void	setBrightness(float fGamma) ;
	virtual void	setContrast(float fGamma) ;
	virtual void	updateGamma() ;

	//	Destroy
	virtual void	OnDeviceDestroy(bool bKeepTextures) ;
	virtual void	ValidateHW() ;
	virtual void	DestroyHW() ;
	//	Init
	virtual void	SetupStates() ;
	virtual void	OnDeviceCreate(LPCSTR shName) ;
	virtual void	SetupGPU(bool bForceGPU_SW, bool bForceGPU_NonPure, bool bForceGPU_REF) ;
	//	Overdraw
	virtual void	overdrawBegin() ;
	virtual void	overdrawEnd() ;

	//	Resources control
	virtual void	DeferredLoad(bool E) ;
	virtual void	ResourcesDeferredUpload() ;
	virtual void	ResourcesDestroyNecessaryTextures() ;
	virtual void	ResourcesStoreNecessaryTextures() ;

	//	HWSupport
	virtual bool	HWSupportsShaderYUV2RGB() ;

	// HW stats

	//	Device state
	virtual DeviceState GetDeviceState() ;
	virtual bool	GetForceGPU_REF() ;
	virtual u32		GetCacheStatPolys() ;
	virtual void	Begin() ;
	virtual void	Clear() ;
	virtual void	End() ;
	virtual void	ClearTarget() ;
	virtual void	SetCacheXform( Fmatrix& mView,  Fmatrix& mProject) ;
	virtual void	OnAssetsChanged() ;
	virtual void GetRenderScale(float& RenderScale) override;
	virtual void	ResourcesDumpMemoryUsage();
	virtual void	ResourcesGetMemoryUsage(u32& m_base, u32& c_base, u32& m_lmaps, u32& c_lmaps);
	void Reset(SDL_Window* window, u32& dwWidth, u32& dwHeight) override;
	void Create(SDL_Window* window, u32& dwWidth, u32& dwHeight, bool) override;
	void ResourcesDeferredUnload() override;
	void SetupDefaultTarget() override;
	void SetCacheXformOld(Fmatrix& mView, Fmatrix& mProject) override;

	const FactoryPtr<IUIShader>& GetSVGShader(const std::string_view& subpath, float width, float height, SVGTintRGBA tint) override;
	const FactoryPtr<IUIShader>& GetSVGShader(const char* pSubpath, float width, float height, SVGTintRGBA tint) override;
	const FactoryPtr<IUIShader>& GetSVGDefaultShader() override;

	Frect GetSVGUV(const std::string_view& subpath, float requested_width, float requested_height, SVGTintRGBA tint) override;
	void	PostCreate() override;
private:
	FactoryPtr<IUIShader> m_empty_default;
};
