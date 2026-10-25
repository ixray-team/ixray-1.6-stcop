#pragma once

class CDS0_RenderDeviceRender:
	public IRenderDeviceRender
{
public:
	CDS0_RenderDeviceRender();
	virtual ~CDS0_RenderDeviceRender() override;
	virtual void	Copy(IRenderDeviceRender &_in) override;

	virtual void	setGamma(float fGamma) override;
	virtual void	setBrightness(float fGamma) override;
	virtual void	setContrast(float fGamma) override;
	virtual void	updateGamma() override;

	//	Destroy
	virtual void	OnDeviceDestroy(bool bKeepTextures) override;
	virtual void	ValidateHW() override;
	virtual void	DestroyHW() override;
	//	Init
	virtual void	SetupStates() override;
	virtual void	OnDeviceCreate(LPCSTR shName) override;
	virtual void	SetupGPU(bool bForceGPU_SW, bool bForceGPU_NonPure, bool bForceGPU_REF) override;
	//	Overdraw
	virtual void	overdrawBegin() override;
	virtual void	overdrawEnd() override;

	//	Resources control
	virtual void	DeferredLoad(bool E) override;
	virtual void	ResourcesDeferredUpload() override;
	virtual void	ResourcesDestroyNecessaryTextures() override;
	virtual void	ResourcesStoreNecessaryTextures() override;

	//	HWSupport
	virtual bool	HWSupportsShaderYUV2RGB() override;

	// HW stats

	//	Device state
	virtual DeviceState GetDeviceState() override;
	virtual bool	GetForceGPU_REF() override;
	virtual u32		GetCacheStatPolys() override;
	virtual void	Begin() override;
	virtual void	Clear() override;
	virtual void	End() override;
	virtual void	ClearTarget() override;
	virtual void	SetCacheXform( Fmatrix& mView,  Fmatrix& mProject) override;
	virtual void	OnAssetsChanged() override;
	virtual void GetRenderScale(float& RenderScale) override;
	virtual void	ResourcesDumpMemoryUsage() override;
	virtual void	ResourcesGetMemoryUsage(u32& m_base, u32& c_base, u32& m_lmaps, u32& c_lmaps) override;
	virtual void Reset(SDL_Window* window, u32& dwWidth, u32& dwHeight) override;
	virtual void Create(SDL_Window* window, u32& dwWidth, u32& dwHeight, bool) override;
	virtual void ResourcesDeferredUnload() override;
	virtual void SetupDefaultTarget() override;
	virtual void SetCacheXformOld(Fmatrix& mView, Fmatrix& mProject) override;

	virtual const FactoryPtr<IUIShader>& GetSVGShader(const std::string_view& subpath, float width, float height, SVGTintRGBA tint) override;
	virtual const FactoryPtr<IUIShader>& GetSVGShader(const char* pSubpath, float width, float height, SVGTintRGBA tint) override;
	virtual const FactoryPtr<IUIShader>& GetSVGDefaultShader() override;

	virtual Frect GetSVGUV(const std::string_view& subpath, float requested_width, float requested_height, SVGTintRGBA tint) override;
	virtual void	PostCreate() override;
private:
	FactoryPtr<IUIShader> m_empty_default;
};
