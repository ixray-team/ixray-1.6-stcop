#pragma once

#	define DEV dxRenderDeviceRender::Instance().Resources

#include "../../Include/xrRender/RenderDeviceRender.h"
#include "xr_effgamma.h"

class CResourceManager;

class dxRenderDeviceRender :
	public IRenderDeviceRender
{
public:
	static dxRenderDeviceRender& Instance() {  return *((dxRenderDeviceRender*)(&*Device.m_pRender));}

	dxRenderDeviceRender();

	virtual void GetRenderScale(float& RenderScale) override;
	virtual void Copy(IRenderDeviceRender &_in) override;
	virtual void setGamma(float fGamma) override;
	virtual void setBrightness(float fGamma) override;
	virtual void setContrast(float fGamma) override;
	virtual void updateGamma() override;
	virtual void OnDeviceDestroy( bool bKeepTextures) override;
	virtual void ValidateHW() override;
	virtual void DestroyHW() override;
	virtual void Reset(SDL_Window* window, u32 &dwWidth, u32 &dwHeight) override;
	virtual void SetupStates() override;
	virtual void OnDeviceCreate(const char* shName) override;
	virtual void Create(SDL_Window* window, u32 &dwWidth, u32 &dwHeight, bool) override;
	virtual void SetupGPU( bool bForceGPU_SW, bool bForceGPU_NonPure, bool bForceGPU_REF) override;
	virtual void overdrawBegin() override;
	virtual void overdrawEnd() override;
	virtual void DeferredLoad(bool E) override;
	virtual void ResourcesDeferredUpload() override;
	virtual void ResourcesDeferredUnload() override;
	virtual void ResourcesGetMemoryUsage(u32& m_base, u32& c_base, u32& m_lmaps, u32& c_lmaps) override;
	virtual bool GetVideoMemoryInfo(SVideoMemoryInfo& Info) override;
	virtual void ResourcesDestroyNecessaryTextures() override;
	virtual void ResourcesStoreNecessaryTextures()override ;
	virtual void ResourcesDumpMemoryUsage() override;
	virtual bool HWSupportsShaderYUV2RGB() override;
	virtual DeviceState GetDeviceState() override;
	virtual bool GetForceGPU_REF() override;
	virtual u32 GetCacheStatPolys() override;
	virtual void GetCacheStats(u32& calls, u32& verts, u32& polys, u32& static_dips) override;
	virtual void Begin() override;
	virtual void Clear() override;
	virtual void End() override;
	virtual void ClearTarget() override;
	virtual void SetupDefaultTarget() override;
	virtual void InvalidateRenderCache() override;
	virtual UIIconLighting* GetUIIconLighting() override;
	virtual void SetCacheXform(Fmatrix &mView, Fmatrix &mProject) override;
	virtual void SetCacheXformOld(Fmatrix &mView, Fmatrix &mProject) override;
	virtual void OnAssetsChanged() override;

	virtual void PostCreate() override;

	virtual const FactoryPtr<IUIShader>& GetSVGShader(const std::string_view& subpath, float width, float height, SVGTintRGBA tint) override;
	virtual const FactoryPtr<IUIShader>& GetSVGShader(const char* pSubpath, float width, float height, SVGTintRGBA tint) override;
	virtual const FactoryPtr<IUIShader>& GetSVGDefaultShader() override;

	virtual Frect GetSVGUV(const std::string_view& subpath, float requested_width, float requested_height, SVGTintRGBA tint) override;

public:
	CResourceManager*	Resources;
	ref_shader			m_WireShader;
	ref_shader			m_SelectionShader;
	FactoryPtr<IUIShader> m_empty_default_shader;
	CGammaControl		m_Gamma;
	UIIconLighting IconLighting;
};
