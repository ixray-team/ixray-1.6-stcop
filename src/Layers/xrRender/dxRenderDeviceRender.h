#ifndef dxRenderDeviceRender_included
#define dxRenderDeviceRender_included
#pragma once

#ifndef _EDITOR
	#define DEV dxRenderDeviceRender::Instance().Resources
#else
	#define DEV EDevice.Resources
#endif

#ifndef _EDITOR

#include "..\..\Include\xrRender\RenderDeviceRender.h"
#include "xr_effgamma.h"

class CResourceManager;

class dxRenderDeviceRender : public IRenderDeviceRender
{
public:
	static dxRenderDeviceRender& Instance() {  return *((dxRenderDeviceRender*)(&*Device.m_pRender));}

	dxRenderDeviceRender();

	void Copy(IRenderDeviceRender &_in) override;
	void setGamma(float fGamma) override;
	void setBrightness(float fGamma) override;
	void setContrast(float fGamma) override;
	void updateGamma() override;
	void OnDeviceDestroy( BOOL bKeepTextures) override;
	void ValidateHW() override;
	void DestroyHW() override;
	void Reset(SDL_Window* window, u32 &dwWidth, u32 &dwHeight, float &fWidth_2, float &fHeight_2) override;
	void SetupStates() override;
	void OnDeviceCreate(LPCSTR shName) override;
	void Create(SDL_Window* window, u32 &dwWidth, u32 &dwHeight, float &fWidth_2, float &fHeight_2, bool) override;
	void SetupGPU( BOOL bForceGPU_SW, BOOL bForceGPU_NonPure, BOOL bForceGPU_REF) override;
	void overdrawBegin() override;
	void overdrawEnd() override;
	void DeferredLoad(BOOL E) override;
	void ResourcesDeferredUpload() override;
	void ResourcesDeferredUnload() override;
	void ResourcesGetMemoryUsage(u32& m_base, u32& c_base, u32& m_lmaps, u32& c_lmaps) override;
	void ResourcesDestroyNecessaryTextures() override;
	void ResourcesStoreNecessaryTextures()override ;
	void ResourcesDumpMemoryUsage() override;
	bool HWSupportsShaderYUV2RGB() override;
	DeviceState GetDeviceState() override;
	BOOL GetForceGPU_REF() override;
	u32 GetCacheStatPolys() override;
	void Begin() override;
	void Clear() override;
	void End() override;
	void ClearTarget() override;
	void SetCacheXform(Fmatrix &mView, Fmatrix &mProject) override;
	virtual void SetCacheXformOld(Fmatrix &mView, Fmatrix &mProject);
	void OnAssetsChanged() override;

public:
	CResourceManager*	Resources;
	ref_shader			m_WireShader;
	ref_shader			m_SelectionShader;

#ifndef USE_DX11
	CHWCaps						Caps;
#endif

private:
	CGammaControl		m_Gamma;
};

#endif //ifndef _EDITOR


#endif	//	RenderDeviceRender_included