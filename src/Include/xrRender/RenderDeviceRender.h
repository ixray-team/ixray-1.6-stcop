#ifndef RenderDeviceRender_included
#define RenderDeviceRender_included
#pragma once

#include "SVGTypes.h"

struct SDL_Window;

class IRenderDeviceRender
{
public:
	enum	DeviceState
	{
		dsOK = 0,
		dsLost,
		dsNeedReset
	};

public:
	virtual ~IRenderDeviceRender() {;}
	virtual void	Copy(IRenderDeviceRender &_in) = 0;

	//	Gamma correction functions
	virtual void	setGamma(float fGamma) = 0;
	virtual void	setBrightness(float fGamma) = 0;
	virtual void	setContrast(float fGamma) = 0;
	virtual void	updateGamma() = 0;

	virtual void GetRenderScale(float& RenderScale) = 0;

	//	Destroy
	virtual void	OnDeviceDestroy( bool bKeepTextures) = 0;
	virtual void	ValidateHW() = 0;
	virtual void	DestroyHW() = 0;
	virtual void	Reset(SDL_Window* window, u32 &dwWidth, u32 &dwHeight) = 0;
	//	Init
	virtual void	SetupStates() = 0;
	virtual void	OnDeviceCreate(const char* shName) = 0;
	virtual void	Create(SDL_Window* window, u32 &dwWidth, u32 &dwHeight, bool ) = 0;
	virtual void	SetupGPU( bool bForceGPU_SW, bool bForceGPU_NonPure, bool bForceGPU_REF) = 0;
	virtual void	PostCreate() = 0;
	//	Overdraw
	virtual void	overdrawBegin() = 0;
	virtual void	overdrawEnd() = 0;

	//	Resources control
	virtual void	DeferredLoad(bool E) = 0;
	virtual void	ResourcesDeferredUpload() = 0;
	virtual void    ResourcesDeferredUnload() = 0;
	virtual void	ResourcesGetMemoryUsage(u32& m_base, u32& c_base, u32& m_lmaps, u32& c_lmaps) = 0;
	virtual void	ResourcesDestroyNecessaryTextures() = 0;
	virtual void	ResourcesStoreNecessaryTextures() = 0;
	virtual void	ResourcesDumpMemoryUsage() = 0;

	//	HWSupport
	virtual bool	HWSupportsShaderYUV2RGB() = 0;

	//	Device state
	virtual DeviceState GetDeviceState() = 0;
	virtual bool	GetForceGPU_REF() = 0;
	virtual u32		GetCacheStatPolys() = 0;
	virtual void	GetCacheStats(u32& calls, u32& verts, u32& polys, u32& static_dips) { calls = verts = polys = static_dips = 0; }
	virtual void	Begin() = 0;
	virtual void	Clear() = 0;
	virtual void	End() = 0;
	virtual void	ClearTarget() = 0;
	virtual void	SetupDefaultTarget() = 0;
	virtual void	SetCacheXform(Fmatrix &mView, Fmatrix &mProject) = 0;
	virtual void	SetCacheXformOld(Fmatrix &mView, Fmatrix &mProject) = 0;
	virtual void	OnAssetsChanged() = 0;
	virtual const FactoryPtr<IUIShader>& GetSVGShader(const std::string_view& subpath, float width, float height, SVGTintRGBA tint = {}) = 0;
	virtual const FactoryPtr<IUIShader>& GetSVGShader(const char* pSubpath, float width, float height, SVGTintRGBA tint = {}) = 0;
	virtual const FactoryPtr<IUIShader>& GetSVGDefaultShader() = 0;

	virtual Frect GetSVGUV(const std::string_view& subpath, float requested_width, float requested_height, SVGTintRGBA tint = {}) = 0;
};

#endif	//	RenderDeviceRender_included