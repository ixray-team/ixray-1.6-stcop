#pragma once
#include "../../Include/xrRender/RenderDeviceRender.h"

namespace Rendering
{
	class RenderDeviceRender : public IRenderDeviceRender
	{
	public:
		~RenderDeviceRender();
		void Copy(IRenderDeviceRender& _in) override;
		void setGamma(float fGamma) override;
		void setBrightness(float fGamma) override;
		void setContrast(float fGamma) override;
		void updateGamma() override;
		void OnDeviceDestroy(BOOL bKeepTextures) override;
		void ValidateHW() override;
		void DestroyHW() override;
		void Reset(HWND hWnd, u32& dwWidth, u32& dwHeight) override;
		void SetupStates() override;
		void OnDeviceCreate(LPCSTR shName) override;
		void Create(HWND hWnd, u32& dwWidth, u32& dwHeight, bool) override;
		void SetupGPU(BOOL bForceGPU_SW, BOOL bForceGPU_NonPure, BOOL bForceGPU_REF) override;
		void overdrawBegin() override;
		void overdrawEnd() override;
		void DeferredLoad(BOOL E) override;
		void ResourcesDeferredUpload() override;
		void ResourcesDeferredUnload() override;
		void ResourcesGetMemoryUsage(u32& m_base, u32& c_base, u32& m_lmaps, u32& c_lmaps) override;
		void ResourcesDestroyNecessaryTextures() override;
		void ResourcesStoreNecessaryTextures() override;
		void ResourcesDumpMemoryUsage() override;
		bool HWSupportsShaderYUV2RGB() override;
		DeviceState GetDeviceState() override;
		BOOL GetForceGPU_REF() override;
		u32	GetCacheStatPolys() override;
		void Begin() override;
		void Clear() override;
		void End() override;
		void ClearTarget() override;
		void SetCacheXform(Fmatrix& mView, Fmatrix& mProject) override;
		void SetCachePrevXform(Fmatrix& mView, Fmatrix& mProject) override;
		void ResetXform(Fmatrix& mView, Fmatrix& mProject) override;
		void ResetPrevXform(Fmatrix& mView, Fmatrix& mProject) override;
		void OnAssetsChanged() override;
	};
}