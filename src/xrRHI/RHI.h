#pragma once
#include "../xrCore/xrCore.h"

#ifdef XR_RHI_EXPORTS
#	define RHI_API __declspec(dllexport)
#else
#	define RHI_API __declspec(dllimport)
#endif

#include "RHIConstants.h"
#include "RHIEnums.h"
#include "RHITextureInterfaces.h"
#include "RHIBuffer.h"
#include "RHIDevice.h"
#include "RHIGPUMark.h"

enum
{
	rsFullscreen = (1ul << 0ul),
	rsClearBB = (1ul << 1ul),
	rsVSync = (1ul << 2ul),
	rsWireframe = (1ul << 3ul),
	rsOcclusion = (1ul << 4ul),
	rsStatistic = (1ul << 5ul),
	rsDetails = (1ul << 6ul),
	rsRefresh60hz = (1ul << 7ul),
	rsConstantFPS = (1ul << 8ul),
	rsDrawStatic = (1ul << 9ul),
	rsDrawDynamic = (1ul << 10ul),
	rsDisableObjectsAsCrows = (1ul << 11ul),

	rsOcclusionDraw = (1ul << 12ul),
	rsOcclusionStats = (1ul << 13ul),

	mtSound = (1ul << 14ul),
	mtPhysics = (1ul << 15ul),
	mtNetwork = (1ul << 16ul),
	mtParticles = (1ul << 17ul),

	rsCameraPos = (1ul << 18ul),
	rsR2 = (1ul << 19ul),
	rsR4 = (1ul << 20ul),

	rsDeviceActive = (1ul << 21ul),
	// 22-32 bit - reserved to Editor
};

extern RHI_API u32 psCurrentVidMode[2];
extern RHI_API Flags32 psDeviceFlags;
extern RHI_API Ivector2 HalfTarget;

class RHI_API CRHI final
{
public:
	~CRHI();
	IRHIDevice* CreateDevice(ERHI_API_LAYER APILevel);
	void ResizeBuffers(u32 Width, u32 Height);

	void* GetContext();
	void* GetSwapchain();

	void ClearRawTarget(void* Target, ERTColor Transparent = ERTColor::Transparent);
	void ClearTarget(IRHIRenderTargetView* Target, ERTColor Transparent = ERTColor::Transparent);
	void CopySurface(IRHISurface* Dest, IRHISurface* Source);
	void CopySurface(IRHIRenderTargetView* Dest, IRHIRenderTargetView* Source);

	void Present();
	xr_vector<shared_str> DisplaySizeArray();

	IRHISurface* CreateTextureFromFile(const char* filename, u32& memorySize);
	IRHISurface* CreateTextureFromMemory(const void* data, u32 size, const RHITextureDesc& desc);
	IRHISurface* CreateRenderTarget(const RHITextureDesc& desc);
	IRHISurface* CreateDepthStencil(const RHITextureDesc& desc);
	IRHIShaderResourceView* CreateShaderResourceView(IRHISurface* surface, const RHIShaderResourceViewDesc* desc);
	IRHIRenderTargetView* CreateRenderTargetView(IRHISurface* surface, const RHIRenderTargetViewDesc& desc = {});
	IRHIDepthStencilView* CreateDepthStencilView(IRHISurface* surface, const RHIDepthStencilViewDesc& desc = {});
	IRHIUnorderedAccessView* CreateUAV(IRHISurface* pTexture, const RHIUAVDesc& desc);
	
	IRHIBuffer* CreateBuffer(const RHIBufferDesc& desc = {}, const RHIBufferSubresource* pSubresource = nullptr);

	void GPUStatsBegin() const;
	const RHI_GPU_EVENT& GPUStats() const;
	void GPUStatsEnd() const;
public:
	IRHIDevice* DevicePtr = nullptr;
	ERHI_API_LAYER APILevel = ERHI_API_LAYER::NOT_CREATED;
	
	bool GPUStatsEnable = false;
};

extern RHI_API CRHI* GRHI;

namespace RHIUtils
{
	inline bool CreateVertexBuffer(IRHIBuffer** ppBuffer, const void* pData, u32 DataSize, bool bImmutable = true)
	{
		RHIBufferDesc desc = {};
		desc.Usage = bImmutable ? ERHI_USAGE::USAGE_DEFAULT : ERHI_USAGE::USAGE_DYNAMIC;
		desc.Size = DataSize;
		desc.Type = ERHI_BUFFER_TYPE::VERTEX;
		desc.CPUAccessFlags = bImmutable ? 0 : ERHI_CPU_ACCESS_FLAG_WRITE;

		RHIBufferSubresource resource = {};
		resource.pSysMem = pData;

		IRHIBuffer* pBuffer = GRHI->CreateBuffer(desc, pData ? &resource : nullptr);
		if (!pBuffer)
			return false;

		*ppBuffer = pBuffer;
		return true;
	}

	inline bool CreateIndexBuffer(IRHIBuffer** ppBuffer, const void* pData, u32 DataSize, bool bImmutable = true)
	{
		RHIBufferDesc desc = {};
		desc.Usage = bImmutable ? ERHI_USAGE::USAGE_DEFAULT : ERHI_USAGE::USAGE_DYNAMIC;
		desc.Size = DataSize;
		desc.Type = ERHI_BUFFER_TYPE::INDEX;
		desc.CPUAccessFlags = bImmutable ? 0 : ERHI_CPU_ACCESS_FLAG_WRITE;

		RHIBufferSubresource resource = {};
		resource.pSysMem = pData;

		IRHIBuffer* pBuffer = GRHI->CreateBuffer(desc, pData ? &resource : nullptr);
		if (!pBuffer)
			return false;

		*ppBuffer = pBuffer;
		return true;
	}

	// Will return nullptr on DX9
	inline bool CreateConstantBuffer(IRHIBuffer** ppBuffer, u32 DataSize)
	{
		RHIBufferDesc desc = {};
		desc.Usage = ERHI_USAGE::USAGE_DYNAMIC;
		desc.Size = DataSize;
		desc.Type = ERHI_BUFFER_TYPE::CONSTANT;

		IRHIBuffer* pBuffer = GRHI->CreateBuffer(desc, nullptr);
		if (!pBuffer)
			return false;

		*ppBuffer = pBuffer;
		return true;
	}
}