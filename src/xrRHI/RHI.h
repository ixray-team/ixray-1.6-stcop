// SPDX-License-Identifier: LicenseRef-IXRAY-NonCommercial
// IX-Ray Engine - Render Hardware Interface (RHI)
// (c) 2019-2025 ForserX, ugo_zapad. All rights reserved.
//
// This file is part of the IX-Ray RHI subsystem.
// Redistribution, modification, or commercial use of this code,
// in whole or in part, is prohibited without explicit written
// permission from the authors.
//
// Developed exclusively for the IX-Ray Engine project.

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
#include "RHITypes.h"
#include "RHIShaderCompiler.h"
#include "RHIShaderDeclaration.h"
#include "RHIDriversExt.h"
#include "RHIShaderResourceCache.h"
#include "RHIStateManager.h"

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
	CRHI();
	~CRHI();

	// Drawing methods
	void SetPrimitiveTopology(ERHI_PRIMITIVE_TOPOLOGY topology);
	void Draw(u32 startVertex, u32 primitiveCount);
	void DrawIndexed(u32 baseVertex, u32 startVertex, u32 vertexCount, u32 startIndex, u32 primitiveCount);
	void DrawIndexedInstanced(u32 baseVertex, u32 startVertex, u32 vertexCount, u32 startIndex, u32 primitiveCount, u32 instanceCount, u32 startInstanceLocation);
	void DrawNoInputAssembly(u32 vertexCount);
	IRHIDevice* CreateDevice(ERHI_API_LAYER APILevel);
	void ResizeBuffers(u32 Width, u32 Height);

	void* GetContext();
	void* GetSwapchain();

	void ClearRawTarget(void* Target, ERTColor Transparent = ERTColor::Transparent);
	void ClearTarget(IRHIRenderTargetView* Target, ERTColor Transparent = ERTColor::Transparent);
	void ClearTarget(IRHIRenderTargetView* Target, const float* Color);
	void ClearDepthStencil(IRHIDepthStencilView* View, ERHI_CLEAR_TARGET TargetFlags, float Depth, u8 Stencil);
	void GenerateMips(IRHIShaderResourceView* SRV);

	void CopySurface(IRHISurface* Dest, IRHISurface* Source);
	void CopySurface(IRHIRenderTargetView* Dest, IRHIRenderTargetView* Source);

	void Present();
	xr_vector<shared_str> DisplaySizeArray();

	[[nodiscard]] IRHISurface* CreateTexture3D(const RHITextureDesc& Desc, RHISubResource* SubResource);
	[[nodiscard]] IRHISurface* CreateTexture2D(const RHITextureDesc& Desc, RHISubResource& SubResource);
	[[nodiscard]] IRHISurface* CreateTextureFromMemory(const void* data, u32 size, const RHITextureDesc& desc);
	[[nodiscard]] IRHISurface* CreateRenderTarget(const RHITextureDesc& desc);
	[[nodiscard]] IRHISurface* CreateDepthStencil(const RHITextureDesc& desc);
	[[nodiscard]] IRHIShaderResourceView* CreateShaderResourceView(IRHIBuffer* surface, const RHIShaderResourceViewDesc* desc);
	[[nodiscard]] IRHIShaderResourceView* CreateShaderResourceView(IRHISurface* surface, const RHIShaderResourceViewDesc* desc);
	[[nodiscard]] IRHIRenderTargetView* CreateRenderTargetView(IRHISurface* surface, const RHIRenderTargetViewDesc& desc = {});
	[[nodiscard]] IRHIDepthStencilView* CreateDepthStencilView(IRHISurface* surface, const RHIDepthStencilViewDesc& desc = {});
	[[nodiscard]] IRHIUnorderedAccessView* CreateUAV(IRHISurface* pTexture, const RHIUAVDesc& desc);
	[[nodiscard]] IRHIBuffer* CreateBuffer(const RHIBufferDesc& desc = {}, const RHIBufferSubresource* pSubresource = nullptr);
	[[nodiscard]] IRHIShaderDeclaration* CreateDecl(const RHIInputElementDesc* Desc, size_t DeclSize);

	void SetConstantBuffers(u32 Min, u32 Max, xr_vector<IRHIBuffer*>, ERHI_SHADER_TYPE Type);
	void SetShader(void* pNativeShader, ERHI_SHADER_TYPE Type);
	void SetViewport(RHIViewport& VP);
	void SetScissorRect(Irect* R);

	void SetRenderTargetView(IRHIRenderTargetView* pRenderTargetView, u32 ID, bool bForce = false);
	void SetDepthStencilView(IRHIDepthStencilView* pDepthStencilView, bool bForce = false);
	void ApplyRenderTargetChange();

	IRHIDepthStencilView* GetDepthStencilView() const;
	IRHIRenderTargetView* GetRenderTargetView(size_t ID) const;

	void GPUStatsBegin() const;
	const RHI_GPU_EVENT& GPUStats() const;
	void GPUStatsEnd() const;

	void ClearVertexBuffer(u32 vb_stride);
	void ClearIndexBuffer();

	bool IsTessPass() const;

	u32 GetInputElementDescStride(const RHIInputElementDesc* Desc, u32 DescSize);

	HRESULT BuildShader
	(
		const void* srcData, size_t srcSize, const char* sourceName, const void* defines, void* include,
		const char* entryPoint, const char* target, u32 flags1, u32 flags2, void** code, void** errors
	);
public:
	IRHIDevice* DevicePtr = nullptr;
	IRHIShaderResourceStateCache* ShaderResourceCache = nullptr;
	CRHIShaderCompilerShell* ShaderCompiler = nullptr;
	IRHIStateManager* StateManager = nullptr;

	ERHI_API_LAYER APILevel = ERHI_API_LAYER::NOT_CREATED;
	
	bool GPUStatsEnable = false;
	IRHIGPU* DriverExt = nullptr;

private:
	void* Shaders[RHI_SHADERS_TYPE_SIZE];
};

extern RHI_API CRHI* GRHI;

#include "RHIUtils.h"
#include "Layout/ImGui/RHIImGuiLayout.h"