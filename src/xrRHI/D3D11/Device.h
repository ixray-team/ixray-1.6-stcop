/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#pragma once
#include <d3d11_4.h>

#include "../RHI.h"
#include "DX11Texture.h"
#include "DX11Buffer.h"

class InternalDevice11:
	public IRHIDevice
{
public:
	InternalDevice11();
	~InternalDevice11();

	// Inherited via IRHIDevice

	virtual void ResizeBuffers(const RHISwapChainDesc& SwapChain) override;
	virtual void ResizeSwapChain(const RHISwapChainDesc& SwapChain) override;
	virtual void ClearTarget(void* Target, ERTColor Transparent) override;
	virtual void ClearTarget(void* Target, const float* Color) override;
	virtual void ClearDepthStencil(IRHIDepthStencilView* View, ERHI_CLEAR_TARGET TargetFlags, float Depth, u8 Stencil) override;
	virtual void GenerateMips(IRHIShaderResourceView* SRV) override;
	virtual void Present() override;

	virtual IRHITextureFactory* GetTextureFactory() override;
	virtual void CopySurface(IRHISurface* Dest, IRHISurface* Source) override;
	virtual void CopySurface(IRHIRenderTargetView* Dest, IRHIRenderTargetView* Source) override;
	virtual void SetViewport(RHIViewport& VP) override;

	IRHIBuffer* CreateBuffer(const RHIBufferDesc& desc, const RHIBufferSubresource* pSubresource) override;

	// Scissor rect
	void SetScissorRect(Irect* R) override;

	void SetRenderTargets(u32 NumViews, IRHIRenderTargetView* const* ppRenderTargetViews, IRHIUnorderedAccessView* const* ppRenderUAViews) override;
	void SetDSV(IRHIDepthStencilView* pDepthStencilView) override;

	// Readback helper
	virtual bool ReadRenderTargetPixels(IRHIRenderTargetView* Rtv, void* Dst, u32 DstSize, u32& OutWidth, u32& OutHeight, u32& OutRowPitch) override;

    // Drawing methods
    virtual void SetPrimitiveTopology(ERHI_PRIMITIVE_TOPOLOGY topology) override;
    void Draw(u32 VertexCount, u32 StartVertex) override;
    void DrawIndexed(u32 IndexCount, u32 StartIndex, u32 BaseVertex) override;
    void DrawInstanced(u32 VertexCount, u32 InstanceCount, u32 StartVertex, u32 StartInstance) override;
    void DrawIndexedInstanced(u32 IndexCount, u32 InstanceCount, u32 StartIndex, u32 BaseVertex, u32 StartInstance) override;

    // Context helpers
    ID3D11DeviceContext* GetImmediateContext() const;

    void* GetContext() override;
    void* GetSwapchain() override;
    void BeginFrame() override;
    IRHIShaderResourceView* CreateShaderResourceView(IRHIBuffer* Buffer, const RHIShaderResourceViewDesc* desc) override;
    IRHIUnorderedAccessView* CreateUAV(IRHIBuffer* Buffer, const RHIUAVDesc& Desc) override;
    void SetConstants(const RHIConstantWrite* Writes, u32 Count) override;
    void ClearIndexBuffer() override;
    void SetShader(RHIObject* shader, ERHI_SHADER_TYPE Type) override;

    HRESULT CreateShader(const void* code, size_t size, ERHI_SHADER_TYPE type, RHIObject** out_shader) override;
    HRESULT CreateInputLayout(const RHIInputElementDesc* desc, size_t count, const void* code, size_t size, RHIObject** out_layout) override;
    void SetInputLayout(RHIObject* layout) override;
    void Dispatch(u32 x, u32 y, u32 z) override;
    HRESULT CreateSamplerState(const RHISampleDesc& desc, RHIObject** out_state) override;
    void SetSamplers(RHIObject* const* States, u32 Count) override;
    void SetComputeResources(u32 start, u32 count, IRHIShaderResourceView* const* views) override;
    void SetComputeUAVs(u32 start, u32 count, IRHIUnorderedAccessView* const* views, const u32* initial_counts) override;
    IRHISurface* CreateTexture1D(const RHITextureDesc& desc, const RHISubResource& data) override;
    bool SupportsTextureSampling(ERHI_FORMAT format, u32& out_flags) override;
    void* GetState(const RHIRasterizerDesc& desc) override;
    void* GetState(const RHIDepthStencilDesc& desc) override;
    void* GetState(const RHIBlendDesc& desc) override;
    HRESULT CreateBlendState(const RHIBlendDesc& desc, RHIObject** out_state) override;
    void SetBlendState(RHIObject* state, const float* factor, u32 mask) override;
    void SetRawBlendState(void* state, const float* factor, u32 mask) override;

private:
    D3D_PRIMITIVE_TOPOLOGY d3dTopology = D3D_PRIMITIVE_TOPOLOGY_UNDEFINED;
    ERHI_PRIMITIVE_TOPOLOGY currentTopology = (ERHI_PRIMITIVE_TOPOLOGY)-1;
	IRHIDepthStencilView* DepthStencilView = nullptr;

public:
	IDXGISwapChain* HWSwapchain = nullptr;
	ID3D11DeviceContext* HWRenderContext = nullptr;

	void InvalidateBindings();
	bool GetShaderCode(RHIObject* Shader, const void*& Code, size_t& Size) override;
	nri::CommandBuffer* BeginExternalPass(IRHISurface* const* Reads, u32 ReadCount, IRHIRenderTargetView* const* Targets, u32 TargetCount, IRHIDepthStencilView* Depth) override;
	void EndExternalPass() override;
	void* ReadExternalBuffer(IRHIBuffer* Buffer) override;

private:
	void ApplyBindings(u32 Stages);

	// D3D11 shader objects do not keep their bytecode, and NRI pipelines are built from it
	xrCriticalSection ShaderCodeLock;
	xr_hash_map<void*, xr_vector<u8>> ShaderCode;

	static constexpr u32 ConstantRingSize = 4u << 20;
	ID3D11DeviceContext1* HWContext1 = nullptr;
	xr_vector<ID3D11Buffer*> ConstantRings;
	u32 RingIndex = 0, RingPos = 0;
	ID3D11Buffer* ConstantBuffers[RHI_MAX_CONSTANT_BUFFERS] = {};
	u32 ConstantCapacity[RHI_MAX_CONSTANT_BUFFERS] = {};
	UINT ConstantFirst[RHI_MAX_CONSTANT_BUFFERS] = {}, ConstantNum[RHI_MAX_CONSTANT_BUFFERS] = {};
	u32 ConstantLive = 0, ConstantPending[6] = {}, GraphicsStages = 0;
	ID3D11SamplerState* Samplers[D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT] = {};
	u32 SamplerCount = 0, SamplerPending = 0;

	bool CreateD3D11();
	void DestroyD3D11();
	bool UpdateBuffersD3D11();
	bool CreateSwapChainView();

	DX11TextureFactory* TextureFactory = nullptr;
};