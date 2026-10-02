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

	virtual void ResizeBuffers(u32 Width, u32 Height) override;
	virtual void ClearTarget(void* Target, ERTColor Transparent) override;
	virtual void ClearTarget(void* Target, const float* Color) override;
	virtual void ClearDepthStencil(IRHIDepthStencilView* View, ERHI_CLEAR_TARGET TargetFlags, float Depth, u8 Stencil) override;
	virtual void GenerateMips(IRHIShaderResourceView* SRV) override;
	virtual void Present() override;

	virtual IRHITextureFactory* GetTextureFactory() override;
	virtual void SetTextureFactory(IRHITextureFactory* factory) override;
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
    virtual void DrawIndexed(u32 baseVertex, u32 startVertex, u32 vertexCount, u32 startIndex, u32 primitiveCount) override;
    virtual void Draw(u32 startVertex, u32 primitiveCount) override;
    virtual void DrawIndexedInstanced(u32 baseVertex, u32 startVertex, u32 vertexCount, u32 startIndex, u32 primitiveCount, u32 instanceCount, u32 startInstanceLocation) override;
    virtual void DrawNoInputAssembly(u32 vertexCount) override;

    // Context helpers
    ID3D11DeviceContext* GetImmediateContext() const;
    ID3D11DeviceContext* CreateDeferredContext();
    void ReleaseDeferredContext(ID3D11DeviceContext* context);

    void* GetContext() override;
    void* GetSwapchain() override;
    void BeginFrame() override;
    IRHIShaderDeclaration* CreateDecl(const RHIInputElementDesc* Desc, size_t DeclSize) override;
    IRHIShaderResourceView* CreateShaderResourceView(IRHIBuffer* Buffer, const RHIShaderResourceViewDesc* desc) override;
    void SetConstantBuffers(u32 Start, u32 Count, IRHIBuffer* const* Buffers, ERHI_SHADER_TYPE Type) override;
    void ClearVertexBuffer(u32 vb_stride) override;
    void ClearIndexBuffer() override;
    void SetShader(RHIObject* shader, ERHI_SHADER_TYPE Type) override;
    HRESULT LoadDDS(const void* data, size_t size, ERHI_USAGE usage, u32 bind_flags, ERHI_CPU_ACCESS_FLAG cpu_flags, int& lod, bool fallback, IRHISurface** out_surface) override;

    HRESULT CreateShader(const void* code, size_t size, ERHI_SHADER_TYPE type, RHIObject** out_shader) override;
    HRESULT CreateInputLayout(const RHIInputElementDesc* desc, size_t count, const void* code, size_t size, RHIObject** out_layout) override;
    void SetInputLayout(RHIObject* layout) override;
    void Dispatch(u32 x, u32 y, u32 z) override;
    HRESULT CreateSamplerState(const RHISampleDesc& desc, RHIObject** out_state) override;
    void SetSamplers(u32 start, u32 count, RHIObject* const* states, ERHI_SHADER_TYPE type) override;
    void SetComputeResources(u32 start, u32 count, IRHIShaderResourceView* const* views) override;
    void SetComputeUAVs(u32 start, u32 count, IRHIUnorderedAccessView* const* views, const u32* initial_counts) override;
    HRESULT CreateOcclusionQuery(RHIObject** out_query) override;
    HRESULT GetQueryData(RHIObject* query, void* data, u32 size, u32 flags) override;
    void BeginQuery(RHIObject* query) override;
    void EndQuery(RHIObject* query) override;
    IRHISurface* CreateTexture1D(const RHITextureDesc& desc, const RHISubResource& data) override;
    void CopySwapchain(IRHISurface* dest) override;
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

private:
	bool CreateD3D11();
	void DestroyD3D11();
	bool UpdateBuffersD3D11();
	
	DX11TextureFactory* TextureFactory = nullptr;
};