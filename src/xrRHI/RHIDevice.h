#pragma once
#include "RHITypes.h"
#include "RHIEnums.h"

class RHIObject;
class IRHIShaderDeclaration;
class IRHIStateManager;

struct RHIRasterizerDesc;
struct RHIDepthStencilDesc;
struct RHIBlendDesc;

class IRHIDevice
{
public:
	void* RawDevice = nullptr;
	IRHIRenderTargetView* RenderRTV = nullptr;
	void* RenderSRV = nullptr;
	IRHIDepthStencilView* RenderDSV = nullptr;
	IRHIRenderTargetView* RenderTexture = nullptr;
	IRHIRenderTargetView* SwapChainRTV = nullptr;

	float RenderScale = 1.f;
	u32 FeatureLevel;
	u32 VertexCache;

	// Texture factory
	IRHITextureFactory* TextureFactory = nullptr;

	// Drawing methods
	virtual void SetPrimitiveTopology(ERHI_PRIMITIVE_TOPOLOGY topology) = 0;
	virtual void DrawIndexed(u32 baseVertex, u32 startVertex, u32 vertexCount, u32 startIndex, u32 primitiveCount) = 0;
	virtual void Draw(u32 startVertex, u32 primitiveCount) = 0;

	virtual void DrawIndexedInstanced(
		u32 baseVertex, u32 startVertex, u32 vertexCount,
		u32 startIndex, u32 primitiveCount,
		u32 instanceCount, u32 startInstanceLocation) = 0;
	
	virtual void DrawNoInputAssembly(u32 vertexCount) = 0;

public:
	virtual ~IRHIDevice() = default;
	virtual void ResizeBuffers(u32 Width, u32 Height) = 0;
	virtual void ClearTarget(void* Target, ERTColor Transparent) = 0;
	virtual void ClearTarget(void* Target, const float* Color) = 0;
	virtual void ClearDepthStencil(IRHIDepthStencilView* View, ERHI_CLEAR_TARGET TargetFlags, float Depth, u8 Stencil) = 0;
	virtual void GenerateMips(IRHIShaderResourceView* SRV) = 0;
	virtual void Present() = 0;
	virtual void CopySurface(IRHISurface* Dest, IRHISurface* Source) = 0;
	virtual void CopySurface(IRHIRenderTargetView* Dest, IRHIRenderTargetView* Source) = 0;

	// Texture management
	virtual IRHITextureFactory* GetTextureFactory() = 0;
	virtual void SetTextureFactory(IRHITextureFactory* factory) = 0;
	virtual void SetViewport(RHIViewport& VP) = 0;

	// Buffer stuff
	virtual IRHIBuffer* CreateBuffer(const RHIBufferDesc& desc = {}, const RHIBufferSubresource* pSubresource = nullptr) = 0;

	// Scissor rect
	virtual void SetScissorRect(Irect* R) = 0;

	// Read pixels from a render target into a contiguous buffer (RGBA8 or 32-bit per pixel layout)
	// Dst: pointer to destination buffer; DstSize: size of the destination buffer in bytes
	// OutWidth/OutHeight: returned width/height of the captured region
	// OutRowPitch: number of bytes per row written into Dst (may be Width*4)
	virtual bool ReadRenderTargetPixels(IRHIRenderTargetView* Rtv, void* Dst, u32 DstSize, u32& OutWidth, u32& OutHeight, u32& OutRowPitch) = 0;

	// Render Taget setup
	virtual void SetRenderTargets(u32 NumViews, IRHIRenderTargetView* const* ppRenderTargetViews, IRHIUnorderedAccessView* const* ppRenderUAViews) = 0;
	virtual void SetDSV(IRHIDepthStencilView* pDepthStencilView) = 0;


    virtual void* GetContext() = 0;
    virtual void* GetSwapchain() = 0;
    virtual void BeginFrame() = 0;
    virtual IRHIShaderDeclaration* CreateDecl(const RHIInputElementDesc* Desc, size_t DeclSize) = 0;
    virtual IRHIShaderResourceView* CreateShaderResourceView(IRHIBuffer* Buffer, const RHIShaderResourceViewDesc* desc) = 0;
    virtual void SetConstantBuffers(u32 Start, u32 Count, IRHIBuffer* const* Buffers, ERHI_SHADER_TYPE Type) = 0;
    virtual void ClearVertexBuffer(u32 vb_stride) = 0;
    virtual void ClearIndexBuffer() = 0;
    virtual void SetShader(RHIObject* shader, ERHI_SHADER_TYPE Type) = 0;
    virtual HRESULT LoadDDS(const void* data, size_t size, ERHI_USAGE usage, u32 bind_flags, ERHI_CPU_ACCESS_FLAG cpu_flags, int& lod, bool fallback, IRHISurface** out_surface) = 0;

    virtual HRESULT CreateShader(const void* code, size_t size, ERHI_SHADER_TYPE type, RHIObject** out_shader) = 0;
    virtual HRESULT ReplaceShader(RHIObject* shader, const void* code, size_t size) { (void)shader; (void)code; (void)size; return E_NOTIMPL; }
    virtual void Flush() {}
    virtual HRESULT CreateInputLayout(const RHIInputElementDesc* desc, size_t count, const void* code, size_t size, RHIObject** out_layout) = 0;
    virtual void SetInputLayout(RHIObject* layout) = 0;
    virtual void Dispatch(u32 x, u32 y, u32 z) = 0;
    virtual HRESULT CreateSamplerState(const RHISampleDesc& desc, RHIObject** out_state) = 0;
    virtual void SetSamplers(u32 start, u32 count, RHIObject* const* states, ERHI_SHADER_TYPE type) = 0;
    virtual void SetComputeResources(u32 start, u32 count, IRHIShaderResourceView* const* views) = 0;
    virtual void SetComputeUAVs(u32 start, u32 count, IRHIUnorderedAccessView* const* views, const u32* initial_counts) = 0;
    virtual HRESULT CreateOcclusionQuery(RHIObject** out_query) = 0;
    virtual HRESULT GetQueryData(RHIObject* query, void* data, u32 size, u32 flags) = 0;
    virtual void BeginQuery(RHIObject* query) = 0;
    virtual void EndQuery(RHIObject* query) = 0;
    virtual IRHISurface* CreateTexture1D(const RHITextureDesc& desc, const RHISubResource& data) = 0;
    virtual void CopySwapchain(IRHISurface* dest) = 0;
    virtual bool SupportsTextureSampling(ERHI_FORMAT format, u32& out_flags) = 0;
    virtual void* GetState(const RHIRasterizerDesc& desc) = 0;
    virtual void* GetState(const RHIDepthStencilDesc& desc) = 0;
    virtual void* GetState(const RHIBlendDesc& desc) = 0;
    virtual HRESULT CreateBlendState(const RHIBlendDesc& desc, RHIObject** out_state) = 0;
    virtual void SetBlendState(RHIObject* state, const float* factor, u32 mask) = 0;
    virtual void SetRawBlendState(void* state, const float* factor, u32 mask) = 0;
    virtual bool SetDepthBounds(bool enable, float minimum, float maximum) { return false; }

	virtual void EvictManagedResources() {};
};