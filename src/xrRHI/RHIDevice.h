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
#include "RHITypes.h"
#include "RHIEnums.h"

namespace nri
{
struct CommandBuffer;
}

class RHIObject;
class IRHIStateManager;

struct RHIRasterizerDesc;
struct RHIDepthStencilDesc;
struct RHIBlendDesc;

struct RHIConstantWrite
{
	u32 Slot;
	u32 Size;
	const void* Data;
};

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
	u32 ConstantEpoch = 0;
	bool NoOverwriteSRV = false;

	// Texture factory
	IRHITextureFactory* TextureFactory = nullptr;

	// Drawing methods
	virtual void SetPrimitiveTopology(ERHI_PRIMITIVE_TOPOLOGY topology) = 0;
	virtual void Draw(u32 VertexCount, u32 StartVertex) = 0;
	virtual void DrawIndexed(u32 IndexCount, u32 StartIndex, u32 BaseVertex) = 0;
	virtual void DrawInstanced(u32 VertexCount, u32 InstanceCount, u32 StartVertex, u32 StartInstance) = 0;
	virtual void DrawIndexedInstanced(u32 IndexCount, u32 InstanceCount, u32 StartIndex, u32 BaseVertex, u32 StartInstance) = 0;

public:
	virtual ~IRHIDevice() = default;
	// The swap chain as SwapChain says and the screen targets at the video mode
	virtual void ResizeBuffers(const RHISwapChainDesc& SwapChain) = 0;
	// The swap chain alone, made again once the GPU is idle: nothing bound to its back buffers may stay bound
	virtual void ResizeSwapChain(const RHISwapChainDesc& SwapChain) = 0;
	virtual void ClearTarget(void* Target, ERTColor Transparent) = 0;
	virtual void ClearTarget(void* Target, const float* Color) = 0;
	virtual void ClearDepthStencil(IRHIDepthStencilView* View, ERHI_CLEAR_TARGET TargetFlags, float Depth, u8 Stencil) = 0;
	virtual void GenerateMips(IRHIShaderResourceView* SRV) = 0;
	virtual void Present() = 0;
	virtual void CopySurface(IRHISurface* Dest, IRHISurface* Source) = 0;
	virtual void CopySurface(IRHIRenderTargetView* Dest, IRHIRenderTargetView* Source) = 0;

	// Texture management
	virtual IRHITextureFactory* GetTextureFactory() = 0;
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

	// Render target setup. A null ppRenderUAViews binds no UAVs.
	virtual void SetRenderTargets(u32 NumViews, IRHIRenderTargetView* const* ppRenderTargetViews, IRHIUnorderedAccessView* const* ppRenderUAViews) = 0;
	virtual void SetDSV(IRHIDepthStencilView* pDepthStencilView) = 0;


    virtual void* GetContext() = 0;
    virtual void* GetSwapchain() = 0;
    virtual void BeginFrame() = 0;
    virtual IRHIShaderResourceView* CreateShaderResourceView(IRHIBuffer* Buffer, const RHIShaderResourceViewDesc* desc) = 0;
    virtual IRHIUnorderedAccessView* CreateUAV(IRHIBuffer* Buffer, const RHIUAVDesc& Desc) = 0;
    virtual void SetConstants(const RHIConstantWrite* Writes, u32 Count) = 0;
    virtual void ClearIndexBuffer() = 0;
    virtual void SetShader(RHIObject* shader, ERHI_SHADER_TYPE Type) = 0;

    virtual HRESULT CreateShader(const void* code, size_t size, ERHI_SHADER_TYPE type, RHIObject** out_shader) = 0;
    virtual HRESULT ReplaceShader(RHIObject* shader, const void* code, size_t size) { (void)shader; (void)code; (void)size; return E_NOTIMPL; }
    virtual void Flush() {}
    virtual HRESULT CreateInputLayout(const RHIInputElementDesc* desc, size_t count, const void* code, size_t size, RHIObject** out_layout) = 0;
    virtual void SetInputLayout(RHIObject* layout) = 0;
    virtual void Dispatch(u32 x, u32 y, u32 z) = 0;
    virtual HRESULT CreateSamplerState(const RHISampleDesc& desc, RHIObject** out_state) = 0;
    virtual void SetSamplers(RHIObject* const* States, u32 Count) = 0;
    virtual void SetComputeResources(u32 start, u32 count, IRHIShaderResourceView* const* views) = 0;
    virtual void SetComputeUAVs(u32 start, u32 count, IRHIUnorderedAccessView* const* views, const u32* initial_counts) = 0;
    virtual IRHISurface* CreateTexture1D(const RHITextureDesc& desc, const RHISubResource& data) = 0;
    virtual bool SupportsTextureSampling(ERHI_FORMAT format, u32& out_flags) = 0;
    virtual void* GetState(const RHIRasterizerDesc& desc) = 0;
    virtual void* GetState(const RHIDepthStencilDesc& desc) = 0;
    virtual void* GetState(const RHIBlendDesc& desc) = 0;
    virtual HRESULT CreateBlendState(const RHIBlendDesc& desc, RHIObject** out_state) = 0;
    virtual void SetBlendState(RHIObject* state, const float* factor, u32 mask) = 0;
    virtual void SetRawBlendState(void* state, const float* factor, u32 mask) = 0;
    virtual bool SetDepthBounds(bool enable, float minimum, float maximum) { return false; }

	// Seams for passes recorded through NRI while this backend still owns the resources.
	// Begin leaves Reads readable by a pixel shader, Targets and Depth writable, and returns the NRI command
	// buffer of this backend's open list (D3D12): every pass of the list records into it in order, with no
	// wrapper and no submit. Null on D3D11, whose passes replay on its context when submitted. End makes
	// this backend forget the pipeline state it had bound.
	virtual bool GetShaderCode(RHIObject* Shader, const void*& Code, size_t& Size) { return false; }
	virtual nri::CommandBuffer* BeginExternalPass(IRHISurface* const* Reads, u32 ReadCount, IRHIRenderTargetView* const* Targets, u32 TargetCount, IRHIDepthStencilView* Depth) { return nullptr; }
	virtual void EndExternalPass() {}
	// The native buffer, left in a state a shader of the pass can read it in
	virtual void* ReadExternalBuffer(IRHIBuffer* Buffer) { return nullptr; }
	// Command buffers recorded elsewhere through NRI, run after everything recorded so far and before what is recorded
	// next: the frame's one submit carries them. With none, what is recorded is submitted now. False
	// where this backend cannot (D3D11 records on one context)
	virtual bool SubmitExternalCommands(nri::CommandBuffer* const* Buffers, u32 Count) { return false; }

	virtual void EvictManagedResources() {};

	// The graphics pipelines the device made at a draw are remembered between runs: the count of them, making one
	// of them ahead of its first draw (true if it was made), and writing the list
	virtual u32 LearnedPipelineCount() { return 0; }
	virtual bool PrecreatePipeline(u32 Index) { return false; }
	virtual void SaveLearnedPipelines() {}
};
