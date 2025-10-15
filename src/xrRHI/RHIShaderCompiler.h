#pragma once

class CRHIShaderCompilerShell
{
	ERHI_API_LAYER Layer;

public:
	CRHIShaderCompilerShell(ERHI_API_LAYER API);

	HRESULT Build
	(
		const void* srcData, size_t srcSize, const char* sourceName, const void* defines, void* include,
		const char* entryPoint, const char* target, u32 flags1, u32 flags2, void** code, void** errors
	);
};

class IRHIShaderCompiler;

#if 0
struct RHIShaderPassDesc
{
	// Shader files
	shared_str VertexShader;
	shared_str PixelShader;
	shared_str GeometryShader;
	shared_str HullShader;
	shared_str DomainShader;
	shared_str ComputeShader;

	// Render states
	bool ZTest = true;
	bool ZWrite = true;
	bool AlphaBlend = false;
	ERHI_BLEND_MODE BlendSrc = ERHI_BLEND_MODE::ONE;
	ERHI_BLEND_MODE BlendDst = ERHI_BLEND_MODE::ZERO;
	ERHI_BLEND_OP BlendOp = ERHI_BLEND_OP::ADD;
	bool AlphaTest = false;
	u32 AlphaRef = 0;
	bool Fog = false;

	// Stencil state
	bool StencilEnable = false;
	ERHI_COMPARISON_FUNC StencilFunc = ERHI_COMPARISON_FUNC::ALWAYS;
	u32 StencilMask = 0x00;
	u32 StencilWriteMask = 0x00;
	ERHI_STENCIL_OP StencilFail = ERHI_STENCIL_OP::KEEP;
	ERHI_STENCIL_OP StencilPass = ERHI_STENCIL_OP::KEEP;
	ERHI_STENCIL_OP StencilZFail = ERHI_STENCIL_OP::KEEP;
	u32 StencilRef = 0;

	// Cull mode
	ERHI_CULL_MODE CullMode = ERHI_CULL_MODE::BACK;

	// Color write enable
	bool ColorWriteR = true;
	bool ColorWriteG = true;
	bool ColorWriteB = true;
	bool ColorWriteA = true;
};

struct RHISamplerDesc
{
	ERHI_FILTER_TYPE MinFilter = ERHI_FILTER_TYPE::LINEAR;
	ERHI_FILTER_TYPE MagFilter = ERHI_FILTER_TYPE::LINEAR;
	ERHI_FILTER_TYPE MipFilter = ERHI_FILTER_TYPE::LINEAR;
	ERHI_TEXTURE_ADDRESS_MODE AddressU = ERHI_TEXTURE_ADDRESS_MODE::WRAP;
	ERHI_TEXTURE_ADDRESS_MODE AddressV = ERHI_TEXTURE_ADDRESS_MODE::WRAP;
	ERHI_TEXTURE_ADDRESS_MODE AddressW = ERHI_TEXTURE_ADDRESS_MODE::WRAP;
	u32 MaxAnisotropy = 1;
	float MipLODBias = 0.0f;
	u32 BorderColor = 0;
	bool ComparisonEnable = false;
	bool ProjectiveDivide = false;

	ERHI_COMPARISON_FUNC ComparisonFunc = ERHI_COMPARISON_FUNC::NEVER;
};

struct PassData
{
	RHIShaderPassDesc desc;
	xr_vector<shared_str> textures;
	xr_vector<RHISamplerDesc> samplers;
	xr_vector<shared_str> constants;
	xr_vector<shared_str> matrices;
};

class IRHIShaderCompiler
{
public:
	virtual ~IRHIShaderCompiler() = default;

	// Pass management
	virtual void BeginPass() = 0;
	virtual void EndPass() = 0;
	virtual u32 GetPassCount() const = 0;

	// Shader pass creation
	virtual void CreatePass(const RHIShaderPassDesc& desc) = 0;
	virtual void CreateComputePass(const shared_str& computeShader) = 0;

	// Texture and sampler binding
	virtual u32 BindTexture(const shared_str& name, const shared_str& texture, const RHISamplerDesc& sampler = {}) = 0;
	virtual void BindRenderTarget(const shared_str& name, const shared_str& texture) = 0;
	virtual void BindDepthStencil(const shared_str& name, const shared_str& texture) = 0;

	// Constant binding
	virtual void BindConstant(const shared_str& name, void* setup) = 0;
	virtual void BindMatrix(const shared_str& name, int channel) = 0;

	// State management
	virtual void SetStencilState(bool enable, ERHI_COMPARISON_FUNC func = ERHI_COMPARISON_FUNC::ALWAYS, 
		u32 mask = 0x00, u32 writeMask = 0x00, ERHI_STENCIL_OP fail = ERHI_STENCIL_OP::KEEP, 
		ERHI_STENCIL_OP pass = ERHI_STENCIL_OP::KEEP, ERHI_STENCIL_OP zFail = ERHI_STENCIL_OP::KEEP) = 0;
	virtual void SetStencilRef(u32 ref) = 0;
	virtual void SetCullMode(ERHI_CULL_MODE mode) = 0;
	virtual void SetColorWriteEnable(bool r = true, bool g = true, bool b = true, bool a = true) = 0;

    // Compilation
    virtual void Compile() = 0;
    virtual void Clear() = 0;
    
    // Data access for r_End
    virtual const xr_vector<PassData>& GetPasses() const = 0;
};

RHI_API IRHIShaderCompiler* CreateShaderCompiler();
#endif