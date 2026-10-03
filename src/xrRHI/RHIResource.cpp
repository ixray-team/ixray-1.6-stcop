#include "RHI.h"
#include <cstring>
#include <d3dcompiler.h>
#include <d3d11shader.h>
#include "RHIDXC.h"

RHIObject::RHIObject(void* value, void (*release)(void*), void (*describe)(void*, RHISampleDesc*)) :
    resource(value), _release(release), _describe(describe)
{
}

void RHIObject::AddRef()
{
    ++references;
}

u32 RHIObject::Release()
{
    u32 remaining = --references;
    if (remaining)
    {
        return remaining;
    }
    // Renderer statics run from DllMain after CRHI is destroyed. Their
    // deleters retire GPU objects and would enter a destroyed context lock.
    if (GRHI)
    {
        _release(resource);
    }
    auto self = this;
    xr_delete(self);
    return 0;
}

void RHIObject::GetDesc(RHISampleDesc* out_desc) const
{
    R_ASSERT(_describe && out_desc);
    _describe(resource, out_desc);
}

RHIBlob::RHIBlob(size_t size) : data(size)
{
}

void
RHIBlob::AddRef()
{
	++references;
}

u32
RHIBlob::Release()
{
	u32 remaining = --references;
	if (remaining) {
		return remaining;
	}
	auto self = this;
	xr_delete(self);
	return 0;
}

void*
RHIBlob::GetBufferPointer()
{
	return data.data();
}

size_t
RHIBlob::GetBufferSize() const
{
	return data.size();
}

static RHIBlob*
RHI_CopyBlob(ID3DBlob* blob)
{
	if (!blob) {
		return nullptr;
	}
	auto result = new RHIBlob(blob->GetBufferSize());
	memcpy(result->GetBufferPointer(), blob->GetBufferPointer(), blob->GetBufferSize());
	blob->Release();
	return result;
}

class RHIShaderIncludeDX11 final : public ID3DInclude
{
public:
	explicit RHIShaderIncludeDX11(IRHIShaderInclude* value) : include(value) {}
	HRESULT __stdcall Open(D3D_INCLUDE_TYPE type, const char* name, const void* parent, const void** out_data, UINT* out_size) override
	{
		return include->Open(static_cast<u32>(type), name, parent, out_data, out_size);
	}
	HRESULT __stdcall Close(const void* data) override { return include->Close(data); }

private:
	IRHIShaderInclude* include;
};


HRESULT
CRHI::CreateBlob(size_t size, RHIBlob** out_blob)
{
	*out_blob = new RHIBlob(size);
	return S_OK;
}

HRESULT
CRHI::GetInputSignature(const void* code, size_t size, RHIBlob** out_blob)
{
	ID3DBlob* blob = nullptr;
	HRESULT result = D3DGetInputSignatureBlob(code, size, &blob);
	if (FAILED(result) && RHI_IsDXIL(code, size)) {
		auto copy = new RHIBlob(size);
		memcpy(copy->GetBufferPointer(), code, size);
		*out_blob = copy;
		return S_OK;
	}
	*out_blob = RHI_CopyBlob(blob);
	return result;
}

HRESULT
CRHI::DisassembleShader(const void* code, size_t size, RHIBlob** out_blob)
{
	ID3DBlob* blob = nullptr;
	HRESULT result = D3DDisassemble(code, size, 0, nullptr, &blob);
	*out_blob = RHI_CopyBlob(blob);
	return result;
}

HRESULT
CRHI::CompileShader(const void* source, size_t size, const char* name, const RHIShaderMacro* macros,
	IRHIShaderInclude* include, const char* entry, const char* target, u32 flags, RHIBlob** out_code, RHIBlob** out_errors)
{
	if (UsesDXIL(target[0])) {
		*out_code = *out_errors = nullptr;
		const HRESULT dxc = RHI_DxcCompile(source, size, name, macros, include, entry, target, flags, out_code, out_errors);
		if (SUCCEEDED(dxc) || *out_errors) {
			return dxc;
		}
	}
	xr_vector<D3D_SHADER_MACRO> defines;
	if (macros) {
		for (u32 macro_idx = 0; macros[macro_idx].Name; ++macro_idx) {
			defines.push_back({ macros[macro_idx].Name, macros[macro_idx].Definition });
		}
		defines.push_back({ nullptr, nullptr });
	}
	RHIShaderIncludeDX11 includer(include);
	ID3DBlob* code = nullptr;
	ID3DBlob* errors = nullptr;
	HRESULT result = D3DCompile(source, size, name, defines.empty() ? nullptr : defines.data(), include ? &includer : nullptr,
		entry, target, flags, 0, &code, &errors);
	*out_code = RHI_CopyBlob(code);
	*out_errors = RHI_CopyBlob(errors);
	return result;
}

HRESULT
CRHI::CreateShader(const void* code, size_t size, ERHI_SHADER_TYPE type, RHIObject** out_shader)
{
    return DevicePtr->CreateShader(code, size, type, out_shader);
}

HRESULT
CRHI::CreateInputLayout(const RHIInputElementDesc* desc, size_t count, const void* code, size_t size, RHIObject** out_layout)
{
    return DevicePtr->CreateInputLayout(desc, count, code, size, out_layout);
}

void
CRHI::SetInputLayout(RHIObject* layout)
{
    DevicePtr->SetInputLayout(layout);
}

void
CRHI::Dispatch(u32 x, u32 y, u32 z)
{
    DevicePtr->Dispatch(x, y, z);
}

HRESULT
CRHI::CreateSamplerState(const RHISampleDesc& desc, RHIObject** out_state)
{
    return DevicePtr->CreateSamplerState(desc, out_state);
}

void
CRHI::SetSamplers(u32 start, u32 count, RHIObject* const* states, ERHI_SHADER_TYPE type)
{
    DevicePtr->SetSamplers(start, count, states, type);
}

void
CRHI::SetComputeResources(u32 start, u32 count, IRHIShaderResourceView* const* views)
{
    DevicePtr->SetComputeResources(start, count, views);
}

void
CRHI::SetComputeUAVs(u32 start, u32 count, IRHIUnorderedAccessView* const* views, const u32* initial_counts)
{
    DevicePtr->SetComputeUAVs(start, count, views, initial_counts);
}

HRESULT
CRHI::CreateOcclusionQuery(RHIObject** out_query)
{
    return DevicePtr->CreateOcclusionQuery(out_query);
}

HRESULT
CRHI::GetQueryData(RHIObject* query, void* data, u32 size, u32 flags)
{
    return DevicePtr->GetQueryData(query, data, size, flags);
}

void
CRHI::BeginQuery(RHIObject* query)
{
    DevicePtr->BeginQuery(query);
}

void
CRHI::EndQuery(RHIObject* query)
{
    DevicePtr->EndQuery(query);
}

template <class TReflection, class TShader, class TBuffer, class TVariable, class TType, class TBinding, class TSignature>
static HRESULT ReflectImpl(TReflection* reflection, RHIShaderReflection& out_reflection)
{
	HRESULT result = S_OK;
	TShader shader = {};
	result = reflection->GetDesc(&shader);
	if (FAILED(result)) {
		reflection->Release();
		return result;
	}
	out_reflection.Buffers.resize(shader.ConstantBuffers);
	out_reflection.Resources.resize(shader.BoundResources);
	out_reflection.Inputs.resize(shader.InputParameters);
	for (u32 buffer_idx = 0; buffer_idx < shader.ConstantBuffers && SUCCEEDED(result); ++buffer_idx) {
		auto buffer = reflection->GetConstantBufferByIndex(buffer_idx);
		TBuffer desc = {};
		result = buffer->GetDesc(&desc);
		if (FAILED(result)) {
			break;
		}
		auto& output = out_reflection.Buffers[buffer_idx];
		output.Name = desc.Name;
		output.Type = desc.Type;
		output.Size = desc.Size;
		TBinding binding = {};
		output.BindPoint = SUCCEEDED(reflection->GetResourceBindingDescByName(desc.Name, &binding)) ? binding.BindPoint : buffer_idx;
		const bool skipVariables = desc.Name && (!strcmp(desc.Name, "cb_frame") || !strcmp(desc.Name, "cb_view") ||
			!strcmp(desc.Name, "cb_object") || !strcmp(desc.Name, "cb_material") || !strcmp(desc.Name, "cb_light") ||
			!strcmp(desc.Name, "cb_pass"));
		if (skipVariables)
			continue;
		output.Variables.resize(desc.Variables);
		for (u32 variable_idx = 0; variable_idx < desc.Variables && SUCCEEDED(result); ++variable_idx) {
			auto variable = buffer->GetVariableByIndex(variable_idx);
			TVariable variable_desc = {};
			TType type = {};
			result = variable->GetDesc(&variable_desc);
			if (SUCCEEDED(result)) {
				result = variable->GetType()->GetDesc(&type);
			}
			if (FAILED(result)) {
				break;
			}
			auto& output_variable = output.Variables[variable_idx];
			output_variable.Name = variable_desc.Name;
			output_variable.StartOffset = variable_desc.StartOffset;
			output_variable.Size = variable_desc.Size;
			output_variable.Type = { static_cast<ERHI_SHADER_VARIABLE_CLASS>(type.Class), static_cast<ERHI_SHADER_VARIABLE_TYPE>(type.Type),
				type.Rows, type.Columns, type.Elements, type.Members, type.Offset, type.Name };
		}
	}
	for (u32 resource_idx = 0; resource_idx < shader.BoundResources && SUCCEEDED(result); ++resource_idx) {
		TBinding desc = {};
		result = reflection->GetResourceBindingDesc(resource_idx, &desc);
		out_reflection.Resources[resource_idx] = { desc.Name, static_cast<ERHI_SHADER_RESOURCE_TYPE>(desc.Type), desc.BindPoint, desc.BindCount };
	}
	for (u32 input_idx = 0; input_idx < shader.InputParameters && SUCCEEDED(result); ++input_idx) {
		TSignature desc = {};
		result = reflection->GetInputParameterDesc(input_idx, &desc);
		out_reflection.Inputs[input_idx] = { desc.SemanticName, desc.SemanticIndex, desc.Register, static_cast<u32>(desc.SystemValueType),
			static_cast<ERHI_SHADER_COMPONENT_TYPE>(desc.ComponentType), desc.Mask };
	}
	reflection->Release();
	if (FAILED(result)) {
		out_reflection = {};
	}
	return result;
}

HRESULT
CRHI::ReflectShader(const void* code, size_t size, RHIShaderReflection& out_reflection)
{
	out_reflection = {};
	if (RHI_IsDXIL(code, size)) {
		ID3D12ShaderReflection* reflection = nullptr;
		const HRESULT result = RHI_DxcReflect(code, size, &reflection);
		return FAILED(result) ? result : ReflectImpl<ID3D12ShaderReflection, D3D12_SHADER_DESC, D3D12_SHADER_BUFFER_DESC,
			D3D12_SHADER_VARIABLE_DESC, D3D12_SHADER_TYPE_DESC, D3D12_SHADER_INPUT_BIND_DESC, D3D12_SIGNATURE_PARAMETER_DESC>(reflection, out_reflection);
	}
	ID3D11ShaderReflection* reflection = nullptr;
	const HRESULT result = D3DReflect(code, size, IID_ID3D11ShaderReflection, reinterpret_cast<void**>(&reflection));
	return FAILED(result) ? result : ReflectImpl<ID3D11ShaderReflection, D3D11_SHADER_DESC, D3D11_SHADER_BUFFER_DESC,
		D3D11_SHADER_VARIABLE_DESC, D3D11_SHADER_TYPE_DESC, D3D11_SHADER_INPUT_BIND_DESC, D3D11_SIGNATURE_PARAMETER_DESC>(reflection, out_reflection);
}

IRHISurface*
CRHI::CreateTexture1D(const RHITextureDesc& desc, const RHISubResource& data)
{
    return DevicePtr->CreateTexture1D(desc, data);
}

void
CRHI::CopySwapchain(IRHISurface* dest)
{
    DevicePtr->CopySwapchain(dest);
}

bool
CRHI::SupportsTextureSampling(ERHI_FORMAT format, u32& out_flags)
{
    return DevicePtr->SupportsTextureSampling(format, out_flags);
}

void*
CRHI::GetState(const RHIRasterizerDesc& desc)
{
    return DevicePtr->GetState(desc);
}

void*
CRHI::GetState(const RHIDepthStencilDesc& desc)
{
    return DevicePtr->GetState(desc);
}

void*
CRHI::GetState(const RHIBlendDesc& desc)
{
    return DevicePtr->GetState(desc);
}

HRESULT
CRHI::CreateBlendState(const RHIBlendDesc& desc, RHIObject** out_state)
{
    return DevicePtr->CreateBlendState(desc, out_state);
}

void
CRHI::SetBlendState(RHIObject* state, const float* factor, u32 mask)
{
    DevicePtr->SetBlendState(state, factor, mask);
}

void
CRHI::SetRawBlendState(void* state, const float* factor, u32 mask)
{
    DevicePtr->SetRawBlendState(state, factor, mask);
}

