#include "Device.h"
#include "RHIStateTypesDX11.h"
#include "RHIStateManagerDX11.h"
#include <DirectXTex.h>

static void ReleaseObject(void* resource)
{
    static_cast<IUnknown*>(resource)->Release();
}

static void DescribeSampler(void* resource, RHISampleDesc* out_desc)
{
	D3D11_SAMPLER_DESC desc = {};
	static_cast<ID3D11SamplerState*>(resource)->GetDesc(&desc);
	*out_desc = { static_cast<ERHI_FILTER>(desc.Filter), static_cast<ERHI_TEXTURE_ADDRESS_MODE>(desc.AddressU),
		static_cast<ERHI_TEXTURE_ADDRESS_MODE>(desc.AddressV), static_cast<ERHI_TEXTURE_ADDRESS_MODE>(desc.AddressW),
		desc.MipLODBias, desc.MaxAnisotropy, static_cast<ERHI_COMPARISON_FUNC>(desc.ComparisonFunc),
		{ desc.BorderColor[0], desc.BorderColor[1], desc.BorderColor[2], desc.BorderColor[3] }, desc.MinLOD, desc.MaxLOD };
}

HRESULT
InternalDevice11::CreateShader(const void* code, size_t size, ERHI_SHADER_TYPE type, RHIObject** out_shader)
{
	auto device = static_cast<ID3D11Device*>(RawDevice);
	IUnknown* shader = nullptr;
	HRESULT result = E_INVALIDARG;
	switch (type) {
	case ERHI_SHADER_TYPE::PS: result = device->CreatePixelShader(code, size, nullptr, reinterpret_cast<ID3D11PixelShader**>(&shader)); break;
	case ERHI_SHADER_TYPE::VS: result = device->CreateVertexShader(code, size, nullptr, reinterpret_cast<ID3D11VertexShader**>(&shader)); break;
	case ERHI_SHADER_TYPE::GS: result = device->CreateGeometryShader(code, size, nullptr, reinterpret_cast<ID3D11GeometryShader**>(&shader)); break;
	case ERHI_SHADER_TYPE::HS: result = device->CreateHullShader(code, size, nullptr, reinterpret_cast<ID3D11HullShader**>(&shader)); break;
	case ERHI_SHADER_TYPE::DS: result = device->CreateDomainShader(code, size, nullptr, reinterpret_cast<ID3D11DomainShader**>(&shader)); break;
	case ERHI_SHADER_TYPE::CS: result = device->CreateComputeShader(code, size, nullptr, reinterpret_cast<ID3D11ComputeShader**>(&shader)); break;
	}
	*out_shader = shader ? new RHIObject(shader, ReleaseObject) : nullptr;
	return result;
}

HRESULT
InternalDevice11::CreateInputLayout(const RHIInputElementDesc* desc, size_t count, const void* code, size_t size, RHIObject** out_layout)
{
	D3D11_INPUT_ELEMENT_DESC elements[32] = {};
	R_ASSERT(count <= std::size(elements));
	for (size_t element_idx = 0; element_idx < count; ++element_idx) {
		const auto& element = desc[element_idx];
		elements[element_idx] = { element.SemanticName, element.SemanticIndex, static_cast<DXGI_FORMAT>(element.Format),
			element.InputSlot, element.AlignedByteOffset, static_cast<D3D11_INPUT_CLASSIFICATION>(element.InputSlotClass), element.InstanceDataStepRate };
	}
	ID3D11InputLayout* layout = nullptr;
	HRESULT result = static_cast<ID3D11Device*>(RawDevice)->CreateInputLayout(elements, static_cast<UINT>(count), code, size, &layout);
	*out_layout = layout ? new RHIObject(layout, ReleaseObject) : nullptr;
	return result;
}

void
InternalDevice11::SetInputLayout(RHIObject* layout)
{
	static_cast<ID3D11DeviceContext*>(GetImmediateContext())->IASetInputLayout(layout ? static_cast<ID3D11InputLayout*>(layout->resource) : nullptr);
}

void
InternalDevice11::Dispatch(u32 x, u32 y, u32 z)
{
	ApplyBindings(1u << 5);
	static_cast<ID3D11DeviceContext*>(GetImmediateContext())->Dispatch(x, y, z);
}

HRESULT
InternalDevice11::CreateSamplerState(const RHISampleDesc& desc, RHIObject** out_state)
{
	D3D11_SAMPLER_DESC native = { static_cast<D3D11_FILTER>(desc.Filter), static_cast<D3D11_TEXTURE_ADDRESS_MODE>(desc.AddressU),
		static_cast<D3D11_TEXTURE_ADDRESS_MODE>(desc.AddressV), static_cast<D3D11_TEXTURE_ADDRESS_MODE>(desc.AddressW),
		desc.MipLODBias, desc.MaxAnisotropy, static_cast<D3D11_COMPARISON_FUNC>(desc.ComparisonFunc),
		{ desc.BorderColor[0], desc.BorderColor[1], desc.BorderColor[2], desc.BorderColor[3] }, desc.MinLOD, desc.MaxLOD };
	ID3D11SamplerState* state = nullptr;
	HRESULT result = static_cast<ID3D11Device*>(RawDevice)->CreateSamplerState(&native, &state);
	*out_state = state ? new RHIObject(state, ReleaseObject, DescribeSampler) : nullptr;
	return result;
}

void
InternalDevice11::SetSamplers(RHIObject* const* States, u32 Count)
{
	R_ASSERT(Count <= std::size(Samplers));
	for (u32 Index = 0; Index < Count; ++Index)
	{
		Samplers[Index] = States[Index] ? static_cast<ID3D11SamplerState*>(States[Index]->resource) : nullptr;
	}
	SamplerCount = Count;
	SamplerPending = 0x3f;
}

void
InternalDevice11::SetComputeResources(u32 start, u32 count, IRHIShaderResourceView* const* views)
{
	ID3D11ShaderResourceView* native[D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT] = {};
	R_ASSERT(start <= std::size(native) && count <= std::size(native) - start);
	for (u32 view_idx = 0; view_idx < count; ++view_idx) {
		native[view_idx] = views[view_idx] ? static_cast<ID3D11ShaderResourceView*>(views[view_idx]->GetRawSRV()) : nullptr;
	}
	static_cast<ID3D11DeviceContext*>(GetImmediateContext())->CSSetShaderResources(start, count, native);
}

void
InternalDevice11::SetComputeUAVs(u32 start, u32 count, IRHIUnorderedAccessView* const* views, const u32* initial_counts)
{
	ID3D11UnorderedAccessView* native[D3D11_PS_CS_UAV_REGISTER_COUNT] = {};
	R_ASSERT(start <= std::size(native) && count <= std::size(native) - start);
	for (u32 view_idx = 0; view_idx < count; ++view_idx) {
		native[view_idx] = views[view_idx] ? static_cast<ID3D11UnorderedAccessView*>(views[view_idx]->GetRaw()) : nullptr;
	}
	static_cast<ID3D11DeviceContext*>(GetImmediateContext())->CSSetUnorderedAccessViews(start, count, native, initial_counts);
}

HRESULT
InternalDevice11::CreateOcclusionQuery(RHIObject** out_query)
{
	D3D11_QUERY_DESC desc = { D3D11_QUERY_OCCLUSION, 0 };
	ID3D11Query* query = nullptr;
	HRESULT result = static_cast<ID3D11Device*>(RawDevice)->CreateQuery(&desc, &query);
	*out_query = query ? new RHIObject(query, ReleaseObject) : nullptr;
	return result;
}

HRESULT
InternalDevice11::GetQueryData(RHIObject* query, void* data, u32 size, u32 flags)
{
	return static_cast<ID3D11DeviceContext*>(GetImmediateContext())->GetData(static_cast<ID3D11Query*>(query->resource), data, size, flags);
}

void
InternalDevice11::BeginQuery(RHIObject* query)
{
	static_cast<ID3D11DeviceContext*>(GetImmediateContext())->Begin(static_cast<ID3D11Query*>(query->resource));
}

void
InternalDevice11::EndQuery(RHIObject* query)
{
	static_cast<ID3D11DeviceContext*>(GetImmediateContext())->End(static_cast<ID3D11Query*>(query->resource));
}

IRHISurface*
InternalDevice11::CreateTexture1D(const RHITextureDesc& desc, const RHISubResource& data)
{
	D3D11_TEXTURE1D_DESC native = { desc.Width, desc.MipLevels, desc.ArraySize, static_cast<DXGI_FORMAT>(desc.Format),
		static_cast<D3D11_USAGE>(desc.Usage), static_cast<UINT>(desc.BindFlags), GetD3D11CPUAccess(static_cast<ERHI_CPU_ACCESS_FLAG>(desc.CPUAccessFlags)), desc.MiscFlags };
	D3D11_SUBRESOURCE_DATA initial = { data.Data, data.RowPitch, data.DepthPitch };
	ID3D11Texture1D* texture = nullptr;
	HRESULT result = static_cast<ID3D11Device*>(RawDevice)->CreateTexture1D(&native, data.Data ? &initial : nullptr, &texture);
	if (FAILED(result)) {
		return nullptr;
	}
	return new DX11Surface(texture);
}

void
InternalDevice11::CopySwapchain(IRHISurface* dest)
{
	ID3D11Texture2D* buffer = nullptr;
	CHK_DX(static_cast<IDXGISwapChain*>(HWSwapchain)->GetBuffer(0, IID_PPV_ARGS(&buffer)));
	static_cast<ID3D11DeviceContext*>(GetImmediateContext())->CopyResource(static_cast<ID3D11Resource*>(dest->GetRawTexture()), buffer);
	buffer->Release();
}

bool
InternalDevice11::SupportsTextureSampling(ERHI_FORMAT format, u32& out_flags)
{
	out_flags = 0;
	HRESULT result = static_cast<ID3D11Device*>(RawDevice)->CheckFormatSupport(static_cast<DXGI_FORMAT>(format), &out_flags);
	u32 required = D3D11_FORMAT_SUPPORT_SHADER_LOAD | D3D11_FORMAT_SUPPORT_SHADER_SAMPLE;
	return SUCCEEDED(result) && (out_flags & required) == required;
}

void*
InternalDevice11::GetState(const RHIRasterizerDesc& desc)
{
	auto native = RHI_ConvertState(desc);
	return static_cast<RHIStateManagerDX11*>(GRHI->StateManager)->GetCache(ERHI_STATE_CACHE_TYPE::RS, &native);
}

void*
InternalDevice11::GetState(const RHIDepthStencilDesc& desc)
{
	auto native = RHI_ConvertState(desc);
	return static_cast<RHIStateManagerDX11*>(GRHI->StateManager)->GetCache(ERHI_STATE_CACHE_TYPE::DS, &native);
}

void*
InternalDevice11::GetState(const RHIBlendDesc& desc)
{
	auto native = RHI_ConvertState(desc);
	return static_cast<RHIStateManagerDX11*>(GRHI->StateManager)->GetCache(ERHI_STATE_CACHE_TYPE::BS, &native);
}

HRESULT
InternalDevice11::CreateBlendState(const RHIBlendDesc& desc, RHIObject** out_state)
{
	auto native = RHI_ConvertState(desc);
	ID3D11BlendState* state = nullptr;
	HRESULT result = static_cast<ID3D11Device*>(RawDevice)->CreateBlendState(&native, &state);
	*out_state = state ? new RHIObject(state, ReleaseObject) : nullptr;
	return result;
}

void
InternalDevice11::SetBlendState(RHIObject* state, const float* factor, u32 mask)
{
	SetRawBlendState(state ? state->resource : nullptr, factor, mask);
}

void
InternalDevice11::SetRawBlendState(void* state, const float* factor, u32 mask)
{
	static_cast<ID3D11DeviceContext*>(GetImmediateContext())->OMSetBlendState(static_cast<ID3D11BlendState*>(state), factor, mask);
}


void InternalDevice11::SetShader(RHIObject* shader, ERHI_SHADER_TYPE Type)
{
    void* NativeShader = shader ? shader->resource : nullptr;
	const u32 Stage = (1u << u32(Type)) & 31u;
	GraphicsStages = (GraphicsStages & ~Stage) | (NativeShader ? Stage : 0u);

		ID3D11DeviceContext* Context = (ID3D11DeviceContext*)GetImmediateContext();

		switch (Type)
		{
			case ERHI_SHADER_TYPE::PS: Context->PSSetShader((ID3D11PixelShader*)NativeShader, nullptr, 0); break;
			case ERHI_SHADER_TYPE::VS: Context->VSSetShader((ID3D11VertexShader*)NativeShader, nullptr, 0); break;
			case ERHI_SHADER_TYPE::GS: Context->GSSetShader((ID3D11GeometryShader*)NativeShader, nullptr, 0); break;
			case ERHI_SHADER_TYPE::HS: Context->HSSetShader((ID3D11HullShader*)NativeShader, nullptr, 0); break;
			case ERHI_SHADER_TYPE::DS: Context->DSSetShader((ID3D11DomainShader*)NativeShader, nullptr, 0); break;
			case ERHI_SHADER_TYPE::CS: Context->CSSetShader((ID3D11ComputeShader*)NativeShader, nullptr, 0); break;
			default: break;
		}
}

void InternalDevice11::ClearIndexBuffer()
{
    GetImmediateContext()->IASetIndexBuffer(nullptr, DXGI_FORMAT_R16_UINT, 0);
}

void InternalDevice11::ClearVertexBuffer(u32 vb_stride)
{
    u32 offset = 0;
    ID3D11Buffer* buffer = nullptr;
    GetImmediateContext()->IASetVertexBuffers(0, 1, &buffer, &vb_stride, &offset);
}

void InternalDevice11::SetConstants(const RHIConstantWrite* Writes, u32 Count)
{
	ID3D11Device* Device = static_cast<ID3D11Device*>(RawDevice);
	D3D11_MAPPED_SUBRESOURCE Mapped = {};
	u32 Mask = 0;
	if (HWContext1 != nullptr)
	{
		u32 Total = 0;
		for (u32 WriteIdx = 0; WriteIdx < Count; ++WriteIdx)
		{
			Total += (Writes[WriteIdx].Size + 255) & ~255u;
		}
		VERIFY(Total <= ConstantRingSize);
		if (RingPos + Total > ConstantRingSize)
		{
			++RingIndex;
			RingPos = 0;
		}
		if (RingIndex == ConstantRings.size())
		{
			const D3D11_BUFFER_DESC Desc = { ConstantRingSize, D3D11_USAGE_DYNAMIC, D3D11_BIND_CONSTANT_BUFFER, D3D11_CPU_ACCESS_WRITE, 0, 0 };
			ConstantRings.push_back(nullptr);
			R_CHK(Device->CreateBuffer(&Desc, nullptr, &ConstantRings.back()));
		}
		ID3D11Buffer* Ring = ConstantRings[RingIndex];
		if (FAILED(HWRenderContext->Map(Ring, 0, RingPos ? D3D11_MAP_WRITE_NO_OVERWRITE : D3D11_MAP_WRITE_DISCARD, 0, &Mapped)))
		{
			return;
		}
		for (u32 WriteIdx = 0; WriteIdx < Count; ++WriteIdx)
		{
			const RHIConstantWrite& Write = Writes[WriteIdx];
			const u32 Aligned = (Write.Size + 255) & ~255u;
			memcpy(static_cast<u8*>(Mapped.pData) + RingPos, Write.Data, Write.Size);
			ConstantBuffers[Write.Slot] = Ring;
			ConstantFirst[Write.Slot] = RingPos / 16;
			ConstantNum[Write.Slot] = Aligned / 16;
			RingPos += Aligned;
			Mask |= 1u << Write.Slot;
		}
		HWRenderContext->Unmap(Ring, 0);
	}
	else
	{
		for (u32 WriteIdx = 0; WriteIdx < Count; ++WriteIdx)
		{
			const RHIConstantWrite& Write = Writes[WriteIdx];
			ID3D11Buffer*& Buffer = ConstantBuffers[Write.Slot];
			if (ConstantCapacity[Write.Slot] < Write.Size)
			{
				if (Buffer != nullptr)
				{
					Buffer->Release();
				}
				const D3D11_BUFFER_DESC Desc = { Write.Size, D3D11_USAGE_DYNAMIC, D3D11_BIND_CONSTANT_BUFFER, D3D11_CPU_ACCESS_WRITE, 0, 0 };
				R_CHK(Device->CreateBuffer(&Desc, nullptr, &Buffer));
				ConstantCapacity[Write.Slot] = Write.Size;
				Mask |= 1u << Write.Slot;
			}
			if (FAILED(HWRenderContext->Map(Buffer, 0, D3D11_MAP_WRITE_DISCARD, 0, &Mapped)))
			{
				continue;
			}
			memcpy(Mapped.pData, Write.Data, Write.Size);
			HWRenderContext->Unmap(Buffer, 0);
		}
	}
	for (u32& Pending : ConstantPending)
	{
		Pending |= Mask;
	}
	ConstantLive |= Mask;
}

void InternalDevice11::ApplyBindings(u32 Stages)
{
	static const decltype(&ID3D11DeviceContext1::PSSetConstantBuffers1) SetRanges[] = { &ID3D11DeviceContext1::PSSetConstantBuffers1, &ID3D11DeviceContext1::VSSetConstantBuffers1,
		&ID3D11DeviceContext1::GSSetConstantBuffers1, &ID3D11DeviceContext1::HSSetConstantBuffers1, &ID3D11DeviceContext1::DSSetConstantBuffers1, &ID3D11DeviceContext1::CSSetConstantBuffers1 };
	static const decltype(&ID3D11DeviceContext::PSSetConstantBuffers) SetBuffers[] = { &ID3D11DeviceContext::PSSetConstantBuffers, &ID3D11DeviceContext::VSSetConstantBuffers,
		&ID3D11DeviceContext::GSSetConstantBuffers, &ID3D11DeviceContext::HSSetConstantBuffers, &ID3D11DeviceContext::DSSetConstantBuffers, &ID3D11DeviceContext::CSSetConstantBuffers };
	for (u32 Stage = 0; Stage < 6; ++Stage)
	{
		const u32 Pending = ConstantPending[Stage];
		if (!(Stages & (1u << Stage)) || Pending == 0)
		{
			continue;
		}
		const u32 Lo = std::countr_zero(Pending);
		const u32 Num = 32 - std::countl_zero(Pending) - Lo;
		if (HWContext1 != nullptr)
		{
			(HWContext1->*SetRanges[Stage])(Lo, Num, ConstantBuffers + Lo, ConstantFirst + Lo, ConstantNum + Lo);
		}
		else
		{
			(HWRenderContext->*SetBuffers[Stage])(Lo, Num, ConstantBuffers + Lo);
		}
		ConstantPending[Stage] = 0;
	}

	static const decltype(&ID3D11DeviceContext::PSSetSamplers) SetSamplerFns[] = { &ID3D11DeviceContext::PSSetSamplers, &ID3D11DeviceContext::VSSetSamplers,
		&ID3D11DeviceContext::GSSetSamplers, &ID3D11DeviceContext::HSSetSamplers, &ID3D11DeviceContext::DSSetSamplers, &ID3D11DeviceContext::CSSetSamplers };
	for (u32 Pending = Stages & SamplerPending; Pending; Pending &= Pending - 1)
	{
		(HWRenderContext->*SetSamplerFns[std::countr_zero(Pending)])(0, SamplerCount, Samplers);
	}
	SamplerPending &= ~Stages;
}

void InternalDevice11::InvalidateBindings()
{
	for (u32& Pending : ConstantPending)
	{
		Pending = ConstantLive;
	}
	SamplerPending = 0x3f;
}

IRHIShaderResourceView* InternalDevice11::CreateShaderResourceView(IRHIBuffer* Buffer, const RHIShaderResourceViewDesc* desc)
{
		ID3D11Device* DxDevice = (ID3D11Device*)RawDevice;
		D3D11_SHADER_RESOURCE_VIEW_DESC Desc = {};

		Desc.Format = (DXGI_FORMAT)desc->Format;
		Desc.Buffer.ElementWidth = desc->ElementWidth;
		Desc.ViewDimension = D3D11_SRV_DIMENSION_BUFFER;

		ID3D11ShaderResourceView* srv = nullptr;
		R_CHK(DxDevice->CreateShaderResourceView(((CD3D11Buffer*)Buffer)->GetD3DObject(), &Desc, &srv));

		return new DX11ShaderResourceView(srv);
}

IRHIUnorderedAccessView* InternalDevice11::CreateUAV(IRHIBuffer* Buffer, const RHIUAVDesc& Desc)
{
	RHIUAVDesc BufferDesc = Desc;
	BufferDesc.ViewDimension = ERHI_VIEW_DIMENSION::Buffer;
	return TextureFactory->CreateUAV(static_cast<CD3D11Buffer*>(Buffer)->GetD3DObject(), BufferDesc);
}
