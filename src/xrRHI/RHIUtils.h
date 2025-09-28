#pragma once

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

	inline bool CreateIndexBuffer(IRHIBuffer** ppBuffer, const void* pData, u32 DataSize, bool bImmutable = true, bool Read = false)
	{
		RHIBufferDesc desc = {};
		desc.Usage = (bImmutable || Read) ? ERHI_USAGE::USAGE_DEFAULT : ERHI_USAGE::USAGE_DYNAMIC;
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
		desc.CPUAccessFlags = ERHI_CPU_ACCESS_FLAG_WRITE;

		IRHIBuffer* pBuffer = GRHI->CreateBuffer(desc, nullptr);
		if (!pBuffer)
			return false;

		*ppBuffer = pBuffer;
		return true;
	}
}