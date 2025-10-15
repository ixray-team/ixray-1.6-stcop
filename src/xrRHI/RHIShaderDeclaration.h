#pragma once
#include "RHITypes.h"

class RHI_API IRHIShaderDeclaration
{
public:
	xr_vector<RHIInputElementDesc> Descriptors;
	u32 VertexSize = (u32)-1;

public:
	IRHIShaderDeclaration(const RHIInputElementDesc* DescList, size_t DescCount);

	virtual ~IRHIShaderDeclaration() {};
	virtual void GenerateLayerDescriptors(void* Signature) = 0;
	virtual void ApplyLayout() = 0;
};

namespace RHIUtils::Shader
{
	RHI_API bool CreateInputLayoutFromFVF(uint32_t fvfCode, xr_vector<RHIInputElementDesc>& il);
	RHI_API size_t ComputeVertexStride(const xr_vector<RHIInputElementDesc>& il);
}