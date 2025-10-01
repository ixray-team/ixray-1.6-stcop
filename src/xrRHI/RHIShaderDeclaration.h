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