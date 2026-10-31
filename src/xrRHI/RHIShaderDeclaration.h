#pragma once
#include "RHITypes.h"
class RHIBlob;

struct RHI_API RHIShaderConstant :
	public xr_resource
{
	class Setup
	{
	public:
		virtual void setup() = 0;
		virtual ~Setup() {}
	};

	shared_str name;
	u16 samp = u16(-1);
	Setup* handler = nullptr;

	RHIShaderConstant() = default;
	RHIShaderConstant& operator=(const RHIShaderConstant& Other) = delete;

	bool equal(const RHIShaderConstant& C) const
	{
		return name == C.name && samp == C.samp && handler == C.handler;
	}
};

class RHI_API IRHIShaderDeclaration
{
public:
	xr_vector<RHIInputElementDesc> Descriptors;
	u32 VertexSize = (u32)-1;

public:
	IRHIShaderDeclaration(const RHIInputElementDesc* DescList, size_t DescCount);

	virtual ~IRHIShaderDeclaration() {};
	virtual void GenerateLayerDescriptors(RHIBlob* Signature) = 0;
	virtual void ApplyLayout() = 0;
};

namespace RHIUtils::Shader
{
	RHI_API bool CreateInputLayoutFromFVF(uint32_t fvfCode, xr_vector<RHIInputElementDesc>& il);
	RHI_API size_t ComputeVertexStride(const xr_vector<RHIInputElementDesc>& il);
}