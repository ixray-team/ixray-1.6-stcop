#pragma once
#include "../RHI.h"
#include <d3d11.h>

class DX11ShaderDeclaration final:
	public IRHIShaderDeclaration
{
public:
	xr_vector<D3D11_INPUT_ELEMENT_DESC> DX11Descriptors;

public:
	DX11ShaderDeclaration(const RHIInputElementDesc* DescList, size_t DescCount) : IRHIShaderDeclaration(DescList, DescCount) {};
	virtual ~DX11ShaderDeclaration() {};
	virtual void GenerateLayerDescriptors(void* Signature) override;
	virtual void ApplyLayout() override;

private:
	ID3D11InputLayout* InputLayout = nullptr;
};