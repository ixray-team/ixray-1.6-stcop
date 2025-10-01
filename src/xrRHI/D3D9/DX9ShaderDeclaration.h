#pragma once
#include "../RHI.h"
#include <d3d9.h>

class DX9ShaderDeclaration final:
	public IRHIShaderDeclaration
{
public:
	xr_vector<D3DVERTEXELEMENT9> DX9Descriptors;

public:
	DX9ShaderDeclaration(const RHIInputElementDesc* DescList, size_t DescCount) : IRHIShaderDeclaration(DescList, DescCount) {};
	virtual ~DX9ShaderDeclaration();
	virtual void GenerateLayerDescriptors(void*) override;
	virtual void ApplyLayout() override;

private:
	IDirect3DVertexDeclaration9* VertDecl = nullptr;
};