#pragma once
#include "RHI.h"

IRHIShaderDeclaration::IRHIShaderDeclaration(const RHIInputElementDesc* DescList, size_t DescCount)
{
	Descriptors.resize(DescCount);

	size_t Iter = 0;
	for (RHIInputElementDesc& Desc : Descriptors)
	{
		Desc.AlignedByteOffset = DescList[Iter].AlignedByteOffset;
		Desc.SemanticName = DescList[Iter].SemanticName;
		Desc.InputSlot = DescList[Iter].InputSlot;
		Desc.SemanticIndex = DescList[Iter].SemanticIndex;
		Desc.Format = DescList[Iter].Format;
		Desc.InputSlotClass = DescList[Iter].InputSlotClass;
		Desc.InstanceDataStepRate = DescList[Iter].InstanceDataStepRate;
		Iter++;
	}

	//memcpy(Descriptors.data(), DescList, DescCount * sizeof(RHIInputElementDesc));
	VertexSize = GRHI->GetInputElementDescStride(DescList, DescCount);
}