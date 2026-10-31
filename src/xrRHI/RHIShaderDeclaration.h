#pragma once
#include "RHITypes.h"
class GfxCmdList;

namespace RHIUtils::Shader
{
	RHI_API bool CreateInputLayoutFromFVF(uint32_t fvfCode, xr_vector<RHIInputElementDesc>& il);
	RHI_API size_t ComputeVertexStride(const xr_vector<RHIInputElementDesc>& il);
}