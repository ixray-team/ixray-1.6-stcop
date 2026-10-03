#pragma once
#include <d3d12shader.h>

bool RHI_IsDXIL(const void* code, size_t size);
bool RHI_IsUnsignedDXIL(const void* code, size_t size);
HRESULT RHI_DxcCompile(const void* source, size_t size, const char* name, const RHIShaderMacro* macros, IRHIShaderInclude* include,
	const char* entry, const char* target, u32 flags, RHIBlob** out_code, RHIBlob** out_errors);
HRESULT RHI_DxcReflect(const void* code, size_t size, ID3D12ShaderReflection** out_reflection);
