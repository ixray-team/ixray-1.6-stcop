#include "RHI.h"
#include "RHIShaderCompiler.h"

#ifdef IXR_WINDOWS
#	include <d3dcompiler.h>
#endif

CRHIShaderCompilerShell::CRHIShaderCompilerShell(ERHI_API_LAYER API) :
	Layer(API)
{
}

HRESULT CRHIShaderCompilerShell::Build(const void* srcData, size_t srcSize, const char* sourceName, const void* defines, void* include, const char* entryPoint, const char* target, u32 flags1, u32 flags2, void** code, void** errors)
{
#ifdef IXR_WINDOWS
	return D3DCompile(srcData, srcSize, sourceName, (D3D_SHADER_MACRO*)defines, (ID3DInclude*)include, entryPoint, target, flags1, flags2, (ID3DBlob**)code, (ID3DBlob**)errors);
#else
	return S_FALSE;
#endif
}