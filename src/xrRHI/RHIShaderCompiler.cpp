#include "RHI.h"
#include "RHIShaderCompiler.h"

#ifdef IXR_WINDOWS
#	include <d3dcompiler.h>
#else
struct D3D_SHADER_MACRO
{
    const char* Name;
    const char* Definition;
};


#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <filesystem>
#include <sstream>

CRHIShaderCompilerShell::CRHIShaderCompilerShell(ERHI_API_LAYER API) :
	Layer(API)
{
}

HRESULT CRHIShaderCompilerShell::Build(const void* srcData, size_t srcSize, const char* sourceName, const void* defines, void* include, 
	                                   const char* entryPoint, const char* target, u32 flags1, u32 flags2, xr_vector<u8>& code, xr_vector<u8>& errors)
{
#ifdef IXR_WINDOWS
    ID3DBlob* code_blob = nullptr;
    ID3DBlob* errors_blob = nullptr;
	HRESULT hr = D3DCompile(srcData, srcSize, sourceName, (D3D_SHADER_MACRO*)defines, (ID3DInclude*)include, entryPoint, target, flags1, flags2, &code_blob, &errors_blob);

	if (code_blob != nullptr) {
		size_t size = code_blob->GetBufferSize();
		if (size != 0) {
			code.resize(size);
			memcpy(code.data(), code_blob->GetBufferPointer(), size);
			code_blob->Release();
		}
	}

	if (errors_blob != nullptr) {
		size_t size = errors_blob->GetBufferSize();
		if (size != 0) {
			errors.resize(size);
			memcpy(errors.data(), errors_blob->GetBufferPointer(), size);
			errors_blob->Release();
			errors.push_back('\0');
		}
	}

	return hr;
#else
    namespace fs = std::filesystem;

    char srcPath[L_tmpnam];
    char outPath[L_tmpnam];
    char errPath[L_tmpnam];

    std::tmpnam(srcPath);
    std::tmpnam(outPath);
    std::tmpnam(errPath);

    {
        std::ofstream src(srcPath, std::ios::binary);
        src.write((const char*)srcData, srcSize);
    }

    std::stringstream cmd;

    cmd << "wine fxc.exe "
        << srcPath
        << " /T " << target
        << " /E " << entryPoint;

    if (defines)
    {
        const D3D_SHADER_MACRO* defs = (const D3D_SHADER_MACRO*)defines;

        for (int i = 0; defs[i].Name != nullptr; ++i)
        {
            cmd << " /D " << defs[i].Name;

            if (defs[i].Definition)
                cmd << "=" << defs[i].Definition;
        }
    }

    cmd << " /Fo " << outPath
        << " > " << errPath << " 2>&1";
    
    int result = std::system(cmd.str().c_str());

    // Read compiled blob
    if (fs::exists(outPath))
    {
        std::ifstream file(outPath, std::ios::binary | std::ios::ate);
        if (file)
        {
            size_t size = (size_t)file.tellg();
            file.seekg(0);

            code.resize(size);
            file.read((char*)code.data(), size);
        }
    }

    // Read compiler output/errors
    if (fs::exists(errPath))
    {
        std::ifstream file(errPath, std::ios::binary | std::ios::ate);
        if (file)
        {
            size_t size = (size_t)file.tellg();
            file.seekg(0);

            errors.resize(size);
            file.read((char*)errors.data(), size);
        }
    }

    std::remove(srcPath);
    std::remove(outPath);
    std::remove(errPath);

    return result == 0 ? S_OK : E_FAIL;
#endif
}