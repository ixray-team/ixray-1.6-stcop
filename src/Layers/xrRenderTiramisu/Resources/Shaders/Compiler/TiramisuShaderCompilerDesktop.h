#pragma once

#include "TiramisuRenderTypes.h"
#if IXR_ENABLE_SHADER_COMPILER
#include <wrl/client.h>
#include "TiramisuShaderCompilerBase.h"

enum class EShaderType;
class TiramisuShaderDefinesContainer;
class IDxcCompiler3;
class IDxcLibrary;

// Desktop DXC compiler для DXIL и SPIR-V permutations.
class TiramisuShaderCompilerDesktop :
	public TiramisuShaderCompilerBase
{
public:
	TiramisuShaderCompilerDesktop(nri::GraphicsAPI InGraphicsAPI, bool NeedCreateShaderPDB, bool DebugShader);
	~TiramisuShaderCompilerDesktop() override = default;
	const char* GetDirectionName() override;
	bool CompileVK(const TiramisuShaderDefinesContainer& Defines, EShaderType ShaderType, const xr_vector<xr_string>& IncludePaths, const char* source_file_name, const char* result_file_name, xr_string& OutMessage);
	bool CompileDX12(const TiramisuShaderDefinesContainer& Defines, EShaderType ShaderType, const xr_vector<xr_string>& IncludePaths, const char* source_file_name, const char* result_file_name, xr_string& OutMessage);
	bool CompileDX11(const TiramisuShaderDefinesContainer& Defines, EShaderType ShaderType, const xr_vector<xr_string>& IncludePaths, const char* source_file_name, const char* result_file_name, xr_string& OutMessage);
	bool Compile(const TiramisuShaderDefinesContainer& Defines, EShaderType ShaderType, const xr_vector<xr_string>& IncludePaths, const char* source_file_name, const char* result_file_name, xr_string& OutMessage) override;

protected:
	virtual void ReadFile(const char* Name, xr_vector<char>& Data) override;
	virtual bool FileExists(const char* name) override;

private:
	Microsoft::WRL::ComPtr<IDxcCompiler3> DxcCompiler;
	Microsoft::WRL::ComPtr<IDxcLibrary> DxcLibrary;
	nri::GraphicsAPI GraphicsAPI;
};

#endif
