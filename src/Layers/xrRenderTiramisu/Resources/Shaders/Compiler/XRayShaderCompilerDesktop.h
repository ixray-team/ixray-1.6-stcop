#pragma once
#if IXR_ENABLE_SHADER_COMPILER
#include <wrl/client.h>
#include "XRayShaderCompilerBase.h"

enum class EXRayShaderType;
class XRayShaderDefinesContainer;
class IDxcCompiler3;
class IDxcLibrary;

class XRayShaderCompilerDesktop:
	public XRayShaderCompilerBase
{
public:
											XRayShaderCompilerDesktop	(nri::GraphicsAPI InGraphicsAPI, bool NeedCreateShaderPDB,bool DebugShader);
											~XRayShaderCompilerDesktop	() override = default;
			const char*						GetDirectionName			() override;
			bool							CompileVK					(const XRayShaderDefinesContainer& Defines, EXRayShaderType ShaderType, const xr_vector<xr_string>& IncludePaths,const char*source_file_name,const char* result_file_name ,xr_string&OutMessage);
			bool							CompileDX12					(const XRayShaderDefinesContainer& Defines, EXRayShaderType ShaderType, const xr_vector<xr_string>& IncludePaths,const char*source_file_name,const char* result_file_name ,xr_string&OutMessage);
			bool							CompileDX11					(const XRayShaderDefinesContainer& Defines, EXRayShaderType ShaderType, const xr_vector<xr_string>& IncludePaths,const char*source_file_name,const char* result_file_name ,xr_string&OutMessage);
			bool							Compile						(const XRayShaderDefinesContainer& Defines, EXRayShaderType ShaderType, const xr_vector<xr_string>& IncludePaths,const char*source_file_name,const char* result_file_name ,xr_string&OutMessage) override;
protected:
	virtual void							ReadFile					(const char* Name, xr_vector<char>& Data) override;
	virtual bool							FileExists					(const char* name) override;
private:
	Microsoft::WRL::ComPtr<IDxcCompiler3>	DxcCompiler;
	Microsoft::WRL::ComPtr<IDxcLibrary>		DxcLibrary;
	nri::GraphicsAPI						GraphicsAPI;
};

#endif