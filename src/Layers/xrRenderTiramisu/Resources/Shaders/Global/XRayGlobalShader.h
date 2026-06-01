#pragma once
#include "Resources/Shaders/Defines/XRayShaderDefinesManager.h"

enum class EXRayShaderType;
class XRayShaderDefinesContainer;

class XRayGlobalShader
{
public:
							XRayGlobalShader	(IReader* Reader);
#if IXR_ENABLE_SHADER_COMPILER
							XRayGlobalShader	(const xr_vector<char>&InData, EXRayShaderType InType, XRayShaderDefinesContainer* InDefinesContainer);
							XRayGlobalShader	(unsigned char* InData, size_t Len, EXRayShaderType InType, XRayShaderDefinesContainer* InDefinesContainer);
#endif
							~XRayGlobalShader	() = default;
	const xr_vector<char>&	Get					() const;
	bool					IsEqual				(EXRayShaderType InType, XRayShaderDefinesContainer* InDefinesContainer) const;
	bool					IsLess				(EXRayShaderType InType, XRayShaderDefinesContainer* InDefinesContainer) const;
	bool					operator<			(const XRayGlobalShader&Right) const;
	
	EXRayShaderType				Type;
	xr_vector<char>				Data;
	XRayShaderDefinesContainer*	DefinesContainer;
};

#if IXR_ENABLE_SHADER_COMPILER
IC IWriter& operator<<(IWriter& Writer, const XRayGlobalShader& Shader)
{
	Writer.w_u32(static_cast<u32>(Shader.Type));
	Writer << *Shader.DefinesContainer;
	Writer.w_u32(static_cast<u32>(Shader.Data.size()));
	Writer.w(Shader.Data.data(), Shader.Data.size());
	return Writer;
}
#endif

IC IReader& operator>>(IReader& Reader, XRayGlobalShader& Shader)
{
	Shader.Type = static_cast<EXRayShaderType>(Reader.r_u32());

	XRayShaderDefinesContainer InDefinesContainer;
	Reader >> InDefinesContainer;
	Shader.DefinesContainer = GRenderResourcesManager->ShaderDefinesManager->RegistryContainer(InDefinesContainer);

	Shader.Data.resize(Reader.r_u32());
	Reader.r(Shader.Data.data(), Shader.Data.size());
	return Reader;
}