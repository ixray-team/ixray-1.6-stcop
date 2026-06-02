#pragma once
#include "Resources/Shaders/Defines/TShaderDefinesManager.h"

enum class EShaderType;
class TShaderDefinesContainer;

class TGlobalShader
{
public:
							TGlobalShader		(IReader* Reader);
#if IXR_ENABLE_SHADER_COMPILER
							TGlobalShader		(const xr_vector<char>&InData, EShaderType InType, TShaderDefinesContainer* InDefinesContainer);
							TGlobalShader		(unsigned char* InData, size_t Len, EShaderType InType, TShaderDefinesContainer* InDefinesContainer);
#endif
							~TGlobalShader		() = default;
	const xr_vector<char>&	Get					() const;
	bool					IsEqual				(EShaderType InType, TShaderDefinesContainer* InDefinesContainer) const;
	bool					IsLess				(EShaderType InType, TShaderDefinesContainer* InDefinesContainer) const;
	bool					operator<			(const TGlobalShader&Right) const;
	
	EShaderType				Type;
	xr_vector<char>				Data;
	TShaderDefinesContainer*	DefinesContainer;
};

#if IXR_ENABLE_SHADER_COMPILER
IC IWriter& operator<<(IWriter& Writer, const TGlobalShader& Shader)
{
	Writer.w_u32(static_cast<u32>(Shader.Type));
	Writer << *Shader.DefinesContainer;
	Writer.w_u32(static_cast<u32>(Shader.Data.size()));
	Writer.w(Shader.Data.data(), Shader.Data.size());
	return Writer;
}
#endif

IC IReader& operator>>(IReader& Reader, TGlobalShader& Shader)
{
	Shader.Type = static_cast<EShaderType>(Reader.r_u32());

	TShaderDefinesContainer InDefinesContainer;
	Reader >> InDefinesContainer;
	Shader.DefinesContainer = GRenderResourcesManager->ShaderDefinesManager->RegistryContainer(InDefinesContainer);

	Shader.Data.resize(Reader.r_u32());
	Reader.r(Shader.Data.data(), Shader.Data.size());
	return Reader;
}