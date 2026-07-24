#pragma once

#include "TiramisuRenderTypes.h"
#include "Resources/Shaders/Defines/TiramisuShaderDefinesManager.h"

enum class EShaderType;
class TiramisuShaderDefinesContainer;

// Одна backend-specific permutation глобального shader.
class TiramisuGlobalShader
{
public:
								TiramisuGlobalShader		(IReader* Reader);
#if IXR_ENABLE_SHADER_COMPILER
								TiramisuGlobalShader		(const xr_vector<char>&InData, EShaderType InType, TiramisuShaderDefinesContainer* InDefinesContainer);
								TiramisuGlobalShader		(unsigned char* InData, size_t Len, EShaderType InType, TiramisuShaderDefinesContainer* InDefinesContainer);
#endif
								~TiramisuGlobalShader		() = default;
	const xr_vector<char>&		Get					() const;
	bool						IsEqual				(EShaderType InType, TiramisuShaderDefinesContainer* InDefinesContainer) const;
	bool						IsLess				(EShaderType InType, TiramisuShaderDefinesContainer* InDefinesContainer) const;
	bool						operator<			(const TiramisuGlobalShader&Right) const;
	
	EShaderType					Type;
	xr_vector<char>				Data;
	TiramisuShaderDefinesContainer*	DefinesContainer;
};

#if IXR_ENABLE_SHADER_COMPILER
IC IWriter& operator<<(IWriter& Writer, const TiramisuGlobalShader& Shader)
{
	Writer.w_u32(static_cast<u32>(Shader.Type));
	Writer << *Shader.DefinesContainer;
	Writer.w_u32(static_cast<u32>(Shader.Data.size()));
	Writer.w(Shader.Data.data(), Shader.Data.size());
	return Writer;
}
#endif

IC IReader& operator>>(IReader& Reader, TiramisuGlobalShader& Shader)
{
	Shader.Type = static_cast<EShaderType>(Reader.r_u32());

	TiramisuShaderDefinesContainer InDefinesContainer;
	Reader >> InDefinesContainer;
	Shader.DefinesContainer = GRenderResourcesManager->ShaderDefinesManager->RegistryContainer(InDefinesContainer);

	Shader.Data.resize(Reader.r_u32());
	Reader.r(Shader.Data.data(), Shader.Data.size());
	return Reader;
}
