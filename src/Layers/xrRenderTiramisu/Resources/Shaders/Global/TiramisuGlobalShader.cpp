#include "TiramisuGlobalShader.h"

#include "Resources/TiramisuRenderResourcesManager.h"
#include "Resources/Shaders/ShaderType.h"
#include "Resources/Shaders/Defines/TiramisuShaderDefinesContainer.h"

TiramisuGlobalShader::TiramisuGlobalShader(IReader* Reader)
{
	*Reader >> *this;
}

#if IXR_ENABLE_SHADER_COMPILER
TiramisuGlobalShader::TiramisuGlobalShader(const xr_vector<char>& InData, EShaderType InType, TiramisuShaderDefinesContainer* InDefinesContainer) :
	Type(InType), DefinesContainer(InDefinesContainer)
{
	Data = InData;
}

TiramisuGlobalShader::TiramisuGlobalShader(unsigned char* InData, size_t Len, EShaderType InType, TiramisuShaderDefinesContainer* InDefinesContainer) :
	Type(InType), DefinesContainer(InDefinesContainer)
{
	Data.resize(Len);
	memcpy(Data.data(), InData, Len);
}
#endif

const xr_vector<char>& TiramisuGlobalShader::Get() const
{
	return Data;
}

bool TiramisuGlobalShader::IsEqual(EShaderType InType, TiramisuShaderDefinesContainer* InDefinesContainer) const
{
	if (Type != InType)
	{
		return false;
	}
	
	if (DefinesContainer != InDefinesContainer)
	{
		return false;
	}
	
	return true;
}

bool TiramisuGlobalShader::IsLess(EShaderType InType, TiramisuShaderDefinesContainer* InDefinesContainer) const
{
	if (Type != InType)
	{
		return Type < InType;
	}
	
	if (DefinesContainer != InDefinesContainer)
	{
		return DefinesContainer < InDefinesContainer;
	}
	
	return false;
}

bool TiramisuGlobalShader::operator<(const TiramisuGlobalShader& Right) const
{
	return IsLess(Right.Type, Right.DefinesContainer);
}