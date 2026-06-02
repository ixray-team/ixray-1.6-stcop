#include "TGlobalShader.h"

#include "Resources/TRenderResourcesManager.h"
#include "Resources/Shaders/ShaderType.h"
#include "Resources/Shaders/Defines/TShaderDefinesContainer.h"

TGlobalShader::TGlobalShader(IReader* Reader)
{
	*Reader >> *this;
}

#if IXR_ENABLE_SHADER_COMPILER
TGlobalShader::TGlobalShader(const xr_vector<char>& InData, EShaderType InType, TShaderDefinesContainer* InDefinesContainer) :
	Type(InType), DefinesContainer(InDefinesContainer)
{
	Data = InData;
}

TGlobalShader::TGlobalShader(unsigned char* InData, size_t Len, EShaderType InType, TShaderDefinesContainer* InDefinesContainer) :
	Type(InType), DefinesContainer(InDefinesContainer)
{
	Data.resize(Len);
	memcpy(Data.data(), InData, Len);
}
#endif

const xr_vector<char>& TGlobalShader::Get() const
{
	return Data;
}

bool TGlobalShader::IsEqual(EShaderType InType, TShaderDefinesContainer* InDefinesContainer) const
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

bool TGlobalShader::IsLess(EShaderType InType, TShaderDefinesContainer* InDefinesContainer) const
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

bool TGlobalShader::operator<(const TGlobalShader& Right) const
{
	return IsLess(Right.Type, Right.DefinesContainer);
}