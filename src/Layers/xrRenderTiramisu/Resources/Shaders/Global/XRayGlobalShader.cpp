#include "XRayGlobalShader.h"

#include "Resources/XRayRenderResourcesManager.h"
#include "Resources/Shaders/XRayShaderType.h"
#include "Resources/Shaders/Defines/XRayShaderDefinesContainer.h"

XRayGlobalShader::XRayGlobalShader(IReader* Reader)
{
	*Reader >> *this;
}

#if IXR_ENABLE_SHADER_COMPILER
XRayGlobalShader::XRayGlobalShader(const xr_vector<char>& InData, EXRayShaderType InType, XRayShaderDefinesContainer* InDefinesContainer) :
	Type(InType), DefinesContainer(InDefinesContainer)
{
	Data = InData;
}

XRayGlobalShader::XRayGlobalShader(unsigned char* InData, size_t Len, EXRayShaderType InType, XRayShaderDefinesContainer* InDefinesContainer) :
	Type(InType), DefinesContainer(InDefinesContainer)
{
	Data.resize(Len);
	memcpy(Data.data(), InData, Len);
}
#endif

const xr_vector<char>& XRayGlobalShader::Get() const
{
	return Data;
}

bool XRayGlobalShader::IsEqual(EXRayShaderType InType, XRayShaderDefinesContainer* InDefinesContainer) const
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

bool XRayGlobalShader::IsLess(EXRayShaderType InType, XRayShaderDefinesContainer* InDefinesContainer) const
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

bool XRayGlobalShader::operator<(const XRayGlobalShader& Right) const
{
	return IsLess(Right.Type, Right.DefinesContainer);
}