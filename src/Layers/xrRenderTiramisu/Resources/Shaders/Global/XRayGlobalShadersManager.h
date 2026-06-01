#pragma once
enum class EXRayShaderType;
class XRayShaderDefinesContainer;
class XRayShaderCompilerBase;
class XRayGlobalShader;


class XRayGlobalShadersManager
{
public:
														XRayGlobalShadersManager	(nri::GraphicsAPI GraphicsAPI, bool NeedCreateShaderPDB, bool DebugShader);
														~XRayGlobalShadersManager	();
	const xr_vector<char>&								GetShader					(shared_str name,EXRayShaderType ShaderType,const XRayShaderDefinesContainer& DefinesContainer);
#if IXR_ENABLE_SHADER_COMPILER
	bool												Cooked						();
	void												Rebuild						();
	bool												RegisterShader 				(xr_vector<shared_str> Defines, const char* Name, EXRayShaderType Type);
#endif
private:
	void												LoadFromBinary				();
#if IXR_ENABLE_SHADER_COMPILER
	bool												LoadFromSourceFiles			();
	XRayShaderCompilerBase*								ShaderCompiler;
#endif
	xr_map<shared_str, xr_vector< XRayGlobalShader*>>	Shaders;
};