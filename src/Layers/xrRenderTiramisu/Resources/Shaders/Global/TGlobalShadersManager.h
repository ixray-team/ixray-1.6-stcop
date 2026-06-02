#pragma once
enum class EShaderType;
class TShaderDefinesContainer;
class TShaderCompilerBase;
class TGlobalShader;


class TGlobalShadersManager
{
public:
														TGlobalShadersManager		(nri::GraphicsAPI GraphicsAPI, bool NeedCreateShaderPDB, bool DebugShader);
														~TGlobalShadersManager		();
	const xr_vector<char>&								GetShader					(shared_str name,EShaderType ShaderType,const TShaderDefinesContainer& DefinesContainer);
#if IXR_ENABLE_SHADER_COMPILER
	bool												Cooked						();
	void												Rebuild						();
	bool												RegisterShader 				(xr_vector<shared_str> Defines, const char* Name, EShaderType Type);
#endif
private:
	void												LoadFromBinary				();
#if IXR_ENABLE_SHADER_COMPILER
	bool												LoadFromSourceFiles			();
	TShaderCompilerBase*								ShaderCompiler;
#endif
	xr_map<shared_str, xr_vector< TGlobalShader*>>	Shaders;
};