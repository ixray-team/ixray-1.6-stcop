#pragma once

#include "TiramisuRenderTypes.h"
enum class EShaderType;
class TiramisuShaderDefinesContainer;
class TiramisuShaderCompilerBase;
class TiramisuGlobalShader;


// Загружает, компилирует и кэширует global shaders renderer.
class TiramisuGlobalShadersManager
{
public:
	TiramisuGlobalShadersManager(nri::GraphicsAPI GraphicsAPI, bool NeedCreateShaderPDB, bool DebugShader);
	~TiramisuGlobalShadersManager();
	const xr_vector<char>& GetShader(shared_str name, EShaderType ShaderType, const TiramisuShaderDefinesContainer& DefinesContainer);
#if IXR_ENABLE_SHADER_COMPILER
	bool Cooked();
	void Rebuild();
	bool RegisterShader(xr_vector<shared_str> Defines, const char* Name, EShaderType Type);
#endif
private:
	void LoadFromBinary();
#if IXR_ENABLE_SHADER_COMPILER
	bool LoadFromSourceFiles();
	TiramisuShaderCompilerBase* ShaderCompiler;
#endif
	xr_map<shared_str, xr_vector<TiramisuGlobalShader*>> Shaders;
};
