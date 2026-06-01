#pragma once
#if IXR_ENABLE_SHADER_COMPILER
enum class EXRayShaderType;
class XRayShaderDefinesContainer;

class XRayShaderCompilerBase
{
public:
	static XRayShaderCompilerBase*	Create					(nri::GraphicsAPI GraphicsAPI, bool NeedCreateShaderPDB, bool DebugShader);
									XRayShaderCompilerBase	(bool NeedCreateShaderPDB, bool DebugShader);
	virtual							~XRayShaderCompilerBase	() = default;

	virtual bool					Compile					(const XRayShaderDefinesContainer& Defines, EXRayShaderType ShaderType, const xr_vector<xr_string>& IncludePaths, const char* source_file_name, const char* result_file_name, xr_string& OutMessage) = 0;
	//virtual bool					CompileRootSignature	(const char* RootSignature, const char* result_file_name, xr_string& OutMessage) = 0;

	bool							Check					(const char* SourceFileName, const XRayShaderDefinesContainer& Defines, const char* ResultFileName);
	bool							Check					(const char* RootSignature, const char* ResultFileName);

	void							RescanIncludes			();
	bool							CheckIncludes			();

	IC u32							GetIncludeCrc32			()const { return IncludeCrc32; }
	IC u32							GetIncludeCount			()const { return IncludeCount; }
	IC u32							GetIncludeSize			()const { return IncludeSize; }
	IC u32							NeedCreateShaderPDB		()const { return bNeedCreateShaderPDB; }
	IC u32							IsDebugShader			()const { return bDebugShader; }

	virtual const char*				GetDirectionName		() = 0;

	void							ReadBinaryFile			(const char* FileName, xr_vector<char>& Data);
	void							ReadRootSignatureFile	(const char* FileName, xr_vector<char>& Data);
	void							ReadTextFile			(const char* FileName, xr_vector<char>& Data, bool& bIsUTF8);
protected:
	virtual void 					ReadFile				(const char* Name, xr_vector<char>& Data) = 0;
	virtual bool					FileExists				(const char* name) = 0;
	bool							bNeedCreateShaderPDB;
	bool							bDebugShader;
	u32								IncludeCrc32;
	u32								IncludeCount;
	u32								IncludeSize;
};

#endif