#include "stdafx.h"

#include "XRayGlobalShadersManager.h"
#include "XRayGlobalShader.h"

#include "Resources/XRayRenderResourcesManager.h"
#include "Resources/Shaders/XRayShaderType.h"
#include "Resources/Shaders/Compiler/XRayShaderCompilerBase.h"
#include "Resources/Shaders/Defines/XRayShaderDefinesContainer.h"
#include "Resources/Shaders/Defines/XRayShaderDefinesManager.h"

XRayGlobalShadersManager::XRayGlobalShadersManager(nri::GraphicsAPI Platform, bool NeedCreateShaderPDB, bool DebugShader)
{
#if IXR_ENABLE_SHADER_COMPILER
	ShaderCompiler = nullptr;
#endif

	if (GRenderResourcesManager->IsCookedMode())
	{
		LoadFromBinary();
	}
#if IXR_ENABLE_SHADER_COMPILER
	else
	{
		ShaderCompiler = XRayShaderCompilerBase::Create(Platform, NeedCreateShaderPDB, DebugShader);
		ShaderCompiler->RescanIncludes();
		R_ASSERT(LoadFromSourceFiles());
	}
#endif
}

const xr_vector<char>& XRayGlobalShadersManager::GetShader(shared_str name, EXRayShaderType ShaderType, const XRayShaderDefinesContainer& DefinesContainer)
{
	XRayShaderDefinesContainer* Defines = GRenderResourcesManager->ShaderDefinesManager->RegistryContainer(DefinesContainer);
	auto Shader = Shaders.find(name);
	R_ASSERT(Shader != Shaders.end());

	auto LowerItem = std::lower_bound
	(
		Shader->second.begin(), Shader->second.end(), 0, 
		[ShaderType, Defines](const XRayGlobalShader* Left, int) 
		{
			return Left->IsLess(ShaderType, Defines); 
		}
	);

	R_ASSERT(LowerItem != Shader->second.end());
	R_ASSERT((*LowerItem)->IsEqual(ShaderType, Defines));

	return (*LowerItem)->Get();
}

XRayGlobalShadersManager::~XRayGlobalShadersManager()
{
#if IXR_ENABLE_SHADER_COMPILER
	xr_delete(ShaderCompiler);
#endif

	for (auto& [_, GlobalShaderPtr] : Shaders)
	{
		for (XRayGlobalShader* a : GlobalShaderPtr)
		{
			xr_delete(a);
		}
	}
}

void XRayGlobalShadersManager::LoadFromBinary()
{
	IReader* Reader = FS.r_open("$game_data$", GLOBAL_SHADERS_SHIPPING_NAME);
	if (!Reader)
	{
		Msg("! Can't load %s", GLOBAL_SHADERS_SHIPPING_NAME);
		R_ASSERT(false);
	}

	for (size_t i = 0;; i++)
	{
		IReader* F = Reader->open_chunk(i);
		if (0 == F)
		{
			break;
		}
		
		shared_str Name;
		F->r_stringZ(Name);
		
		Shaders[Name].push_back(new XRayGlobalShader(F));
		FS.r_close(F);
	}

	for (auto& [_, GlobalShaderPtr] : Shaders)
	{
		std::ranges::sort
		(
			GlobalShaderPtr,
			[](const XRayGlobalShader* Left, const XRayGlobalShader* Right)
			{
				return *Left < *Right;
			}
		);

	}
	FS.r_close(Reader);
}

#if IXR_ENABLE_SHADER_COMPILER
bool XRayGlobalShadersManager::Cooked()
{
	R_ASSERT(!GRenderResourcesManager->IsCookedMode());
	Msg("Cooked %s", GLOBAL_SHADERS_SHIPPING_NAME);

	if (FS.exist("$game_data$", GLOBAL_SHADERS_SHIPPING_NAME))
	{
		FS.file_delete("$game_data$", GLOBAL_SHADERS_SHIPPING_NAME);
	}

	IWriter* Writer = FS.w_open("$game_data$", GLOBAL_SHADERS_SHIPPING_NAME);
	if (!Writer)
	{
		Msg("! can't create file %s", GLOBAL_SHADERS_SHIPPING_NAME);
		return false;
	}

	Msg("Write Engine Shadres");

	size_t Count = 0;                      
	for (const auto& [Name, Shaders] : Shaders)
	{
		for (XRayGlobalShader* GlobalShader : Shaders)
		{
			Writer->open_chunk(Count++);
			Writer->w_stringZ(Name);
			*Writer << *GlobalShader;
			Writer->close_chunk();
		}
	}

	Msg("Engine Shadres is %d", Count);
	FS.w_close(Writer);
	return true;
}

void XRayGlobalShadersManager::Rebuild()
{
	R_ASSERT(!GRenderResourcesManager->IsCookedMode());

	for (auto& [name, shaderVec] : Shaders)
	{
		for (XRayGlobalShader* a : shaderVec)
		{
			xr_delete(a);
		}
	}

	Shaders.clear();

	bool bResult = true;
	do
	{
		bResult = LoadFromSourceFiles();

		if (!bResult)
		{
			xrLogger::FlushLog();
			INT ID = MessageBox(nullptr, TEXT("Failed to compile shaders.\r\n See log!."), TEXT("Error"), MB_ICONERROR | MB_RETRYCANCEL);
			switch (ID)
			{
			case IDNO:
			case IDCANCEL: exit(-1); break;
			case IDRETRY:
			case IDYES:
			case IDOK: break;
			default: exit(-1); break;
			}
		}

	} 
	while (!bResult);
}

static void LogShaderMessage(const xr_string& InFileName, const xr_string& Message, const xr_string& Header, const xr_string& Footer)
{
	xr_string Text;
	Text.reserve(Header.size() + InFileName.size() + Message.size() + Footer.size() + 20);
	Text.append(Header);
	Text.append("File:");
	Text.append(InFileName);
	Text.append("\n");
	Text.append(Message);
	Text.append("\n");
	Text.append(Footer);
	Msg(Text.c_str());
}

bool XRayGlobalShadersManager::RegisterShader(xr_vector<shared_str> Defines, const char* Name, EXRayShaderType Type)
{
	auto GetTypeByString = [](EXRayShaderType Type)
	{
		switch (Type)
		{
			case EXRayShaderType::Vertex:   return ".vs";
			case EXRayShaderType::Hull:     return ".hs";
			case EXRayShaderType::Domain:   return ".ds";
			case EXRayShaderType::Geometry: return ".gs";
			case EXRayShaderType::Pixel:    return ".ps";
			case EXRayShaderType::Compute:  return ".cs";
			default:
				NODEFAULT;
				return "";
		}
	};

	bool bResult = true;

	EXRayShaderType ShaderType = Type;
	shared_str ShaderName = Name;

	xr_vector<xr_string> IncludePaths;
	auto ApplyInclude = [&IncludePaths](const char* Path)
	{
		string_path IncludePath;
		xr_strcpy(IncludePath, Path);

		if (strrchr(IncludePath, Platform::kPreferredSeparator[0]))
		{
			strrchr(IncludePath, Platform::kPreferredSeparator[0])[0] = 0;
		}

		IncludePaths.push_back(IncludePath);
	};

	string_path CommonPath;
	xr_strcpy(CommonPath, "r5\\common\\");
	FS.update_path(CommonPath, "$game_shaders$", CommonPath);
	ApplyInclude(CommonPath);

	string_path InFileName;
	xr_strconcat(InFileName, "r5\\global\\", ShaderName.c_str(), GetTypeByString(ShaderType), ".hlsl");
	FS.update_path(InFileName, "$game_shaders$", InFileName);
	ApplyInclude(InFileName);

	xr_vector<XRayGlobalShader*>& InShaders = Shaders[ShaderName];

	R_ASSERT(Defines.size() < 16);
	const u16 CountShader = u16(1) << Defines.size();

	for (u16 i = 0; i < CountShader; i++)
	{
		Flags16 CurrentFlags; 
		CurrentFlags.assign(i);

		XRayShaderDefinesContainer DefinesContainer;

		for (size_t a = 0; a < Defines.size(); a++)
		{
			if (CurrentFlags.bitTest(a))
			{
				DefinesContainer.Add(Defines[a]);
			}
		}

		DefinesContainer.UpdateCRC32();

		shared_str NameFile;
		NameFile.printf("%04x", i);

		string_path OutFileName;
		xr_strconcat(OutFileName, ShaderCompiler->GetDirectionName(), "global\\", ShaderName.c_str(), "\\", NameFile.c_str(), GetTypeByString(ShaderType), ".bin");
		FS.update_path(OutFileName, "$intermediate$", OutFileName);

		if (!ShaderCompiler->Check(InFileName, DefinesContainer, OutFileName))
		{
			Msg("* Compile Engine Shader: %s%s.hlsl[%s%s%s]", ShaderName.c_str(), GetTypeByString(ShaderType), NameFile.c_str(), GetTypeByString(ShaderType), ".bin");
			xr_string MessageOut;

			if (!ShaderCompiler->Compile(DefinesContainer, ShaderType, IncludePaths, InFileName, OutFileName, MessageOut))
			{
				LogShaderMessage(InFileName, MessageOut, "!! SHADER ERROR !!\n", "!! End Message !!");
				bResult = false;
			}
			else if (MessageOut.size())
			{
				LogShaderMessage(InFileName, MessageOut, "!! SHADER WARNING !!\n", "!! End Message !!");
			}
		}

		if (bResult)
		{
			xr_vector<char> Data;
			if (bResult)
			{
				ShaderCompiler->ReadBinaryFile(OutFileName, Data);
				InShaders.emplace_back(new XRayGlobalShader(Data, ShaderType, GRenderResourcesManager->ShaderDefinesManager->RegistryContainer(DefinesContainer)));
			}
		}
	}

	std::ranges::sort
	(
		InShaders,
		[](const auto Left, const auto Right)
		{
			return *Left < *Right;
		}
	);

	return bResult;
};

bool XRayGlobalShadersManager::LoadFromSourceFiles()
{
	bool bResult = true;

	bResult = RegisterShader({}, "output", EXRayShaderType::Pixel);
	bResult = bResult && RegisterShader({}, "ui_no_transform", EXRayShaderType::Vertex);
	bResult = bResult && RegisterShader({}, "ui_screen_transform", EXRayShaderType::Vertex);
	bResult = bResult && RegisterShader({}, "ui", EXRayShaderType::Pixel);
	return bResult;
}
#endif