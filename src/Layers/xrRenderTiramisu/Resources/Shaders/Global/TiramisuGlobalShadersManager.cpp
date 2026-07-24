#include "stdafx.h"

#include "TiramisuGlobalShadersManager.h"
#include "TiramisuGlobalShader.h"

#include "Resources/TiramisuRenderResourcesManager.h"
#include "Resources/Shaders/ShaderType.h"
#include "Resources/Shaders/Compiler/TiramisuShaderCompilerBase.h"
#include "Resources/Shaders/Defines/TiramisuShaderDefinesContainer.h"
#include "Resources/Shaders/Defines/TiramisuShaderDefinesManager.h"

TiramisuGlobalShadersManager::TiramisuGlobalShadersManager(nri::GraphicsAPI Platform, bool NeedCreateShaderPDB, bool DebugShader)
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
		ShaderCompiler = TiramisuShaderCompilerBase::Create(Platform, NeedCreateShaderPDB, DebugShader);
		ShaderCompiler->RescanIncludes();
		R_ASSERT(LoadFromSourceFiles());
	}
#endif
}

const xr_vector<char>& TiramisuGlobalShadersManager::GetShader(shared_str name, EShaderType ShaderType, const TiramisuShaderDefinesContainer& DefinesContainer)
{
	CheckIsRenderThread();
	TiramisuShaderDefinesContainer* Defines = GRenderResourcesManager->ShaderDefinesManager->RegistryContainer(DefinesContainer);
	auto Shader = Shaders.find(name);
	R_ASSERT(Shader != Shaders.end());

	auto LowerItem = std::lower_bound(
		Shader->second.begin(), Shader->second.end(), 0, [ShaderType, Defines](const TiramisuGlobalShader* Left, int)
		{ return Left->IsLess(ShaderType, Defines); }
	);

	R_ASSERT(LowerItem != Shader->second.end());
	R_ASSERT((*LowerItem)->IsEqual(ShaderType, Defines));

	return (*LowerItem)->Get();
}

TiramisuGlobalShadersManager::~TiramisuGlobalShadersManager()
{
#if IXR_ENABLE_SHADER_COMPILER
	xr_delete(ShaderCompiler);
#endif

	for (auto& [_, GlobalShaderPtr] : Shaders)
	{
		for (TiramisuGlobalShader* a : GlobalShaderPtr)
		{
			xr_delete(a);
		}
	}
}

void TiramisuGlobalShadersManager::LoadFromBinary()
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

		Shaders[Name].push_back(new TiramisuGlobalShader(F));
		FS.r_close(F);
	}

	for (auto& [_, GlobalShaderPtr] : Shaders)
	{
		std::ranges::sort(
			GlobalShaderPtr,
			[](const TiramisuGlobalShader* Left, const TiramisuGlobalShader* Right)
			{
				return *Left < *Right;
			}
		);
	}
	FS.r_close(Reader);
}

#if IXR_ENABLE_SHADER_COMPILER
bool TiramisuGlobalShadersManager::Cooked()
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
		for (TiramisuGlobalShader* GlobalShader : Shaders)
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

void TiramisuGlobalShadersManager::Rebuild()
{
	CheckIsRenderThread();
	R_ASSERT(!GRenderResourcesManager->IsCookedMode());

	for (auto& [name, shaderVec] : Shaders)
	{
		for (TiramisuGlobalShader* a : shaderVec)
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
				case IDCANCEL:
					exit(-1);
					break;
				case IDRETRY:
				case IDYES:
				case IDOK:
					break;
				default:
					exit(-1);
					break;
			}
		}

	} while (!bResult);
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

bool TiramisuGlobalShadersManager::RegisterShader(xr_vector<shared_str> Defines, const char* Name, EShaderType Type)
{
	auto GetTypeByString = [](EShaderType Type)
	{
		switch (Type)
		{
			case EShaderType::Vertex:
				return ".vs";
			case EShaderType::Hull:
				return ".hs";
			case EShaderType::Domain:
				return ".ds";
			case EShaderType::Geometry:
				return ".gs";
			case EShaderType::Pixel:
				return ".ps";
			case EShaderType::Compute:
				return ".cs";
			default:
				NODEFAULT;
				return "";
		}
	};

	bool bResult = true;

	EShaderType ShaderType = Type;
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

	xr_vector<TiramisuGlobalShader*>& InShaders = Shaders[ShaderName];

	R_ASSERT(Defines.size() < 16);
	const u16 CountShader = u16(1) << Defines.size();

	for (u16 i = 0; i < CountShader; i++)
	{
		Flags16 CurrentFlags;
		CurrentFlags.assign(i);

		TiramisuShaderDefinesContainer DefinesContainer;

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
				InShaders.emplace_back(new TiramisuGlobalShader(Data, ShaderType, GRenderResourcesManager->ShaderDefinesManager->RegistryContainer(DefinesContainer)));
			}
		}
	}

	std::ranges::sort(
		InShaders,
		[](const auto Left, const auto Right)
		{
			return *Left < *Right;
		}
	);

	return bResult;
};

bool TiramisuGlobalShadersManager::LoadFromSourceFiles()
{
	bool bResult = true;

	bResult = RegisterShader({}, "output", EShaderType::Pixel);
	bResult = bResult && RegisterShader({}, "ui_no_transform", EShaderType::Vertex);
	bResult = bResult && RegisterShader({}, "ui_screen_transform", EShaderType::Vertex);
	bResult = bResult && RegisterShader({}, "ui", EShaderType::Pixel);
	bResult = bResult && RegisterShader({}, "scene_vertex", EShaderType::Vertex);
	bResult = bResult && RegisterShader({}, "scene_vertex", EShaderType::Pixel);
	bResult = bResult && RegisterShader({}, "scene_lmap", EShaderType::Vertex);
	bResult = bResult && RegisterShader({}, "scene_lmap", EShaderType::Pixel);
	return bResult;
}
#endif