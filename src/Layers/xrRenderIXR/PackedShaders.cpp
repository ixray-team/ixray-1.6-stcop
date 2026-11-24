/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "stdafx.h"
#include "ixr.h"
#include <nlohmann/json.hpp>

namespace
{
	struct PackedShader
	{
		xr_string File;
		xr_string Defines;
	};

	xrCriticalSection PackedLock;
	xr_hash_map<xr_string, PackedShader> Packed;
	bool PackedLoaded = false;
	const char* const Folders[] = { "", "common\\", "surface\\", "lighting\\", "postfx\\", "effects\\", "ui\\", "screens\\", "editor\\", "d3d11\\" };

	class CPackedShaderReader final : public IReader
	{
	public:
		CPackedShaderReader(char* Text, size_t Length) : IReader(Text, Length)
		{
		}

		~CPackedShaderReader() override
		{
			xr_free(data);
		}
	};

	// Names are file names: an XML blend may spell one in another case
	xr_string PackedKey(xr_string Name)
	{
		std::transform(Name.begin(), Name.end(), Name.begin(), [](char Symbol) { return char(tolower(u8(Symbol))); });
		return Name;
	}

	void LoadPacked(const char* Directory, const char* Name)
	{
		string_path Path;
		xr_strconcat(Path, Directory, Name);
		IReader* File = FS.r_open(_game_shaders_, Path);

		if (!File)
		{
			return;
		}

		const char* Text = static_cast<const char*>(File->pointer());
		const nlohmann::json Data = nlohmann::json::parse(Text, Text + File->length(), nullptr, false);
		FS.r_close(File);

		if (Data.is_discarded() || !Data.contains("permutations"))
		{
			Msg("! Shader description '%s' has no permutations", Path);
			return;
		}

		const xr_string Source = xr_string(Directory) + xr_string(Name, strext(Name) - Name);

		for (const auto& [Stage, Names] : Data["permutations"].items())
		{
			for (const auto& [Alias, Defines] : Names.items())
			{
				PackedShader& Shader = Packed[PackedKey(xr_string(Alias.c_str()) + "." + Stage.c_str() + ".hlsl")];
				Shader.File = Source;
				Shader.Defines.clear();

				for (const auto& Define : Defines)
				{
					xr_string Line = Define.get<std::string>().c_str();
					std::replace(Line.begin(), Line.end(), '=', ' ');
					Shader.Defines += "#define " + Line + "\n";
				}
			}
		}
	}
}

// A stage is asked for by name. An addon's file of the old layout (<name>.<stage>.hlsl) wins; then a name a
// description lists (a permutation: its defines over the described file); then the shader's own file in one of the
// folders. For the last two the source handed to the compiler is generated (the defines, one include) and the
// entry point is <stage>_main; an old-layout file has main
IReader* CRender::OpenShader(string_path& Path, const char* Name, const char* Extension, string16& Entry)
{
	xr_strcpy(Entry, "main");
	ShaderPath(Path, Name, Extension);
	FS.update_path(Path, _game_shaders_, Path);

	if (IReader* File = FS.r_open(Path))
	{
		return File;
	}

	xrCriticalSectionGuard Guard(PackedLock);

	if (!PackedLoaded)
	{
		PackedLoaded = true;

		for (const char* Directory : Folders)
		{
			xr_vector<char*>* Folder = FS.file_list_open(_game_shaders_, Directory, FS_ListFiles | FS_RootOnly);

			if (!Folder)
			{
				continue;
			}

			for (char* File : *Folder)
			{
				if (const char* Ext = strext(File); Ext && !xr_strcmp(Ext, ".json"))
				{
					LoadPacked(Directory, File);
				}
			}

			FS.file_list_close(Folder);
		}
	}

	auto Found = Packed.find(PackedKey(xr_string(Name) + Extension));

	// A shader's own file holds all its stages and is named after it, in one of the folders
	for (u32 Index = 0; Found == Packed.end() && Index < std::size(Folders); ++Index)
	{
		string_path Own;
		xr_strconcat(Own, Folders[Index], Name, ".hlsl");

		if (FS.exist(_game_shaders_, Own))
		{
			PackedShader Shader;
			Shader.File = xr_string(Folders[Index]) + Name;
			Found = Packed.emplace(PackedKey(xr_string(Name) + Extension), Shader).first;
		}
	}

	if (Found == Packed.end())
	{
		return nullptr;
	}

	xr_strconcat(Entry, xr_string(Extension + 1, 2).c_str(), "_main");
	const xr_string Text = Found->second.Defines + "#include \"" + Found->second.File + ".hlsl\"\n";
	char* Copy = xr_alloc<char>(Text.size() + 1);
	CopyMemory(Copy, Text.c_str(), Text.size() + 1);
	return new CPackedShaderReader(Copy, Text.size());
}
