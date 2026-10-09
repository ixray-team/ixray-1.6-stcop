/**************************************************************************************
 * Copyright (C) 2026 Anton Kovalev (vertver)
 * New Sound Engine
 ***************************************************************************************
 * Source code is licensed under the following terms:
 *
 * 1. IX-Ray Team License
 *    Non-exclusive, royalty-free, perpetual license is hereby granted to:
 *      - ForserX   (https://github.com/ForserX)
 *      - Drombeys  (https://github.com/Drombeys)
 *      - v2v3v4    (https://github.com/v2v3v4)
 *
 *    Permitted rights:
 *      - Copy, modify, merge, publish and distribute this Software
 *        and its documentation.
 *
 * 2. Public Access License
 *    Non-exclusive, "access-view-study" rights granted to everyone else.
 *
 *    Permitted rights:
 *      - Private copying is allowed, provided that no distribution occurs.
 *      - Public cloning (i.e. "forking") is allowed, but any source code
 *        modification or binary redistribution is prohibited.
 *
 * Usage of this Software beyond the rights granted above is strictly prohibited.
 *
 * The above copyright notice and this license text must be included in all
 * copies or substantial portions of the Software.
 **************************************************************************************/
#include "SoundConfig.h"

static const char SoundDefaultConfig[] =
	"[bus:master]\n"
	"effects = compressor\n"
	"compressor.mix = 0.5\n"
	"[bus:effects]\n"
	"voice_effects = spatialize|spatial\n"
	"zone_send = 1.0\n"
	"[bus:music]\n"
	"voice_effects = spatialize|spatial\n"
	"[bus:shooting]\n"
	"voice_effects = spatial\n"
	"zone_send = 1.0\n"
	"direct_ratio = 0.6\n"
	"far_send = shooting_far\n"
	"indoor_send = shooting_indoor\n"
	"[bus:shooting_far]\n"
	"effects = convolution\n"
	"convolution.resource = ir\\ir_default_far\n"
	"[bus:shooting_indoor]\n"
	"effects = convolution\n";

static SoundConfig GConfig;

void Snd_SetConfigValue(SoundConfigSection* Section, const shared_str& Key, const shared_str& Value)
{
	for (CInifile::Item& Existing : Section->Items)
	{
		if (Existing.first == Key)
		{
			Existing.second = Value;
			return;
		}
	}

	CInifile::Item& Item = Section->Items.emplace_back();
	Item.first = Key;
	Item.second = Value;
}

static void Snd_MergeConfig(SoundConfig* Config, CInifile* Ini, const shared_str& File)
{
	for (CInifile::Sect& Section : Ini->sections())
	{
		SoundConfigSection& Target = (*Config)[Section.Name];
		Target.File = File;
		for (const CInifile::Item& Item : Section.Data)
		{
			Snd_SetConfigValue(&Target, Item.first, Item.second);
		}
	}
}

static void Snd_MergeConfigFile(SoundConfig* Config, const char* Name)
{
	string_path Path;
	FS.update_path(Path, _game_config_, Name);
	CInifile Ini(Path);
	Snd_MergeConfig(Config, &Ini, Name);
}

void Snd_LoadConfig()
{
	SoundConfig* Config = &GConfig;
	Config->clear();

	IReader Reader((void*)SoundDefaultConfig, sizeof(SoundDefaultConfig) - 1);
	CInifile Defaults(&Reader);
	Snd_MergeConfig(Config, &Defaults, "<default>");

	FS_FileSet Files;
	FS.file_list(Files, _game_config_, FS_ListFiles, "sounds\\*.ltx");

	for (u32 Pass = 0; Pass < 2; Pass++)
	{
		for (const FS_File& File : Files)
		{
			const char* Slash = strrchr(File.name.c_str(), '\\');
			const char* BaseName = Slash != nullptr ? Slash + 1 : File.name.c_str();
			bool IsVanilla = xr_strcmp(BaseName, "vanilla.ltx") == 0;
			if (strstr(BaseName, "mod_") == BaseName || IsVanilla != (Pass == 0))
			{
				continue;
			}

			Snd_MergeConfigFile(Config, File.name.c_str());
		}
	}
}

void Snd_UnloadConfig()
{
	GConfig.clear();
}

const SoundConfig& Snd_GetConfig()
{
	return GConfig;
}

SoundConfigSection* Snd_EditConfig(const char* Name)
{
	return &GConfig[shared_str(Name)];
}

bool Snd_SaveConfig(const char* Name)
{
	auto Found = GConfig.find(shared_str(Name));
	if (Found == GConfig.end())
	{
		return false;
	}

	SoundConfigSection& Section = Found->second;
	if (Section.File.size() == 0 || Section.File.c_str()[0] == '<')
	{
		Section.File = "sounds\\vanilla.ltx";
	}

	string_path Path;
	FS.update_path(Path, _game_config_, Section.File.c_str());
	CInifile Ini(Path, false, true, false);
	if (Section.IsDeleted)
	{
		CInifile::Root& Sections = Ini.sections();
		Sections.erase(std::remove_if(Sections.begin(), Sections.end(), [Name](const CInifile::Sect& Sect) { return xr_strcmp(Sect.Name.c_str(), Name) == 0; }), Sections.end());
		GConfig.erase(Found);
		return Ini.save_as();
	}

	for (const CInifile::Item& Item : Section.Items)
	{
		if (Item.second.size() != 0)
		{
			Ini.w_string(Name, Item.first.c_str(), Item.second.c_str());
		}
		else if (Ini.line_exist(Name, Item.first.c_str()))
		{
			Ini.remove_line(Name, Item.first.c_str());
		}
	}

	return Ini.save_as();
}

const SoundConfigSection* Snd_FindConfig(const char* Name)
{
	auto Found = GConfig.find(shared_str(Name));
	return Found != GConfig.end() ? &Found->second : nullptr;
}

const char* Snd_ConfigValue(const SoundConfigSection* Section, const char* Key)
{
	for (const CInifile::Item& Item : Section->Items)
	{
		if (xr_strcmp(Item.first.c_str(), Key) == 0)
		{
			return Item.second.c_str();
		}
	}

	return nullptr;
}
