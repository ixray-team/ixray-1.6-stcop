#include "StdAfx.h"
#include "EmbeddedLevelResources.h"

namespace Sandbox
{
#include "EmbeddedLevelFlat.inc"
#include "EmbeddedLevelPlane.inc"

constexpr EmbeddedFile FlatFiles[] =
{
	{ "level.ai", gamedata_levels_viewer_flat_level_ai, gamedata_levels_viewer_flat_level_ai_len },
	{ "level.cform", gamedata_levels_viewer_flat_level_cform, gamedata_levels_viewer_flat_level_cform_len },
	{ "level.game", gamedata_levels_viewer_flat_level_game, gamedata_levels_viewer_flat_level_game_len },
	{ "level.ltx", gamedata_levels_viewer_flat_level_ltx, gamedata_levels_viewer_flat_level_ltx_len },
	{ "level.som", gamedata_levels_viewer_flat_level_som, gamedata_levels_viewer_flat_level_som_len },
};

constexpr EmbeddedFile PlaneFiles[] =
{
	{ "level.ai", gamedata_levels_viewer_plane_level_ai, gamedata_levels_viewer_plane_level_ai_len },
	{ "level.cform", gamedata_levels_viewer_plane_level_cform, gamedata_levels_viewer_plane_level_cform_len },
	{ "level.game", gamedata_levels_viewer_plane_level_game, gamedata_levels_viewer_plane_level_game_len },
	{ "level.ltx", gamedata_levels_viewer_plane_level_ltx, gamedata_levels_viewer_plane_level_ltx_len },
	{ "level.som", gamedata_levels_viewer_plane_level_som, gamedata_levels_viewer_plane_level_som_len },
};

constexpr EmbeddedLevel EmbeddedLevels[] =
{
	{ "viewer_flat", FlatFiles, std::size(FlatFiles) },
	{ "viewer_plane", PlaneFiles, std::size(PlaneFiles) },
};

const EmbeddedLevel* FindEmbeddedLevel(const char* folder)
{
	if (folder == nullptr)
	{
		return nullptr;
	}
	for (const EmbeddedLevel& level : EmbeddedLevels)
	{
		if (xr_strcmp(level.Folder, folder) == 0)
		{
			return &level;
		}
	}
	return nullptr;
}

EmbeddedLevelRegistration::~EmbeddedLevelRegistration()
{
	Reset();
}

EmbeddedLevelRegistration::EmbeddedLevelRegistration(EmbeddedLevelRegistration&& other) noexcept :
	Registered(std::move(other.Registered)), Count(other.Count), Installed(other.Installed)
{
	other.Registered = {};
	other.Count = 0;
	other.Installed = false;
}

EmbeddedLevelRegistration& EmbeddedLevelRegistration::operator=(EmbeddedLevelRegistration&& other) noexcept
{
	if (this == &other)
	{
		return *this;
	}
	Reset();
	Registered = std::move(other.Registered);
	Count = other.Count;
	Installed = other.Installed;
	other.Registered = {};
	other.Count = 0;
	other.Installed = false;
	return *this;
}

bool EmbeddedLevelRegistration::Install(const EmbeddedLevel& level)
{
	Reset();
	if (level.Folder == nullptr || level.Folder[0] == 0 || level.Files == nullptr || level.Count == 0 || level.Count + 2 > Registered.size())
	{
		return false;
	}

	SRegisteredFile& rootRegistration = Registered[Count];
	string_path root;
	FS.update_path(root, _game_levels_, "");
	rootRegistration.Path = root;
	rootRegistration.Restore = FS.RegisterMemoryFile(rootRegistration.Path.c_str(), nullptr, 0);
	++Count;

	for (size_t index = 0; index < level.Count; ++index)
	{
		const EmbeddedFile& file = level.Files[index];
		if (file.Name == nullptr || file.Name[0] == 0 || file.Bytes == nullptr)
		{
			Reset();
			return false;
		}
		SRegisteredFile& registered = Registered[Count];
		const xr_string relative = xr_string(level.Folder) + "\\" + file.Name;
		string_path fullPath;
		FS.update_path(fullPath, _game_levels_, relative.c_str());
		registered.Path = fullPath;
		registered.Restore = FS.RegisterMemoryFile(registered.Path.c_str(), file.Bytes, file.Size);
		if (!FS.exist(registered.Path.c_str()))
		{
			Reset();
			return false;
		}
		++Count;
	}

	xr_string folder = level.Folder;
	if (folder.back() != '\\' && folder.back() != '/')
		folder += '\\';
	string_path folderPath;
	FS.update_path(folderPath, _game_levels_, folder.c_str());
	SRegisteredFile& registered = Registered[Count];
	registered.Path = folderPath;
	registered.Restore = FS.RegisterMemoryFile(registered.Path.c_str(), nullptr, 0);
	if (!FS.exist(registered.Path.c_str()))
	{
		Reset();
		return false;
	}
	++Count;
	Installed = true;
	return true;
}

void EmbeddedLevelRegistration::Reset()
{
	while (Count != 0)
	{
		--Count;
		FS.UnregisterMemoryFile(Registered[Count].Path.c_str(), Registered[Count].Restore);
		Registered[Count] = {};
	}
	Installed = false;
}
}
