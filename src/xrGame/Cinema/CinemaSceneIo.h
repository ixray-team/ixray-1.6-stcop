#pragma once
#include "CinemaScene.h"

namespace Cinema
{
struct ActorPreset
{
	xr_string Name;
	ActorData Setup;
	bool IsOld = false;
};
}

namespace Cinema::Io
{
inline constexpr const char* SceneAlias = "$game_config$";
inline constexpr const char* LegacyAlias = "$app_data_root$";
inline constexpr const char* UnsavedFile = "cinema_editor\\unsaved_%s.ltx";
inline constexpr const char* PresetFile = "cinema_editor\\actor_presets.ltx";
inline constexpr const char* LegacyPresetFile = "cinema_editor\\presets.ltx";
inline constexpr u32 PresetVersion = 5, MaxPresets = 512;

struct Warnings
{
	xr_vector<xr_string> Lines;
	u32 Dropped = 0;
	void Add(const char* Format, ...);
};

struct LoadResult
{
	bool IsOk = false;
	u32 SourceVersion = 0;
	xr_string Error;
	Warnings Notes;
};

void SceneFolder(string_path& Out, const char* Alias, const shared_str& LevelName);
void UnsavedPath(string_path& Out, const shared_str& LevelName);
bool CleanName(const xr_string& Utf8, xr_string& File, xr_string& Error);
void ListScenes(const char* Alias, const shared_str& LevelName, xr_vector<xr_string>& Names);
bool IniSafe(const char* Text);
bool IsOmfPath(const char* Path);
bool DecodeName(const char* Hex, xr_string& Out);
bool ReadBool(const CInifile& Ini, const char* Section, const char* Line, bool Default, Warnings& Notes);
u32 ReadU32(const CInifile& Ini, const char* Section, const char* Line, u32 Cap, Warnings& Notes);
bool ReadFloats(const CInifile& Ini, const char* Section, const char* Line, float* Out, u32 Count, Warnings& Notes);
bool PreScan(IReader& File, xr_string& Error);
LoadResult Load(const char* Path, Scene& Out);
bool Save(const Scene& Data, const char* Path, xr_string& Error);
bool SaveIni(const CInifile& Ini, const char* Path, xr_string& Error);
LoadResult LoadPresets(const char* Path, xr_vector<ActorPreset>& Out, xr_vector<xr_string>& Hidden);
bool SavePresets(const xr_vector<ActorPreset>& Presets, const xr_vector<xr_string>& Hidden, const char* Path, xr_string& Error);
}

namespace Cinema::Actors
{
void Unpack(xr_vector<ActorBlock>& Blocks, const xr_vector<float>& Blends);
}

namespace Cinema::V4
{
bool Import(const CInifile& Ini, s32 Version, Scene& Out, Io::Warnings& Notes);
void ImportPresets(const CInifile& Ini, xr_vector<ActorPreset>& Out, Io::Warnings& Notes);
}
