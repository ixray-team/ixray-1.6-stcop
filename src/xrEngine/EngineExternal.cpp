#include "stdafx.h"
#include <magic_enum/magic_enum.hpp>

ENGINE_API CEngineExternal* g_pEngineExternal = nullptr;

CEngineExternal::CEngineExternal()
{
	string_path fname;
	FS.update_path(fname, "$game_config$", "engine_external.ltx");
	pOptions = new CInifile(fname);

	if (pOptions->section_exist("shaders_options"))
	{
		for (auto& Line : pOptions->r_section("shaders_options").Data)
		{
			if (Line.second != "0")
			{
				ShadersOptions[*Line.first] = *Line.second;
			}
			else
			{
				Msg("Disabled shader option: %s", *Line.first);
			}
		}
	}
	if (!ClearSkyMode() && !CallOfPripyatMode())
	{
		R_ASSERT2(false, "Unknown platform mode specified. Please check your engine_external.ltx.");
	}
}

CEngineExternal::~CEngineExternal() 
{
	xr_delete(pOptions);
}

xr_string CEngineExternal::GetTitle() const
{
	return READ_IF_EXISTS(pOptions, r_string_wb, "general", "title", "IX-Ray Platform").c_str();
}

const char* CEngineExternal::GetPlayerHudOmfAdditional() const
{
	return READ_IF_EXISTS(pOptions, r_string_wb, "player_hud", "PlayerHudOmfAdditional", "").c_str();
}

const xr_vector<shared_str> CEngineExternal::StepWallmarksMaterials() const
{
	xr_vector<shared_str> TempVector;
	xr_string Items = READ_IF_EXISTS(pOptions, r_string_wb, "step_wallmark", "materials", "").c_str();

	size_t MaterialsCount = _GetItemCount(Items.c_str());
	TempVector.resize(MaterialsCount);

	size_t Iter = 0;
	for (shared_str& Item : TempVector)
	{
		xr_string _temp;
		Item = _GetItem(Items.c_str(), (int)Iter, _temp);
		Iter++;
	}

	return std::move(TempVector);
}

const xr_string CEngineExternal::WallmarkLeft() const
{
	return READ_IF_EXISTS(pOptions, r_string_wb, "step_wallmark", "left_mark", "").c_str();
}

const xr_string CEngineExternal::WallmarkRight() const
{
	return READ_IF_EXISTS(pOptions, r_string_wb, "step_wallmark", "right_mark", "").c_str();
}

bool CEngineExternal::operator[](const EEngineExternalUI& ID) const
{
	return READ_IF_EXISTS(pOptions, r_bool, "ui", magic_enum::enum_name(ID).data(), false);
}

bool CEngineExternal::operator[](const EEngineExternalPhysical& ID) const
{
	return READ_IF_EXISTS(pOptions, r_bool, "physics", magic_enum::enum_name(ID).data(), false);
}

bool CEngineExternal::operator[](const EEngineExternalGame& ID) const
{
	return READ_IF_EXISTS(pOptions, r_bool, "gameplay", magic_enum::enum_name(ID).data(), false);
}

bool CEngineExternal::operator[](const EEngineExternalRender& ID) const
{
	return READ_IF_EXISTS(pOptions, r_bool, "render", magic_enum::enum_name(ID).data(), false);
}

bool CEngineExternal::operator[](const EEngineExternalEnvironment& ID) const {
	return READ_IF_EXISTS(pOptions, r_bool, "environment", magic_enum::enum_name(ID).data(), false);
}

ENGINE_API CEngineExternal& EngineExternal() 
{
	if (g_pEngineExternal == nullptr) {
		g_pEngineExternal = new CEngineExternal;
	}
	return *g_pEngineExternal;
}

u32 CEngineExternal::GetFontAltasSize() const
{
	return READ_IF_EXISTS(pOptions, r_u32, "render", "FontAtlasSize", 4096);
}

float CEngineExternal::GetWeaponIconScaling() const
{
	return READ_IF_EXISTS(pOptions, r_float, "ui", "WeaponIconScale", 0.8f);
}

const char* CEngineExternal::PlatformMode() const
{
	return READ_IF_EXISTS(pOptions, r_string, "general", "Platform", "cop");
}

bool CEngineExternal::ClearSkyMode() const
{
	return !xr_strcmp(PlatformMode(), "cs");
}

bool CEngineExternal::CallOfPripyatMode() const
{
	return !xr_strcmp(PlatformMode(), "cop");
}
