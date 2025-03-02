#include "stdafx.h"
#include <magic_enum/magic_enum.hpp>

ENGINE_API CEngineExternal* g_pEngineExternal = nullptr;

CEngineExternal::CEngineExternal() : m_is_modification_gunslinger{}, pOptions{}, m_pTitle{}
{
	string_path fname{};
	FS.update_path(fname, "$game_config$", "engine_external.ltx");
	pOptions = new CInifile(fname);

	if (pOptions)
	{
		m_pTitle = READ_IF_EXISTS(pOptions, r_string_wb, "general", "title", "IX-Ray Platform").c_str();
		m_is_modification_gunslinger = pOptions->r_bool("gunslinger", magic_enum::enum_name(EEngineExternalGunslinger::EnableGunslingerMode).data());
	}
}

CEngineExternal::~CEngineExternal() {
	xr_delete(pOptions);
}

const char* CEngineExternal::GetTitle() const {
	return m_pTitle;
}

bool CEngineExternal::isModificationGunslinger(void) const
{
	return m_is_modification_gunslinger;
}

const char* CEngineExternal::GetPlayerHudOmfAdditional() const
{
	return READ_IF_EXISTS(pOptions, r_string_wb, "player_hud", "PlayerHudOmfAdditional", "").c_str();
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

bool CEngineExternal::operator[](const EEngineExternalGunslinger& ID) const {
	return pOptions->r_bool("gunslinger", magic_enum::enum_name(ID).data());
}

ENGINE_API CEngineExternal& EngineExternal()
{
	if (g_pEngineExternal == nullptr) {
		g_pEngineExternal = new CEngineExternal;
	}
	return *g_pEngineExternal;
}
