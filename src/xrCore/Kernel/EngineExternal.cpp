#include "stdafx.h"

#include "EngineExternal.h"

XRCORE_API CEngineExternal* g_pEngineExternal = nullptr;

CEngineExternal::CEngineExternal() : 
	m_platform_type(EEngineExternalPlatform::Unknown),
	pOptions(nullptr)
{
	string_path fname;
	FS.update_path(fname, _game_config_, "engine_external.ltx");
	pOptions = new CInifile(fname);
	MonsterLogicEnabled = pOptions->read_if_exists<bool>("gameplay", "UseDeathOrDieMonstersLogic", false);
	FS.update_path(fname, _game_config_, "new_monsters_logic.ltx");
	MonsterLogicConfigAvailable = FS.exist(_game_config_, "new_monsters_logic.ltx");
	MonsterLogicOptions = new CInifile(fname, true, MonsterLogicConfigAvailable);
	float SharingMin = MonsterLogicOptions->read_if_exists<float>("monsters_logic", "MonsterEnemySharingIntervalMin", 3.f);
	float SharingMax = MonsterLogicOptions->read_if_exists<float>("monsters_logic", "MonsterEnemySharingIntervalMax", 5.f);
	if (!_valid(SharingMin) || SharingMin < .25f || SharingMin > 60.f)
	{
		SharingMin = 3.f;
	}
	if (!_valid(SharingMax) || SharingMax < .25f || SharingMax > 60.f)
	{
		SharingMax = 5.f;
	}
	if (SharingMax < SharingMin)
	{
		std::swap(SharingMin, SharingMax);
	}
	MonsterEnemySharingIntervalMin = u32(SharingMin * 1000.f);
	MonsterEnemySharingIntervalMax = u32(SharingMax * 1000.f);
	MonsterEnemySharingCloseRatio = MonsterLogicOptions->read_if_exists<float>("monsters_logic", "MonsterEnemySharingCloseRatio", .25f);
	if (!_valid(MonsterEnemySharingCloseRatio) || MonsterEnemySharingCloseRatio < 0.f || MonsterEnemySharingCloseRatio > 1.f)
	{
		MonsterEnemySharingCloseRatio = .25f;
	}

	if (pOptions->section_exist("shaders_options"))
	{
		for (auto& Line : pOptions->r_section("shaders_options").Data)
		{
			if (!!xr_strcmp(Line.second, "0"))
			{
				Msg("Enable shader option: %s", *Line.first);
				ShadersOptions[*Line.first] = *Line.second;
			}
			else
			{
				Msg("Disabled shader option: %s", *Line.first);
			}
		}
	}

	if (pOptions->section_exist("general"))
	{
		InitPlatform(GetCurrentPlatformName());
	}

	if (!ClearSkyMode() && !CallOfPripyatMode() && !ShadowOfChernobylMode())
	{
		//R_ASSERT2(false, "Unknown platform mode specified. Please check your engine_external.ltx.");
	}

	gamesaveSize = pOptions->read_if_exists<Ivector2>("general", "SaveImageSize", Ivector2().set(128, 128));
}

CEngineExternal::~CEngineExternal() 
{
	xr_delete(MonsterLogicOptions);
	xr_delete(pOptions);
}

xr_string CEngineExternal::GetTitle() const
{
	auto Title = pOptions->r_string_wb_nullable("general", "title");
	if (Title.size())
	{
		return Title.c_str();
	}
	return "IX-Ray Platform";
}

const char* CEngineExternal::GetPlayerHudOmfAdditional() const
{
	auto HUD = pOptions->r_string_wb_nullable("player_hud", "PlayerHudOmfAdditional");
	if (HUD.size())
	{
		return HUD.c_str();
	}
	return "";
}

const char* CEngineExternal::GetPreferredFallbackLanguage() const
{
	auto Lang = pOptions->r_string_wb_nullable("localization", "PreferedFallbackLanguage");
	if (Lang.size())
	{
		return Lang.c_str();
	}
	return "eng";
}

const xr_vector<shared_str> CEngineExternal::StepWallmarksMaterials() const
{
	xr_vector<shared_str> TempVector;
	xr_string Items = "";
	{
		auto SItems = pOptions->r_string_wb_nullable("step_wallmark", "materials");
		if (SItems.size())
		{
			Items = SItems.c_str();
		}
	}

	size_t MaterialsCount = _GetItemCount(Items.c_str());
	TempVector.resize(MaterialsCount);

	size_t Iter = 0;
	for (shared_str& Item : TempVector)
	{
		xr_string _temp;
		Item = _GetItem(Items.c_str(), (int)Iter, _temp);
		Iter++;
	}

	return TempVector;
}

const xr_string CEngineExternal::WallmarkLeft() const
{
	auto Mark = pOptions->r_string_wb_nullable("step_wallmark", "left_mark");
	if (Mark.size())
	{
		return Mark.c_str();
	}
	return "";
}

const xr_string CEngineExternal::WallmarkRight() const
{
	auto Mark = pOptions->r_string_wb_nullable("step_wallmark", "right_mark");
	if (Mark.size())
	{
		return Mark.c_str();
	}
	return "";
}

void CEngineExternal::InitPlatform(const char* pPlatformName)
{
	if (!pPlatformName)
		return;

	for (unsigned char i = 0; i < static_cast<unsigned char>(EEngineExternalPlatform::EnumSize); ++i)
	{
		if (!xr_strcmp(pPlatformName, g_PlatformNames[i]))
		{
			m_platform_type = g_Platforms[i];
			break;
		}
	}
}

const char* Translate_EEngineExternalPlatform(EEngineExternalPlatform platform)
{
	switch (platform)
	{
	case EEngineExternalPlatform::ShadowOfChernobyl:
	{
		return "Shadow Of Chernobyl";
	}
	case EEngineExternalPlatform::ClearSky:
	{
		return "Clear Sky";
	}
	case EEngineExternalPlatform::CallOfPripyat:
	{
		return "Call Of Pripyat";
	}
	default:
	{
		R_ASSERT(false && "unknown platform");
		return "EENGINEXTERNALPLATFORM_UNKNOWN";
	}
	}
}

const char* CEngineExternal::GetCurrentPlatformFullName()
{
	return Translate_EEngineExternalPlatform(m_platform_type);
}

const char* CEngineExternal::GetCurrentPlatformName()
{
	auto Platform = pOptions->r_string_wb_nullable("general", "Platform");
	if (Platform.size())
	{
		return Platform.c_str();
	}
	return "cop";
}

EEngineExternalPlatform CEngineExternal::GetCurrentPlatform()
{
	return m_platform_type;
}

XRCORE_API CEngineExternal& EngineExternal()
{
	if (g_pEngineExternal == nullptr) {
		g_pEngineExternal = new CEngineExternal;
	}
	return *g_pEngineExternal;
}

u32 CEngineExternal::GetFontAltasSize() const
{
	return pOptions->read_if_exists<u32>("render", "FontAtlasSize", 4096);
}

float CEngineExternal::GetWeaponIconScaling() const
{
	return pOptions->read_if_exists<float>("ui", "WeaponIconScale", 0.8f);
}

const char* CEngineExternal::PlatformMode() const
{
	return pOptions->read_if_exists<str_c>("general", "Platform", "cop");
}

bool CEngineExternal::ClearSkyMode() const
{
	return m_platform_type == EEngineExternalPlatform::ClearSky;
}

bool CEngineExternal::CallOfPripyatMode() const
{
	return m_platform_type == EEngineExternalPlatform::CallOfPripyat;
}

bool CEngineExternal::ShadowOfChernobylMode() const
{
	return m_platform_type == EEngineExternalPlatform::ShadowOfChernobyl;
}

Fvector4 CEngineExternal::GetTalkDof() const
{
	return pOptions->read_if_exists<Fvector4>("gameplay", "TalkDof", Fvector4().set(0.0f, 0.5f, 5.0f, 0.0f));
}

float CEngineExternal::GetTalkFovScale() const
{
	return pOptions->read_if_exists<float>("gameplay", "DialogFovScale", 0.7f);
}

float CEngineExternal::GetSprintFovFactor() const
{
	return pOptions->read_if_exists<float>("gameplay", "SprintFovFactor", 7.0f);
}

shared_str CEngineExternal::GetInventoryItemCountPrefix()
{
	auto Prefix = pOptions->r_string_wb_nullable("ui", "InventoryItemCountPrefix");
	if (Prefix.size())
	{
		return Prefix.c_str();
	}
	return "x";
}

CInifile* CEngineExternal::GetIniFile()
{
	return pOptions;
}