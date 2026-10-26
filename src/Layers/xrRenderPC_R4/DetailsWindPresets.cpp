#include "stdafx.h"
#include "DetailsWind.h"
#include <cmath>
#include <cstdlib>

namespace DetailWindPresets
{
static bool ReadBoolean(const char* Raw, bool& Value)
{
	if (!Raw)
	{
		return false;
	}
	if (xr_stricmp(Raw, "true") == 0 || xr_stricmp(Raw, "on") == 0 || xr_stricmp(Raw, "yes") == 0 || xr_strcmp(Raw, "1") == 0)
	{
		Value = true;
		return true;
	}
	if (xr_stricmp(Raw, "false") == 0 || xr_stricmp(Raw, "off") == 0 || xr_stricmp(Raw, "no") == 0 || xr_strcmp(Raw, "0") == 0)
	{
		Value = false;
		return true;
	}
	return false;
}

static bool ValidName(const char* Name)
{
	if (!Name || !Name[0] || xr_strlen(Name) > 120 || xr_strcmp(Name, "wind_base") == 0)
	{
		return false;
	}
	for (const char* C = Name; *C; ++C)
	{
		if (static_cast<unsigned char>(*C) <= 32 || strchr("[]:;=,!#\"\\/", *C))
		{
			return false;
		}
	}
	return true;
}
} // namespace DetailWindPresets

CDetailWindPresets& CDetailWindPresets::Get()
{
	static CDetailWindPresets Instance;
	return Instance;
}

void CDetailWindPresets::Clear()
{
	Config.reset();
	Sections.clear();
	Selected = nullptr;
	DefaultSection = nullptr;
	ListSection = nullptr;
	Status.clear();
	DisplayName[0] = 0;
}

bool CDetailWindPresets::ReadRegistry()
{
	Sections.clear();
	const char* Default = Config->r_string("wind_base", "default_section");
	const char* List = Config->r_string("wind_base", "list_sections");
	if (!Default || !List || !DetailWindPresets::ValidName(Default) || !DetailWindPresets::ValidName(List) || xr_strcmp(Default, List) == 0 || !Config->section_exist(Default) || !Config->section_exist(List))
	{
		Status = "wind_base: invalid default_section or list_sections";
		return false;
	}
	DefaultSection = Default;
	ListSection = List;
	for (const auto& Item : Config->r_section(List).Data)
	{
		const char* Value = Item.second.c_str();
		bool Enabled = false;
		if (!DetailWindPresets::ValidName(Item.first.c_str()) || Item.first == ListSection || !Config->section_exist(Item.first) || !DetailWindPresets::ReadBoolean(Value, Enabled))
		{
			Status = "Invalid preset entry in list_sections";
			Sections.clear();
			return false;
		}
		Sections.push_back(Item.first);
	}
	// Sort by the strongest configured tilt, then force; section names remain arbitrary.
	auto Peak = [&](const char* Section, const char* Key)
	{
		float Result = 0;
		for (const char* Suffix : {"_min", "_max"})
		{
			if (const char* Raw = Config->r_string_nullable(Section, (xr_string(Key) + Suffix).c_str()))
			{
				const float Value = std::strtof(Raw, nullptr);
				if (std::isfinite(Value))
				{
					Result = std::max(Result, Value);
				}
			}
		}
		return Result;
	};
	std::stable_sort(Sections.begin(), Sections.end(), [&](const shared_str& A, const shared_str& B)
	{
		const float AngleA = Peak(A.c_str(), "max_angle"), AngleB = Peak(B.c_str(), "max_angle");
		if (AngleA != AngleB) { return AngleA < AngleB; }
		const float ForceA = Peak(A.c_str(), "force"), ForceB = Peak(B.c_str(), "force");
		return ForceA != ForceB ? ForceA < ForceB : xr_strcmp(A.c_str(), B.c_str()) < 0;
	});
	return true;
}

bool CDetailWindPresets::Reload()
{
	string_path Path;
	FS.update_path(Path, "$game_config$", "mod_system_wind_presets.ltx");
	if (!FS.exist(Path))
	{
		Status = "mod_system_wind_presets.ltx not found";
		return false;
	}
	auto Data = xr_make_unique<CInifile>(Path, false, true, false);
	Data->set_override_names(true);
	if (!Data->line_exist("wind_base", "default_section") || !Data->line_exist("wind_base", "list_sections"))
	{
		Status = "wind_base requires default_section and list_sections";
		return false;
	}
	auto Previous = std::move(Config);
	const auto PreviousSections = Sections;
	const shared_str PreviousDefault = DefaultSection;
	const shared_str PreviousList = ListSection;
	Config = std::move(Data);
	if (!ReadRegistry())
	{
		Config = std::move(Previous);
		Sections = PreviousSections;
		DefaultSection = PreviousDefault;
		ListSection = PreviousList;
		return false;
	}
	// Explicit reload discards unsaved edits and resolves disabled/deleted selections.
	if (Selected.size() && IsEnabled(Selected.c_str()))
	{
		return Select(Selected.c_str());
	}
	return SelectAvailable();
}

bool CDetailWindPresets::IsListed(const char* Section) const
{
	return Config && Section && Config->line_exist(ListSection.c_str(), Section);
}

bool CDetailWindPresets::IsEnabled(const char* Section) const
{
	bool Enabled = false;
	return IsListed(Section) && Config->r_bool_nullable(ListSection.c_str(), Section, Enabled) && Enabled;
}

bool CDetailWindPresets::SelectAvailable()
{
	if (!Config)
	{
		return false;
	}
	if (IsEnabled(DefaultSection.c_str()))
	{
		return Select(DefaultSection.c_str());
	}
	for (const auto& Section : Sections)
	{
		if (IsEnabled(Section.c_str()))
		{
			return Select(Section.c_str());
		}
	}
	// The fallback is independent of membership/enabled flags in the registry.
	return Select(DefaultSection.c_str());
}

bool CDetailWindPresets::Select(const char* Section)
{
	if (!Config || !Section || !Config->section_exist(Section) || (!IsListed(Section) && xr_strcmp(Section, DefaultSection.c_str()) != 0))
	{
		Status = "Preset section not found";
		return false;
	}
	CDetailWind::Settings Settings;
	const auto& Definitions = CDetailWind::Definitions();
	for (size_t Index = 0; Index < Definitions.size(); ++Index)
	{
		const auto& Parameter = Definitions[Index];
		for (int Endpoint = 0; Endpoint < 2; ++Endpoint)
		{
			const xr_string Key = Parameter.Key + (Endpoint ? "_max" : "_min");
			if (!Config->line_exist(Section, Key.c_str()))
			{
				continue;
			}
			const char* Raw = Config->r_string_nullable(Section, Key.c_str());
			char* End = nullptr;
			const float Value = Raw ? std::strtof(Raw, &End) : 0;
			if (!Raw || End == Raw || *End || !std::isfinite(Value) || Value < Parameter.Lower || Value > Parameter.Upper)
			{
				Status = xr_string("Invalid parameter: ") + Key;
				return false;
			}
			Settings.Values[Index][Endpoint] = Value;
		}
	}
	for (int Layer = 0; Layer < CDetailWind::LayerCount; ++Layer)
	{
		const xr_string Key = "layer" + xr_string(std::to_string(Layer + 1).c_str()) + "_enabled";
		if (Config->line_exist(Section, Key.c_str()))
		{
			const char* Raw = Config->r_string_nullable(Section, Key.c_str());
			if (!DetailWindPresets::ReadBoolean(Raw, Settings.Enabled[Layer]))
			{
				Status = xr_string("Invalid boolean: ") + Key;
				return false;
			}
		}
	}
	for (int Index = 0; Index < CDetailWind::BehaviorCount; ++Index)
	{
		auto& Behavior = Settings.Behaviors[Index];
		const xr_string Prefix = "behavior" + xr_string(std::to_string(Index + 1).c_str()) + "_";
		if (Config->line_exist(Section, (Prefix + "enabled").c_str()))
		{
			if (!DetailWindPresets::ReadBoolean(Config->r_string_nullable(Section, (Prefix + "enabled").c_str()), Behavior.Enabled))
			{
				Status = "Invalid behavior enabled flag";
				return false;
			}
		}
		float* Values[] = {Behavior.Influence, Behavior.Duration, Behavior.Interval};
		const char* Names[] = {"influence", "duration", "interval"};
		const float Minimum[] = {0, .5f, .5f}, Maximum[] = {3, 120, 300};
		for (int Parameter = 0; Parameter < 3; ++Parameter)
		{
			for (int Endpoint = 0; Endpoint < 2; ++Endpoint)
			{
				const xr_string Key = Prefix + Names[Parameter] + (Endpoint ? "_max" : "_min");
				if (const char* Raw = Config->r_string_nullable(Section, Key.c_str()))
				{
					char* End = nullptr;
					const float Value = std::strtof(Raw, &End);
					if (End == Raw || *End || !std::isfinite(Value) || Value < Minimum[Parameter] || Value > Maximum[Parameter])
					{
						Status = xr_string("Invalid behavior parameter: ") + Key;
						return false;
					}
					Values[Parameter][Endpoint] = Value;
				}
			}
		}
	}
	if (const char* Raw = Config->r_string_nullable(Section, "seed"))
	{
		char* End = nullptr;
		const unsigned long long Seed = std::strtoull(Raw, &End, 10);
		if (End == Raw || *End || Seed > 0xffffffffULL)
		{
			Status = "Invalid seed";
			return false;
		}
		Settings.Seed = u32(Seed);
	}
	const xr_string Name = GetName(Section);
	if (Name.size() >= sizeof(DisplayName))
	{
		Status = "Preset name is too long";
		return false;
	}
	CDetailWind::Editable() = Settings;
	xr_strcpy(DisplayName, sizeof(DisplayName), Name.c_str());
	Selected = Section;
	Status = xr_string("Loaded: ") + Section;
	return true;
}

void CDetailWindPresets::WriteParameters(CInifile& Data, const char* Section)
{
	const auto& Settings = CDetailWind::Editable();
	const auto& Definitions = CDetailWind::Definitions();
	for (size_t Index = 0; Index < Definitions.size(); ++Index)
	{
		Data.w_float(Section, (Definitions[Index].Key + "_min").c_str(), Settings.Values[Index][0]);
		Data.w_float(Section, (Definitions[Index].Key + "_max").c_str(), Settings.Values[Index][1]);
	}
	for (int Layer = 0; Layer < CDetailWind::LayerCount; ++Layer)
	{
		const xr_string Key = "layer" + xr_string(std::to_string(Layer + 1).c_str()) + "_enabled";
		Data.w_string(Section, Key.c_str(), Settings.Enabled[Layer] ? "true" : "false");
	}
	for (int Index = 0; Index < CDetailWind::BehaviorCount; ++Index)
	{
		const auto& Behavior = Settings.Behaviors[Index];
		const xr_string Prefix = "behavior" + xr_string(std::to_string(Index + 1).c_str()) + "_";
		Data.w_string(Section, (Prefix + "enabled").c_str(), Behavior.Enabled ? "true" : "false");
		const float* Values[] = {Behavior.Influence, Behavior.Duration, Behavior.Interval};
		const char* Names[] = {"influence", "duration", "interval"};
		for (int Parameter = 0; Parameter < 3; ++Parameter)
		{
			Data.w_float(Section, (Prefix + Names[Parameter] + "_min").c_str(), Values[Parameter][0]);
			Data.w_float(Section, (Prefix + Names[Parameter] + "_max").c_str(), Values[Parameter][1]);
		}
	}
	Data.w_u32(Section, "seed", Settings.Seed);
	xr_string Name = DisplayName[0] ? DisplayName : Section;
	std::replace(Name.begin(), Name.end(), '"', '\'');
	Data.w_string(Section, "name", ("\"" + Name + "\"").c_str());
}

bool CDetailWindPresets::Commit(CInifile& Data)
{
	// save_as resolves the mounted file's wrap path back into its owning addon.
	const auto* File = FS.exist(Data.fname());
	if (!File || File->vfs != u32(-1))
	{
		Status = "Cannot save mod_system_wind_presets.ltx (missing or archived file)";
		return false;
	}
	const xr_string Backup = xr_string(File->wrap ? File->wrap : File->name) + ".bak";
	FS.file_copy(Data.fname(), Backup.c_str());
	if (!Data.save_as())
	{
		Status = "Cannot save mod_system_wind_presets.ltx";
		return false;
	}
	return true;
}

bool CDetailWindPresets::Save()
{
	if (!Config || !Selected.size())
	{
		return false;
	}
	CInifile Data(Config->fname(), false, true, false);
	Data.set_override_names(true);
	WriteParameters(Data, Selected.c_str());
	if (!Commit(Data))
	{
		return false;
	}
	const shared_str Saved = Selected;
	if (!Reload() || !Select(Saved.c_str()))
	{
		return false;
	}
	Status = xr_string("Saved: ") + Saved.c_str();
	return true;
}

bool CDetailWindPresets::Add(const char* Section)
{
	string128 Generated;
	if (Config && (!Section || !Section[0]))
	{
		u32 Index = 1;
		do
		{
			xr_sprintf(Generated, "wind_%02u", Index++);
		} while (Config->section_exist(Generated));
		Section = Generated;
	}
	xr_string Canonical = Section ? Section : "";
	xr_strlwr(Canonical.data());
	Section = Canonical.c_str();
	if (!Config || !DetailWindPresets::ValidName(Section) || (Config->section_exist(Section) && (IsListed(Section) || xr_strcmp(Section, DefaultSection.c_str()) != 0)))
	{
		Status = "Use a unique valid LTX section name (up to 120 characters)";
		return false;
	}
	CInifile Data(Config->fname(), false, true, false);
	Data.set_override_names(true);
	WriteParameters(Data, Section);
	Data.w_string(ListSection.c_str(), Section, "true");
	if (!Commit(Data) || !Reload())
	{
		return false;
	}
	return Select(Section);
}

bool CDetailWindPresets::Remove()
{
	if (!Config || !IsListed(Selected.c_str()))
	{
		return false;
	}
	CInifile Data(Config->fname(), false, true, false);
	Data.set_override_names(true);
	Data.remove_line(ListSection.c_str(), Selected.c_str());
	// Preserve the configured fallback even when it is removed from the list.
	if (Selected != DefaultSection)
	{
		auto& Root = Data.sections();
		Root.erase(std::remove_if(Root.begin(), Root.end(), [this](const CInifile::Sect& Section)
								  { return Section.Name == Selected; }),
				   Root.end());
	}
	if (!Commit(Data))
	{
		return false;
	}
	Selected = nullptr;
	return Reload();
}

bool CDetailWindPresets::SetEnabled(bool Enabled)
{
	if (!Config || !IsListed(Selected.c_str()))
	{
		return false;
	}
	CInifile Data(Config->fname(), false, true, false);
	Data.set_override_names(true);
	Data.w_string(ListSection.c_str(), Selected.c_str(), Enabled ? "true" : "false");
	if (!Commit(Data))
	{
		return false;
	}
	// Registry edits do not implicitly save unsaved parameter changes.
	return Reload();
}

xr_string CDetailWindPresets::GetName(const char* Section) const
{
	const shared_str Name = Config ? Config->r_string_wb_nullable(Section, "name") : shared_str{};
	return Name.size() ? xr_string(Name.c_str()) : xr_string(Section ? Section : "");
}
