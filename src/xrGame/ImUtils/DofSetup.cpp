#include "StdAfx.h"
#include "DofSetup.h"

#ifdef DEBUG_DRAW
#include "ImUtils.h"
#include "../DofManager.h"
#include "../Actor.h"
#include "../Inventory.h"
#include "../HudItem.h"
#include "../HudAnimatorManager.h"
#include "../HUDManager.h"
#include "../Level.h"
#include "../../xrEngine/XR_IOConsole.h"
#include "../../xrEngine/xr_ioc_cmd.h"
#include "../../xrEngine/ImGuiManager.h"
#include <array>

namespace DofSetup
{
enum class FieldKind
{
	Float,
	Vector2,
	Vector3,
	Vector4,
	Bool,
	Mode,
	Resolution,
	Reference
};

struct Field
{
	const char* Key;
	const char* Label;
	FieldKind Kind = FieldKind::Float;
	const char* Default = "0";
	float Min = -10000.0f;
	float Max = 10000.0f;
};

static const Field CameraFields[] = {
	{"enabled", "Enable event profile", FieldKind::Bool, "true"},
	{"ndof_mode", "Focus mode", FieldKind::Mode, "autofocus"},
	{"f_number", "Aperture (f-number)", FieldKind::Float, "4", 0.7f, 64.0f},
	{"manual_focal_length", "Use manual focal length", FieldKind::Bool, "false"},
	{"focal_length", "Focal length (mm)", FieldKind::Float, "10", 1.0f, 1000.0f},
	{"focus_distance", "Focus distance (m)", FieldKind::Float, "1.4", 0.01f, 10000.0f},
	{"focus_point", "Autofocus point (screen UV)", FieldKind::Vector2, "0.5, 0.495", 0.0f, 1.0f},
	{"focus_time", "Focus response (s)", FieldKind::Float, "0.1", 0.01f, 10.0f},
	{"transition_time", "Lens transition (s)", FieldKind::Float, "0.1", 0.01f, 10.0f}
};

static const Field GeneralFields[] = {
	{"r2_dof_enable", "Enable DOF (shader restart required)", FieldKind::Bool},
	{"r4_ndof_mode", "DOF mode", FieldKind::Mode},
	{"r4_ndof_manual_focal_length", "Use manual focal length", FieldKind::Bool},
	{"r4_ndof_focal_length", "Focal length (mm)"},
	{"r4_ndof_f_number", "Aperture (f-number)"},
	{"r4_ndof_focus_distance", "Focus distance (m)"},
	{"r4_ndof_focus_time", "Focus response (s)"},
	{"r4_ndof_transition_time", "Lens transition (s)"},
	{"r4_ndof_resolution", "Bokeh resolution", FieldKind::Resolution},
	{"r2_dof", "Static near / focus / far (m)", FieldKind::Vector3},
	{"r2_dof_kernel", "Blur size (kernel; maximum radius is 4x)"},
	{"r2_dof_sky", "Static sky distance (m)"},
	{"r2_dynamic_dof", "Static weapon action profiles", FieldKind::Bool},
	{"r2_const_zoomdof", "Static fixed aiming distances", FieldKind::Bool},
	{"enable_dof_reload", "Static reload DOF", FieldKind::Bool},
	{"enable_dof_hud_animator", "Static HUD animator / PDA DOF", FieldKind::Bool},
	{"enable_dof_talk", "Static dialogue DOF", FieldKind::Bool},
	{"r4_ndof_inventory", "Static inventory DOF", FieldKind::Bool}
};

using ChangeMap = xr_map<xr_string, xr_map<xr_string, xr_string>>;

class Editor final
{
public:
	Editor() = default;
	~Editor() = default;
	void Draw();
	void OnFrame(CDofManager& Manager);

private:
	bool Load();
	void ReadMod();
	xr_string ModText() const;
	bool WriteMod() const;
	void Index();
	void ReadGeneral();
	void ApplyGeneral(const xr_map<xr_string, xr_string>& Values);
	void Set(const char* Section, const char* Key, const char* Value);
	void Commit(const char* Section, const Field& Definition, const char* Value);
	void GeneralTab();
	void PresetsTab();
	void EventsTab();
	void HudTab();
	void CameraEditor();
	void StaticEditor();
	void DrawField(const char* Section, const Field& Definition);
	bool DrawInput(const Field& Definition, const xr_string& Value, xr_string& Edited, bool AllowStaticMode = false);
	void ProfileReference(const char* Section, const char* Key, const char* Label);
	void StartPreview(bool IsCamera);
	void UpdatePreview();
	void StopPreview(CDofManager& Manager);
	bool IsLocal(const ChangeMap& Map, const char* Section, const char* Key) const;
	xr_string Effective(const char* Section, const char* Key, const char* Default) const;
	xr_string CameraDefault(const Field& Definition) const;
	xr_string StaticDefault(const char* Key, const char* Fallback) const;
	xr_string CopySection(const char* Section, bool IsCamera) const;
	void PickCurrent(int Index);

	xr_unique_ptr<CInifile> Config;
	ChangeMap Changes;
	ChangeMap Saved;
	ChangeMap Startup;
	xr_set<xr_string> Added;
	xr_map<xr_string, xr_string> General;
	xr_map<xr_string, xr_string> SavedGeneral;
	xr_map<xr_string, xr_pair<float, float>> GeneralRanges;
	xr_vector<xr_string> Profiles;
	xr_vector<xr_string> Sections;
	xr_vector<xr_string> GroupKeys;
	xr_map<xr_string, xr_string> CopiedGroup;
	xr_map<xr_string, xr_string> GroupDefaults;
	xr_string ModFile;
	xr_string Profile = "ndof_neutral";
	xr_string HudSection;
	xr_string Status;
	xr_string ActiveProfile;
	xr_string ActiveBinding;
	xr_array<xr_string, 4> CurrentSections;
	float RayFocus = 1.4f;
	xr_string Mode;
	EDofEvent ActiveEvent = EDofEvent::Count;
	SDofCameraSettings PreviewCamera;
	Fvector4 PreviewValues = {};
	xr_string PreviewSection;
	bool IsPreviewCamera = false;
	bool IsPreview = false;
	bool HasPendingPreview = false;
	bool HasPendingStop = false;
	bool HasStartup = false;
	bool IsLoaded = false;
	bool ShouldLoad = true;
	bool ShouldSave = false;
	bool ShouldReload = false;
	bool ShouldApplyGeneral = false;
	bool OpenPresets = false;
	bool IsLevelReady = false;
	bool IsMasterEnabled = false;
	int StaticGroup = 0;
	int PreviewGroup = 0;
	char ProfileFilter[128] = {};
	char SectionFilter[128] = {};
	char NewName[128] = {};
	char Motion[128] = "anm_reload";
};

static xr_unique_ptr<Editor> g_State;

static bool IsTrue(const xr_string& Value)
{
	return Value == "true" || Value == "on" || Value == "yes" || Value == "1";
}

static bool IsIdentifier(xr_string_view Text)
{
	return !Text.empty() && std::all_of(Text.begin(), Text.end(), [](char Value)
										{ return (Value >= 'a' && Value <= 'z') || (Value >= '0' && Value <= '9') || Value == '_' || Value == '-' || Value == '.'; });
}

static xr_string Format(const float* Values, size_t Count)
{
	xr_string Result;
	for (size_t Index = 0; Index < Count; ++Index)
	{
		char Text[48];
		xr_sprintf(Text, "%.9g", Values[Index]);
		Result += (Index ? ", " : "");
		Result += Text;
	}
	return Result;
}

static xr_string Canonical(FieldKind Kind, const char* Text)
{
	if (Kind == FieldKind::Bool)
	{
		return IsTrue(Text) ? "true" : "false";
	}
	if (Kind == FieldKind::Mode)
	{
		return !xr_strcmp(Text, "auto") ? "autofocus" : Text;
	}
	if (Kind == FieldKind::Resolution)
	{
		return Text;
	}
	float Numbers[3] = {};
	sscanf(Text, "%f , %f , %f", &Numbers[0], &Numbers[1], &Numbers[2]);
	return Format(Numbers, Kind == FieldKind::Vector3 ? 3 : 1);
}

static xr_unique_ptr<CInifile> OpenConfig()
{
	auto Result = xr_make_unique<CInifile>(pSettings->fname(), false, true, false);
	Result->set_override_names(true);
	return Result;
}

bool Editor::Load()
{
	Config = OpenConfig();
	if (!Config->section_count())
	{
		Status = "Cannot open the game configuration";
		Config.reset();
		IsLoaded = false;
		return false;
	}
	const xr_string Root = pSettings->fname();
	const size_t Name = Root.find_last_of("\\/") + 1;
	ModFile.assign(Root, 0, Name);
	ModFile += "mod_";
	ModFile.append(Root, Name, Root.rfind('.') - Name);
	ModFile += "_zz_dof_setup.ltx";
	ReadMod();
	Saved = Changes;
	if (!HasStartup)
	{
		Startup = Changes;
		HasStartup = true;
	}
	ReadGeneral();
	SavedGeneral = General;
	Index();
	IsLoaded = true;
	return true;
}

void Editor::ReadMod()
{
	Changes.clear();
	Added.clear();
	if (!FS.exist(ModFile.c_str()))
	{
		return;
	}
	auto Reader = FS.rg_open(ModFile.c_str());
	if (!Reader)
	{
		return;
	}
	auto TrimLambda = [](const xr_string& Text, size_t First, size_t Last)
	{
		const size_t Begin = Text.find_first_not_of(" \t", First);
		const size_t End = Begin == xr_string::npos || Begin >= Last ? xr_string::npos : Text.find_last_not_of(" \t\r\n", Last - 1);
		return End == xr_string::npos || End < Begin ? xr_string() : xr_string(Text.c_str() + Begin, static_cast<u32>(End - Begin + 1));
	};
	xr_string Raw, Section;
	while (!Reader->eof())
	{
		Reader->r_string(Raw);
		const xr_string Line = TrimLambda(Raw, 0, Raw.size());
		if (Line.empty() || Line[0] == ';')
		{
			continue;
		}
		const bool IsOverride = Line.starts_with("![");
		const size_t Close = Line.find(']');
		if ((IsOverride || Line[0] == '[') && Close != xr_string::npos)
		{
			const size_t Open = IsOverride ? 2 : 1;
			Section = TrimLambda(Line, Open, Close);
			Changes[Section];
			if (!IsOverride)
			{
				Added.insert(Section);
			}
			continue;
		}
		const size_t Equals = Line.find('=');
		if (!Section.empty() && Equals != xr_string::npos)
		{
			Changes[Section][TrimLambda(Line, 0, Equals)] = TrimLambda(Line, Equals + 1, Line.size());
		}
	}
}

xr_string Editor::ModText() const
{
	xr_string Text = "; Written by the DOF setup tool. Delete this file to return to the game's values.\r\n";
	for (const auto& [Section, Keys] : Changes)
	{
		Text += Added.contains(Section) ? "\r\n[" : "\r\n![";
		Text += Section;
		Text += "]\r\n";
		for (const auto& [Key, Value] : Keys)
		{
			Text += Key;
			Text += " = ";
			Text += Value;
			Text += "\r\n";
		}
	}
	return Text;
}

bool Editor::WriteMod() const
{
	IWriter* Writer = FS.w_open(ModFile.c_str());
	if (!Writer)
	{
		return false;
	}
	const xr_string Text = ModText();
	Writer->w(Text.data(), static_cast<u32>(Text.size()));
	FS.w_close(Writer, true);
	return true;
}

void Editor::ReadGeneral()
{
	General.clear();
	GeneralRanges.clear();
	for (const auto& Definition : GeneralFields)
	{
		if (auto* Command = Console->GetCommand(Definition.Key))
		{
			IConsole_Command::TStatus Value = {};
			Command->Status(Value);
			if (Definition.Kind == FieldKind::Float)
			{
				float Min = 0.0f, Max = 0.0f;
				Console->GetFloat(Definition.Key, Min, Max);
				GeneralRanges[Definition.Key] = {Min, Max};
			}
			General[Definition.Key] = Canonical(Definition.Kind, Value);
		}
	}
}

void Editor::ApplyGeneral(const xr_map<xr_string, xr_string>& Values)
{
	ReadGeneral();
	for (const auto& Definition : GeneralFields)
	{
		const auto It = Values.find(Definition.Key);
		const auto Current = General.find(Definition.Key);
		if (It != Values.end() && Current != General.end() && Current->second != It->second)
		{
			const xr_string Value = Definition.Kind == FieldKind::Bool ? IsTrue(It->second) ? "1" : "0" : It->second;
			Console->ExecuteCommand((xr_string(Definition.Key) + " " + Value).c_str(), false);
		}
	}
	ReadGeneral();
}

void Editor::Index()
{
	Profiles.clear();
	Sections.clear();
	xr_hash_set<shared_str> Referenced;
	for (const auto& Section : Config->sections())
	{
		Sections.emplace_back(Section.Name.c_str());
		for (const auto& Entry : Section.Data)
		{
			if (Entry.first.size() && Entry.second.size() && (xr_string_view(Entry.first.c_str()).starts_with("ndof_profile_") || Section.Name == "ndof_events"))
			{
				Referenced.insert(Entry.second);
			}
		}
	}
	for (const auto& Section : Config->sections())
	{
		const char* Name = Section.Name.c_str();
		if (Section.Name != "ndof_events" && (Referenced.contains(Section.Name) || xr_string_view(Name).starts_with("ndof_") || Added.contains(Name) || Config->line_exist(Name, "ndof_mode") || Config->line_exist(Name, "f_number")))
		{
			Profiles.emplace_back(Name);
		}
	}
}

bool Editor::IsLocal(const ChangeMap& Map, const char* Section, const char* Key) const
{
	const auto It = Map.find(Section);
	return It != Map.end() && It->second.contains(Key);
}

xr_string Editor::Effective(const char* Section, const char* Key, const char* Default) const
{
	const char* Value = Config->r_string_nullable(Section, Key);
	return Value ? Value : Default;
}

xr_string Editor::CameraDefault(const Field& Definition) const
{
	const char* Command = nullptr;
	if (!xr_strcmp(Definition.Key, "enabled"))
	{
		return "true";
	}
	if (!xr_strcmp(Definition.Key, "focus_point"))
	{
		return Definition.Default;
	}
	if (!xr_strcmp(Definition.Key, "ndof_mode"))
	{
		Command = "r4_ndof_mode";
	}
	const xr_string Name = Command ? Command : xr_string("r4_ndof_") + Definition.Key;
	const auto It = General.find(Name);
	return It != General.end() && It->second != "static" ? It->second : Definition.Default;
}

xr_string Editor::StaticDefault(const char* Key, const char* Fallback) const
{
	return Effective("gunslinger_base", Key, Fallback);
}

void Editor::Set(const char* Section, const char* Key, const char* Value)
{
	HasPendingPreview = true;
	if (Value)
	{
		Changes[Section][Key] = Value;
		Config->w_string(Section, Key, Value);
		return;
	}
	if (const auto It = Changes.find(Section); It != Changes.end())
	{
		It->second.erase(Key);
		if (It->second.empty() && !Added.contains(Section))
		{
			Changes.erase(It);
		}
	}
	if (IsLocal(Startup, Section, Key))
	{
		Status = "This field was saved in an earlier session: it shows the game's value after Save.";
		return;
	}
	if (pSettings->section_exist(Section) && pSettings->line_exist(Section, Key))
	{
		Config->w_string(Section, Key, pSettings->r_string(Section, Key));
	}
	else if (Config->section_exist(Section))
	{
		Config->remove_line(Section, Key);
	}
}

void Editor::Commit(const char* Section, const Field& Definition, const char* Value)
{
	if (Value && (Definition.Kind == FieldKind::Vector3 || Definition.Kind == FieldKind::Vector4))
	{
		float Numbers[4] = {};
		sscanf(Value, "%f , %f , %f , %f", &Numbers[0], &Numbers[1], &Numbers[2], &Numbers[3]);
		if (Numbers[0] > Numbers[1] || Numbers[1] > Numbers[2] || Numbers[3] < 0.0f)
		{
			Status = "Expected near <= focus <= far and a release of 0 or more";
			return;
		}
	}
	Status.clear();
	Set(Section, Definition.Key, Value);
}

bool Editor::DrawInput(const Field& Definition, const xr_string& Value, xr_string& Edited, bool AllowStaticMode)
{
	Edited = Value;
	bool Changed = false;
	ImGui::PushID(Definition.Key);
	if (Definition.Kind == FieldKind::Bool)
	{
		bool Enabled = IsTrue(Value);
		if (ImGui::Checkbox(Definition.Label, &Enabled))
		{
			Edited = Enabled ? "true" : "false";
			Changed = true;
		}
	}
	else if (Definition.Kind == FieldKind::Mode || Definition.Kind == FieldKind::Resolution || Definition.Kind == FieldKind::Reference)
	{
		if (ImGui::BeginCombo(Definition.Label, Value.c_str()))
		{
			auto OptionLambda = [&](const char* Name)
			{
				if (ImGui::Selectable(Name, Value == Name))
				{
					Edited = Name;
					Changed = true;
				}
			};
			if (Definition.Kind == FieldKind::Reference)
			{
				for (const auto& Name : Profiles)
				{
					OptionLambda(Name.c_str());
				}
			}
			else if (Definition.Kind == FieldKind::Mode)
			{
				OptionLambda("autofocus");
				OptionLambda("manual");
				if (AllowStaticMode)
				{
					OptionLambda("static");
				}
			}
			else
			{
				OptionLambda("half");
				OptionLambda("quarter");
			}
			ImGui::EndCombo();
		}
	}
	else
	{
		const int Count = Definition.Kind == FieldKind::Vector4 ? 4 : Definition.Kind == FieldKind::Vector3 ? 3
																  : Definition.Kind == FieldKind::Vector2	? 2
																											: 1;
		float Numbers[4] = {};
		sscanf(Value.c_str(), "%f , %f , %f , %f", &Numbers[0], &Numbers[1], &Numbers[2], &Numbers[3]);
		if (ImGui::DragScalarN(Definition.Label, ImGuiDataType_Float, Numbers, Count, 0.01f, &Definition.Min, &Definition.Max, "%.4f", ImGuiSliderFlags_AlwaysClamp) && std::all_of(Numbers, Numbers + Count, [](float Number)
																																																  { return _valid(Number); }))
		{
			Edited = Format(Numbers, Count);
			Changed = true;
		}
	}
	ImGui::SetItemTooltip("%s\nDrag to adjust; Ctrl+click for exact input.", Definition.Key);
	ImGui::PopID();
	return Changed;
}

void Editor::DrawField(const char* Section, const Field& Definition)
{
	ImGui::PushID(Section);
	ImGui::PushID(Definition.Key);
	const bool IsCamera = Section == Profile && std::find_if(std::begin(CameraFields), std::end(CameraFields), [&](const Field& Entry)
															 { return !xr_strcmp(Entry.Key, Definition.Key); }) != std::end(CameraFields);
	const xr_string Default = IsCamera ? CameraDefault(Definition) : xr_string(Definition.Default);
	xr_string Value = Effective(Section, Definition.Key, Default.c_str());
	bool IsSet = IsLocal(Changes, Section, Definition.Key);
	if (ImGui::Checkbox("##Local", &IsSet))
	{
		if (IsSet && Value.empty() && Definition.Kind == FieldKind::Reference)
		{
			Value = Profile;
		}
		Commit(Section, Definition, IsSet ? Value.c_str() : nullptr);
	}
	ImGui::SetItemTooltip("Ticked: the DOF setup file sets this field. Clear it to return to the game's value.");
	ImGui::SameLine();
	ImGui::PopID();
	xr_string Edited;
	if (DrawInput(Definition, Value, Edited))
	{
		Commit(Section, Definition, Edited.c_str());
	}
	ImGui::PopID();
}

void Editor::GeneralTab()
{
	ImGui::TextWrapped("Physical camera controls affect R4 autofocus/manual. Static controls affect classic DOF. Event profiles may override the base camera.");
	ImGui::TextWrapped("These are console settings: they act at once and Save stores them in the user config.");
	for (const auto& Definition : GeneralFields)
	{
		const auto It = General.find(Definition.Key);
		if (It == General.end())
		{
			continue;
		}
		if (!xr_strcmp(Definition.Key, "r4_ndof_mode"))
		{
			ImGui::SeparatorText("R4 camera / physical controls");
		}
		if (!xr_strcmp(Definition.Key, "r2_dof"))
		{
			ImGui::SeparatorText("Classic static controls");
		}
		Field Active = Definition;
		if (const auto Range = GeneralRanges.find(Definition.Key); Range != GeneralRanges.end())
		{
			Active.Min = Range->second.first;
			Active.Max = Range->second.second;
		}
		xr_string Edited;
		if (!DrawInput(Active, It->second, Edited, true))
		{
			continue;
		}
		float Numbers[3] = {};
		if (Definition.Kind == FieldKind::Vector3 && (sscanf(Edited.c_str(), "%f , %f , %f", &Numbers[0], &Numbers[1], &Numbers[2]) != 3 || Numbers[0] > Numbers[1] || Numbers[1] > Numbers[2]))
		{
			Status = "Expected near <= focus <= far";
			continue;
		}
		It->second = Edited;
		ShouldApplyGeneral = HasPendingPreview = true;
	}
}

void Editor::ProfileReference(const char* Section, const char* Key, const char* Label)
{
	Field Definition{Key, Label, FieldKind::Reference, ""};
	DrawField(Section, Definition);
	const xr_string Bound = Effective(Section, Key, "");
	if (!Bound.empty())
	{
		ImGui::SameLine();
		ImGui::PushID(Key);
		if (ImGui::SmallButton("Edit profile"))
		{
			Profile = Bound;
			HasPendingStop = true;
			OpenPresets = true;
		}
		ImGui::PopID();
	}
}

void Editor::CameraEditor()
{
	if (Profile.empty() || !Config->section_exist(Profile.c_str()))
	{
		return;
	}
	ImGui::Text("[%s]", Profile.c_str());
	ImGui::TextWrapped("Ticked fields are set by the DOF setup file; the others show the game's or the base camera's value. A disabled profile suppresses its event, including lower-priority bindings for that event.");
	for (const auto& Definition : CameraFields)
	{
		DrawField(Profile.c_str(), Definition);
	}
	ImGui::BeginDisabled(!psDeviceFlags.test(rsR4) || !IsLevelReady || !IsMasterEnabled);
	if (ImGui::Button("Preview profile"))
	{
		StartPreview(true);
	}
	ImGui::EndDisabled();
	ImGui::SameLine();
	if (ImGui::Button("Copy profile LTX"))
	{
		ImGui::SetClipboardText(CopySection(Profile.c_str(), true).c_str());
	}
}

void Editor::PresetsTab()
{
	ImGui::InputText("Filter profiles", ProfileFilter, sizeof(ProfileFilter));
	if (ImGui::BeginCombo("Profile", Profile.c_str()))
	{
		for (const auto& Name : Profiles)
		{
			if ((!ProfileFilter[0] || Name.find(ProfileFilter) != xr_string::npos) && ImGui::Selectable(Name.c_str(), Name == Profile))
			{
				HasPendingStop = true;
				Profile = Name;
			}
		}
		ImGui::EndCombo();
	}
	ImGui::InputText("New profile name", NewName, sizeof(NewName));
	const bool ShouldCreate = ImGui::Button("Create");
	ImGui::SameLine();
	const bool ShouldCopy = ImGui::Button("Duplicate");
	if ((ShouldCreate || ShouldCopy) && (!IsIdentifier(NewName) || Config->section_exist(NewName)))
	{
		Status = "A profile name must be new and use only a-z, 0-9, _, - and .";
	}
	else if (ShouldCreate || ShouldCopy)
	{
		Status.clear();
		Added.insert(NewName);
		Set(NewName, "enabled", "true");
		for (const auto& Definition : CameraFields)
		{
			if (ShouldCopy && Config->line_exist(Profile.c_str(), Definition.Key))
			{
				Set(NewName, Definition.Key, Effective(Profile.c_str(), Definition.Key, Definition.Default).c_str());
			}
		}
		Profile = NewName;
		Index();
	}
	ImGui::Separator();
	CameraEditor();
}

void Editor::EventsTab()
{
	ImGui::TextWrapped("Priority is fixed by the DOF manager. HUD/item/action/scope bindings can override this table. The first eligible enabled event wins.");
	for (u32 Index = 0; Index < static_cast<u32>(EDofEvent::Count); ++Index)
	{
		const char* Name = CDofManager::EventName(static_cast<EDofEvent>(Index));
		ImGui::PushID(static_cast<int>(Index));
		ImGui::SeparatorText(Name);
		ProfileReference("ndof_events", Name, "Global profile");
		if (ActiveEvent == static_cast<EDofEvent>(Index))
		{
			ImGui::TextWrapped("Active: [%s], binding: %s", ActiveProfile.c_str(), ActiveBinding.c_str());
		}
		ImGui::PopID();
	}
}

void Editor::PickCurrent(int Index)
{
	if (!CurrentSections[Index].empty())
	{
		HudSection = CurrentSections[Index];
		if (StaticGroup == 7 || StaticGroup == 8 || StaticGroup == 9)
		{
			StaticGroup = 0;
		}
		HasPendingStop = true;
	}
}

static SHudDofRequest ResolveStatic(const CInifile& Config, const char* Section, int Group, const char* Motion, float RayFocus)
{
	SHudDofDefaults Defaults;
	Defaults.LoadFrom(Config, "gunslinger_base");
	SHudDofDesc Desc;
	Desc.LoadFrom(Config, Section, Defaults);
	switch (Group)
	{
		case 0: return Desc.Zoom;
		case 1: return Desc.AlterZoom;
		case 2: return Desc.Grenade;
		case 3: return Desc.Lens;
		case 4:
		{
			const auto It = Desc.Actions.find(Motion);
			return It != Desc.Actions.end() ? It->second.Request : Defaults.Action.Request;
		}
		case 5: return Desc.Reload.Request;
		case 6: return Desc.Usable.Request;
		case 7: return {Defaults.Zoom, Defaults.Action.Request.SpeedIn, Defaults.Action.Request.SpeedOut};
		case 8: return Defaults.Inventory;
		case 9: return {Fvector().set(RayFocus + Defaults.PickNear, RayFocus, RayFocus + Defaults.PickFar), Defaults.Speed, Defaults.Speed};
		case 10: return Desc.Pda;
		default: return Defaults.Action.Request;
	}
}

void Editor::StaticEditor()
{
	static const char* Groups[] = {"Normal aim", "Alternate aim", "Grenade aim", "3D lens", "Animation", "Legacy reload", "Legacy usable item", "Global defaults", "Inventory defaults", "Zone ray offsets", "PDA"};
	if (ImGui::Combo("Static group", &StaticGroup, Groups, IM_ARRAYSIZE(Groups)))
	{
		if (StaticGroup == 7 || StaticGroup == 8)
		{
			HudSection = "gunslinger_base";
		}
		if (StaticGroup == 9)
		{
			HudSection = "zone_pick_dof";
		}
		HasPendingStop = true;
	}
	if (StaticGroup == 4 && (!IsIdentifier(Motion) || !xr_string_view(Motion).starts_with("anm_")))
	{
		ImGui::TextWrapped("Select an animation alias beginning with anm_.");
		return;
	}
	const auto Resolved = ResolveStatic(*Config, HudSection.c_str(), StaticGroup, Motion, RayFocus);
	GroupKeys.clear();
	GroupDefaults.clear();
	auto FieldLambda = [&](const xr_string& Key, const char* Label, FieldKind Kind, const xr_string& Default, float Min = -flt_max, float Max = flt_max)
	{
		GroupKeys.push_back(Key);
		GroupDefaults[Key] = Default;
		const Field Definition{Key.c_str(), Label, Kind, Default.c_str(), Min, Max};
		DrawField(HudSection.c_str(), Definition);
	};
	if (StaticGroup <= 3 || StaticGroup == 4 || StaticGroup == 7 || StaticGroup == 8 || StaticGroup == 10)
	{
		const xr_string Prefix = (StaticGroup == 0 || StaticGroup == 10) ? "zoom_dof" : StaticGroup == 1 ? "alter_zoom_dof"
																					: StaticGroup == 2	 ? "gl_dof"
																					: StaticGroup == 3	 ? "lens_dof"
																					: StaticGroup == 4	 ? xr_string("dof_") + Motion
																					: StaticGroup == 7	 ? "default_zoom_dof"
																										 : "inventory_dof";
		const xr_string Near = Format(&Resolved.Values.x, 1);
		const xr_string Focus = Format(&Resolved.Values.y, 1);
		const xr_string Far = Format(&Resolved.Values.z, 1);
		FieldLambda(Prefix + "_near", "Near (m)", FieldKind::Float, StaticGroup == 1 ? Effective(HudSection.c_str(), "zoom_dof_near", Near.c_str()) : Near);
		FieldLambda(Prefix + "_focus", "Focus (m)", FieldKind::Float, StaticGroup == 1 ? Effective(HudSection.c_str(), "zoom_dof_focus", Focus.c_str()) : Focus);
		FieldLambda(Prefix + "_far", "Far (m)", FieldKind::Float, StaticGroup == 1 ? Effective(HudSection.c_str(), "zoom_dof_far", Far.c_str()) : Far);
		float Order[3] = {};
		for (u32 Index = 0; Index < 3; ++Index)
		{
			Order[Index] = static_cast<float>(atof(Effective(HudSection.c_str(), GroupKeys[Index].c_str(), GroupDefaults.at(GroupKeys[Index]).c_str()).c_str()));
		}
		if (Order[0] > Order[1] || Order[1] > Order[2])
		{
			ImGui::TextColored(ImVec4(1, 0.4f, 0.3f, 1), "Near <= focus <= far is required: the game ignores these three values until they are in order.");
		}
		const xr_string SpeedIn = StaticDefault("default_dof_speed_in", "3");
		const xr_string SpeedOut = StaticDefault("default_dof_speed_out", "1");
		if (StaticGroup == 4)
		{
			FieldLambda(xr_string("use_dof_") + Motion, "Enable animation DOF", FieldKind::Bool, xr_string_view(Motion).starts_with("anm_reload") || xr_string_view(Motion).starts_with("anm_open") || xr_string_view(Motion).starts_with("anm_close") || xr_string_view(Motion).starts_with("anm_add_cartridge") ? "true" : "false");
			FieldLambda(xr_string("dof_speed_in_") + Motion, "Enter rate (1/s)", FieldKind::Float, SpeedIn, 0, 1000);
			FieldLambda(xr_string("dof_speed_out_") + Motion, "Return rate (1/s)", FieldKind::Float, SpeedOut, 0, 1000);
			FieldLambda(xr_string("dof_time_offset_") + Motion, "Release offset (s)", FieldKind::Float, StaticDefault("default_dof_time_offset", "-0.4"), -flt_max, flt_max);
		}
		else if (StaticGroup == 7)
		{
			for (const char* Suffix : {"near", "focus", "far"})
			{
				const xr_string Key = xr_string("default_action_dof_") + Suffix;
				FieldLambda(Key, Key.c_str(), FieldKind::Float, !xr_strcmp(Suffix, "near") ? "0" : !xr_strcmp(Suffix, "focus") ? "0.5"
																															   : "5");
			}
			FieldLambda("default_dof_speed", "Default rate (1/s)", FieldKind::Float, "5", 0, 1000);
			FieldLambda("default_dof_speed_in", "Enter rate (1/s)", FieldKind::Float, "3", 0, 1000);
			FieldLambda("default_dof_speed_out", "Return rate (1/s)", FieldKind::Float, "1", 0, 1000);
			FieldLambda("default_dof_time_offset", "Release offset (s)", FieldKind::Float, "-0.4", -flt_max, flt_max);
		}
		else
		{
			const char* EnterKey = (StaticGroup == 0 || StaticGroup == 10) ? "zoom_in_dof_speed" : StaticGroup == 1 ? "alter_zoom_in_dof_speed"
																							   : StaticGroup == 2	? "gl_in_dof_speed"
																							   : StaticGroup == 8	? "inventory_dof_speed_in"
																													: "zoom_in_dof_speed";
			FieldLambda(EnterKey, "Enter rate (1/s)", FieldKind::Float, SpeedIn, 0, 1000);
			FieldLambda(StaticGroup == 8 ? "inventory_dof_speed_out" : "zoom_out_dof_speed", "Return rate (1/s)", FieldKind::Float, SpeedOut, 0, 1000);
		}
	}
	else if (StaticGroup == 5 || StaticGroup == 6)
	{
		FieldLambda(StaticGroup == 5 ? "reload_dof" : "dof", "Near / focus / far / release (m, s)", FieldKind::Vector4, "0, 0.5, 5, 0");
	}
	else
	{
		FieldLambda("near", "Ray near offset (m)", FieldKind::Float, "-70", -flt_max, 0);
		FieldLambda("far", "Ray far offset (m)", FieldKind::Float, "70", 0, flt_max);
	}
	ImGui::BeginDisabled(!IsLevelReady || !IsMasterEnabled);
	if (ImGui::Button("Preview static group"))
	{
		StartPreview(false);
	}
	ImGui::EndDisabled();
	ImGui::SameLine();
	if (ImGui::Button("Copy group"))
	{
		CopiedGroup.clear();
		for (const auto& Key : GroupKeys)
		{
			CopiedGroup[Key] = Effective(HudSection.c_str(), Key.c_str(), GroupDefaults.at(Key).c_str());
		}
	}
	ImGui::SameLine();
	ImGui::BeginDisabled(CopiedGroup.empty());
	if (ImGui::Button("Paste group"))
	{
		for (const auto& [Key, Value] : CopiedGroup)
		{
			Set(HudSection.c_str(), Key.c_str(), Value.c_str());
		}
	}
	ImGui::EndDisabled();
	ImGui::TextWrapped("Release offset: positive = seconds from start; negative = seconds before end; zero = full duration. Preview holds the camera; it does not play an animation.");
}

void Editor::HudTab()
{
	ImGui::InputText("Find section", SectionFilter, sizeof(SectionFilter));
	if (ImGui::BeginCombo("Section", HudSection.c_str()))
	{
		xr_vector<const xr_string*> Visible;
		for (const auto& Name : Sections)
		{
			if (!SectionFilter[0] || Name.find(SectionFilter) != xr_string::npos)
			{
				Visible.push_back(&Name);
			}
		}
		ImGuiListClipper Clipper;
		Clipper.Begin(static_cast<int>(Visible.size()));
		while (Clipper.Step())
		{
			for (int Index = Clipper.DisplayStart; Index < Clipper.DisplayEnd; ++Index)
			{
				if (ImGui::Selectable(Visible[Index]->c_str(), HudSection == *Visible[Index]))
				{
					HasPendingStop = true;
					HudSection = *Visible[Index];
					if (StaticGroup == 7 || StaticGroup == 8 || StaticGroup == 9)
					{
						StaticGroup = 0;
					}
				}
			}
		}
		ImGui::EndCombo();
	}
	static const char* CurrentButtons[] = {"Current item", "Current HUD", "Attached scope", "HUD animator"};
	for (int Index = 0; Index < 4; ++Index)
	{
		if (Index)
		{
			ImGui::SameLine();
		}
		if (ImGui::Button(CurrentButtons[Index]))
		{
			PickCurrent(Index);
		}
	}
	if (ImGui::Button("Global defaults"))
	{
		HasPendingStop = true;
		HudSection = "gunslinger_base";
		StaticGroup = 7;
	}
	ImGui::SameLine();
	if (ImGui::Button("Zone offsets"))
	{
		HasPendingStop = true;
		HudSection = "zone_pick_dof";
		StaticGroup = 9;
	}
	if (HudSection.empty() || !Config->section_exist(HudSection.c_str()))
	{
		ImGui::TextWrapped("Select an existing HUD, item, scope or animator section.");
		return;
	}
	if (ImGui::InputText("Animation alias", Motion, sizeof(Motion)))
	{
		HasPendingStop = true;
	}
	if (ImGui::BeginCombo("Existing aliases", Motion))
	{
		xr_hash_set<shared_str> Seen;
		for (const auto& Entry : Config->r_section(HudSection.c_str()).Data)
		{
			const shared_str Alias = Entry.first.size() ? DofSettings::AnimationAlias(Entry.first.c_str()) : shared_str();
			if (Alias.size() && Seen.insert(Alias).second && ImGui::Selectable(Alias.c_str()))
			{
				xr_strcpy(Motion, sizeof(Motion), Alias.c_str());
				HasPendingStop = true;
			}
		}
		ImGui::EndCombo();
	}
	if (ImGui::CollapsingHeader("Physical event / animation bindings", ImGuiTreeNodeFlags_DefaultOpen))
	{
		for (u32 Index = 0; Index < static_cast<u32>(EDofEvent::Count); ++Index)
		{
			const char* Event = CDofManager::EventName(static_cast<EDofEvent>(Index));
			ProfileReference(HudSection.c_str(), (xr_string("ndof_profile_") + Event).c_str(), Event);
		}
		if (IsIdentifier(Motion) && xr_string_view(Motion).starts_with("anm_"))
		{
			ProfileReference(HudSection.c_str(), (xr_string("ndof_profile_") + Motion).c_str(), "Animation profile");
		}
	}
	if (ImGui::CollapsingHeader("Classic static DOF", ImGuiTreeNodeFlags_DefaultOpen))
	{
		DrawField(HudSection.c_str(), {"disable_zoom_dof", "Disable static aiming DOF", FieldKind::Bool, "false"});
		StaticEditor();
	}
	if (ImGui::Button("Copy section DOF LTX"))
	{
		ImGui::SetClipboardText(CopySection(HudSection.c_str(), false).c_str());
	}
}

xr_string Editor::CopySection(const char* Section, bool IsCamera) const
{
	xr_string Text = xr_string("[") + Section + "]\r\n";
	auto LineLambda = [&Text](const char* Key, const char* Value)
	{
		Text += Key;
		Text += " = ";
		Text += Value;
		Text += "\r\n";
	};
	if (const auto It = Changes.find(Section); !IsCamera && It != Changes.end())
	{
		for (const auto& [Key, Value] : It->second)
		{
			LineLambda(Key.c_str(), Value.c_str());
		}
	}
	for (const auto& Entry : Config->r_section(Section).Data)
	{
		if (IsCamera && Entry.first.size())
		{
			LineLambda(Entry.first.c_str(), Entry.second.size() ? Entry.second.c_str() : "");
		}
	}
	return Text;
}

void Editor::StartPreview(bool IsCamera)
{
	HasPendingStop = false;
	IsPreview = HasPendingPreview = true;
	IsPreviewCamera = IsCamera;
	PreviewSection = IsCamera ? Profile : HudSection;
	PreviewGroup = StaticGroup;
}

void Editor::UpdatePreview()
{
	HasPendingPreview = false;
	if (!IsPreviewCamera)
	{
		const SHudDofRequest Request = ResolveStatic(*Config, PreviewSection.c_str(), PreviewGroup, Motion, RayFocus);
		const float Focus = std::max(Request.Values.y, Request.Values.x + EPS_L);
		const auto Kernel = General.find("r2_dof_kernel");
		PreviewValues.set(Request.Values.x, Focus, std::max(Request.Values.z, Focus + EPS_L), Kernel != General.end() ? static_cast<float>(atof(Kernel->second.c_str())) : 5.0f);
		return;
	}
	PreviewCamera = {};
	auto FloatLambda = [&](const char* Name, float& Value)
	{
		if (const auto It = General.find(Name); It != General.end())
		{
			Value = static_cast<float>(atof(It->second.c_str()));
		}
	};
	FloatLambda("r4_ndof_f_number", PreviewCamera.FNumber);
	FloatLambda("r4_ndof_focal_length", PreviewCamera.FocalLength);
	FloatLambda("r4_ndof_focus_distance", PreviewCamera.FocusDistance);
	FloatLambda("r4_ndof_focus_time", PreviewCamera.FocusTime);
	FloatLambda("r4_ndof_transition_time", PreviewCamera.TransitionTime);
	if (const auto It = General.find("r4_ndof_manual_focal_length"); It != General.end())
	{
		PreviewCamera.ManualFocalLength = IsTrue(It->second);
	}
	if (const auto It = General.find("r4_ndof_mode"); It != General.end())
	{
		PreviewCamera.Mode = It->second == "manual" ? EDofMode::Manual : EDofMode::Autofocus;
	}
	SDofCameraDesc Desc;
	Desc.LoadFrom(*Config, PreviewSection.c_str());
	Desc.Apply(PreviewCamera);
}

void Editor::StopPreview(CDofManager& Manager)
{
	if (IsPreview)
	{
		Manager.SetForced(false);
		Manager.SetCinema(nullptr);
		Manager.SetCinemaCamera(nullptr);
	}
	IsPreview = HasPendingPreview = HasPendingStop = false;
}

void Editor::OnFrame(CDofManager& Manager)
{
	const bool IsShown = Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_DofSetup)] && CImGuiManager::Instance().IsGUIRendering();
	if (!IsShown && !ShouldSave && !ShouldReload)
	{
		StopPreview(Manager);
		return;
	}
	if (!IsLoaded)
	{
		if (!ShouldLoad)
		{
			return;
		}
		ShouldLoad = false;
		if (!Load())
		{
			return;
		}
	}
	if (ShouldApplyGeneral)
	{
		ShouldApplyGeneral = false;
		ApplyGeneral(xr_map<xr_string, xr_string>(General));
	}
	IsLevelReady = g_pGameLevel && g_pGameLevel->bReady;
	const auto Master = General.find("r2_dof_enable");
	IsMasterEnabled = Master != General.end() && IsTrue(Master->second);
	CurrentSections = {};
	if (CActor* Actor = IsLevelReady ? ::Actor() : nullptr)
	{
		CInventoryItem* Item = Actor->inventory().ActiveItem();
		if (CHudItem* Hud = Item ? Item->cast_hud_item() : nullptr)
		{
			const auto& Source = Hud->DofSource;
			CurrentSections[0] = Source.ItemSection.size() ? Source.ItemSection.c_str() : "";
			CurrentSections[1] = Source.HudSection.size() ? Source.HudSection.c_str() : "";
			CurrentSections[2] = Source.ScopeSection.size() ? Source.ScopeSection.c_str() : "";
		}
		if (Actor->HudAnimator() && Actor->HudAnimator()->CurrentAnimator())
		{
			const auto& Section = Actor->HudAnimator()->CurrentAnimator()->DofSource.HudSection;
			CurrentSections[3] = Section.size() ? Section.c_str() : "";
		}
		RayFocus = HUD().GetCurrentRayQuery().range;
	}
	if (!IsShown || !IsLevelReady || !IsMasterEnabled || HasPendingStop || ShouldSave || ShouldReload)
	{
		StopPreview(Manager);
	}
	if (ShouldSave || ShouldReload)
	{
		const bool IsSaving = ShouldSave;
		const auto Previous = SavedGeneral;
		ShouldSave = ShouldReload = false;
		if (IsSaving && !WriteMod())
		{
			Status = "Cannot write " + ModFile;
		}
		else if (Load())
		{
			DofSettings::Apply(OpenConfig());
			if (IsSaving)
			{
				Console->ExecuteCommand("cfg_save", false);
				Status = "Saved " + ModFile + " and the user config";
			}
			else
			{
				ApplyGeneral(Previous);
				SavedGeneral = General;
				Status = "Reloaded from disk";
			}
		}
	}
	if (IsPreview && HasPendingPreview)
	{
		UpdatePreview();
	}
	if (IsPreview)
	{
		Manager.SetForced(!IsPreviewCamera);
		Manager.SetCinema(IsPreviewCamera ? nullptr : &PreviewValues);
		Manager.SetCinemaCamera(IsPreviewCamera ? &PreviewCamera : nullptr);
	}
	ActiveEvent = Manager.GetActiveEvent();
	ActiveProfile = Manager.GetActiveProfile().size() ? Manager.GetActiveProfile().c_str() : "base/static";
	ActiveBinding = Manager.GetActiveBinding().size() ? Manager.GetActiveBinding().c_str() : "base";
	const char* Token = Console->GetCommand("r4_ndof_mode") ? Console->GetToken("r4_ndof_mode") : "static";
	Mode = Token ? Token : "static";
}

void Editor::Draw()
{
	bool& IsOpen = Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_DofSetup)];
	if (!IsOpen)
	{
		return;
	}
	ImGui::SetNextWindowSize(ImVec2(800, 720), ImGuiCond_FirstUseEver);
	if (!ImGui::Begin("DOF setup", &IsOpen))
	{
		ImGui::End();
		return;
	}
	if (!IsLoaded)
	{
		ImGui::TextWrapped("%s", Status.empty() ? "Loading DOF configuration..." : Status.c_str());
		if (ImGui::Button("Retry"))
		{
			ShouldLoad = true;
		}
		ImGui::End();
		return;
	}
	const bool HasUnsaved = Changes != Saved || General != SavedGeneral;
	ImGui::Text("%s | %s | event: %s", psDeviceFlags.test(rsR4) ? "R4" : psDeviceFlags.test(rsR2) ? "R2"
																								  : "R1",
				Mode.c_str(),
				CDofManager::EventName(ActiveEvent));
	ImGui::TextWrapped("Profile: %s | binding: %s", ActiveProfile.c_str(), ActiveBinding.c_str());
	ImGui::TextUnformatted(HasUnsaved ? "Unsaved changes: the game uses profile and weapon values after Save" : "Saved");
	if (IsPreview)
	{
		ImGui::TextColored(ImVec4(1, 0.8f, 0.2f, 1), "FORCED PREVIEW: [%s]", PreviewSection.c_str());
		ImGui::SameLine();
		if (ImGui::Button("Stop preview"))
		{
			HasPendingStop = true;
		}
	}
	if (ImGui::Button("Save"))
	{
		ShouldSave = true;
	}
	ImGui::SetItemTooltip("Writes %s and the user config, then reloads", ModFile.c_str());
	ImGui::SameLine();
	if (ImGui::Button("Reload from disk"))
	{
		if (HasUnsaved)
		{
			ImGui::OpenPopup("Discard unsaved DOF changes?");
		}
		else
		{
			ShouldReload = true;
		}
	}
	ImGui::SameLine();
	if (ImGui::Button("Copy LTX"))
	{
		ImGui::SetClipboardText(ModText().c_str());
	}
	if (ImGui::BeginPopupModal("Discard unsaved DOF changes?", nullptr, ImGuiWindowFlags_AlwaysAutoResize))
	{
		ImGui::TextUnformatted("Reload drops every change made since the last Save.");
		if (ImGui::Button("Discard and reload"))
		{
			ShouldReload = true;
			ImGui::CloseCurrentPopup();
		}
		ImGui::SameLine();
		if (ImGui::Button("Cancel"))
		{
			ImGui::CloseCurrentPopup();
		}
		ImGui::EndPopup();
	}
	if (!Status.empty())
	{
		ImGui::TextWrapped("%s", Status.c_str());
	}
	if (!IsLevelReady)
	{
		ImGui::TextDisabled("Load a level to preview DOF. Config editing is available.");
	}
	if (!IsMasterEnabled && General.contains("r2_dof_enable"))
	{
		ImGui::TextDisabled("DOF master is off: enable it in General to preview.");
	}
	if (ImGui::BeginTabBar("DofTabs"))
	{
		if (ImGui::BeginTabItem("General"))
		{
			GeneralTab();
			ImGui::EndTabItem();
		}
		if (ImGui::BeginTabItem("NDOF presets", nullptr, OpenPresets ? ImGuiTabItemFlags_SetSelected : 0))
		{
			OpenPresets = false;
			PresetsTab();
			ImGui::EndTabItem();
		}
		if (ImGui::BeginTabItem("Events"))
		{
			EventsTab();
			ImGui::EndTabItem();
		}
		if (ImGui::BeginTabItem("HUD / Weapons"))
		{
			HudTab();
			ImGui::EndTabItem();
		}
		ImGui::EndTabBar();
	}
	ImGui::End();
}
} // namespace DofSetup

void RenderDofSetup()
{
	if (!DofSetup::g_State && Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_DofSetup)])
	{
		DofSetup::g_State = xr_make_unique<DofSetup::Editor>();
	}
	if (DofSetup::g_State)
	{
		DofSetup::g_State->Draw();
	}
}

void ProcessDofSetupFrame(CDofManager& Manager)
{
	if (DofSetup::g_State)
	{
		DofSetup::g_State->OnFrame(Manager);
	}
}

void ShutdownDofSetup()
{
	DofSetup::g_State.reset();
}
#endif
