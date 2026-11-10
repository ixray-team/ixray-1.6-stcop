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
#include "../../xrCore/LtxDocument.h"
#include "../../xrEngine/XR_IOConsole.h"
#include "../../xrEngine/xr_ioc_cmd.h"
#include "../../xrEngine/ImGuiManager.h"
#include <array>
#include <cmath>

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
	{"r4_ndof_focal_length", "Focal length (mm)", FieldKind::Float, "10", 1.0f, 1000.0f},
	{"r4_ndof_f_number", "Aperture (f-number)", FieldKind::Float, "4", 0.7f, 64.0f},
	{"r4_ndof_sensor_height", "Sensor height (m)", FieldKind::Float, "0.024", 0.001f, 0.1f},
	{"r4_ndof_focus_distance", "Focus distance (m)", FieldKind::Float, "1.4", 0.01f, 10000.0f},
	{"r4_ndof_focus_point_x", "Autofocus point X (UV)", FieldKind::Float, "0.5", 0.0f, 1.0f},
	{"r4_ndof_focus_point_y", "Autofocus point Y (UV)", FieldKind::Float, "0.495", 0.0f, 1.0f},
	{"r4_ndof_focus_time", "Focus response (s)", FieldKind::Float, "0.1", 0.01f, 10.0f},
	{"r4_ndof_transition_time", "Lens transition (s)", FieldKind::Float, "0.1", 0.01f, 10.0f},
	{"r4_ndof_max_radius", "Maximum radius (output pixels)", FieldKind::Float, "20", 1.0f, 40.0f},
	{"r4_ndof_resolution", "Bokeh resolution", FieldKind::Resolution},
	{"r2_dof", "Static near / focus / far (m)", FieldKind::Vector3, "-1.25, 1.4, 600"},
	{"r2_dof_kernel", "Static blur kernel", FieldKind::Float, "5", 0.0f, 80.0f},
	{"r2_dof_sky", "Static sky distance (m)", FieldKind::Float, "30"},
	{"r2_dof_lens_kernel_scale", "Static lens radius multiplier", FieldKind::Float, "2", 1.0f, 4.0f},
	{"r2_dynamic_dof", "Static weapon action profiles", FieldKind::Bool},
	{"r2_const_zoomdof", "Static fixed aiming distances", FieldKind::Bool},
	{"enable_dof_reload", "Static reload DOF", FieldKind::Bool},
	{"enable_dof_hud_animator", "Static HUD animator / PDA DOF", FieldKind::Bool},
	{"enable_dof_talk", "Static dialogue DOF", FieldKind::Bool},
	{"r4_ndof_inventory", "Static inventory DOF", FieldKind::Bool}
};

enum class Operation
{
	None,
	Apply,
	Save,
	Reload,
	Revert
};

struct InputValue
{
	xr_array<float, 4> Numbers = {};
	xr_string Text;
};

class Editor final
{
public:
	Editor() = default;
	~Editor() = default;
	void Draw();
	void OnFrame(CDofManager& Manager);

private:
	bool Load();
	void Index();
	void ReadGeneral();
	bool ReadSavedGeneral(xr_map<xr_string, xr_string>& Values, xr_string& Error, bool RequireAll = false) const;
	void ApplyGeneral(const xr_map<xr_string, xr_string>& Values);
	void RefreshChanges();
	bool Commit(const char* Section, const Field& Definition, const char* Value);
	void GeneralTab();
	void PresetsTab();
	void EventsTab();
	void HudTab();
	void CameraEditor();
	void StaticEditor();
	bool DrawField(const char* Section, const Field& Definition, bool IsGeneral = false);
	bool DrawInput(const char* Section, const Field& Definition, const xr_string& Value, xr_string& Edited, bool AllowStaticMode = false);
	void ProfileReference(const char* Section, const char* Key, const char* Label);
	void StartPreview(bool IsCamera);
	void UpdatePreview();
	SHudDofRequest StaticPreview() const;
	xr_string Effective(const char* Section, const char* Key, const char* Default) const;
	xr_string CameraDefault(const Field& Definition) const;
	xr_string StaticDefault(const char* Key, const char* Fallback) const;
	xr_string CopySection(const char* Section, bool IsCamera) const;
	void PickCurrent(int Index);

	LtxDocument Document;
	CInifile::SourceTextMap AppliedTexts;
	xr_map<xr_string, xr_string> General;
	xr_map<xr_string, xr_string> AppliedGeneral;
	xr_map<xr_string, xr_string> SavedGeneral;
	xr_map<xr_string, xr_pair<float, float>> GeneralRanges;
	xr_map<xr_string, InputValue> Inputs;
	xr_vector<xr_string> Profiles;
	xr_vector<xr_string> Sections;
	xr_vector<xr_string> ChangedFiles;
	xr_vector<xr_string> GroupKeys;
	xr_map<xr_string, xr_string> CopiedGroup;
	xr_map<xr_string, xr_string> GroupDefaults;
	xr_string Root;
	xr_string UserConfig;
	xr_string NdoFile;
	xr_string Profile = "ndof_neutral";
	xr_string HudSection;
	xr_string Status;
	xr_string SaveIssue;
	xr_string ActiveProfile;
	xr_string ActiveBinding;
	xr_string ParentSection;
	xr_array<xr_string, 4> CurrentSections;
	float RayFocus = 1.4f;
	xr_string Mode;
	EDofEvent ActiveEvent = EDofEvent::Count;
	Operation Pending = Operation::None;
	SDofCameraDesc PreviewCamera;
	SHudDofRequest PreviewRequest;
	xr_string PreviewSection;
	bool IsPreviewCamera = false;
	bool IsPreview = false;
	bool HasPendingPreview = false;
	bool HasPendingStop = false;
	bool IsLoaded = false;
	bool ShouldLoad = true;
	bool HasUnsaved = false;
	bool HasUnapplied = false;
	bool OpenPresets = false;
	bool IsLevelReady = false;
	bool WasLevelReady = false;
	bool IsMasterEnabled = false;
	bool CanWriteSources = false;
	int StaticGroup = 0;
	int PreviewGroup = 0;
	char ProfileFilter[128] = {};
	char SectionFilter[128] = {};
	char NewName[128] = {};
	char Motion[128] = "anm_reload";
	char ParentNames[512] = {};
};

static xr_unique_ptr<Editor> g_State;

static bool IsTrue(const xr_string& Value)
{
	return Value == "true" || Value == "on" || Value == "yes" || Value == "1";
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

static bool CanonicalGeneral(const Field& Definition, const char* Text, xr_string& Value)
{
	if (Definition.Kind == FieldKind::Bool)
	{
		const xr_string Raw = Text;
		if (Raw != "true" && Raw != "false" && Raw != "on" && Raw != "off" && Raw != "1" && Raw != "0")
		{
			return false;
		}
		Value = IsTrue(Raw) ? "true" : "false";
		return true;
	}
	if (Definition.Kind == FieldKind::Mode)
	{
		Value = !xr_strcmp(Text, "auto") ? "autofocus" : Text;
		return Value == "autofocus" || Value == "manual" || Value == "static";
	}
	if (Definition.Kind == FieldKind::Resolution)
	{
		Value = Text;
		return Value == "half" || Value == "quarter";
	}
	float Numbers[3] = {};
	char Extra = 0;
	const int Count = Definition.Kind == FieldKind::Vector3 ? 3 : 1;
	const int Read = Count == 3 ? sscanf(Text, "%f , %f , %f %c", &Numbers[0], &Numbers[1], &Numbers[2], &Extra) : sscanf(Text, "%f %c", &Numbers[0], &Extra);
	if (Read != Count)
	{
		return false;
	}
	float Min = Definition.Min;
	float Max = Definition.Max;
	for (int Index = 0; Index < Count; ++Index)
	{
		if (!_valid(Numbers[Index]) || Numbers[Index] < Min || Numbers[Index] > Max)
		{
			return false;
		}
	}
	if (Count == 3 && (Numbers[0] > Numbers[1] || Numbers[1] > Numbers[2] || Numbers[2] < 0))
	{
		return false;
	}
	Value = Format(Numbers, Count);
	return true;
}

bool Editor::Load()
{
	if (Root.empty())
	{
		Root = pSettings->fname();
	}
	if (!Document.Load(Root.c_str(), Status))
	{
		return false;
	}
	ReadGeneral();
	AppliedTexts = Document.GetTexts();
	AppliedGeneral = SavedGeneral = General;
	xr_path Name(Console->ConfigFile);
	Name.replace_extension(".ltx");
	if (Name.is_absolute())
	{
		UserConfig = Name.xstring();
	}
	else
	{
		string_path Path;
		FS.update_path(Path, _app_data_root_, Name.xstring().c_str());
		UserConfig = Path;
	}
	xr_string ReadError;
	ReadSavedGeneral(SavedGeneral, ReadError);
	Inputs.clear();
	ParentSection.clear();
	Index();
	RefreshChanges();
	IsLoaded = true;
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
			Field Active = Definition;
			if (Definition.Kind == FieldKind::Float)
			{
				Console->GetFloat(Definition.Key, Active.Min, Active.Max);
				GeneralRanges[Definition.Key] = {Active.Min, Active.Max};
			}
			xr_string Canonical;
			General[Definition.Key] = CanonicalGeneral(Active, Value, Canonical) ? Canonical : Value;
		}
	}
}

bool Editor::ReadSavedGeneral(xr_map<xr_string, xr_string>& Values, xr_string& Error, bool RequireAll) const
{
	auto Reader = FS.rg_open(UserConfig.c_str());
	if (!Reader)
	{
		Error = "Cannot read user config: " + UserConfig;
		return false;
	}
	auto Loaded = Values;
	xr_set<xr_string> Seen;
	xr_string Line;
	while (!Reader->eof())
	{
		Reader->r_string(Line);
		const size_t Begin = Line.find_first_not_of(" \t");
		if (Begin == xr_string::npos)
		{
			continue;
		}
		const size_t Split = Line.find_first_of(" \t", Begin);
		if (Split == xr_string::npos)
		{
			continue;
		}
		const xr_string Key = Line.substr(Begin, Split - Begin);
		const auto Definition = std::find_if(std::begin(GeneralFields), std::end(GeneralFields), [&](const Field& Entry)
											 { return Key == Entry.Key; });
		if (Definition == std::end(GeneralFields) || !General.contains(Key))
		{
			continue;
		}
		const size_t First = Line.find_first_not_of(" \t", Split);
		const size_t Last = Line.find_last_not_of(" \t\r\n");
		const xr_string Text = First != xr_string::npos ? Line.substr(First, Last - First + 1) : "";
		xr_string Canonical;
		Field Active = *Definition;
		if (const auto Range = GeneralRanges.find(Key); Range != GeneralRanges.end())
		{
			Active.Min = Range->second.first;
			Active.Max = Range->second.second;
		}
		if (!CanonicalGeneral(Active, Text.c_str(), Canonical))
		{
			Error = "Invalid saved DOF command: " + Key;
			return false;
		}
		Loaded[Key] = Canonical;
		Seen.insert(Key);
	}
	if (RequireAll && Seen.size() != General.size())
	{
		Error = "User config is incomplete: " + UserConfig;
		return false;
	}
	Values = std::move(Loaded);
	Error.clear();
	return true;
}

void Editor::ApplyGeneral(const xr_map<xr_string, xr_string>& Values)
{
	for (const auto& Definition : GeneralFields)
	{
		const auto It = Values.find(Definition.Key);
		if (It != Values.end())
		{
			IConsole_Command::TStatus Current = {};
			Console->GetCommand(Definition.Key)->Status(Current);
			xr_string Canonical;
			Field Active = Definition;
			if (const auto Range = GeneralRanges.find(Definition.Key); Range != GeneralRanges.end())
			{
				Active.Min = Range->second.first;
				Active.Max = Range->second.second;
			}
			if (!CanonicalGeneral(Active, Current, Canonical) || Canonical != It->second)
			{
				const xr_string Value = Definition.Kind == FieldKind::Bool ? IsTrue(It->second) ? "1" : "0" : It->second;
				Console->ExecuteCommand((xr_string(Definition.Key) + " " + Value).c_str(), false);
			}
		}
	}
	ReadGeneral();
}

void Editor::Index()
{
	Profiles.clear();
	Sections.clear();
	const auto* Config = Document.GetConfig();
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
	NdoFile.clear();
	if (const auto* Source = Config->GetSource("ndof_events"))
	{
		NdoFile = Source->File;
	}
	else if (const auto* Source = Config->GetSource("ndof_neutral"))
	{
		NdoFile = Source->File;
	}
	for (const auto& Section : Config->sections())
	{
		const char* Name = Section.Name.c_str();
		const auto* Source = Config->GetSource(Name);
		if (Section.Name != "ndof_events" && (Referenced.contains(Section.Name) || Config->line_exist(Name, "ndof_mode") || Config->line_exist(Name, "f_number") || (Source && !NdoFile.empty() && Source->File == NdoFile)))
		{
			Profiles.emplace_back(Name);
		}
	}
}

void Editor::RefreshChanges()
{
	ChangedFiles = Document.GetChangedFiles();
	HasUnsaved = Document.IsDirty() || General != SavedGeneral;
	HasUnapplied = Document.GetTexts() != AppliedTexts || General != AppliedGeneral;
}

xr_string Editor::Effective(const char* Section, const char* Key, const char* Default) const
{
	const char* Value = Document.GetConfig()->r_string_nullable(Section, Key);
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
		const auto X = General.find("r4_ndof_focus_point_x");
		const auto Y = General.find("r4_ndof_focus_point_y");
		return X != General.end() && Y != General.end() ? X->second + ", " + Y->second : Definition.Default;
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

bool Editor::Commit(const char* Section, const Field& Definition, const char* Value)
{
	const auto Before = Document.GetTexts();
	if (!Document.Set(Section, Definition.Key, Value, Status))
	{
		Inputs.clear();
		if (IsPreview)
		{
			UpdatePreview();
		}
		return false;
	}
	if (!DofSettings::Validate(*Document.GetConfig(), Status))
	{
		xr_string Error;
		Document.Restore(Before, Error);
		Inputs.clear();
		if (IsPreview)
		{
			UpdatePreview();
		}
		return false;
	}
	if (HudSection == Section && StaticGroup == 10 && xr_string_view(Definition.Key).starts_with("zoom_dof_"))
	{
		SHudDofDefaults Defaults;
		Defaults.LoadFrom(*Document.GetConfig(), "gunslinger_base");
		Fvector Values = Defaults.Action.Request.Values;
		const char* Keys[] = {"zoom_dof_near", "zoom_dof_focus", "zoom_dof_far"};
		for (u32 Index = 0; Index < 3; ++Index)
		{
			Values[Index] = Document.GetConfig()->read_if_exists<float>(Section, Keys[Index], Values[Index]);
		}
		if (Values.x > Values.y || Values.y > Values.z)
		{
			xr_string Error;
			Document.Restore(Before, Error);
			Inputs.clear();
			Status = "PDA requires near <= focus <= far, including action defaults";
			if (IsPreview)
			{
				UpdatePreview();
			}
			return false;
		}
	}
	Inputs.clear();
	RefreshChanges();
	if (IsPreview)
	{
		UpdatePreview();
	}
	return true;
}

bool Editor::DrawInput(const char* Section, const Field& Definition, const xr_string& Value, xr_string& Edited, bool AllowStaticMode)
{
	const xr_string Id = xr_string(Section) + ":" + Definition.Key;
	auto [It, IsNew] = Inputs.try_emplace(Id);
	auto& Input = It->second;
	if (IsNew)
	{
		Input.Text = Value;
		sscanf(Value.c_str(), "%f , %f , %f , %f", &Input.Numbers[0], &Input.Numbers[1], &Input.Numbers[2], &Input.Numbers[3]);
	}
	ImGui::PushID(Definition.Key);
	bool CommitValue = false;
	if (Definition.Kind == FieldKind::Bool)
	{
		bool Enabled = IsTrue(Input.Text);
		if (ImGui::Checkbox(Definition.Label, &Enabled))
		{
			Input.Text = Enabled ? "true" : "false";
			CommitValue = true;
		}
	}
	else if (Definition.Kind == FieldKind::Mode || Definition.Kind == FieldKind::Resolution || Definition.Kind == FieldKind::Reference)
	{
		if (ImGui::BeginCombo(Definition.Label, Input.Text.c_str()))
		{
			auto OptionLambda = [&](const char* Name)
			{
				if (ImGui::Selectable(Name, Input.Text == Name))
				{
					Input.Text = Name;
					CommitValue = true;
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
		bool Changed = ImGui::DragScalarN(Definition.Label, ImGuiDataType_Float, Input.Numbers.data(), Count, 0.01f, &Definition.Min, &Definition.Max, "%.4f", ImGuiSliderFlags_AlwaysClamp);
		CommitValue = ImGui::IsItemDeactivatedAfterEdit();
		if (Changed)
		{
			bool IsValid = true;
			for (int Index = 0; Index < Count; ++Index)
			{
				IsValid = IsValid && _valid(Input.Numbers[Index]);
			}
			if (!IsValid)
			{
				Status = "DOF values must be finite numbers";
				sscanf(Value.c_str(), "%f , %f , %f , %f", &Input.Numbers[0], &Input.Numbers[1], &Input.Numbers[2], &Input.Numbers[3]);
				CommitValue = false;
			}
			Input.Text = Format(Input.Numbers.data(), Count);
			if (IsPreview)
			{
				UpdatePreview();
			}
		}
	}
	ImGui::SetItemTooltip("%s\nDrag to adjust; Ctrl+click for exact input.", Definition.Key);
	Edited = Input.Text;
	ImGui::PopID();
	return CommitValue;
}

bool Editor::DrawField(const char* Section, const Field& Definition, bool IsGeneral)
{
	ImGui::PushID(Section);
	xr_string Default = Definition.Default;
	if (!IsGeneral && Section == Profile && std::find_if(std::begin(CameraFields), std::end(CameraFields), [&](const Field& Entry)
														 { return !xr_strcmp(Entry.Key, Definition.Key); }) != std::end(CameraFields))
	{
		Default = CameraDefault(Definition);
	}
	xr_string Value = IsGeneral ? General.at(Definition.Key) : Effective(Section, Definition.Key, Default.c_str());
	bool Changed = false;
	if (!IsGeneral)
	{
		const auto* Source = Document.GetConfig()->GetSource(Section, Definition.Key);
		bool IsLocal = Source && Source->Section == Section;
		ImGui::PushID(Definition.Key);
		if (ImGui::Checkbox("##Local", &IsLocal))
		{
			if (IsLocal && Value.empty() && Definition.Kind == FieldKind::Reference)
			{
				Value = Profile;
			}
			Changed = Commit(Section, Definition, IsLocal ? Value.c_str() : nullptr);
		}
		ImGui::SetItemTooltip("Override this field locally. Clear to inherit.");
		ImGui::SameLine();
		ImGui::PopID();
		// The commit rebuilds provenance; obtain the source again.
		Source = Document.GetConfig()->GetSource(Section, Definition.Key);
		IsLocal = Source && Source->Section == Section;
		ImGui::BeginDisabled(!IsLocal);
		xr_string Edited;
		if (DrawInput(Section, Definition, Value, Edited))
		{
			Changed = Commit(Section, Definition, Edited.c_str());
		}
		ImGui::EndDisabled();
		Source = Document.GetConfig()->GetSource(Section, Definition.Key);
		if (Source)
		{
			ImGui::TextDisabled("  %s: [%s] %s:%u", Source->Section == Section ? "Local" : "Inherited", Source->Section.c_str(), Source->File.c_str(), Source->Line);
		}
		else
		{
			ImGui::TextDisabled("  Base/default: %s", Default.c_str());
		}
	}
	else
	{
		Field ActiveDefinition = Definition;
		if (const auto Range = GeneralRanges.find(Definition.Key); Range != GeneralRanges.end())
		{
			ActiveDefinition.Min = Range->second.first;
			ActiveDefinition.Max = Range->second.second;
		}
		xr_string Edited;
		if (DrawInput(Section, ActiveDefinition, Value, Edited, true))
		{
			if (Definition.Kind == FieldKind::Vector3)
			{
				const auto& Values = Inputs.at(xr_string(Section) + ":" + Definition.Key).Numbers;
				if (Values[0] > Values[1] || Values[1] > Values[2])
				{
					Status = "Expected near <= focus <= far";
					Inputs.clear();
					ImGui::PopID();
					return false;
				}
			}
			xr_string Canonical;
			if (!CanonicalGeneral(ActiveDefinition, Edited.c_str(), Canonical))
			{
				Status = "Invalid console DOF value or near/focus/far order";
				Inputs.clear();
				ImGui::PopID();
				return false;
			}
			General[Definition.Key] = Canonical;
			Inputs.clear();
			RefreshChanges();
			if (IsPreview)
			{
				UpdatePreview();
			}
			Changed = true;
		}
	}
	ImGui::PopID();
	return Changed;
}

void Editor::GeneralTab()
{
	ImGui::TextWrapped("Physical camera controls affect R4 autofocus/manual. Static controls affect classic DOF. Event profiles may override the base camera.");
	ImGui::TextWrapped("Changing r2_dof_enable requires a renderer restart to update the shader variant.");
	for (const auto& Definition : GeneralFields)
	{
		if (General.contains(Definition.Key))
		{
			if (!xr_strcmp(Definition.Key, "r4_ndof_mode"))
			{
				ImGui::SeparatorText("R4 camera / physical controls");
			}
			if (!xr_strcmp(Definition.Key, "r2_dof"))
			{
				ImGui::SeparatorText("Classic static controls");
			}
			DrawField("Console", Definition, true);
		}
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
			Inputs.clear();
			OpenPresets = true;
		}
		ImGui::PopID();
	}
}

void Editor::CameraEditor()
{
	if (Profile.empty() || !Document.GetConfig()->section_exist(Profile.c_str()))
	{
		return;
	}
	ImGui::Text("[%s]", Profile.c_str());
	const auto* Base = Document.GetConfig()->GetSource(Profile.c_str());
	if (Base)
	{
		ImGui::TextWrapped("Source: %s", Base->File.c_str());
		if (ParentSection != Profile)
		{
			xr_strcpy(ParentNames, sizeof(ParentNames), Base->Parents.c_str());
			ParentSection = Profile;
		}
		ImGui::InputText("Parents (comma-separated)", ParentNames, sizeof(ParentNames));
		ImGui::SameLine();
		if (ImGui::Button("Set parents"))
		{
			const auto Before = Document.GetTexts();
			if (Document.SetParents(Profile.c_str(), ParentNames, Status) && DofSettings::Validate(*Document.GetConfig(), Status))
			{
				Inputs.clear();
				RefreshChanges();
				if (IsPreview)
				{
					UpdatePreview();
				}
			}
			else
			{
				xr_string Error;
				Document.Restore(Before, Error);
			}
		}
		if (ImGui::BeginCombo("Parent preset", ParentNames))
		{
			auto ParentLambda = [&](const char* Name, const char* Label)
			{
				if (ImGui::Selectable(Label, !xr_strcmp(Name, ParentNames)))
				{
					const auto Before = Document.GetTexts();
					if (Document.SetParents(Profile.c_str(), Name, Status) && DofSettings::Validate(*Document.GetConfig(), Status))
					{
						xr_strcpy(ParentNames, sizeof(ParentNames), Name);
						Inputs.clear();
						RefreshChanges();
						if (IsPreview)
						{
							UpdatePreview();
						}
					}
					else
					{
						xr_string Error;
						Document.Restore(Before, Error);
					}
				}
			};
			ParentLambda("", "<base camera>");
			for (const auto& Name : Profiles)
			{
				if (Name != Profile)
				{
					ParentLambda(Name.c_str(), Name.c_str());
				}
			}
			ImGui::EndCombo();
		}
	}
	ImGui::TextWrapped("Checked fields are local overrides. Unchecked fields show inherited or base values. A disabled profile suppresses its event, including lower-priority bindings for that event.");
	for (const auto& Definition : CameraFields)
	{
		DrawField(Profile.c_str(), Definition);
	}
	ImGui::BeginDisabled(!psDeviceFlags.test(rsR4) || !IsLevelReady);
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
			if (!ProfileFilter[0] || Name.find(ProfileFilter) != xr_string::npos)
			{
				if (ImGui::Selectable(Name.c_str(), Name == Profile))
				{
					HasPendingStop = true;
					Profile = Name;
					Inputs.clear();
				}
			}
		}
		ImGui::EndCombo();
	}
	ImGui::InputText("New profile name", NewName, sizeof(NewName));
	ImGui::BeginDisabled(NdoFile.empty());
	if (ImGui::Button("Create"))
	{
		if (Document.AddSection(NdoFile.c_str(), NewName, "", Status))
		{
			Profile = NewName;
			Index();
			Inputs.clear();
			RefreshChanges();
		}
	}
	ImGui::SameLine();
	if (ImGui::Button("Duplicate"))
	{
		const auto Before = Document.GetTexts();
		bool Success = Document.AddSection(NdoFile.c_str(), NewName, "", Status);
		for (const auto& Definition : CameraFields)
		{
			if (Success && Document.GetConfig()->line_exist(Profile.c_str(), Definition.Key))
			{
				const xr_string Value = Effective(Profile.c_str(), Definition.Key, Definition.Default);
				Success = Document.Set(NewName, Definition.Key, Value.c_str(), Status);
			}
		}
		if (Success)
		{
			Profile = NewName;
			Index();
			Inputs.clear();
			RefreshChanges();
		}
		else
		{
			xr_string Error;
			Document.Restore(Before, Error);
		}
	}
	ImGui::EndDisabled();
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
		Inputs.clear();
		HasPendingStop = true;
	}
}

static SHudDofRequest ResolveStatic(const CInifile& Config, const char* Section, int Group, const char* Motion, float RayFocus);

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
		Inputs.clear();
	}
	if (StaticGroup == 4 && (!LtxDocument::IsIdentifier(Motion) || !xr_string_view(Motion).starts_with("anm_")))
	{
		ImGui::TextWrapped("Select an animation alias beginning with anm_.");
		return;
	}
	const auto Resolved = ResolveStatic(*Document.GetConfig(), HudSection.c_str(), StaticGroup, Motion, RayFocus);
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
		const bool IsAction = StaticGroup == 4 || StaticGroup == 8 || StaticGroup == 10;
		const xr_string DefaultPrefix = IsAction ? "default_action_dof" : "default_zoom_dof";
		const xr_string Near = Format(&Resolved.Values.x, 1);
		const xr_string Focus = Format(&Resolved.Values.y, 1);
		const xr_string Far = Format(&Resolved.Values.z, 1);
		FieldLambda(Prefix + "_near", "Near (m)", FieldKind::Float, StaticGroup == 1 ? Effective(HudSection.c_str(), "zoom_dof_near", Near.c_str()) : Near);
		FieldLambda(Prefix + "_focus", "Focus (m)", FieldKind::Float, StaticGroup == 1 ? Effective(HudSection.c_str(), "zoom_dof_focus", Focus.c_str()) : Focus);
		FieldLambda(Prefix + "_far", "Far (m)", FieldKind::Float, StaticGroup == 1 ? Effective(HudSection.c_str(), "zoom_dof_far", Far.c_str()) : Far);
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
	ImGui::BeginDisabled(!IsLevelReady || !General.contains("r2_dof"));
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
			const auto It = Inputs.find(HudSection + ":" + Key);
			CopiedGroup[Key] = It != Inputs.end() ? It->second.Text : Effective(HudSection.c_str(), Key.c_str(), GroupDefaults.at(Key).c_str());
		}
	}
	ImGui::SameLine();
	ImGui::BeginDisabled(CopiedGroup.empty());
	if (ImGui::Button("Paste group"))
	{
		const auto Before = Document.GetTexts();
		bool Success = true;
		for (const auto& [Key, Value] : CopiedGroup)
		{
			Success = Success && Document.Set(HudSection.c_str(), Key.c_str(), Value.c_str(), Status);
		}
		if (!Success || !DofSettings::Validate(*Document.GetConfig(), Status))
		{
			xr_string Error;
			Document.Restore(Before, Error);
		}
		Inputs.clear();
		RefreshChanges();
		if (IsPreview)
		{
			UpdatePreview();
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
					Inputs.clear();
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
		Inputs.clear();
	}
	ImGui::SameLine();
	if (ImGui::Button("Zone offsets"))
	{
		HasPendingStop = true;
		HudSection = "zone_pick_dof";
		StaticGroup = 9;
		Inputs.clear();
	}
	if (HudSection.empty() || !Document.GetConfig()->section_exist(HudSection.c_str()))
	{
		ImGui::TextWrapped("Select an existing HUD, item, scope or animator section.");
		return;
	}
	if (const auto* Source = Document.GetConfig()->GetSource(HudSection.c_str()))
	{
		ImGui::TextWrapped("Source: %s", Source->File.c_str());
	}
	if (ImGui::InputText("Animation alias", Motion, sizeof(Motion)))
	{
		HasPendingStop = true;
	}
	if (ImGui::BeginCombo("Existing aliases", Motion))
	{
		xr_hash_set<shared_str> Seen;
		for (const auto& Entry : Document.GetConfig()->r_section(HudSection.c_str()).Data)
		{
			const shared_str Alias = Entry.first.size() ? DofSettings::AnimationAlias(Entry.first.c_str()) : shared_str();
			if (Alias.size() && Seen.insert(Alias).second && ImGui::Selectable(Alias.c_str()))
			{
				xr_strcpy(Motion, sizeof(Motion), Alias.c_str());
				HasPendingStop = true;
				Inputs.clear();
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
		if (LtxDocument::IsIdentifier(Motion) && xr_string_view(Motion).starts_with("anm_"))
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
	xr_string Text;
	if (const auto* Source = Document.GetConfig()->GetSource(Section))
	{
		Text = "; Target: " + Source->File + "\r\n";
	}
	Text += xr_string("[") + Section + "]\r\n";
	for (const auto& Entry : Document.GetConfig()->r_section(Section).Data)
	{
		if (Entry.first.size() && (IsCamera || DofSettings::IsDofKey(Entry.first.c_str())))
		{
			Text += xr_string(Entry.first.c_str()) + " = " + (Entry.second.size() ? Entry.second.c_str() : "") + "\r\n";
		}
	}
	return Text;
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

SHudDofRequest Editor::StaticPreview() const
{
	SHudDofRequest Result = ResolveStatic(*Document.GetConfig(), PreviewSection.c_str(), PreviewGroup, Motion, RayFocus);
	const xr_string Prefix = PreviewGroup == 0 || PreviewGroup == 10 ? "zoom_dof" : PreviewGroup == 1 ? "alter_zoom_dof"
																				: PreviewGroup == 2	  ? "gl_dof"
																				: PreviewGroup == 3	  ? "lens_dof"
																				: PreviewGroup == 4	  ? xr_string("dof_") + Motion
																				: PreviewGroup == 7	  ? "default_zoom_dof"
																									  : "inventory_dof";
	if (PreviewGroup == 5 || PreviewGroup == 6)
	{
		const xr_string Key = PreviewGroup == 5 ? "reload_dof" : "dof";
		if (const auto It = Inputs.find(PreviewSection + ":" + Key); It != Inputs.end())
		{
			for (u32 Index = 0; Index < 3; ++Index)
			{
				Result.Values[Index] = It->second.Numbers[Index];
			}
		}
	}
	else if (PreviewGroup == 9)
	{
		if (const auto It = Inputs.find(PreviewSection + ":near"); It != Inputs.end())
		{
			Result.Values.x = RayFocus + It->second.Numbers[0];
		}
		if (const auto It = Inputs.find(PreviewSection + ":far"); It != Inputs.end())
		{
			Result.Values.z = RayFocus + It->second.Numbers[0];
		}
	}
	else
	{
		const char* Suffixes[] = {"near", "focus", "far"};
		for (u32 Index = 0; Index < 3; ++Index)
		{
			if (const auto It = Inputs.find(PreviewSection + ":" + Prefix + "_" + Suffixes[Index]); It != Inputs.end())
			{
				Result.Values[Index] = It->second.Numbers[0];
			}
		}
	}
	const xr_string SpeedKey = PreviewGroup == 4 ? xr_string("dof_speed_in_") + Motion : PreviewGroup == 7 ? "default_dof_speed_in"
																					 : PreviewGroup == 8   ? "inventory_dof_speed_in"
																					 : PreviewGroup == 9   ? "default_dof_speed"
																					 : PreviewGroup == 1   ? "alter_zoom_in_dof_speed"
																					 : PreviewGroup == 2   ? "gl_in_dof_speed"
																										   : "zoom_in_dof_speed";
	if (const auto It = Inputs.find(PreviewSection + ":" + SpeedKey); It != Inputs.end())
	{
		Result.SpeedIn = It->second.Numbers[0];
	}
	return Result;
}

void Editor::StartPreview(bool IsCamera)
{
	HasPendingStop = false;
	IsPreview = true;
	IsPreviewCamera = IsCamera;
	PreviewSection = IsCamera ? Profile : HudSection;
	PreviewGroup = StaticGroup;
	UpdatePreview();
}

void Editor::UpdatePreview()
{
	if (IsPreviewCamera)
	{
		PreviewCamera = {};
		PreviewCamera.LoadFrom(*Document.GetConfig(), PreviewSection.c_str());
		// Fill omitted fields from the working base camera, then apply the explicit profile.
		SDofCameraSettings Base;
		auto FloatLambda = [&](const char* Name, float& Value)
		{
			if (const auto It = General.find(Name); It != General.end())
			{
				Value = static_cast<float>(atof(It->second.c_str()));
			}
		};
		FloatLambda("r4_ndof_f_number", Base.FNumber);
		FloatLambda("r4_ndof_focal_length", Base.FocalLength);
		FloatLambda("r4_ndof_focus_distance", Base.FocusDistance);
		FloatLambda("r4_ndof_focus_time", Base.FocusTime);
		FloatLambda("r4_ndof_transition_time", Base.TransitionTime);
		FloatLambda("r4_ndof_focus_point_x", Base.FocusPoint.x);
		FloatLambda("r4_ndof_focus_point_y", Base.FocusPoint.y);
		if (const auto It = General.find("r4_ndof_manual_focal_length"); It != General.end())
		{
			Base.ManualFocalLength = IsTrue(It->second);
		}
		if (const auto It = General.find("r4_ndof_mode"); It != General.end())
		{
			Base.Mode = It->second == "manual" ? EDofMode::Manual : EDofMode::Autofocus;
		}
		PreviewCamera.Apply(Base);
		PreviewCamera.Camera = Base;
		PreviewCamera.Fields.set(u8(255), true);
		for (const auto& Definition : CameraFields)
		{
			const auto It = Inputs.find(PreviewSection + ":" + Definition.Key);
			if (It == Inputs.end())
			{
				continue;
			}
			const auto& Values = It->second.Numbers;
			if (!xr_strcmp(Definition.Key, "f_number"))
			{
				PreviewCamera.Camera.FNumber = Values[0];
			}
			else if (!xr_strcmp(Definition.Key, "focal_length"))
			{
				PreviewCamera.Camera.FocalLength = Values[0];
			}
			else if (!xr_strcmp(Definition.Key, "focus_distance"))
			{
				PreviewCamera.Camera.FocusDistance = Values[0];
			}
			else if (!xr_strcmp(Definition.Key, "focus_time"))
			{
				PreviewCamera.Camera.FocusTime = Values[0];
			}
			else if (!xr_strcmp(Definition.Key, "transition_time"))
			{
				PreviewCamera.Camera.TransitionTime = Values[0];
			}
			else if (!xr_strcmp(Definition.Key, "focus_point"))
			{
				PreviewCamera.Camera.FocusPoint.set(Values[0], Values[1]);
			}
		}
	}
	else
	{
		PreviewRequest = StaticPreview();
		if (PreviewRequest.Values.x > PreviewRequest.Values.y || PreviewRequest.Values.y > PreviewRequest.Values.z)
		{
			Status = "Preview requires near <= focus <= far";
			return;
		}
	}
	HasPendingPreview = true;
}

void Editor::OnFrame(CDofManager& Manager)
{
	if ((!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_DofSetup)] || !CImGuiManager::Instance().IsGUIRendering()) && Pending == Operation::None)
	{
		Manager.StopPreview();
		IsPreview = HasPendingPreview = HasPendingStop = false;
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
	IsLevelReady = g_pGameLevel && g_pGameLevel->bReady;
	if (auto* Command = Console->GetCommand("r2_dof_enable"))
	{
		IConsole_Command::TStatus Value = {};
		Command->Status(Value);
		IsMasterEnabled = IsTrue(Value);
	}
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
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_DofSetup)] || !CImGuiManager::Instance().IsGUIRendering() || !IsLevelReady || (WasLevelReady && !Manager.IsPreviewing() && IsPreview && !HasPendingPreview))
	{
		HasPendingStop = true;
	}
	WasLevelReady = IsLevelReady;
	if (HasPendingStop)
	{
		Manager.StopPreview();
		IsPreview = HasPendingPreview = HasPendingStop = false;
	}
	if (Pending != Operation::None)
	{
		const Operation Action = Pending;
		Pending = Operation::None;
		Manager.StopPreview();
		IsPreview = HasPendingPreview = false;
		if (Action == Operation::Reload)
		{
			auto SavedValues = SavedGeneral;
			xr_string ReadError;
			ReadSavedGeneral(SavedValues, ReadError);
			if (Load())
			{
				auto Applied = Document.CreateConfig();
				if (Applied && Applied->GetSourceError().empty())
				{
					DofSettings::Apply(std::move(Applied));
					ApplyGeneral(SavedValues);
					AppliedGeneral = SavedGeneral = General;
					RefreshChanges();
					Status = ReadError.empty() ? "Reloaded DOF configuration and saved base camera" : "Reloaded DOF configuration; restored the previous saved base snapshot. " + ReadError;
				}
			}
		}
		else if (Action == Operation::Revert)
		{
			if (Document.Restore(AppliedTexts, Status))
			{
				General = AppliedGeneral;
				ParentSection.clear();
				Inputs.clear();
				Index();
				RefreshChanges();
			}
		}
		else if (DofSettings::Validate(*Document.GetConfig(), Status))
		{
			auto Config = Document.CreateConfig();
			if (Config && Config->GetSourceError().empty() && (Action != Operation::Save || Document.Save(Status)))
			{
				const xr_string WriteWarning = Status;
				DofSettings::Apply(std::move(Config));
				ApplyGeneral(General);
				AppliedTexts = Document.GetTexts();
				Inputs.clear();
				AppliedGeneral = General;
				if (Action == Operation::Save)
				{
					Console->ExecuteCommand("cfg_save", false);
					FS.file_update(UserConfig.c_str());
					auto Verified = General;
					xr_string SaveError;
					if (ReadSavedGeneral(Verified, SaveError, true) && Verified == General)
					{
						SavedGeneral = General;
						Status = "Saved and verified source LTX and user config; applied DOF settings";
					}
					else
					{
						Status = "Source LTX saved and applied, but user-config save could not be verified. " + SaveError;
					}
				}
				else
				{
					Status = "Applied DOF settings without saving";
				}
				if (!WriteWarning.empty())
				{
					Status += "\n" + WriteWarning;
				}
				RefreshChanges();
			}
		}
	}
	if (HasPendingPreview && IsLevelReady)
	{
		if (IsPreviewCamera && psDeviceFlags.test(rsR4))
		{
			Manager.SetPreview(PreviewCamera);
		}
		else if (!IsPreviewCamera)
		{
			Manager.SetPreview(PreviewRequest);
		}
		HasPendingPreview = false;
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
	ImGui::Text("%s | %s | event: %s", psDeviceFlags.test(rsR4) ? "R4" : psDeviceFlags.test(rsR2) ? "R2"
																								  : "R1",
				Mode.c_str(),
				CDofManager::EventName(ActiveEvent));
	ImGui::TextWrapped("Profile: %s | binding: %s", ActiveProfile.c_str(), ActiveBinding.c_str());
	ImGui::Text("%s | %s", HasUnsaved ? "Unsaved changes" : "Saved", HasUnapplied ? "Working copy differs from applied" : "Applied");
	if (IsPreview)
	{
		ImGui::TextColored(ImVec4(1, 0.8f, 0.2f, 1), "FORCED PREVIEW: [%s]", PreviewSection.c_str());
		ImGui::SameLine();
		if (ImGui::Button("Stop preview"))
		{
			HasPendingStop = true;
		}
	}
	if (ImGui::Button("Apply"))
	{
		Pending = Operation::Apply;
	}
	ImGui::SameLine();
	if (ImGui::Button("Revert changes"))
	{
		Pending = Operation::Revert;
	}
	ImGui::SameLine();
	if (ImGui::Button("Reload from disk"))
	{
		if (HasUnsaved)
		{
			ImGui::OpenPopup("Discard unsaved DOF changes?");
		}
		else
		{
			Pending = Operation::Reload;
		}
	}
	ImGui::SameLine();
	if (ImGui::Button("Save"))
	{
		CanWriteSources = Document.CanSave(SaveIssue);
		ImGui::OpenPopup("Save DOF source files");
	}
	if (ImGui::BeginPopupModal("Discard unsaved DOF changes?", nullptr, ImGuiWindowFlags_AlwaysAutoResize))
	{
		ImGui::TextUnformatted("Reload replaces the working and applied DOF configuration.");
		if (ImGui::Button("Discard and reload"))
		{
			Pending = Operation::Reload;
			ImGui::CloseCurrentPopup();
		}
		ImGui::SameLine();
		if (ImGui::Button("Cancel"))
		{
			ImGui::CloseCurrentPopup();
		}
		ImGui::EndPopup();
	}
	if (ImGui::BeginPopupModal("Save DOF source files", nullptr, ImGuiWindowFlags_AlwaysAutoResize))
	{
		ImGui::TextUnformatted("Write only edited keys in these source files; keep .bak backups:");
		for (const auto& File : ChangedFiles)
		{
			ImGui::BulletText("%s", File.c_str());
		}
		ImGui::BulletText("%s (cfg_save)", UserConfig.c_str());
		if (!CanWriteSources)
		{
			ImGui::TextWrapped("%s", SaveIssue.c_str());
		}
		ImGui::BeginDisabled(!CanWriteSources);
		if (ImGui::Button("Save and apply"))
		{
			Pending = Operation::Save;
			ImGui::CloseCurrentPopup();
		}
		ImGui::EndDisabled();
		ImGui::SameLine();
		if (ImGui::Button("Copy LTX"))
		{
			ImGui::SetClipboardText(Document.ExportChanges().c_str());
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
		ImGui::TextDisabled("DOF master is off. Enable it in General, Apply, then restart the renderer to update shaders.");
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
