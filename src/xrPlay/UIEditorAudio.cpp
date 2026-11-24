#include "../xrEngine/stdafx.h"
#include "../xrEngine/XR_IOConsole.h"
#include "../xrSound/New/SoundMixer.h"
#include "../xrSound/New/SoundMixerInternal.h"
#include "UIEditorMain.h"
#include <imgui.h>

using namespace XRay::Sound;

enum EAudioSoundFlag : u8
{
	AudioLoaded = 1 << 0,
	AudioPlaying = 1 << 1,
	AudioStopped = 1 << 2,
	AudioSimulated = 1 << 3,
	AudioDelayed = 1 << 4
};

static const char* AudioFilterNames[] = {"All", "Loaded", "Playing", "Stopped", "Simulated", "Delayed"};
static const u8 AudioFilterMasks[] = {0, AudioLoaded, AudioPlaying, AudioStopped, AudioSimulated, AudioDelayed};

constexpr u32 AudioRowBandCount = 64;
constexpr u32 AudioDetailBandCount = 240;
// Left, right, mid (SND_SCOPE_MID)
constexpr u32 AudioTraceCount = SND_CHANNEL_COUNT + 1;
constexpr float AudioAnalyzerFloorDb = -150.0f;
constexpr float AudioAnalyzerMinHz = 20.0f;
constexpr float AudioAnalyzerMaxHz = 20000.0f;

// Smoothed analyzer curve of one channel, in dB
struct AudioAnalyzerTrace
{
	float Level[AudioDetailBandCount] = {};
	float Peak[AudioDetailBandCount] = {};
	bool IsValid = false;
};

struct AudioAnalyzerSettings
{
	// AudioAnalyzerChannelNames: L / R, Mid, Left, Right
	int Channels = 0;
	int Resolution = 1;
	int Speed = 1;
	int Range = 1;
	int Tilt = 2;
	bool PeakHold = true;
};

struct AudioEditorState
{
	xr_vector<shared_str> DiskSounds;
	xr_hash_set<shared_str> DiskSet;
	xr_vector<shared_str> LoadedSounds;
	xr_hash_map<shared_str, u8> Flags;
	xr_vector<shared_str> Rows;
	ImGuiTextFilter NameFilter;
	int Filter = 0;
	bool IsFrozen = false;
	xr_hash_set<shared_str> Frozen;
	shared_str Selected;
	sound_config Config = {};
	bool IsConfigValid = false;
	bool IsSelectionChanged = false;
	bool IsPicked = false;
	const char* SaveStatus = "";
	bool DrawSounds = false;
	bool DrawZones = false;
	u32 SelectedBus = 0;
	float BusMeters[SND_BUS_COUNT] = {};
	char NewBusName[64] = {};
	const char* BusStatus = "";
	bool GraphShowZones = false;
	bool GraphShowSends = true;
	bool GraphShowSpectrum = true;
	bool GraphShowReleased = true;
	AudioAnalyzerSettings Analyzer;
	AudioAnalyzerTrace RowTraces[SND_BUS_COUNT][AudioTraceCount];
	AudioAnalyzerTrace DetailTraces[AudioTraceCount];
	float GraphMeters[SND_BUS_COUNT][SND_CHANNEL_COUNT] = {};
	u32 DetailBus = 0;
	xr_vector<SoundDynamicBusInfo> DynamicBuses;
	float LoadHistory[120] = {};
	u32 HistoryIdx = 0;
	ref_sound* Preview = nullptr;
};

static AudioEditorState GAudioEditor;

static void Audio_StopPreview()
{
	if (GAudioEditor.Preview != nullptr)
	{
		GAudioEditor.Preview->stop();
		delete GAudioEditor.Preview;
		GAudioEditor.Preview = nullptr;
	}
}

static shared_str Audio_Normalize(const char* Name)
{
	string_path Result;
	xr_strcpy(Result, Name);
	xr_strlwr(Result);
	if (char* Ext = strext(Result))
	{
		*Ext = 0;
	}

	return Result;
}

static void Audio_ScanDisk()
{
	FS_FileSet Files;
	FS.file_list(Files, _game_sounds_, FS_ListFiles, "*.ogg");

	GAudioEditor.DiskSounds.clear();
	GAudioEditor.DiskSet.clear();
	for (const FS_File& File : Files)
	{
		shared_str Name = Audio_Normalize(File.name.c_str());
		GAudioEditor.DiskSounds.push_back(Name);
		GAudioEditor.DiskSet.insert(Name);
	}

	std::sort(GAudioEditor.DiskSounds.begin(), GAudioEditor.DiskSounds.end(), [](const shared_str& Left, const shared_str& Right) { return xr_strcmp(Left, Right) < 0; });
}

static u8 Audio_SlotFlag(const sound_slot_state& Slot)
{
	switch (Slot.state)
	{
	case Mixer::State::Playing:
	{
		bool IsSpatial = (Slot.flags & (u16)Mixer::Flags::Spatial) != 0;
		float Distance = Device.vCameraPosition.distance_to(Slot.parameters[(u32)Mixer::ParameterId::Position]);
		return (IsSpatial && Distance > Slot.parameters[(u32)Mixer::ParameterId::DistanceRange].y) ? AudioSimulated : AudioPlaying;
	}
	case Mixer::State::Delay:
		return AudioDelayed;
	default:
		return AudioStopped;
	}
}

static void Audio_UpdateFlags()
{
	GAudioEditor.Flags.clear();

	Mixer::GetLoadedSources(GAudioEditor.LoadedSounds);
	for (const shared_str& Name : GAudioEditor.LoadedSounds)
	{
		GAudioEditor.Flags[Name] |= AudioLoaded;
	}

	xrSRWLockGuard Guard(Mixer::GetUpdateMutex(), true);
	const xr_vector<sound_slot_state>& Slots = Mixer::GetSlots();
	for (u32 SlotIdx = 0; SlotIdx < Slots.size(); SlotIdx++)
	{
		if (!Slots[SlotIdx].sound_name.empty())
		{
			GAudioEditor.Flags[Audio_Normalize(Slots[SlotIdx].sound_name.c_str())] |= Audio_SlotFlag(Slots[SlotIdx]);
		}
	}
}

static void Audio_BuildRows()
{
	GAudioEditor.Rows.clear();
	u8 Mask = AudioFilterMasks[GAudioEditor.Filter];
	auto AddRow = [Mask](const shared_str& Name)
	{
		auto Found = GAudioEditor.Flags.find(Name);
		u8 Flags = Found != GAudioEditor.Flags.end() ? Found->second : 0;
		bool IsMatched = Mask == 0 || (Flags & Mask) != 0;
		if (IsMatched && GAudioEditor.IsFrozen)
		{
			GAudioEditor.Frozen.insert(Name);
		}

		if ((IsMatched || GAudioEditor.Frozen.contains(Name)) && GAudioEditor.NameFilter.PassFilter(Name.c_str()))
		{
			GAudioEditor.Rows.push_back(Name);
		}
	};

	for (const shared_str& Name : GAudioEditor.DiskSounds)
	{
		AddRow(Name);
	}

	for (const auto& [Name, Flags] : GAudioEditor.Flags)
	{
		if (!GAudioEditor.DiskSet.contains(Name))
		{
			AddRow(Name);
		}
	}
}

static void Audio_Select(const shared_str& Name)
{
	GAudioEditor.Selected = Name;
	GAudioEditor.IsConfigValid = Mixer::GetSoundConfig(Name.c_str(), &GAudioEditor.Config);
	GAudioEditor.IsSelectionChanged = true;
	GAudioEditor.SaveStatus = "";
}

static const char* Audio_StateName(u8 Flags)
{
	if (Flags & AudioPlaying)
	{
		return "Playing";
	}
	if (Flags & AudioSimulated)
	{
		return "Simulated";
	}
	if (Flags & AudioDelayed)
	{
		return "Delayed";
	}
	if (Flags & AudioStopped)
	{
		return "Stopped";
	}

	return (Flags & AudioLoaded) ? "Loaded" : "";
}

static bool Audio_Project(const Fvector& Position, ImVec2& Out)
{
	Fvector4 Clip;
	Device.mFullTransform.transform(Clip, Position);
	if (Clip.w <= EPS_S)
	{
		return false;
	}

	const ImGuiViewport* Viewport = ImGui::GetMainViewport();
	Out.x = Viewport->Pos.x + (Clip.x / Clip.w * 0.5f + 0.5f) * Viewport->Size.x;
	Out.y = Viewport->Pos.y + (0.5f - Clip.y / Clip.w * 0.5f) * Viewport->Size.y;
	return true;
}

static void Audio_DrawZones()
{
	ImDrawList* DrawList = ImGui::GetBackgroundDrawList();
	for (const sound_zone_params& Zone : Mixer::GetZones())
	{
		ImVec2 Corners[8];
		bool IsVisible = true;
		for (u32 Corner = 0; Corner < 8 && IsVisible; Corner++)
		{
			Fvector Position = {(Corner & 1) ? Zone.max.x : Zone.min.x, (Corner & 2) ? Zone.max.y : Zone.min.y, (Corner & 4) ? Zone.max.z : Zone.min.z};
			IsVisible = Audio_Project(Position, Corners[Corner]);
		}

		if (!IsVisible)
		{
			continue;
		}

		static const u8 Edges[12][2] = {{0, 1}, {2, 3}, {4, 5}, {6, 7}, {0, 2}, {1, 3}, {4, 6}, {5, 7}, {0, 4}, {1, 5}, {2, 6}, {3, 7}};
		for (const auto& Edge : Edges)
		{
			DrawList->AddLine(Corners[Edge[0]], Corners[Edge[1]], IM_COL32(0, 255, 255, 160));
		}
	}
}

static void Audio_DrawSounds()
{
	static const ImU32 Colors[] = {IM_COL32(45, 166, 69, 255), IM_COL32(117, 45, 166, 255), IM_COL32(252, 157, 3, 255), IM_COL32(128, 128, 128, 255)};

	ImDrawList* DrawList = ImGui::GetBackgroundDrawList();
	ImVec2 Mouse = ImGui::GetMousePos();
	bool IsPicking = ImGui::IsMouseClicked(ImGuiMouseButton_Left) && !ImGui::GetIO().WantCaptureMouse;
	float PickDistance = 12.0f * 12.0f;
	shared_str Picked;

	xrSRWLockGuard Guard(Mixer::GetUpdateMutex(), true);
	const xr_vector<sound_slot_state>& Slots = Mixer::GetSlots();
	for (u32 SlotIdx = 0; SlotIdx < Slots.size(); SlotIdx++)
	{
		const sound_slot_state& Slot = Slots[SlotIdx];
		if (Slot.sound_name.empty() || Slot.state == Mixer::State::Stopped || (Slot.flags & (u16)Mixer::Flags::Spatial) == 0)
		{
			continue;
		}

		ImVec2 Screen;
		if (!Audio_Project(Slot.parameters[(u32)Mixer::ParameterId::Position], Screen))
		{
			continue;
		}

		u8 Flag = Audio_SlotFlag(Slot);
		u32 ColorIdx = Flag == AudioPlaying ? 0 : Flag == AudioSimulated ? 1 : Flag == AudioDelayed ? 2 : 3;
		shared_str Name = Audio_Normalize(Slot.sound_name.c_str());
		bool IsSelected = Name == GAudioEditor.Selected;
		DrawList->AddCircle(Screen, IsSelected ? 9.0f : 6.0f, Colors[ColorIdx], 0, IsSelected ? 3.0f : 1.5f);

		float DX = Screen.x - Mouse.x;
		float DY = Screen.y - Mouse.y;
		float Distance = DX * DX + DY * DY;
		if (IsSelected || Distance < 12.0f * 12.0f)
		{
			DrawList->AddText(ImVec2(Screen.x + 10.0f, Screen.y - 7.0f), Colors[ColorIdx], Name.c_str());
		}

		if (IsPicking && Distance < PickDistance)
		{
			PickDistance = Distance;
			Picked = Name;
		}
	}

	if (Picked.size() != 0)
	{
		Audio_Select(Picked);
		GAudioEditor.IsPicked = true;
	}
}

// A frequency parameter: named so, or with a range up into the kHz
static bool Audio_IsFrequencyParam(const SoundEffectParam& Param)
{
	const char* Names[] = {"cut", "freq", "hz", "crossover"};
	for (const char* Name : Names)
	{
		if (strstr(Param.Name, Name) != nullptr)
		{
			return true;
		}
	}

	return Param.Min >= 10.0f && Param.Max >= 1000.0f;
}

// Slider for a shader or effect parameter: plain numbers (no exponent), frequencies in Hz on a log scale
static bool Audio_ParamSlider(const SoundEffectParam& Param, float* Value)
{
	const float Range = Param.Max - Param.Min;
	if (Audio_IsFrequencyParam(Param))
	{
		const char* Format = *Value < 100.0f ? "%.1f Hz" : "%.0f Hz";
		return ImGui::SliderFloat(Param.Name, Value, Param.Min, Param.Max, Format, Param.Min > 0.0f ? ImGuiSliderFlags_Logarithmic : ImGuiSliderFlags_None);
	}

	const char* Format = Range >= 1000.0f ? "%.0f" : Range >= 100.0f ? "%.1f" : Range >= 10.0f ? "%.2f" : Range > 1.0f ? "%.3f" : "%.4f";
	return ImGui::SliderFloat(Param.Name, Value, Param.Min, Param.Max, Format);
}

// Edit requested by a shader chain widget. Mixer calls take play_lock, so they run only after the widget
// released GetUpdateMutex: PlayNoFeedback takes play_lock and then update_lock, the reverse order would deadlock
struct AudioShaderEdit
{
	enum class EKind : u8
	{
		None,
		Add,
		Remove,
		Clear,
		SetParam
	};

	EKind Kind = EKind::None;
	bool IsBus = false;
	u32 Target = 0;
	SoundShaderType Type = SoundShaderType::None;
	// Points into the static class info of the shader
	const char* Param = nullptr;
	float Value = 0.0f;
	// Remove: which kind of bus owner to drop
	bool SpatialOnly = false;
};

static void Audio_ApplyShaderEdit(const AudioShaderEdit& Edit)
{
	switch (Edit.Kind)
	{
	case AudioShaderEdit::EKind::Add:
		Edit.IsBus ? Mixer::AddBusShader(Edit.Target, Edit.Type) : Mixer::AddShader(Edit.Target, Edit.Type);
		break;
	case AudioShaderEdit::EKind::Remove:
		Edit.IsBus ? Mixer::RemoveBusShader(Edit.Target, Edit.Type, Edit.SpatialOnly) : Mixer::RemoveShader(Edit.Target, Edit.Type);
		break;
	case AudioShaderEdit::EKind::Clear:
		Edit.IsBus ? Mixer::ClearBusShaders(Edit.Target) : Mixer::ClearShaders(Edit.Target);
		break;
	case AudioShaderEdit::EKind::SetParam:
		Edit.IsBus ? Mixer::SetBusShaderParam(Edit.Target, Edit.Type, Edit.Param, Edit.Value) : Mixer::SetShaderParam(Edit.Target, Edit.Type, Edit.Param, Edit.Value);
		break;
	default:
		break;
	}
}

// Shader chain of a voice slot or a bus, drawn under GetUpdateMutex. At most one edit per frame is returned in Edit,
// the caller applies it with Audio_ApplyShaderEdit after releasing the lock
static void Audio_RenderShaderChain(const SoundShaderChain* Chain, bool IsBus, u32 Target, AudioShaderEdit& Edit)
{
	auto Request = [&](AudioShaderEdit::EKind Kind, SoundShaderType Type, const char* Param = nullptr, float Value = 0.0f)
	{
		if (Edit.Kind == AudioShaderEdit::EKind::None)
		{
			Edit = {Kind, IsBus, Target, Type, Param, Value};
		}
	};

	const u32 Count = Chain != nullptr ? Chain->GetCount() : 0;
	for (u32 Index = 0; Index < Count; Index++)
	{
		const ISoundShader* Shader = Chain->Get(Index);
		const SoundShaderType Type = Shader->GetType();
		ImGui::PushID((int)Type);
		const u32 Owners = Chain->GetOwnerCount(Index);
		const bool IsSpatialOnly = Chain->IsSpatialOnly(Index);
		if (ImGui::SmallButton("X") && Edit.Kind == AudioShaderEdit::EKind::None)
		{
			Request(AudioShaderEdit::EKind::Remove, Type);
			Edit.SpatialOnly = IsSpatialOnly;
		}
		ImGui::SetItemTooltip(Owners > 1 ? "Drops one owner, the shader stays while others hold it" : "Remove");

		ImGui::SameLine();
		const bool IsOpen = ImGui::TreeNodeEx("##Shader", ImGuiTreeNodeFlags_DefaultOpen, "%s", Shader::GetName(Type));
		if (Owners > 1 || IsSpatialOnly)
		{
			ImGui::SameLine();
			ImGui::TextDisabled("%s%s", IsSpatialOnly ? "3D only " : "", Owners > 1 ? "(shared)" : "");
			if (Owners > 1)
			{
				ImGui::SetItemTooltip("%u owners", Owners);
			}
		}

		if (IsOpen)
		{
			for (u32 ParamIdx = 0; ParamIdx < Shader->GetParamCount(); ParamIdx++)
			{
				const SoundEffectParam* Param = Shader->GetParamDesc(ParamIdx);
				float Value = Shader->GetParam(ParamIdx);
				if (Audio_ParamSlider(*Param, &Value))
				{
					Request(AudioShaderEdit::EKind::SetParam, Type, Param->Name, Value);
				}

				if (ImGui::BeginPopupContextItem())
				{
					if (ImGui::MenuItem("Reset to default"))
					{
						Request(AudioShaderEdit::EKind::SetParam, Type, Param->Name, Param->Default);
					}
					ImGui::EndPopup();
				}
			}

			ImGui::TreePop();
		}

		ImGui::PopID();
	}

	if (Count < SND_SHADER_CHAIN_COUNT && ImGui::BeginCombo("##AddShader", "Add shader...", ImGuiComboFlags_NoArrowButton))
	{
		for (u32 Type = 1; Type < (u32)SoundShaderType::Count; Type++)
		{
			const bool IsPresent = Chain != nullptr && Chain->Find((SoundShaderType)Type) != nullptr;
			if (!IsPresent && ImGui::Selectable(Shader::GetName((SoundShaderType)Type)))
			{
				Request(AudioShaderEdit::EKind::Add, (SoundShaderType)Type);
			}
		}

		ImGui::EndCombo();
	}

	if (Count > 0)
	{
		ImGui::SameLine();
		if (ImGui::SmallButton("Clear"))
		{
			Request(AudioShaderEdit::EKind::Clear, SoundShaderType::None);
		}
	}
}

// Shaders of the sound config; true when the preset changed
static bool Audio_RenderShaderPreset(SoundShaderPreset& Preset)
{
	bool IsChanged = false;
	int RemoveIdx = -1;
	for (u32 Index = 0; Index < Preset.Count; Index++)
	{
		const SoundShaderType Type = (SoundShaderType)Preset.Types[Index];
		const Shader::ClassInfo* Info = Shader::GetClass(Type);
		ImGui::PushID((int)Index);
		if (ImGui::SmallButton("X"))
		{
			RemoveIdx = (int)Index;
		}

		ImGui::SameLine();
		if (Info == nullptr)
		{
			ImGui::TextDisabled("unknown shader");
		}
		else if (ImGui::TreeNodeEx("##PresetShader", ImGuiTreeNodeFlags_DefaultOpen, "%s", Info->Name))
		{
			for (u32 ParamIdx = 0; ParamIdx < Info->ParamCount; ParamIdx++)
			{
				const SoundEffectParam& Param = Info->Params[ParamIdx];
				float& Value = Preset.Params[Index][ParamIdx];
				IsChanged |= Audio_ParamSlider(Param, &Value);
				if (ImGui::BeginPopupContextItem())
				{
					if (ImGui::MenuItem("Reset to default"))
					{
						Value = Param.Default;
						IsChanged = true;
					}
					ImGui::EndPopup();
				}
			}

			ImGui::TreePop();
		}

		ImGui::PopID();
	}

	if (Preset.Count < SND_SHADER_CHAIN_COUNT && ImGui::BeginCombo("##AddPresetShader", "Add shader...", ImGuiComboFlags_NoArrowButton))
	{
		for (u32 Type = 1; Type < (u32)SoundShaderType::Count; Type++)
		{
			bool IsPresent = false;
			for (u32 Index = 0; Index < Preset.Count; Index++)
			{
				IsPresent |= Preset.Types[Index] == (u8)Type;
			}

			if (!IsPresent && ImGui::Selectable(Shader::GetName((SoundShaderType)Type)))
			{
				IsChanged |= Shader::PresetAdd(Preset, (SoundShaderType)Type);
			}
		}

		ImGui::EndCombo();
	}

	if (RemoveIdx >= 0)
	{
		Shader::PresetRemove(Preset, (u32)RemoveIdx);
		IsChanged = true;
	}

	return IsChanged;
}

static void Audio_RenderDetails()
{
	if (GAudioEditor.Selected.size() == 0)
	{
		ImGui::TextDisabled("Select a sound in the list, or click a marker with 'Draw sounds' enabled.");
		return;
	}

	ImGui::TextUnformatted(GAudioEditor.Selected.c_str());
	if (!GAudioEditor.IsConfigValid)
	{
		ImGui::TextDisabled("File not found");
		return;
	}

	sound_config& Config = GAudioEditor.Config;
	ImGui::TextDisabled("Source: %s", Config.file.size() != 0 ? Config.file.c_str() : "ogg comment");
	ImGui::Separator();

	bool IsChanged = false;
	IsChanged |= ImGui::SliderFloat("Volume", &Config.volume, 0.0f, 4.0f);
	IsChanged |= ImGui::DragFloat("Min distance", &Config.min_distance, 0.1f, 0.0f, 1000.0f);
	IsChanged |= ImGui::DragFloat("Max distance", &Config.max_distance, 0.5f, 0.0f, 5000.0f);
	IsChanged |= ImGui::DragFloat("AI distance", &Config.max_ai_distance, 0.5f, 0.0f, 5000.0f);
	IsChanged |= ImGui::InputScalar("AI type", ImGuiDataType_U32, &Config.game_type, nullptr, nullptr, "%08X", ImGuiInputTextFlags_CharsHexadecimal);

	SoundBus* Buses = Mixer::GetBuses();
	const char* BusName = (Config.bus != 0 && Config.bus <= SND_BUS_COUNT && Buses[Config.bus - 1].IsUsed) ? Buses[Config.bus - 1].Name.c_str() : "<by sound type>";
	if (ImGui::BeginCombo("Bus", BusName))
	{
		if (ImGui::Selectable("<by sound type>", Config.bus == 0))
		{
			Config.bus = 0;
			IsChanged = true;
		}

		for (u32 BusIdx = 0; BusIdx < SND_BUS_COUNT; BusIdx++)
		{
			if (Buses[BusIdx].IsUsed && !Buses[BusIdx].IsRuntime() && ImGui::Selectable(Buses[BusIdx].Name.c_str(), Config.bus == BusIdx + 1))
			{
				Config.bus = BusIdx + 1;
				IsChanged = true;
			}
		}

		ImGui::EndCombo();
	}

	ImGui::SeparatorText("Shaders");
	IsChanged |= Audio_RenderShaderPreset(Config.shaders);
	ImGui::Separator();

	if (IsChanged)
	{
		Mixer::SetSoundConfig(GAudioEditor.Selected.c_str(), &Config);
		GAudioEditor.SaveStatus = "Modified";
	}

	if (ImGui::Button("Save"))
	{
		GAudioEditor.SaveStatus = Mixer::SaveSoundConfig(GAudioEditor.Selected.c_str()) ? "Saved" : "Nothing to save";
	}

	ImGui::SameLine();
	if (ImGui::Button("Revert"))
	{
		Audio_Select(GAudioEditor.Selected);
	}

	ImGui::SameLine();
	if (ImGui::Button("Play"))
	{
		Audio_StopPreview();
		GAudioEditor.Preview = new ref_sound();
		GAudioEditor.Preview->create(GAudioEditor.Selected.c_str(), st_Effect, 0);
		GAudioEditor.Preview->play(nullptr, sm_2D);
	}

	ImGui::SameLine();
	if (ImGui::Button("Stop"))
	{
		Audio_StopPreview();
	}

	ImGui::SameLine();
	ImGui::TextDisabled("%s", GAudioEditor.SaveStatus);

	ImGui::SeparatorText("Instances");
	AudioShaderEdit Edit;
	{
		xrSRWLockGuard Guard(Mixer::GetUpdateMutex(), true);
		const xr_vector<sound_slot_state>& Slots = Mixer::GetSlots();
		for (u32 SlotIdx = 0; SlotIdx < Slots.size(); SlotIdx++)
		{
			const sound_slot_state& Slot = Slots[SlotIdx];
			if (Slot.sound_name.empty() || Audio_Normalize(Slot.sound_name.c_str()) != GAudioEditor.Selected)
			{
				continue;
			}

			const Fvector& Position = Slot.parameters[(u32)Mixer::ParameterId::Position];
			const Fvector& Volume = Slot.parameters[(u32)Mixer::ParameterId::VolumePerChannel];
			ImGui::Text("#%u %-9s %.2fs  vol %.2f  occ %.2f", SlotIdx + 1, Audio_StateName(Audio_SlotFlag(Slot)), Mixer::GetPlaytime(SlotIdx + 1), Volume.x * Volume.y, Volume.z);
			if (Slot.flags & (u16)Mixer::Flags::Spatial)
			{
				ImGui::SameLine();
				ImGui::Text(" %.1fm", Device.vCameraPosition.distance_to(Position));
			}

			ImGui::PushID((int)SlotIdx);
			if (ImGui::TreeNode("##SlotShaders", "Runtime shaders (%u)", Slot.Shaders.GetCount()))
			{
				Audio_RenderShaderChain(&Slot.Shaders, false, SlotIdx + 1, Edit);
				ImGui::TreePop();
			}
			ImGui::PopID();
		}
	}

	Audio_ApplyShaderEdit(Edit);
}

static void Audio_RenderInspector()
{
	Audio_UpdateFlags();

	if (GAudioEditor.DiskSounds.empty() || ImGui::Button("Rescan"))
	{
		Audio_ScanDisk();
	}

	ImGui::SameLine();
	ImGui::SetNextItemWidth(120.0f);
	if (ImGui::Combo("##Filter", &GAudioEditor.Filter, AudioFilterNames, IM_ARRAYSIZE(AudioFilterNames)))
	{
		GAudioEditor.Frozen.clear();
	}

	ImGui::SameLine();
	if (ImGui::Checkbox("Freeze", &GAudioEditor.IsFrozen) && !GAudioEditor.IsFrozen)
	{
		GAudioEditor.Frozen.clear();
	}

	ImGui::SetNextItemWidth(std::max(ImGui::GetContentRegionAvail().x, 200.0f));
	if (ImGui::InputText("##Search", GAudioEditor.NameFilter.InputBuf, IM_ARRAYSIZE(GAudioEditor.NameFilter.InputBuf)))
	{
		GAudioEditor.NameFilter.Build();
	}

	Audio_BuildRows();

	if (ImGui::BeginChild("##AudioList", ImVec2(0.0f, ImGui::GetContentRegionAvail().y * 0.5f), ImGuiChildFlags_Borders | ImGuiChildFlags_ResizeY))
	{
		if (ImGui::BeginTable("##AudioRows", 2, ImGuiTableFlags_ScrollY | ImGuiTableFlags_RowBg))
		{
			ImGui::TableSetupScrollFreeze(0, 1);
			ImGui::TableSetupColumn("Sound", ImGuiTableColumnFlags_WidthStretch);
			ImGui::TableSetupColumn("State", ImGuiTableColumnFlags_WidthFixed, 70.0f);
			ImGui::TableHeadersRow();

			ImGuiListClipper Clipper;
			Clipper.Begin((int)GAudioEditor.Rows.size());
			if (GAudioEditor.IsSelectionChanged)
			{
				auto Found = std::find(GAudioEditor.Rows.begin(), GAudioEditor.Rows.end(), GAudioEditor.Selected);
				if (Found != GAudioEditor.Rows.end())
				{
					Clipper.IncludeItemByIndex((int)(Found - GAudioEditor.Rows.begin()));
				}
			}

			while (Clipper.Step())
			{
				for (int RowIdx = Clipper.DisplayStart; RowIdx < Clipper.DisplayEnd; RowIdx++)
				{
					const shared_str& Name = GAudioEditor.Rows[RowIdx];
					auto Found = GAudioEditor.Flags.find(Name);
					bool IsSelected = Name == GAudioEditor.Selected;

					ImGui::TableNextRow();
					ImGui::TableNextColumn();
					if (ImGui::Selectable(Name.c_str(), IsSelected, ImGuiSelectableFlags_SpanAllColumns))
					{
						Audio_Select(Name);
					}

					if (IsSelected && GAudioEditor.IsSelectionChanged)
					{
						ImGui::SetScrollHereY();
					}

					ImGui::TableNextColumn();
					ImGui::TextDisabled("%s", Found != GAudioEditor.Flags.end() ? Audio_StateName(Found->second) : "");
				}
			}

			GAudioEditor.IsSelectionChanged = false;
			ImGui::EndTable();
		}
	}

	ImGui::EndChild();
	if (ImGui::BeginChild("##AudioDetails"))
	{
		Audio_RenderDetails();
	}

	ImGui::EndChild();
}

static void Audio_RenderDebug()
{
	ImGui::SeparatorText("Volume");
	ImGui::SliderFloat("Effects", Mixer::GetMasterVolume(), 0.0f, 1.0f);
	ImGui::SliderFloat("Music", Mixer::GetBusVolume("music"), 0.0f, 1.0f);
	ImGui::SliderFloat("Shooting", Mixer::GetBusVolume("shooting"), 0.0f, 1.0f);

	ImGui::SeparatorText("Processing");
	bool IsHrtf = psSoundFlags.test(ss_HRTF);
	if (ImGui::Checkbox("HRTF", &IsHrtf))
	{
		psSoundFlags.set(ss_HRTF, IsHrtf);
	}

	ImGui::SameLine();
	bool IsEfx = psSoundFlags.test(ss_EFX);
	if (ImGui::Checkbox("Zone reverb (EFX)", &IsEfx))
	{
		psSoundFlags.set(ss_EFX, IsEfx);
	}

	ImGui::SliderFloat("Compression", &psSoundCompression, 0.0f, 1.0f);
	ImGui::SliderFloat("Doppler", &psSoundDoppler, 0.0f, 10.0f);
	ImGui::SliderFloat("Rolloff", &psSoundRolloff, 0.1f, 2.0f);
	ImGui::SliderFloat("Occlusion scale", &psSoundOcclusionScale, 0.1f, 0.5f);
	ImGui::SliderFloat("Shooting reverb", &psSoundShootingReverb, 0.0f, 1.0f);

	ImGui::SeparatorText("Draw");
	ImGui::Checkbox("Draw sounds", &GAudioEditor.DrawSounds);
	ImGui::SameLine();
	ImGui::Checkbox("Draw zones", &GAudioEditor.DrawZones);

	ImGui::SeparatorText("Config");
	if (ImGui::Button("Restart sound"))
	{
		Console->Execute("snd_restart");
	}

	ImGui::SameLine();
	if (ImGui::Button("Export config"))
	{
		Mixer::ExportConfig();
	}
}

static const SoundEffectEntry* Audio_ResolveEffect(const char* Item, SoundEffectScope Scope)
{
	u32 AlternativeCount = (u32)_GetItemCount(Item, '|');
	for (u32 AlternativeIdx = 0; AlternativeIdx < AlternativeCount; AlternativeIdx++)
	{
		string128 Alternative;
		_GetItem(Item, AlternativeIdx, Alternative, '|');
		for (u32 EffectId = 1; EffectId <= Mixer::GetEffectCount(); EffectId++)
		{
			const SoundEffectEntry* Entry = Mixer::GetEffect((u8)EffectId);
			if (Entry->Desc.Scope == Scope && xr_strcmp(Entry->Name.c_str(), Alternative) == 0)
			{
				return Entry;
			}
		}
	}

	return nullptr;
}

static const char* Audio_BusName(u32 BusIdx)
{
	SoundBus* Buses = Mixer::GetBuses();
	return (BusIdx != 0 && BusIdx <= SND_BUS_COUNT && Buses[BusIdx - 1].IsUsed) ? Buses[BusIdx - 1].Name.c_str() : "<none>";
}

// HasDynamic: list dynamic buses too (runtime routes; the config cannot name them)
static bool Audio_BusCombo(const char* Label, u32 Current, u32 Exclude, bool HasNone, const char** OutName, bool HasDynamic = false)
{
	bool IsChanged = false;
	if (ImGui::BeginCombo(Label, Audio_BusName(Current)))
	{
		if (HasNone && ImGui::Selectable("<none>", Current == 0))
		{
			*OutName = "";
			IsChanged = true;
		}

		SoundBus* Buses = Mixer::GetBuses();
		for (u32 BusIdx = 1; BusIdx <= SND_BUS_COUNT; BusIdx++)
		{
			const SoundBus& Bus = Buses[BusIdx - 1];
			if (Bus.IsUsed && !Bus.IsGenerated && (HasDynamic || !Bus.IsDynamic) && BusIdx != Exclude && ImGui::Selectable(Bus.Name.c_str(), BusIdx == Current))
			{
				*OutName = Bus.Name.c_str();
				IsChanged = true;
			}
		}

		ImGui::EndCombo();
	}

	return IsChanged;
}

static void Audio_DrawMeter(u32 BusIdx, const SoundBus& Bus)
{
	float Peak = std::max(Bus.Peak[0], Bus.Peak[1]);
	bool IsValid = Peak == Peak && Peak < FLT_MAX;
	float& Meter = GAudioEditor.BusMeters[BusIdx - 1];
	Meter = IsValid ? std::max(Peak, Meter * 0.9f) : 0.0f;

	float Db = 20.0f * log10f(std::max(Meter, 1e-6f));
	float Fraction = std::clamp((Db + 60.0f) / 60.0f, 0.0f, 1.0f);
	ImU32 Color = Db > -6.0f ? IM_COL32(220, 60, 50, 255) : Db > -18.0f ? IM_COL32(220, 190, 50, 255) : IM_COL32(60, 180, 75, 255);

	ImVec2 Min = ImGui::GetCursorScreenPos();
	ImVec2 Size(std::max(ImGui::GetContentRegionAvail().x, 1.0f), ImGui::GetTextLineHeight());
	ImVec2 Max(Min.x + Size.x, Min.y + Size.y);
	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	DrawList->AddRectFilled(Min, Max, ImGui::GetColorU32(ImGuiCol_FrameBg));
	DrawList->AddRectFilled(Min, ImVec2(Min.x + Size.x * Fraction, Max.y), Color);

	string32 Text;
	xr_sprintf(Text, !IsValid ? "NaN" : Meter > 1e-6f ? "%.1f dB" : "-inf", Db);
	DrawList->AddText(ImVec2(Min.x + 4.0f, Min.y), IsValid ? ImGui::GetColorU32(ImGuiCol_Text) : IM_COL32(255, 0, 255, 255), Text);
	ImGui::Dummy(Size);
}

static void Audio_RenderBusRow(u32 BusIdx, u32 Depth)
{
	SoundBus* Buses = Mixer::GetBuses();
	const SoundBus& Bus = Buses[BusIdx - 1];

	bool HasChildren = false;
	for (u32 ChildIdx = 1; ChildIdx <= SND_BUS_COUNT && !HasChildren; ChildIdx++)
	{
		HasChildren = Buses[ChildIdx - 1].IsUsed && ChildIdx != BusIdx && Buses[ChildIdx - 1].GetOutput() == BusIdx;
	}

	ImGui::TableNextRow();
	ImGui::TableNextColumn();
	ImGuiTreeNodeFlags Flags = ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_SpanAllColumns;
	Flags |= HasChildren ? 0 : ImGuiTreeNodeFlags_Leaf;
	Flags |= GAudioEditor.SelectedBus == BusIdx ? ImGuiTreeNodeFlags_Selected : 0;
	bool IsOpen = ImGui::TreeNodeEx((void*)(uintptr_t)BusIdx, Flags, "%s%s%s", Bus.Name.c_str(), Bus.IsDynamic ? " (dynamic)" : "", Bus.Route != 0 ? " (routed)" : "");
	if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
	{
		GAudioEditor.SelectedBus = BusIdx;
		GAudioEditor.BusStatus = "";
	}

	ImGui::TableNextColumn();
	Audio_DrawMeter(BusIdx, Bus);

	if (!IsOpen)
	{
		return;
	}

	u32 ZoneCount = 0;
	for (u32 ChildIdx = 1; ChildIdx <= SND_BUS_COUNT && Depth < SND_BUS_COUNT; ChildIdx++)
	{
		const SoundBus& Child = Buses[ChildIdx - 1];
		if (Child.IsUsed && ChildIdx != BusIdx && Child.GetOutput() == BusIdx)
		{
			ZoneCount += Child.IsGenerated ? 1 : 0;
			if (!Child.IsGenerated)
			{
				Audio_RenderBusRow(ChildIdx, Depth + 1);
			}
		}
	}

	if (ZoneCount != 0)
	{
		ImGui::TableNextRow();
		ImGui::TableNextColumn();
		if (ImGui::TreeNodeEx("##ZoneBuses", ImGuiTreeNodeFlags_SpanAllColumns, "zone buses (%u)", ZoneCount))
		{
			for (u32 ChildIdx = 1; ChildIdx <= SND_BUS_COUNT; ChildIdx++)
			{
				const SoundBus& Child = Buses[ChildIdx - 1];
				if (Child.IsUsed && Child.IsGenerated && Child.GetOutput() == BusIdx)
				{
					Audio_RenderBusRow(ChildIdx, Depth + 1);
				}
			}

			ImGui::TreePop();
		}
	}

	ImGui::TreePop();
}

static void Audio_RenderEffectChain(u32 BusIdx, bool IsVoice)
{
	SoundBus& Bus = Mixer::GetBuses()[BusIdx - 1];
	const char* Key = IsVoice ? "voice_effects" : "effects";
	SoundEffectScope Scope = IsVoice ? SoundEffectScope::Voice : SoundEffectScope::Bus;
	ImGui::SeparatorText(IsVoice ? "Voice effects" : "Bus effects");
	ImGui::PushID(Key);

	xr_vector<xr_string> Items;
	const char* Value = Mixer::GetBusValue(BusIdx, Key);
	if (Bus.IsRuntime())
	{
		for (u32 EffectIdx = 0; EffectIdx < Bus.EffectCount; EffectIdx++)
		{
			const SoundEffectEntry* Entry = Mixer::GetEffect(Bus.Effects[EffectIdx].Effect);
			Items.push_back(Entry != nullptr ? Entry->Name.c_str() : "?");
		}
	}
	else if (Value != nullptr)
	{
		u32 ItemCount = (u32)_GetItemCount(Value);
		for (u32 ItemIdx = 0; ItemIdx < ItemCount; ItemIdx++)
		{
			string128 Item;
			Items.push_back(_GetItem(Value, ItemIdx, Item));
		}
	}

	int MoveFrom = -1, MoveTo = -1, Remove = -1;
	bool IsChainChanged = false;
	u32 EffectIdx = 0;
	u32 EffectCount = IsVoice ? Bus.VoiceEffectCount : Bus.EffectCount;
	for (u32 ItemIdx = 0; ItemIdx < Items.size(); ItemIdx++)
	{
		ImGui::PushID((int)ItemIdx);
		const SoundEffectEntry* Entry = Audio_ResolveEffect(Items[ItemIdx].c_str(), Scope);
		bool IsResolved = Entry != nullptr && EffectIdx < EffectCount;
		SoundBusEffect* Effect = IsResolved ? (IsVoice ? &Bus.VoiceEffects[EffectIdx] : &Bus.Effects[EffectIdx]) : nullptr;
		Entry = Effect != nullptr ? Mixer::GetEffect(Effect->Effect) : nullptr;

		if (!Bus.IsRuntime())
		{
			if (ImGui::ArrowButton("##Up", ImGuiDir_Up) && ItemIdx > 0)
			{
				MoveFrom = ItemIdx;
				MoveTo = ItemIdx - 1;
			}

			ImGui::SameLine();
			if (ImGui::ArrowButton("##Down", ImGuiDir_Down) && ItemIdx + 1 < Items.size())
			{
				MoveFrom = ItemIdx;
				MoveTo = ItemIdx + 1;
			}

			ImGui::SameLine();
			if (ImGui::SmallButton("X"))
			{
				Remove = ItemIdx;
			}

			ImGui::SameLine();
		}

		if (Entry == nullptr)
		{
			ImGui::TextDisabled("%s (unavailable)", Items[ItemIdx].c_str());
		}
		else if (ImGui::TreeNodeEx("##Effect", ImGuiTreeNodeFlags_DefaultOpen, "%s", Items[ItemIdx].c_str()))
		{
			for (u32 ParamIdx = 0; ParamIdx < Entry->Desc.ParamCount; ParamIdx++)
			{
				const SoundEffectParam& Param = Entry->Desc.Params[ParamIdx];
				float ParamValue = Effect->Params[ParamIdx];
				if (Audio_ParamSlider(Param, &ParamValue))
				{
					Mixer::SetBusEffectParam(BusIdx, IsVoice, EffectIdx, ParamIdx, ParamValue);
					GAudioEditor.BusStatus = "Modified";
				}
			}

			if (Effect->Resource.size() != 0)
			{
				ImGui::TextDisabled("resource: %s", Effect->Resource.c_str());
			}

			ImGui::TreePop();
		}

		EffectIdx += IsResolved ? 1 : 0;
		ImGui::PopID();
	}

	if (!Bus.IsRuntime() && ImGui::BeginCombo("##Add", "Add effect...", ImGuiComboFlags_NoArrowButton))
	{
		for (u32 EffectId = 1; EffectId <= Mixer::GetEffectCount(); EffectId++)
		{
			const SoundEffectEntry* Entry = Mixer::GetEffect((u8)EffectId);
			if (Entry->Desc.Scope == Scope && ImGui::Selectable(Entry->Name.c_str()))
			{
				Items.push_back(Entry->Name.c_str());
				IsChainChanged = true;
			}
		}

		ImGui::EndCombo();
	}

	if (MoveFrom >= 0)
	{
		std::swap(Items[MoveFrom], Items[MoveTo]);
		IsChainChanged = true;
	}

	if (Remove >= 0)
	{
		Items.erase(Items.begin() + Remove);
		IsChainChanged = true;
	}

	if (IsChainChanged)
	{
		xr_string Chain;
		for (const xr_string& Item : Items)
		{
			Chain += Chain.empty() ? "" : ", ";
			Chain += Item;
		}

		Mixer::SetBusValue(BusIdx, Key, Chain.c_str());
		GAudioEditor.BusStatus = "Modified";
	}

	ImGui::PopID();
}

static void Audio_SetBusFloat(u32 BusIdx, const char* Key, float Value)
{
	string32 Text;
	xr_sprintf(Text, "%g", Value);
	Mixer::SetBusValue(BusIdx, Key, Text);
	GAudioEditor.BusStatus = "Modified";
}

static void Audio_RenderBusDetails()
{
	u32 BusIdx = GAudioEditor.SelectedBus;
	if (BusIdx == 0 || BusIdx > SND_BUS_COUNT || !Mixer::GetBuses()[BusIdx - 1].IsUsed)
	{
		ImGui::TextDisabled("Select a bus.");
		return;
	}

	SoundBus& Bus = Mixer::GetBuses()[BusIdx - 1];
	u32 Master = Mixer::FindBus("master");
	ImGui::TextUnformatted(Bus.Name.c_str());
	if (Bus.IsGenerated)
	{
		ImGui::TextDisabled("Generated from a zone environment; changes are live only.");
	}
	else if (Bus.IsDynamic)
	{
		ImGui::TextDisabled("Dynamic bus (%u owners), runtime only.", Bus.DynamicOwners);
	}

	const char* Name = nullptr;
	if (BusIdx != Master && Audio_BusCombo("Route", Bus.Route, BusIdx, true, &Name, true))
	{
		Mixer::RouteBus(BusIdx, Name[0] != 0 ? Mixer::FindBus(Name) : 0);
	}
	ImGui::SetItemTooltip("Runtime output override (scripts, snd_bus_route), not saved to the config");

	ImGui::BeginDisabled(Bus.IsRuntime());
	if (BusIdx != Master && Audio_BusCombo("Output", Bus.Output, BusIdx, false, &Name))
	{
		Mixer::SetBusValue(BusIdx, "output", Name);
		GAudioEditor.BusStatus = "Modified";
	}

	float Volume = Bus.Volume;
	if (ImGui::SliderFloat("Volume", &Volume, 0.0f, 4.0f))
	{
		Audio_SetBusFloat(BusIdx, "volume", Volume);
	}

	float ZoneSend = Bus.ZoneSend;
	if (ImGui::SliderFloat("Zone send", &ZoneSend, 0.0f, 4.0f))
	{
		Audio_SetBusFloat(BusIdx, "zone_send", ZoneSend);
	}

	float DirectRatio = Bus.DirectRatio;
	if (ImGui::SliderFloat("Direct ratio", &DirectRatio, 0.0f, 1.0f))
	{
		Audio_SetBusFloat(BusIdx, "direct_ratio", DirectRatio);
	}

	if (Audio_BusCombo("Far send", Bus.FarSend, BusIdx, true, &Name))
	{
		Mixer::SetBusValue(BusIdx, "far_send", Name);
		GAudioEditor.BusStatus = "Modified";
	}

	if (Audio_BusCombo("Indoor send", Bus.IndoorSend, BusIdx, true, &Name))
	{
		Mixer::SetBusValue(BusIdx, "indoor_send", Name);
		GAudioEditor.BusStatus = "Modified";
	}

	ImGui::EndDisabled();
	ImGui::SliderFloat("User volume", &Bus.UserVolume, 0.0f, 1.0f);
	ImGui::SetItemTooltip("Player setting (snd_volume_*), not saved to the config");

	Audio_RenderEffectChain(BusIdx, false);
	Audio_RenderEffectChain(BusIdx, true);

	ImGui::SeparatorText("Shaders");
	ImGui::TextDisabled("Runtime only (scripts, snd_bus_shader). Add a shader to Bus effects to keep it in the config.");
	AudioShaderEdit Edit;
	{
		xrSRWLockGuard Guard(Mixer::GetUpdateMutex(), true);
		ImGui::PushID("shaders");
		Audio_RenderShaderChain(Mixer::GetBusShaders(BusIdx), true, BusIdx, Edit);
		ImGui::PopID();
	}
	Audio_ApplyShaderEdit(Edit);
}

static void Audio_RenderBuses()
{
	ImGui::SetNextItemWidth(180.0f);
	ImGui::InputTextWithHint("##NewBus", "new bus name", GAudioEditor.NewBusName, sizeof(GAudioEditor.NewBusName));
	ImGui::SameLine();
	if (ImGui::Button("Create") && GAudioEditor.NewBusName[0] != 0)
	{
		xr_strlwr(GAudioEditor.NewBusName);
		GAudioEditor.SelectedBus = Mixer::CreateBus(GAudioEditor.NewBusName);
		GAudioEditor.NewBusName[0] = 0;
		GAudioEditor.BusStatus = "Created";
	}

	u32 Master = Mixer::FindBus("master");
	ImGui::SameLine();
	ImGui::BeginDisabled(GAudioEditor.SelectedBus == 0 || GAudioEditor.SelectedBus == Master);
	if (ImGui::Button("Delete"))
	{
		Mixer::DeleteBus(GAudioEditor.SelectedBus);
		GAudioEditor.BusStatus = "Deleted, save to remove it from the config";
	}

	ImGui::EndDisabled();
	ImGui::SameLine();
	ImGui::BeginDisabled(GAudioEditor.SelectedBus == 0);
	if (ImGui::Button("Save"))
	{
		GAudioEditor.BusStatus = Mixer::SaveBus(GAudioEditor.SelectedBus) ? "Saved" : "Nothing to save";
	}

	ImGui::EndDisabled();
	ImGui::SameLine();
	bool& IsGraphOpen = Engine.External.EditorStates[static_cast<u8>(EditorUI::Audio_BusGraph)];
	if (ImGui::Button(IsGraphOpen ? "Close graph###BusGraph" : "Open graph###BusGraph"))
	{
		IsGraphOpen = !IsGraphOpen;
	}
	ImGui::SameLine();
	ImGui::TextDisabled("%s", GAudioEditor.BusStatus);

	if (ImGui::BeginChild("##BusTree", ImVec2(0.0f, ImGui::GetContentRegionAvail().y * 0.4f), ImGuiChildFlags_Borders | ImGuiChildFlags_ResizeY))
	{
		if (ImGui::BeginTable("##Buses", 2, ImGuiTableFlags_RowBg))
		{
			ImGui::TableSetupColumn("Bus", ImGuiTableColumnFlags_WidthStretch);
			ImGui::TableSetupColumn("Peak", ImGuiTableColumnFlags_WidthFixed, 160.0f);
			if (Master != 0)
			{
				Audio_RenderBusRow(Master, 0);
			}

			ImGui::EndTable();
		}

		if (ImGui::TreeNode("Zones"))
		{
			for (const sound_zone_params& Zone : Mixer::GetZones())
			{
				ImGui::Text("%s -> %s", Zone.name.c_str(), Audio_BusName(Zone.bus));
			}

			ImGui::TreePop();
		}
	}

	ImGui::EndChild();
	if (ImGui::BeginChild("##BusDetails"))
	{
		Audio_RenderBusDetails();
	}

	ImGui::EndChild();
}

// Bus chain (own window): the buses in processing order, every bus before the one it outputs to, master last
struct AudioChainItem
{
	u32 Bus = 0;
	// A released dynamic bus, listed before its last output
	const SoundDynamicBusInfo* Ghost = nullptr;
	u32 Depth = 0;
	u32 HiddenZones = 0;
};

struct AudioChainGhost
{
	const SoundDynamicBusInfo* Info = nullptr;
	u32 Parent = 0;
};

struct AudioChainState
{
	xr_vector<AudioChainItem> Items;
	xr_vector<AudioChainGhost> Ghosts;
	bool IsListed[SND_BUS_COUNT] = {};
	u32 Voices[SND_BUS_COUNT] = {};
	u32 RoutedVoices[SND_BUS_COUNT] = {};
};

constexpr float AudioChainIndent = 14.0f;
constexpr float AudioChainTextWidth = 300.0f;
constexpr float AudioChainAnalyzerWidth = 340.0f;
constexpr float AudioChainMeterWidth = 120.0f;
constexpr float AudioChainGap = 6.0f;
static const char* AudioChannelNames[SND_CHANNEL_COUNT] = {"L", "R"};

static float Audio_ChainItemHeight()
{
	return ImGui::GetTextLineHeight() * 3.0f + 10.0f;
}

static ImU32 Audio_ChainAlpha(ImU32 Color, float Alpha)
{
	u32 A = (u32)(((Color >> IM_COL32_A_SHIFT) & 0xFF) * std::clamp(Alpha, 0.0f, 1.0f));
	return (Color & ~IM_COL32_A_MASK) | (A << IM_COL32_A_SHIFT);
}

static ImU32 Audio_LevelColor(float Db)
{
	// Same thresholds as the peak meters: -18 dB yellow, -6 dB red
	return Db > -6.0f ? IM_COL32(220, 60, 50, 255) : Db > -18.0f ? IM_COL32(220, 190, 50, 255) : IM_COL32(60, 180, 75, 255);
}

static float Audio_PeakDb(float Peak)
{
	return (Peak == Peak && Peak > 1e-6f) ? 20.0f * log10f(Peak) : -INFINITY;
}

static const char* AudioAnalyzerChannelNames[] = {"L / R", "Mid", "Left", "Right"};
static const char* AudioAnalyzerResolutionNames[] = {"Low (1024)", "Medium (2048)", "High (4096)"};
static const u32 AudioAnalyzerFftSizes[] = {1024, 2048, 4096};
static const char* AudioAnalyzerSpeedNames[] = {"Slow", "Medium", "Fast"};
static const float AudioAnalyzerReleaseDb[] = {12.0f, 30.0f, 70.0f};
static const char* AudioAnalyzerRangeNames[] = {"60 dB", "90 dB", "120 dB"};
static const float AudioAnalyzerRangeDb[] = {60.0f, 90.0f, 120.0f};
static const char* AudioAnalyzerTiltNames[] = {"0 dB/oct", "3 dB/oct", "4.5 dB/oct", "6 dB/oct"};
static const float AudioAnalyzerTiltDb[] = {0.0f, 3.0f, 4.5f, 6.0f};
// Trace colors: left, right, mid
static const ImU32 AudioAnalyzerColors[AudioTraceCount] = {IM_COL32(120, 190, 255, 255), IM_COL32(255, 165, 95, 255), IM_COL32(205, 225, 255, 255)};

// Traces the current channel setting draws, in drawing order
static u32 Audio_AnalyzerChannels(u32* Channels)
{
	switch (GAudioEditor.Analyzer.Channels)
	{
	case 1:
		Channels[0] = SND_SCOPE_MID;
		return 1;
	case 2:
		Channels[0] = 0;
		return 1;
	case 3:
		Channels[0] = 1;
		return 1;
	default:
		Channels[0] = 1;
		Channels[1] = 0;
		return 2;
	}
}

static float Audio_AnalyzerBandHz(u32 Band, u32 BandCount)
{
	const float LogMin = std::log(AudioAnalyzerMinHz);
	const float LogStep = (std::log(AudioAnalyzerMaxHz) - LogMin) / (float)BandCount;
	return std::exp(LogMin + LogStep * ((float)Band + 0.5f));
}

// Spectrum of one channel (pffft in xrSound) into the trace: instant attack, release by the speed setting,
// the peak hold line falls 3 dB/s
static void Audio_UpdateTrace(u32 BusIdx, u32 Channel, AudioAnalyzerTrace& Trace, u32 BandCount)
{
	float Bands[AudioDetailBandCount];
	BandCount = std::min(BandCount, AudioDetailBandCount);
	const AudioAnalyzerSettings& Settings = GAudioEditor.Analyzer;
	const bool IsValid = Mixer::GetBusSpectrum(BusIdx, Channel, Bands, BandCount, AudioAnalyzerMinHz, AudioAnalyzerMaxHz, AudioAnalyzerFftSizes[Settings.Resolution]);
	const float DeltaTime = ImGui::GetIO().DeltaTime;
	const float Release = AudioAnalyzerReleaseDb[Settings.Speed] * DeltaTime;
	const float PeakRelease = 3.0f * DeltaTime;
	for (u32 Band = 0; Band < BandCount; Band++)
	{
		const float Value = IsValid ? std::max(Bands[Band], AudioAnalyzerFloorDb) : AudioAnalyzerFloorDb;
		Trace.Level[Band] = Trace.IsValid ? std::max(Value, Trace.Level[Band] - Release) : Value;
		Trace.Peak[Band] = Trace.IsValid ? std::max(Trace.Level[Band], Trace.Peak[Band] - PeakRelease) : Value;
	}

	Trace.IsValid = true;
}

static void Audio_UpdateTraces(u32 BusIdx, AudioAnalyzerTrace* Traces, u32 BandCount)
{
	u32 Channels[AudioTraceCount];
	const u32 Count = Audio_AnalyzerChannels(Channels);
	for (u32 Index = 0; Index < Count; Index++)
	{
		Audio_UpdateTrace(BusIdx, Channels[Index], Traces[Channels[Index]], BandCount);
	}
}

static float Audio_AnalyzerY(float Db, float Hz, const ImVec2& Min, const ImVec2& Max)
{
	const AudioAnalyzerSettings& Settings = GAudioEditor.Analyzer;
	const float Range = AudioAnalyzerRangeDb[Settings.Range];
	// Tilt pivots at 1 kHz, as music has less energy up high
	Db += AudioAnalyzerTiltDb[Settings.Tilt] * std::log2(Hz / 1000.0f);
	const float Fraction = std::clamp((Db + Range) / Range, 0.0f, 1.0f);
	return Max.y - (Max.y - Min.y) * Fraction;
}

// Filled curve with a vertical gradient: strong under the line, fading to the floor
static void Audio_DrawTraceFill(ImDrawList* DrawList, const ImVec2* Points, u32 Count, const ImVec2& Min, const ImVec2& Max, ImU32 Color)
{
	const ImVec2 Uv = ImGui::GetFontTexUvWhitePixel();
	const float Height = std::max(Max.y - Min.y, 1.0f);
	const ImU32 Bottom = Audio_ChainAlpha(Color, 0.03f);
	auto TopColor = [&](float Y) { return Audio_ChainAlpha(Color, 0.08f + 0.4f * (1.0f - (Y - Min.y) / Height)); };
	for (u32 Index = 0; Index + 1 < Count; Index++)
	{
		const ImVec2& A = Points[Index];
		const ImVec2& B = Points[Index + 1];
		DrawList->PrimReserve(6, 4);
		const ImDrawIdx Base = (ImDrawIdx)DrawList->_VtxCurrentIdx;
		DrawList->PrimWriteIdx(Base);
		DrawList->PrimWriteIdx((ImDrawIdx)(Base + 1));
		DrawList->PrimWriteIdx((ImDrawIdx)(Base + 2));
		DrawList->PrimWriteIdx(Base);
		DrawList->PrimWriteIdx((ImDrawIdx)(Base + 2));
		DrawList->PrimWriteIdx((ImDrawIdx)(Base + 3));
		DrawList->PrimWriteVtx(A, Uv, TopColor(A.y));
		DrawList->PrimWriteVtx(B, Uv, TopColor(B.y));
		DrawList->PrimWriteVtx(ImVec2(B.x, Max.y), Uv, Bottom);
		DrawList->PrimWriteVtx(ImVec2(A.x, Max.y), Uv, Bottom);
	}
}

static void Audio_DrawAnalyzerGrid(ImDrawList* DrawList, const ImVec2& Min, const ImVec2& Max, bool IsCompact)
{
	const float Width = Max.x - Min.x;
	const float LogMin = std::log(AudioAnalyzerMinHz);
	const float LogRange = std::log(AudioAnalyzerMaxHz) - LogMin;
	const ImU32 Minor = IM_COL32(255, 255, 255, IsCompact ? 8 : 12);
	const ImU32 Major = IM_COL32(255, 255, 255, IsCompact ? 22 : 34);
	const ImU32 Label = IM_COL32(255, 255, 255, 110);

	for (float Decade = 10.0f; Decade <= 10000.0f; Decade *= 10.0f)
	{
		for (u32 Multiple = 1; Multiple <= 9; Multiple++)
		{
			const float Hz = Decade * (float)Multiple;
			if (Hz < AudioAnalyzerMinHz || Hz > AudioAnalyzerMaxHz || (IsCompact && Multiple != 1))
			{
				continue;
			}

			const bool IsMajor = Multiple == 1 || Multiple == 2 || Multiple == 5;
			const float X = Min.x + Width * (std::log(Hz) - LogMin) / LogRange;
			DrawList->AddLine(ImVec2(X, Min.y), ImVec2(X, Max.y), IsMajor ? Major : Minor);
			if (!IsCompact && IsMajor)
			{
				string16 Text;
				Hz >= 1000.0f ? xr_sprintf(Text, "%gk", Hz / 1000.0f) : xr_sprintf(Text, "%g", Hz);
				DrawList->AddText(ImVec2(X + 3.0f, Max.y - ImGui::GetTextLineHeight() - 1.0f), Label, Text);
			}
		}
	}

	const float Range = AudioAnalyzerRangeDb[GAudioEditor.Analyzer.Range];
	const float Step = IsCompact ? Range / 3.0f : (Range > 60.0f ? 12.0f : 6.0f);
	for (float Db = -Step; Db > -Range + 0.5f; Db -= Step)
	{
		const float Y = Max.y - (Max.y - Min.y) * (Db + Range) / Range;
		DrawList->AddLine(ImVec2(Min.x, Y), ImVec2(Max.x, Y), Minor);
		if (!IsCompact)
		{
			string16 Text;
			xr_sprintf(Text, "%.0f", Db);
			DrawList->AddText(ImVec2(Max.x - ImGui::CalcTextSize(Text).x - 4.0f, Y - ImGui::GetTextLineHeight() * 0.5f), Label, Text);
		}
	}
}

// Analyzer panel: grid, a gradient filled curve per channel, peak hold lines
static void Audio_DrawAnalyzer(ImDrawList* DrawList, const ImVec2& Min, const ImVec2& Max, const AudioAnalyzerTrace* Traces, u32 BandCount, bool IsCompact)
{
	DrawList->AddRectFilled(Min, Max, IM_COL32(14, 16, 21, 255), 3.0f);
	DrawList->PushClipRect(Min, Max, true);
	Audio_DrawAnalyzerGrid(DrawList, Min, Max, IsCompact);

	u32 Channels[AudioTraceCount];
	const u32 ChannelCount = Audio_AnalyzerChannels(Channels);
	ImVec2 Points[AudioDetailBandCount];
	ImVec2 PeakPoints[AudioDetailBandCount];
	const float Width = Max.x - Min.x;
	for (u32 Index = 0; Index < ChannelCount; Index++)
	{
		const AudioAnalyzerTrace& Trace = Traces[Channels[Index]];
		if (!Trace.IsValid)
		{
			continue;
		}

		for (u32 Band = 0; Band < BandCount; Band++)
		{
			const float Hz = Audio_AnalyzerBandHz(Band, BandCount);
			const float X = Min.x + Width * ((float)Band + 0.5f) / (float)BandCount;
			Points[Band] = ImVec2(X, Audio_AnalyzerY(Trace.Level[Band], Hz, Min, Max));
			PeakPoints[Band] = ImVec2(X, Audio_AnalyzerY(Trace.Peak[Band], Hz, Min, Max));
		}

		const ImU32 Color = AudioAnalyzerColors[Channels[Index]];
		Audio_DrawTraceFill(DrawList, Points, BandCount, Min, Max, ChannelCount > 1 ? Audio_ChainAlpha(Color, 0.75f) : Color);
		if (GAudioEditor.Analyzer.PeakHold)
		{
			DrawList->AddPolyline(PeakPoints, (int)BandCount, Audio_ChainAlpha(Color, 0.35f), ImDrawFlags_None, 1.0f);
		}
		DrawList->AddPolyline(Points, (int)BandCount, Color, ImDrawFlags_None, IsCompact ? 1.0f : 1.6f);
	}

	DrawList->PopClipRect();
	DrawList->AddRect(Min, Max, IM_COL32(255, 255, 255, 25), 3.0f);
}

static void Audio_NoteName(float Hz, char* Text, u32 Size)
{
	static const char* Names[] = {"C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"};
	const s32 Midi = (s32)std::lround(69.0f + 12.0f * std::log2(Hz / 440.0f));
	const s32 Note = ((Midi % 12) + 12) % 12;
	xr_sprintf(Text, Size, "%s%d", Names[Note], Midi / 12 - 1);
}

static void Audio_RenderAnalyzerSettings()
{
	AudioAnalyzerSettings& Settings = GAudioEditor.Analyzer;
	auto Combo = [](const char* Label, int& Value, const char* const* Names, int Count, float Width)
	{
		ImGui::SameLine();
		ImGui::SetNextItemWidth(Width);
		ImGui::Combo(Label, &Value, Names, Count);
	};

	ImGui::TextDisabled("Analyzer");
	Combo("##Channels", Settings.Channels, AudioAnalyzerChannelNames, (int)std::size(AudioAnalyzerChannelNames), 70.0f);
	ImGui::SetItemTooltip("Channels");
	Combo("##Resolution", Settings.Resolution, AudioAnalyzerResolutionNames, (int)std::size(AudioAnalyzerResolutionNames), 120.0f);
	ImGui::SetItemTooltip("FFT size: higher resolves the lows better and reacts slower");
	Combo("##Speed", Settings.Speed, AudioAnalyzerSpeedNames, (int)std::size(AudioAnalyzerSpeedNames), 80.0f);
	ImGui::SetItemTooltip("Release speed");
	Combo("##Range", Settings.Range, AudioAnalyzerRangeNames, (int)std::size(AudioAnalyzerRangeNames), 80.0f);
	ImGui::SetItemTooltip("Level range");
	Combo("##Tilt", Settings.Tilt, AudioAnalyzerTiltNames, (int)std::size(AudioAnalyzerTiltNames), 95.0f);
	ImGui::SetItemTooltip("Tilt around 1 kHz, so pink-ish music reads flat");
	ImGui::SameLine();
	ImGui::Checkbox("Peak hold", &Settings.PeakHold);
}

// Large analyzer of the selected bus with a cursor readout
static void Audio_RenderSpectrumView(u32 BusIdx)
{
	if (GAudioEditor.DetailBus != BusIdx)
	{
		GAudioEditor.DetailBus = BusIdx;
		for (AudioAnalyzerTrace& Trace : GAudioEditor.DetailTraces)
		{
			Trace.IsValid = false;
		}
	}

	Audio_UpdateTraces(BusIdx, GAudioEditor.DetailTraces, AudioDetailBandCount);

	const SoundBus& Bus = Mixer::GetBuses()[BusIdx - 1];
	ImGui::SeparatorText("Analyzer");
	ImGui::Text("%s   peak L %.1f dB, R %.1f dB", Bus.Name.c_str(), Audio_PeakDb(Bus.Peak[0]), Audio_PeakDb(Bus.Peak[1]));

	const ImVec2 Size(std::max(ImGui::GetContentRegionAvail().x, 64.0f), 220.0f);
	ImGui::InvisibleButton("##Analyzer", Size);
	const ImVec2 Min = ImGui::GetItemRectMin();
	const ImVec2 Max = ImGui::GetItemRectMax();
	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	Audio_DrawAnalyzer(DrawList, Min, Max, GAudioEditor.DetailTraces, AudioDetailBandCount, false);

	if (!ImGui::IsItemHovered())
	{
		return;
	}

	// Cursor readout: frequency, note and the level of every shown channel
	const float MouseX = std::clamp(ImGui::GetIO().MousePos.x, Min.x, Max.x - 1.0f);
	const float Fraction = (MouseX - Min.x) / (Max.x - Min.x);
	const float Hz = std::exp(std::log(AudioAnalyzerMinHz) + (std::log(AudioAnalyzerMaxHz) - std::log(AudioAnalyzerMinHz)) * Fraction);
	const u32 Band = std::min((u32)(Fraction * (float)AudioDetailBandCount), AudioDetailBandCount - 1);
	DrawList->AddLine(ImVec2(MouseX, Min.y), ImVec2(MouseX, Max.y), IM_COL32(255, 255, 255, 70));

	string16 Note;
	Audio_NoteName(Hz, Note, sizeof(Note));
	string128 Text;
	Hz >= 1000.0f ? xr_sprintf(Text, "%.2f kHz  %s", Hz / 1000.0f, Note) : xr_sprintf(Text, "%.0f Hz  %s", Hz, Note);

	u32 Channels[AudioTraceCount];
	const u32 ChannelCount = Audio_AnalyzerChannels(Channels);
	static const char* TraceNames[AudioTraceCount] = {"L", "R", "Mid"};
	for (u32 Index = ChannelCount; Index-- > 0;)
	{
		const AudioAnalyzerTrace& Trace = GAudioEditor.DetailTraces[Channels[Index]];
		string32 Level;
		xr_sprintf(Level, "   %s %.1f dB", TraceNames[Channels[Index]], Trace.Level[Band]);
		xr_strcat(Text, Level);
	}

	const ImVec2 TextSize = ImGui::CalcTextSize(Text);
	const float TextX = std::min(MouseX + 8.0f, Max.x - TextSize.x - 8.0f);
	DrawList->AddRectFilled(ImVec2(TextX - 4.0f, Min.y + 4.0f), ImVec2(TextX + TextSize.x + 4.0f, Min.y + 8.0f + TextSize.y), IM_COL32(0, 0, 0, 170), 3.0f);
	DrawList->AddText(ImVec2(TextX, Min.y + 6.0f), IM_COL32(235, 235, 235, 255), Text);
}

// L / R peak meters with their level in dB, decaying like the tree meters
static void Audio_DrawChannelMeters(ImDrawList* DrawList, u32 BusIdx, const SoundBus& Bus, const ImVec2& Min, const ImVec2& Max)
{
	const float Height = (Max.y - Min.y - 2.0f) / (float)SND_CHANNEL_COUNT;
	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		float& Meter = GAudioEditor.GraphMeters[BusIdx - 1][Channel];
		const float Peak = Bus.Peak[Channel];
		Meter = (Peak == Peak && Peak < FLT_MAX) ? std::max(Peak, Meter * 0.9f) : 0.0f;
		const float Db = 20.0f * log10f(std::max(Meter, 1e-6f));
		const float Fraction = std::clamp((Db + 60.0f) / 60.0f, 0.0f, 1.0f);
		const ImVec2 BarMin(Min.x, Min.y + (Height + 2.0f) * (float)Channel);
		const ImVec2 BarMax(Max.x, BarMin.y + Height);
		DrawList->AddRectFilled(BarMin, BarMax, ImGui::GetColorU32(ImGuiCol_WindowBg), 2.0f);
		DrawList->AddRectFilled(BarMin, ImVec2(BarMin.x + (BarMax.x - BarMin.x) * Fraction, BarMax.y), Audio_LevelColor(Db), 2.0f);

		string32 Text;
		xr_sprintf(Text, Meter > 1e-6f ? "%s %.1f" : "%s -inf", AudioChannelNames[Channel], Db);
		DrawList->AddText(ImVec2(BarMin.x + 3.0f, BarMin.y + (Height - ImGui::GetTextLineHeight()) * 0.5f), ImGui::GetColorU32(ImGuiCol_Text), Text);
	}
}

static bool Audio_ChainIsShown(const SoundBus& Bus)
{
	return Bus.IsUsed && (!Bus.IsGenerated || GAudioEditor.GraphShowZones);
}

// Post-order walk: the inputs of a bus (and its released dynamic buses) come right before it
static void Audio_ChainCollect(AudioChainState& Chain, u32 BusIdx, u32 Depth)
{
	SoundBus* Buses = Mixer::GetBuses();
	Chain.IsListed[BusIdx - 1] = true;

	u32 HiddenZones = 0;
	for (u32 ChildIdx = 1; ChildIdx <= SND_BUS_COUNT && Depth < SND_BUS_COUNT; ChildIdx++)
	{
		const SoundBus& Child = Buses[ChildIdx - 1];
		if (!Child.IsUsed || ChildIdx == BusIdx || Child.GetOutput() != BusIdx || Chain.IsListed[ChildIdx - 1])
		{
			continue;
		}

		if (!Audio_ChainIsShown(Child))
		{
			HiddenZones++;
			continue;
		}

		Audio_ChainCollect(Chain, ChildIdx, Depth + 1);
	}

	for (const AudioChainGhost& Ghost : Chain.Ghosts)
	{
		if (Ghost.Parent == BusIdx)
		{
			Chain.Items.push_back({0, Ghost.Info, Depth + 1, 0});
		}
	}

	Chain.Items.push_back({BusIdx, nullptr, Depth, HiddenZones});
}

static void Audio_ChainShaderNames(u32 BusIdx, char* Text, u32 Size)
{
	Text[0] = 0;
	const SoundShaderChain* Shaders = Mixer::GetBusShaders(BusIdx);
	for (u32 ShaderIdx = 0; Shaders != nullptr && ShaderIdx < Shaders->GetCount(); ShaderIdx++)
	{
		const ISoundShader* Shader = Shaders->Get(ShaderIdx);
		if (Shader != nullptr)
		{
			xr_strcat(Text, Size, ShaderIdx == 0 ? "" : ", ");
			xr_strcat(Text, Size, Shader->GetClass().Name);
		}
	}
}

static void Audio_ChainEffectNames(const SoundBus& Bus, char* Text, u32 Size)
{
	Text[0] = 0;
	for (u32 EffectIdx = 0; EffectIdx < Bus.EffectCount; EffectIdx++)
	{
		const SoundEffectEntry* Entry = Mixer::GetEffect(Bus.Effects[EffectIdx].Effect);
		xr_strcat(Text, Size, EffectIdx == 0 ? "" : ", ");
		xr_strcat(Text, Size, Entry != nullptr ? Entry->Name.c_str() : "?");
	}
}

static void Audio_ChainTooltip(u32 BusIdx, const SoundBus& Bus, const AudioChainState& Chain, const AudioChainItem& Item)
{
	string512 Text;
	ImGui::TextUnformatted(Bus.Name.c_str());
	ImGui::Separator();
	ImGui::Text("Output: %s", Audio_BusName(Bus.Output));
	if (Bus.Route != 0)
	{
		ImGui::Text("Route: %s", Audio_BusName(Bus.Route));
	}

	ImGui::Text("Volume: %.2f x %.2f (user)", Bus.Volume, Bus.UserVolume);
	ImGui::Text("Peak: L %.1f dB, R %.1f dB", Audio_PeakDb(Bus.Peak[0]), Audio_PeakDb(Bus.Peak[1]));
	if (Bus.FarSend != 0 || Bus.IndoorSend != 0)
	{
		ImGui::Text("Sends: far %s, indoor %s", Audio_BusName(Bus.FarSend), Audio_BusName(Bus.IndoorSend));
	}

	Audio_ChainEffectNames(Bus, Text, sizeof(Text));
	ImGui::Text("Effects: %s", Text[0] != 0 ? Text : "-");
	Audio_ChainShaderNames(BusIdx, Text, sizeof(Text));
	ImGui::Text("Shaders: %s", Text[0] != 0 ? Text : "-");
	ImGui::Text("Voices: %u own, %u routed in", Chain.Voices[BusIdx - 1], Chain.RoutedVoices[BusIdx - 1]);
	if (Bus.IsDynamic)
	{
		ImGui::Text("Dynamic, %u owners", Bus.DynamicOwners);
	}

	if (Item.HiddenZones != 0)
	{
		ImGui::Text("Hidden zone buses: %u", Item.HiddenZones);
	}
}

static void Audio_ChainGhostTooltip(const SoundDynamicBusInfo& Info)
{
	ImGui::TextUnformatted(Info.Name.c_str());
	ImGui::Separator();
	ImGui::TextDisabled("Released dynamic bus");
	ImGui::Text("Last output: %s", Info.Output.size() ? Info.Output.c_str() : "master");
	ImGui::Text("Last shaders: %s", Info.Shaders.size() ? Info.Shaders.c_str() : "-");
	ImGui::Text("Created %u times", Info.CreateCount);
	ImGui::TextDisabled("Comes back here when acquired again; Forget released drops it");
}

static void Audio_ChainDrawBus(ImDrawList* DrawList, const ImVec2& Min, const ImVec2& Max, const AudioChainState& Chain, const AudioChainItem& Item, bool IsMaster)
{
	const u32 BusIdx = Item.Bus;
	const SoundBus& Bus = Mixer::GetBuses()[BusIdx - 1];
	const bool IsSelected = GAudioEditor.SelectedBus == BusIdx;
	const ImU32 RouteColor = IM_COL32(230, 150, 50, 255);
	const ImU32 SendColor = IM_COL32(100, 165, 240, 255);
	const ImU32 Stripe = Bus.IsDynamic ? IM_COL32(170, 110, 230, 255) : Bus.IsGenerated ? IM_COL32(90, 150, 200, 255) : IsMaster ? IM_COL32(220, 220, 220, 255) : ImGui::GetColorU32(ImGuiCol_Border);
	DrawList->AddRectFilled(Min, Max, ImGui::GetColorU32(IsSelected ? ImGuiCol_Header : ImGuiCol_FrameBg), 4.0f);
	DrawList->AddRectFilled(Min, ImVec2(Min.x + 4.0f, Max.y), Stripe, 4.0f, ImDrawFlags_RoundCornersLeft);
	if (IsSelected || IsMaster)
	{
		DrawList->AddRect(Min, Max, IsSelected ? ImGui::GetColorU32(ImGuiCol_NavCursor) : Stripe, 4.0f, 0, IsSelected ? 2.0f : 1.0f);
	}

	// Right to left: meters, R spectrum, L spectrum; the text gets what is left
	const float Inner = 5.0f;
	float Right = Max.x - Inner;
	Audio_DrawChannelMeters(DrawList, BusIdx, Bus, ImVec2(Right - AudioChainMeterWidth, Min.y + Inner), ImVec2(Right, Max.y - Inner));
	Right -= AudioChainMeterWidth + AudioChainGap;
	if (GAudioEditor.GraphShowSpectrum)
	{
		const ImVec2 AnalyzerMin(Right - AudioChainAnalyzerWidth, Min.y + Inner);
		Audio_DrawAnalyzer(DrawList, AnalyzerMin, ImVec2(Right, Max.y - Inner), GAudioEditor.RowTraces[BusIdx - 1], AudioRowBandCount, true);
		Right -= AudioChainAnalyzerWidth + AudioChainGap;
	}

	DrawList->PushClipRect(Min, ImVec2(std::max(Right, Min.x + 40.0f), Max.y), true);
	const float LineHeight = ImGui::GetTextLineHeight();
	ImVec2 Cursor(Min.x + 10.0f, Min.y + Inner);
	DrawList->AddText(Cursor, ImGui::GetColorU32(ImGuiCol_Text), Bus.Name.c_str());

	string128 Tags;
	xr_sprintf(Tags, "%s%s  voices %u", Bus.IsDynamic ? "  dynamic" : "", Bus.IsGenerated ? "  zone" : "", Chain.Voices[BusIdx - 1] + Chain.RoutedVoices[BusIdx - 1]);
	if (Chain.RoutedVoices[BusIdx - 1] != 0)
	{
		string32 Routed;
		xr_sprintf(Routed, " (%u routed in)", Chain.RoutedVoices[BusIdx - 1]);
		xr_strcat(Tags, Routed);
	}
	if (Item.HiddenZones != 0)
	{
		string32 Zones;
		xr_sprintf(Zones, "  +%u zones", Item.HiddenZones);
		xr_strcat(Tags, Zones);
	}
	DrawList->AddText(ImVec2(Cursor.x + ImGui::CalcTextSize(Bus.Name.c_str()).x, Cursor.y), ImGui::GetColorU32(ImGuiCol_TextDisabled), Tags);

	// Where the signal goes next
	Cursor.y += LineHeight;
	string256 Line;
	if (IsMaster)
	{
		xr_strcpy(Line, "-> output device");
	}
	else if (Bus.Route != 0)
	{
		xr_sprintf(Line, "-> %s (route, config %s)", Audio_BusName(Bus.Route), Audio_BusName(Bus.Output));
	}
	else
	{
		xr_sprintf(Line, "-> %s", Audio_BusName(Bus.Output));
	}
	DrawList->AddText(Cursor, Bus.Route != 0 ? RouteColor : ImGui::GetColorU32(ImGuiCol_Text), Line);

	if (GAudioEditor.GraphShowSends && (Bus.FarSend != 0 || Bus.IndoorSend != 0))
	{
		string128 Sends;
		xr_sprintf(Sends, "   sends: %s%s%s", Bus.FarSend != 0 ? Audio_BusName(Bus.FarSend) : "", Bus.FarSend != 0 && Bus.IndoorSend != 0 ? ", " : "", Bus.IndoorSend != 0 ? Audio_BusName(Bus.IndoorSend) : "");
		DrawList->AddText(ImVec2(Cursor.x + ImGui::CalcTextSize(Line).x, Cursor.y), SendColor, Sends);
	}

	Cursor.y += LineHeight;
	string256 Shaders;
	Audio_ChainShaderNames(BusIdx, Shaders, sizeof(Shaders));
	const bool HasShaders = Shaders[0] != 0;
	if (!HasShaders)
	{
		Audio_ChainEffectNames(Bus, Shaders, sizeof(Shaders));
	}
	DrawList->AddText(Cursor, HasShaders ? IM_COL32(230, 170, 70, 255) : ImGui::GetColorU32(ImGuiCol_TextDisabled), Shaders[0] != 0 ? Shaders : "-");
	DrawList->PopClipRect();
}

static void Audio_ChainDrawGhost(ImDrawList* DrawList, const ImVec2& Min, const ImVec2& Max, const SoundDynamicBusInfo& Info)
{
	DrawList->AddRectFilled(Min, Max, ImGui::GetColorU32(ImGuiCol_FrameBg, 0.35f), 4.0f);
	DrawList->AddRectFilled(Min, ImVec2(Min.x + 4.0f, Max.y), IM_COL32(170, 110, 230, 90), 4.0f, ImDrawFlags_RoundCornersLeft);

	const ImU32 Color = ImGui::GetColorU32(ImGuiCol_TextDisabled);
	const float LineHeight = ImGui::GetTextLineHeight();
	ImVec2 Cursor(Min.x + 10.0f, Min.y + 5.0f);
	DrawList->PushClipRect(Min, Max, true);
	string256 Line;
	xr_sprintf(Line, "%s  released, created %ux", Info.Name.c_str(), Info.CreateCount);
	DrawList->AddText(Cursor, Color, Line);
	Cursor.y += LineHeight;
	xr_sprintf(Line, "-> %s", Info.Output.size() ? Info.Output.c_str() : "master");
	DrawList->AddText(Cursor, Color, Line);
	Cursor.y += LineHeight;
	DrawList->AddText(Cursor, Color, Info.Shaders.size() ? Info.Shaders.c_str() : "-");
	DrawList->PopClipRect();
}

static void Audio_RenderBusGraph()
{
	ImGui::Checkbox("Zone buses", &GAudioEditor.GraphShowZones);
	ImGui::SameLine();
	ImGui::Checkbox("Sends", &GAudioEditor.GraphShowSends);
	ImGui::SameLine();
	ImGui::Checkbox("Analyzer", &GAudioEditor.GraphShowSpectrum);
	ImGui::SameLine();
	ImGui::Checkbox("Released buses", &GAudioEditor.GraphShowReleased);
	ImGui::SameLine();
	if (ImGui::Button("Forget released"))
	{
		Mixer::ForgetDynamicBuses();
	}
	ImGui::SetItemTooltip("Drops the released dynamic buses from the list");

	ImGui::TextColored(ImVec4(0.9f, 0.6f, 0.2f, 1.0f), "route");
	ImGui::SameLine();
	ImGui::TextColored(ImVec4(0.4f, 0.65f, 0.95f, 1.0f), "send");
	ImGui::SameLine();
	ImGui::TextColored(ImVec4(0.67f, 0.43f, 0.9f, 1.0f), "dynamic");
	ImGui::SameLine();
	ImGui::TextDisabled("| processing order, every bus before its output, master last; indent = depth");
	if (GAudioEditor.GraphShowSpectrum)
	{
		Audio_RenderAnalyzerSettings();
	}

	const u32 Master = Mixer::FindBus("master");
	SoundBus* Buses = Mixer::GetBuses();
	AudioChainState Chain;
	{
		xrSRWLockGuard Guard(Mixer::GetUpdateMutex(), true);
		for (const sound_slot_state& Slot : Mixer::GetSlots())
		{
			if (Slot.state != Mixer::State::Playing)
			{
				continue;
			}

			bool IsRouted = Slot.RouteBus != 0 && Slot.RouteBus <= SND_BUS_COUNT && Buses[Slot.RouteBus - 1].IsUsed;
			u32 BusIdx = IsRouted ? Slot.RouteBus : Slot.bus;
			if (BusIdx != 0 && BusIdx <= SND_BUS_COUNT)
			{
				(IsRouted ? Chain.RoutedVoices[BusIdx - 1] : Chain.Voices[BusIdx - 1])++;
			}
		}
	}

	// Dynamic buses are listed from their creation on; a released one stays before its last output
	Mixer::GetDynamicBuses(GAudioEditor.DynamicBuses);
	for (const SoundDynamicBusInfo& Info : GAudioEditor.DynamicBuses)
	{
		const bool IsAlive = Info.Bus != 0 && Info.Bus <= SND_BUS_COUNT && Buses[Info.Bus - 1].IsUsed && Buses[Info.Bus - 1].Name == Info.Name;
		if (IsAlive || !GAudioEditor.GraphShowReleased)
		{
			continue;
		}

		u32 Parent = Info.Output.size() ? Mixer::FindBus(Info.Output.c_str()) : 0;
		Parent = (Parent == 0 || !Audio_ChainIsShown(Buses[Parent - 1])) ? Master : Parent;
		Chain.Ghosts.push_back({&Info, Parent});
	}

	// Buses that do not reach master (the mixer reroutes them, so only for a moment) go first
	for (u32 BusIdx = 1; BusIdx <= SND_BUS_COUNT; BusIdx++)
	{
		const SoundBus& Bus = Buses[BusIdx - 1];
		if (BusIdx != Master && Audio_ChainIsShown(Bus) && Bus.GetOutput() == 0 && !Chain.IsListed[BusIdx - 1])
		{
			Audio_ChainCollect(Chain, BusIdx, 0);
		}
	}

	if (Master != 0)
	{
		Audio_ChainCollect(Chain, Master, 0);
	}

	const float ListHeight = std::max(ImGui::GetContentRegionAvail().y * 0.6f, 160.0f);
	if (ImGui::BeginChild("##BusChain", ImVec2(0.0f, ListHeight), ImGuiChildFlags_Borders | ImGuiChildFlags_ResizeY))
	{
		// Indent grows with depth, master sits at the left edge at the bottom
		ImDrawList* DrawList = ImGui::GetWindowDrawList();
		const float ItemHeight = Audio_ChainItemHeight();
		xrSRWLockGuard Guard(Mixer::GetUpdateMutex(), true);
		for (u32 ItemIdx = 0; ItemIdx < Chain.Items.size(); ItemIdx++)
		{
			const AudioChainItem& Item = Chain.Items[ItemIdx];
			const float Indent = AudioChainIndent * (float)Item.Depth;
			const float Width = std::max(ImGui::GetContentRegionAvail().x - Indent, 200.0f);
			ImGui::SetCursorPosX(ImGui::GetCursorPosX() + Indent);
			ImGui::PushID((int)ItemIdx);
			const bool IsClicked = ImGui::InvisibleButton("##Item", ImVec2(Width, Item.Ghost != nullptr ? ItemHeight * 0.75f : ItemHeight));
			const bool IsHovered = ImGui::IsItemHovered();
			const ImVec2 Min = ImGui::GetItemRectMin();
			const ImVec2 Max = ImGui::GetItemRectMax();
			ImGui::PopID();

			if (Item.Ghost != nullptr)
			{
				Audio_ChainDrawGhost(DrawList, Min, Max, *Item.Ghost);
				if (IsHovered && ImGui::BeginTooltip())
				{
					Audio_ChainGhostTooltip(*Item.Ghost);
					ImGui::EndTooltip();
				}
				continue;
			}

			if (GAudioEditor.GraphShowSpectrum)
			{
				Audio_UpdateTraces(Item.Bus, GAudioEditor.RowTraces[Item.Bus - 1], AudioRowBandCount);
			}

			Audio_ChainDrawBus(DrawList, Min, Max, Chain, Item, Item.Bus == Master);
			if (IsClicked)
			{
				GAudioEditor.SelectedBus = Item.Bus;
				GAudioEditor.BusStatus = "";
			}

			if (IsHovered && ImGui::BeginTooltip())
			{
				Audio_ChainTooltip(Item.Bus, Buses[Item.Bus - 1], Chain, Item);
				ImGui::EndTooltip();
			}
		}
	}

	ImGui::EndChild();
	if (ImGui::BeginChild("##GraphBusDetails"))
	{
		const u32 Selected = GAudioEditor.SelectedBus;
		if (GAudioEditor.GraphShowSpectrum && Selected != 0 && Selected <= SND_BUS_COUNT && Buses[Selected - 1].IsUsed)
		{
			Audio_RenderSpectrumView(Selected);
		}

		Audio_RenderBusDetails();
	}

	ImGui::EndChild();
}

static void Audio_RenderStatistics()
{
	Audio_UpdateFlags();
	const sound_stats* Stats = Mixer::GetStats();
	const float BlockMs = (float)SND_BLOCKSIZE * 1000.0f / (float)SND_SAMPLERATE;
	float RenderMs = (float)Stats->render_time_micros / 1000.0f;

	GAudioEditor.LoadHistory[GAudioEditor.HistoryIdx] = RenderMs / BlockMs * 100.0f;
	GAudioEditor.HistoryIdx = (GAudioEditor.HistoryIdx + 1) % std::size(GAudioEditor.LoadHistory);

	string32 Overlay;
	xr_sprintf(Overlay, "load %.1f%%", RenderMs / BlockMs * 100.0f);
	float PlotWidth = ImGui::GetContentRegionAvail().x;
	PlotWidth = (PlotWidth > 1.0f && PlotWidth < 16384.0f) ? PlotWidth : 400.0f;
	ImGui::PlotLines("##Load", GAudioEditor.LoadHistory, (int)std::size(GAudioEditor.LoadHistory), (int)GAudioEditor.HistoryIdx, Overlay, 0.0f, 100.0f, ImVec2(PlotWidth, 80.0f));

	if (ImGui::BeginTable("##Stats", 2, ImGuiTableFlags_SizingFixedFit))
	{
		auto Row = [](const char* Name, const char* Format, auto... Args)
		{
			ImGui::TableNextRow();
			ImGui::TableNextColumn();
			ImGui::TextDisabled("%s", Name);
			ImGui::TableNextColumn();
			ImGui::Text(Format, Args...);
		};

		u32 StateCounts[5] = {};
		u32 ActiveBuses = 0;
		for (const auto& [Name, Flags] : GAudioEditor.Flags)
		{
			for (u32 FlagIdx = 0; FlagIdx < 5; FlagIdx++)
			{
				StateCounts[FlagIdx] += (Flags >> FlagIdx) & 1;
			}
		}

		SoundBus* Buses = Mixer::GetBuses();
		for (u32 BusIdx = 0; BusIdx < SND_BUS_COUNT; BusIdx++)
		{
			ActiveBuses += (Buses[BusIdx].IsUsed && (Buses[BusIdx].HasInput || Buses[BusIdx].TailFrames != 0)) ? 1 : 0;
		}

		u32 CacheTotal = Stats->cache_hit_count + Stats->cache_miss_count;
		Row("Block", "%.2f ms", BlockMs);
		Row("Render", "%.2f ms", RenderMs);
		Row("Precache", "%.2f ms", (float)Stats->precache_time_micros / 1000.0f);
		Row("Update", "%.2f ms", (float)Stats->update_time_micros / 1000.0f);
		Row("Callback interval", "%.2f ms", (float)Stats->frame_time_micros / 1000.0f);
		Row("Slots", "%u (free %d)", (u32)Mixer::GetSlots().size(), Stats->possible_free_count);
		Row("Sounds", "loaded %u, playing %u, simulated %u, stopped %u, delayed %u", StateCounts[0], StateCounts[1], StateCounts[3], StateCounts[2], StateCounts[4]);
		Row("Cache lines", "%u / %u free", Stats->cache_lines_free, Stats->cache_lines_total);
		Row("Cache", "%u hits, %u misses (%.1f%%)", Stats->cache_hit_count, Stats->cache_miss_count, CacheTotal != 0 ? (float)Stats->cache_miss_count * 100.0f / (float)CacheTotal : 0.0f);
		Row("Render cache misses", "%u", Stats->render_cache_miss);
		Row("Active buses", "%u", ActiveBuses);
		Row("Zones", "%u", (u32)Mixer::GetZones().size());
#ifdef DEBUG_DRAW
		Row("Output", "L %.1f dB, R %.1f dB", Stats->channel_volumes[0], Stats->channel_volumes[1]);
#endif
		ImGui::EndTable();
	}
}

void RenderUIAudio()
{
	bool& IsOpen = Engine.External.EditorStates[static_cast<u8>(EditorUI::Audio_General)];
	if (!IsOpen)
	{
		return;
	}

	if (GAudioEditor.DrawZones)
	{
		Audio_DrawZones();
	}

	if (GAudioEditor.DrawSounds)
	{
		Audio_DrawSounds();
	}

	ImGui::SetNextWindowSize(ImVec2(900.0f, 520.0f), ImGuiCond_FirstUseEver);
	if (!ImGui::Begin("Audio Editor", &IsOpen))
	{
		ImGui::End();
		return;
	}

	if (ImGui::BeginTabBar("##AudioEditorTabs"))
	{
		if (ImGui::BeginTabItem("Inspector", nullptr, GAudioEditor.IsPicked ? ImGuiTabItemFlags_SetSelected : 0))
		{
			GAudioEditor.IsPicked = false;
			Audio_RenderInspector();
			ImGui::EndTabItem();
		}

		if (ImGui::BeginTabItem("Buses"))
		{
			Audio_RenderBuses();
			ImGui::EndTabItem();
		}

		if (ImGui::BeginTabItem("Statistics"))
		{
			Audio_RenderStatistics();
			ImGui::EndTabItem();
		}

		if (ImGui::BeginTabItem("Debug"))
		{
			Audio_RenderDebug();
			ImGui::EndTabItem();
		}

		ImGui::EndTabBar();
	}

	ImGui::End();
}

void RenderUIAudioBusGraph()
{
	bool& IsOpen = Engine.External.EditorStates[static_cast<u8>(EditorUI::Audio_BusGraph)];
	if (!IsOpen)
	{
		return;
	}

	ImGui::SetNextWindowSize(ImVec2(1100.0f, 720.0f), ImGuiCond_FirstUseEver);
	if (ImGui::Begin("Audio Bus Graph", &IsOpen))
	{
		Audio_RenderBusGraph();
	}

	ImGui::End();
}
