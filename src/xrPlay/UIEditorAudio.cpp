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
	float LoadHistory[120] = {};
	u32 HistoryIdx = 0;
};

static AudioEditorState GAudioEditor;

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
			if (Buses[BusIdx].IsUsed && !Buses[BusIdx].IsGenerated && ImGui::Selectable(Buses[BusIdx].Name.c_str(), Config.bus == BusIdx + 1))
			{
				Config.bus = BusIdx + 1;
				IsChanged = true;
			}
		}

		ImGui::EndCombo();
	}

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
	ImGui::TextDisabled("%s", GAudioEditor.SaveStatus);

	ImGui::SeparatorText("Instances");
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
	}
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

	ImGui::SetItemTooltip("Keep sounds in the list after they stop matching the filter");
	ImGui::SetNextItemWidth(-FLT_MIN);
	if (ImGui::InputTextWithHint("##Search", "search sounds (a,b include, -c exclude)", GAudioEditor.NameFilter.InputBuf, IM_ARRAYSIZE(GAudioEditor.NameFilter.InputBuf)))
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

static bool Audio_BusCombo(const char* Label, u32 Current, u32 Exclude, bool HasNone, const char** OutName)
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
			if (Bus.IsUsed && !Bus.IsGenerated && BusIdx != Exclude && ImGui::Selectable(Bus.Name.c_str(), BusIdx == Current))
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
		HasChildren = Buses[ChildIdx - 1].IsUsed && ChildIdx != BusIdx && Buses[ChildIdx - 1].Output == BusIdx;
	}

	ImGui::TableNextRow();
	ImGui::TableNextColumn();
	ImGuiTreeNodeFlags Flags = ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_SpanAllColumns;
	Flags |= HasChildren ? 0 : ImGuiTreeNodeFlags_Leaf;
	Flags |= GAudioEditor.SelectedBus == BusIdx ? ImGuiTreeNodeFlags_Selected : 0;
	bool IsOpen = ImGui::TreeNodeEx((void*)(uintptr_t)BusIdx, Flags, "%s", Bus.Name.c_str());
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
		if (Child.IsUsed && ChildIdx != BusIdx && Child.Output == BusIdx)
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
				if (Child.IsUsed && Child.IsGenerated && Child.Output == BusIdx)
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
	if (Bus.IsGenerated)
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

		if (!Bus.IsGenerated)
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
				if (ImGui::SliderFloat(Param.Name, &ParamValue, Param.Min, Param.Max, "%.4g"))
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

	if (!Bus.IsGenerated && ImGui::BeginCombo("##Add", "Add effect...", ImGuiComboFlags_NoArrowButton))
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

	ImGui::BeginDisabled(Bus.IsGenerated);
	const char* Name = nullptr;
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
