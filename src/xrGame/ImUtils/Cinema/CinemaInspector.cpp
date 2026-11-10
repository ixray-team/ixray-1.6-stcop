#include "StdAfx.h"
#include "CinemaEditor.h"
#include "../../Cinema/CinemaSceneIo.h"
#include "../../Cinema/CinemaShake.h"
#include "../../../xrRHI/RHI.h"

namespace Cinema::InspectorUi
{
constexpr const char* Modes[6] = { "Linear", "Ease in", "Ease out", "Ease in-out", "Bezier", "Hold" };
constexpr const char* Tangents[3] = { "Auto", "Aligned", "Broken" };
constexpr const char* ShapeNames[3] = { "Tension", "Continuity", "Bias" };
constexpr const char* DofModes[3] = { "Off", "Manual", "Focus on object" };
constexpr float PoseKey::*ShapeFields[3] = { &PoseKey::Tension, &PoseKey::Continuity, &PoseKey::Bias };
constexpr u32 Rates[4] = { 24, 25, 30, 60 };
constexpr float ListWidth = 360.0f;
constexpr const char* GizmoLabels[5] = { "T", "R", "S", "", "TRS" };
constexpr const char* GizmoNames[5] = { "Translate", "Rotate", "Scale", "Range", "Universal" };
constexpr const char* StepNames[4] = { "Move step", "Rotate step", "Scale step", "Range step" };
constexpr const char* StepFormats[4] = { "%.2f m", "%.1f deg", "%.2f", "%.2f m" };
constexpr float StepLo[4] = { 0.01f, 0.5f, 0.01f, 0.01f }, StepHi[4] = { 10.0f, 180.0f, 1.0f, 10.0f };
constexpr const char* LinkProblems[5] = { "Parent is missing: hidden", "Bone is missing: the origin is used", "Look at target is missing: ignored", "Look bone is missing: the origin is used", "Parent is missing: the block passes through" };

float ValueAt(const Object& Target, s32 Block, Param Id, u8 Layer, u8 Component, float Time)
{
	const float Static = *StaticField(const_cast<Object&>(Target), Block, Id, Layer, Component);
	const Lane* Found = Curves::FindLane(LaneList(Target, Block), Id, Layer);
	return Found ? Curves::Result(Found->Curves[Component], Time - TargetStart(Target, Block), Static, Info(Id)) : Static;
}

KeyState StateAt(const Object& Target, s32 Block, Param Id, u8 Layer, float Time, float HalfFrame)
{
	const Lane* Found = Curves::FindLane(LaneList(Target, Block), Id, Layer);
	for (u8 Component = 0; Found && Component < Info(Id).Components; ++Component)
	{
		if (Curves::KeyAt(Found->Curves[Component], Time - TargetStart(Target, Block), HalfFrame) != u32(-1))
		{
			return KeyState::OnKey;
		}
	}
	return Found ? KeyState::Animated : KeyState::Static;
}

float& PoseField(PoseKey& Pose, Param Id, u8 Component)
{
	return (Id == Param::Rotation ? Pose.Rotation : Pose.Position)[Component];
}

void Diamond(ImDrawList* List, const ImVec2& Centre, float Radius, u32 Color, bool IsFilled)
{
	const ImVec2 Points[4] = { ImVec2(Centre.x, Centre.y - Radius), ImVec2(Centre.x + Radius, Centre.y), ImVec2(Centre.x, Centre.y + Radius), ImVec2(Centre.x - Radius, Centre.y) };
	if (IsFilled)
	{
		List->AddConvexPolyFilled(Points, 4, Color);
		return;
	}
	List->AddPolyline(Points, 4, Color, ImDrawFlags_Closed, 1.5f);
}

void ListFiles(xr_vector<shared_str>& Out, const char* Alias, const char* Mask, u32 Flags, u32 Limit)
{
	FS_FileSet Files;
	FS.file_list(Files, Alias, Flags, Mask);
	for (const FS_File& File : Files)
	{
		xr_string Path = File.name;
		std::replace(Path.begin(), Path.end(), '/', '\\');
		if (Path.size() <= Limit && Io::IniSafe(Path.c_str()))
		{
			Out.emplace_back(Path.c_str());
		}
	}
}

void SortNames(xr_vector<shared_str>& Names)
{
	std::sort(Names.begin(), Names.end(), [](const shared_str& Left, const shared_str& Right) { return xr_strcmp(Left, Right) < 0; });
}

void Ellipsis(const ImRect& Box, const char* Text, u32 Color)
{
	if (!Text || Box.Max.x <= Box.Min.x)
	{
		return;
	}
	ImGui::PushStyleColor(ImGuiCol_Text, Color);
	ImGui::RenderTextEllipsis(ImGui::GetWindowDrawList(), Box.Min, Box.Max, Box.Max.x, Text, nullptr, nullptr);
	ImGui::PopStyleColor();
}

u32 TokenRank(const char* Text, const char* Token, const char* End)
{
	u32 Best = 2;
	for (const char* At = Text; Text && Best && (At = ImStristr(At, nullptr, Token, End)) != nullptr; ++At)
	{
		Best = std::min(Best, At == Text || !isalnum(static_cast<u8>(At[-1])) ? 0u : 1u);
	}
	return Best;
}

u32 MatchRank(const char* Query, const char* Text, const char* Pack)
{
	u32 Total = 0;
	for (const char* Token = Query; *Token;)
	{
		const char* End = Token;
		while (*End && *End != ' ')
		{
			++End;
		}
		const u32 Best = End == Token ? 0u : std::min(TokenRank(Text, Token, End), std::max(TokenRank(Pack, Token, End), 1u));
		if (Best > 1)
		{
			return u32(-1);
		}
		Total += Best;
		Token = *End ? End + 1 : End;
	}
	return Total;
}
}

namespace Cinema
{
void Editor::DrawInspector(Player& Core)
{
	ViewRight = FLT_MAX;
	HighlightBone = nullptr;
	if (!Settings.ShowInspector)
	{
		return;
	}
	const ImGuiViewport* View = ImGui::GetMainViewport();
	ImGui::SetNextWindowPos(ImVec2(View->Pos.x + View->Size.x * 0.76f, View->WorkPos.y), ImGuiCond_FirstUseEver);
	ImGui::SetNextWindowSize(ImVec2(View->Size.x * 0.24f, View->Pos.y + View->Size.y * 0.72f - View->WorkPos.y), ImGuiCond_FirstUseEver);
	const bool IsVisible = ImGui::Begin("Inspector###CinemaInspector", &Settings.ShowInspector, ImGuiWindowFlags_NoNavInputs);
	IsOwnWindowFocused |= ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows);
	ViewRight = ImGui::GetWindowPos().x;
	std::erase_if(PinnedIds, [&Core](u32 Id) { return !Find(Core.Data, Id); });
	IsPinned = IsPinned && !PinnedIds.empty();
	xr_vector<u32>* const Lists[4] = { &InspectActors, &InspectCameras, &InspectLights, &InspectParticles };
	for (xr_vector<u32>* List : Lists)
	{
		List->clear();
	}
	for (u32 Id : IsPinned ? PinnedIds : Selected)
	{
		if (const Object* Item = Find(Core.Data, Id))
		{
			Lists[static_cast<u32>(Item->Type)]->push_back(Id);
		}
	}
	if (!IsVisible)
	{
		ImGui::End();
		return;
	}
	const bool HasKeys = Focus == FocusKind::Keys && !SelectedKeys.empty();
	ImGui::BeginDisabled(!HasKeys && Selected.empty());
	BeginRow("Offset", false, false);
	const bool IsChanged = OffsetField("##Offset", OffsetValue, Core.Data.Header.FrameRate);
	if (ImGui::IsItemActivated() && HasKeys)
	{
		IsOffsetKeys = true;
		IsDragCopy = false;
		BeginKeyMove(Core, "Offset keys");
	}
	else if (ImGui::IsItemActivated())
	{
		IsOffsetKeys = false;
		Undo.Begin("Offset clips");
		DragIds = Selected;
		CaptureStarts(Core.Data, false);
	}
	if (IsChanged)
	{
		IsOffsetKeys ? MoveKeys(Core, OffsetValue, 0.0f) : ShiftClips(Core, OffsetValue);
	}
	OffsetValue = ImGui::IsItemActive() ? OffsetValue : 0.0f;
	EndRow(KeyState::None);
	ImGui::EndDisabled();
	const bool HasLightPin = InspectActors.empty(), HasParticlePin = HasLightPin && InspectLights.empty(), HasCameraPin = HasParticlePin && InspectParticles.empty();
	if (HasCameraPin && InspectCameras.empty())
	{
		DrawSceneSection(Core);
	}
	if (!InspectActors.empty())
	{
		DrawActorSection(Core, InspectActors);
	}
	if (!InspectLights.empty())
	{
		DrawLightSection(Core, InspectLights, HasLightPin);
	}
	if (!InspectParticles.empty())
	{
		DrawParticleSection(Core, InspectParticles, HasParticlePin);
	}
	if (!InspectCameras.empty())
	{
		DrawCameraSection(Core, InspectCameras, HasCameraPin);
	}
	ImGui::End();
}

void Editor::DrawPresets()
{
	if (!Settings.ShowPresets)
	{
		return;
	}
	LoadPresets();
	const ImGuiViewport* View = ImGui::GetMainViewport();
	ImGui::SetNextWindowPos(ImVec2(View->Pos.x + View->Size.x * 0.62f, View->WorkPos.y), ImGuiCond_FirstUseEver);
	ImGui::SetNextWindowSize(ImVec2(View->Size.x * 0.14f, View->Pos.y + View->Size.y * 0.72f - View->WorkPos.y), ImGuiCond_FirstUseEver);
	const bool IsVisible = ImGui::Begin("Presets###CinemaPresets", &Settings.ShowPresets, ImGuiWindowFlags_NoNavInputs);
	IsOwnWindowFocused |= ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows);
	if (!IsVisible)
	{
		ImGui::End();
		return;
	}
	ImGui::SetNextItemWidth(-FLT_MIN);
	ImGui::InputTextWithHint("##Filter", "Filter", PresetFilter, sizeof(PresetFilter));
	PresetRows.clear();
	for (u32 Index = 0; Index < Presets.size(); ++Index)
	{
		if (!PresetFilter[0] || ImStristr(Presets[Index].Name.c_str(), nullptr, PresetFilter, nullptr))
		{
			PresetRows.push_back(Index);
		}
	}
	const bool IsListed = PresetSelected >= 0 && PresetSelected < static_cast<s32>(Presets.size());
	const auto BeginRename = [this]()
	{
		RenamePreset = PresetSelected;
		PresetFrames = 3;
		xr_sprintf(PresetBuffer, "%.127s", Presets[PresetSelected].Name.c_str());
	};
	bool ShouldOpenMenu = false, ShouldCommit = false;
	ImGui::BeginChild("##Presets");
	const float Right = ImGui::GetCursorPosX() + ImGui::GetContentRegionAvail().x - ImGui::CalcTextSize("(old)").x;
	ImGuiListClipper Clipper;
	Clipper.Begin(static_cast<s32>(PresetRows.size()));
	while (Clipper.Step())
	{
		for (s32 Row = Clipper.DisplayStart; Row < Clipper.DisplayEnd; ++Row)
		{
			const s32 Index = static_cast<s32>(PresetRows[Row]);
			ImGui::PushID(Index);
			if (Index == RenamePreset)
			{
				if (PresetFrames == 3)
				{
					ImGui::SetKeyboardFocusHere();
				}
				ImGui::SetNextItemWidth(-FLT_MIN);
				ImGui::InputText("##Rename", PresetBuffer, sizeof(PresetBuffer), ImGuiInputTextFlags_AutoSelectAll);
				ShouldCommit = ImGui::IsItemDeactivatedAfterEdit();
				if (PresetFrames)
				{
					--PresetFrames;
				}
				else if (!ImGui::IsItemActive() && !ShouldCommit)
				{
					RenamePreset = -1;
				}
			}
			else
			{
				const bool IsClicked = ImGui::Selectable("##Preset", PresetSelected == Index, ImGuiSelectableFlags_AllowDoubleClick);
				const ImVec2 Min = ImGui::GetItemRectMin(), Max = ImGui::GetItemRectMax();
				InspectorUi::Ellipsis(ImRect(Min.x, Min.y, Max.x - (Presets[Index].IsOld ? ImGui::CalcTextSize("(old)").x + ImGui::GetStyle().ItemSpacing.x : 0.0f), Max.y), Presets[Index].Name.c_str(), ImGui::GetColorU32(ImGuiCol_Text));
				if (IsClicked)
				{
					PresetSelected = Index;
					if (ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
					{
						BeginRename();
					}
				}
				if (ImGui::IsItemClicked(ImGuiMouseButton_Right))
				{
					PresetSelected = Index;
					ShouldOpenMenu = true;
				}
				if (Presets[Index].IsOld)
				{
					ImGui::SameLine(Right);
					ImGui::TextDisabled("(old)");
				}
			}
			ImGui::PopID();
		}
	}
	if (ShouldOpenMenu)
	{
		ImGui::OpenPopup("##PresetMenu");
	}
	if (ImGui::BeginPopup("##PresetMenu"))
	{
		if (ImGui::MenuItem("Rename", nullptr, false, IsListed))
		{
			BeginRename();
		}
		if (ImGui::MenuItem("Delete...", nullptr, false, IsListed))
		{
			Modal = ModalKind::DeletePreset;
		}
		ImGui::EndPopup();
	}
	ImGui::EndChild();
	ImGui::End();
	if (!ShouldCommit || RenamePreset < 0 || RenamePreset >= static_cast<s32>(Presets.size()))
	{
		return;
	}
	const s32 Renamed = std::exchange(RenamePreset, -1);
	const s32 Taken = FindPreset(PresetBuffer);
	if (Taken == Renamed)
	{
		return;
	}
	if (!PresetBuffer[0] || Taken >= 0)
	{
		SetStatus(true, "Enter a preset name that is not taken.");
		return;
	}
	xr_vector<ActorPreset> Next = Presets;
	const xr_string Old = Next[Renamed].Name;
	Next[Renamed].Name = PresetBuffer;
	Next[Renamed].IsOld = false;
	if (StorePresets(std::move(Next), Old))
	{
		PresetSelected = FindPreset(PresetBuffer);
	}
}

void Editor::DrawHeader(Player& Core, const xr_vector<u32>& Ids, bool HasPin)
{
	const Object& Primary = *Find(Core.Data, Ids[0]);
	const char* Type = TypeNames[static_cast<u32>(Primary.Type)];
	const ImGuiStyle& Style = ImGui::GetStyle();
	const float Right = ImGui::GetCursorPosX() + ImGui::GetContentRegionAvail().x;
	const float PinWidth = HasPin ? ImGui::CalcTextSize("Pin").x + 2.0f * Style.FramePadding.x : 0.0f;
	ImGui::PushID(Type);
	float Color[3] = {};
	for (u32 Channel = 0; Channel < 3; ++Channel)
	{
		Color[Channel] = static_cast<float>((Primary.Color >> (Channel * 8)) & 255u) / 255.0f;
	}
	if (ImGui::ColorEdit3("##Colour", Color, ImGuiColorEditFlags_NoInputs | ImGuiColorEditFlags_NoLabel | ImGuiColorEditFlags_NoTooltip))
	{
		const auto Byte = [](float Value) { return static_cast<u32>(std::clamp(Value, 0.0f, 1.0f) * 255.0f + 0.5f); };
		for (u32 Id : Ids)
		{
			Edit(Core, Id, "Colour")->Color = Rgb(Byte(Color[0]), Byte(Color[1]), Byte(Color[2]));
		}
	}
	ImGui::SameLine();
	if (Ids.size() > 1)
	{
		ImGui::AlignTextToFramePadding();
		ImGui::Text("%s x%u", Type, static_cast<u32>(Ids.size()));
	}
	else
	{
		if (ImGui::GetActiveID() != ImGui::GetID("##Name"))
		{
			xr_sprintf(NameEdit, "%.127s", Primary.Name.c_str());
		}
		ImGui::PushStyleColor(ImGuiCol_Text, Primary.IsNameLocked ? OverrideColor : Style.Colors[ImGuiCol_Text]);
		ImGui::SetNextItemWidth(std::max(Right - ImGui::GetCursorPosX() - PinWidth - ImGui::CalcTextSize(Type).x - 2.0f * Style.ItemSpacing.x, 1.0f));
		ImGui::InputText("##Name", NameEdit, sizeof(NameEdit));
		ImGui::PopStyleColor();
		if (ImGui::IsItemDeactivatedAfterEdit() && Primary.Name != NameEdit)
		{
			Object* Item = Edit(Core, Primary.Id, "Rename");
			Item->Name = NameEdit;
			Item->IsNameLocked = true;
		}
		if (ImGui::IsItemHovered() && ImGui::IsMouseReleased(ImGuiMouseButton_Right))
		{
			ImGui::OpenPopup("##NameMenu");
		}
		if (ImGui::BeginPopup("##NameMenu"))
		{
			if (ImGui::MenuItem("Reset", nullptr, false, Primary.IsNameLocked))
			{
				Object* Item = Edit(Core, Primary.Id, "Reset name");
				Item->IsNameLocked = false;
				Item->Name = DefaultName(*Item);
			}
			ImGui::EndPopup();
		}
		ImGui::SameLine();
		ImGui::TextDisabled("%s", Type);
	}
	if (HasPin)
	{
		ImGui::SameLine(Right - PinWidth);
		ImGui::PushStyleColor(ImGuiCol_Button, Style.Colors[IsPinned ? ImGuiCol_ButtonActive : ImGuiCol_FrameBg]);
		if (ImGui::Button("Pin"))
		{
			IsPinned = !IsPinned;
			PinnedIds = Selected;
		}
		ImGui::PopStyleColor();
	}
	ImGui::Separator();
	ImGui::PopID();
}

void Editor::DrawSceneSection(Player& Core)
{
	SceneHeader& Header = Core.Data.Header;
	ImGui::TextUnformatted("Scene");
	ImGui::Separator();
	const RowAction Action = BeginRow("Frame rate", Header.FrameRate != 30, false);
	s32 Choice = static_cast<s32>(std::find(std::begin(InspectorUi::Rates), std::end(InspectorUi::Rates), Header.FrameRate) - std::begin(InspectorUi::Rates));
	if (ImGui::Combo("##Rate", &Choice, "24\0" "25\0" "30\0" "60\0") || Action == RowAction::Reset)
	{
		Undo.Begin("Frame rate");
		Undo.TouchHeader(Core.Data);
		Header.FrameRate = Action == RowAction::Reset ? 30 : InspectorUi::Rates[std::clamp(Choice, 0, 3)];
	}
	EndRow(KeyState::None);
}

void Editor::DrawActorSection(Player& Core, const xr_vector<u32>& Ids)
{
	const Object& Primary = *Find(Core.Data, Ids[0]);
	const ActorData& Setup = Primary.Actor;
	const float Spacing = ImGui::GetStyle().ItemInnerSpacing.x;
	shared_str Source, Name;
	DrawHeader(Core, Ids, true);
	BeginRow("Preset", false, false);
	ImGui::SetNextItemWidth(std::max(RowWidth - ImGui::CalcTextSize("Save as").x - 2.0f * ImGui::GetStyle().FramePadding.x - Spacing, 1.0f));
	if (Chooser(Core, ChooserKind::Preset, "##Preset", "Apply preset...", Ids, Source, Name))
	{
		ApplyPreset(Core, Ids, Source.c_str());
	}
	ImGui::SameLine(0.0f, Spacing);
	ImGui::BeginDisabled(Ids.size() != 1 || !Setup.Visual.size());
	if (ImGui::Button("Save as"))
	{
		LoadPresets();
		PresetDraft.Setup = Setup;
		PresetDraft.Setup.Position.set(0.0f, 0.0f, 0.0f);
		PresetDraft.Setup.Rotation.set(0.0f, 0.0f, 0.0f);
		float Lead = MaxTime;
		for (u8 Layer = 0; !Setup.IsRelative && Layer <= Setup.Layers.size(); ++Layer)
		{
			for (const ActorBlock& Block : BlocksOf(Setup, Layer))
			{
				Lead = std::min(Lead, Block.Start);
			}
		}
		for (u8 Layer = 0; Lead < MaxTime && Layer <= Setup.Layers.size(); ++Layer)
		{
			for (ActorBlock& Block : BlocksOf(PresetDraft.Setup, Layer))
			{
				Block.Start -= Lead;
			}
		}
		xr_sprintf(NameBuffer, "%.127s", ModelName(Setup.Visual).c_str());
		Modal = Modal == ModalKind::None ? ModalKind::SavePreset : Modal;
	}
	ImGui::EndDisabled();
	EndRow(KeyState::None);
	BeginRow("Model", false, false);
	if (Chooser(Core, ChooserKind::Model, "##Model", Setup.Visual.size() ? Setup.Visual.c_str() : "None", Ids, Source, Name))
	{
		for (u32 Id : Ids)
		{
			Object* Item = Edit(Core, Id, "Model");
			Item->Actor.Visual = Source;
			Item->Name = Item->IsNameLocked ? Item->Name : ModelName(Source);
			Refit(Core, *Item);
		}
	}
	EndRow(KeyState::None);
	RowAction Action = BeginRow("Item", Setup.Item.size() != 0, false);
	if (Chooser(Core, ChooserKind::Item, "##Item", Setup.Item.size() ? Setup.Item.c_str() : "None", Ids, Source, Name) || Action == RowAction::Reset)
	{
		for (u32 Id : Ids)
		{
			Edit(Core, Id, "Item")->Actor.Item = Action == RowAction::Reset ? shared_str() : Source;
		}
	}
	EndRow(KeyState::None);
	ClipRow(Core, Ids);
	if (ImGui::CollapsingHeader("Transform", ImGuiTreeNodeFlags_DefaultOpen))
	{
		DrawGizmoRow(true);
		ParamRow(Core, Ids, -1, Param::Position, 0, 0.01f, "%.2f", 0, 0.0f);
		ParamRow(Core, Ids, -1, Param::Rotation, 0, 0.5f, "%.1f", 6, 0.0f);
		ParamRow(Core, Ids, -1, Param::Scale, 0, 0.01f, "%.2f", 7, 1.0f);
		KeyRow(Core, Ids);
	}
	DrawAttach(Core, Ids, -1);
	if (ImGui::CollapsingHeader("Animation", ImGuiTreeNodeFlags_DefaultOpen))
	{
		ParamRow(Core, Ids, -1, Param::TimeScale, 0, 0.01f, "%.2f", 1, 1.0f);
		BeginRow("Add", false, false);
		if (Chooser(Core, ChooserKind::Animation, "##Add", "+ Animation...", Ids, Source, Name))
		{
			for (u32 Id : Ids)
			{
				const Object& Item = *Find(Core.Data, Id);
				const u8 Layer = Id == FocusObject && FocusLayer <= Item.Actor.Layers.size() ? FocusLayer : 0;
				const xr_vector<ActorBlock>& Blocks = BlocksOf(Item.Actor, Layer);
				const ActorRuntime* Runtime = Core.FindActor(Id);
				const bool IsPlaced = !Blocks.empty() && Runtime && Runtime->IsComplete() && Runtime->Track(Layer).Starts.size() == Blocks.size();
				const float End = IsPlaced ? TimeAt(Item, *Runtime, Runtime->Track(Layer).Starts.back() + Runtime->BlockDuration(Blocks.back())) : Item.Span.Start;
				AddAnimationAt(Core, Id, Layer, std::max(Core.Time, End), Source, Name);
			}
		}
		EndRow(KeyState::None);
	}
	if (Ids.size() == 1 && ImGui::CollapsingHeader("Rows", ImGuiTreeNodeFlags_DefaultOpen))
	{
		const float Size = ImGui::GetFrameHeight();
		ChooserOwners.assign(1, Primary.Id);
		for (u8 Layer = 0; Layer <= Setup.Layers.size(); ++Layer)
		{
			string32 Caption;
			xr_sprintf(Caption, "Animation %u", Layer + 1u);
			const bool IsMuted = RowOf(Setup, Layer).IsMuted, IsAdd = RowOf(Setup, Layer).Mode == RowMode::Add;
			ImGui::PushID(Layer);
			BeginRow(Caption, false, false);
			PartsCombo(Core, Primary, Layer, RowWidth - 3.0f * (Size + Spacing));
			ImGui::SameLine(0.0f, Spacing);
			ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(IsAdd ? ImGuiCol_ButtonActive : ImGuiCol_FrameBg));
			const bool ShouldSwitch = ImGui::Button("+", ImVec2(Size, 0.0f));
			ImGui::PopStyleColor();
			ImGui::SetItemTooltip("Add");
			ImGui::SameLine(0.0f, Spacing);
			ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(IsMuted ? ImGuiCol_ButtonActive : ImGuiCol_FrameBg));
			const bool ShouldMute = ImGui::Button("M", ImVec2(Size, 0.0f));
			ImGui::PopStyleColor();
			ImGui::SetItemTooltip("Mute");
			ImGui::SameLine(0.0f, Spacing);
			ImGui::BeginDisabled(Setup.Layers.empty());
			const bool ShouldRemove = ImGui::Button("x", ImVec2(Size, 0.0f));
			ImGui::EndDisabled();
			ImGui::SetItemTooltip("Remove row");
			EndRow(KeyState::None);
			if (ShouldSwitch)
			{
				SetRowMode(Core, Primary.Id, Layer, !IsAdd);
			}
			if (!ShouldRemove)
			{
				ParamRow(Core, ChooserOwners, -1, Param::LayerWeight, Layer, 0.01f, "%.2f", 1, 1.0f);
			}
			ImGui::PopID();
			if (ShouldMute)
			{
				Object& Item = *Edit(Core, Primary.Id, IsMuted ? "Unmute row" : "Mute row");
				RowOf(Item.Actor, Layer).IsMuted = !IsMuted;
				Refit(Core, Item);
			}
			if (ShouldRemove)
			{
				RemoveLayer(Core, Primary.Id, Layer);
				break;
			}
		}
		ImGui::BeginDisabled(Setup.Layers.size() >= MaxActorLayers);
		if (ImGui::Button("+ Row", ImVec2(-FLT_MIN, 0.0f)))
		{
			AddLayer(Core, Primary.Id);
		}
		ImGui::EndDisabled();
	}
	DrawBlockSection(Core, Primary);
}

bool Editor::PartsCombo(Player& Core, const Object& Target, u8 Layer, float Width)
{
	const ActorRuntime* Runtime = Core.FindActor(Target.Id);
	const xr_vector<shared_str>& Parts = RowOf(Target.Actor, Layer).Parts;
	const auto IsKnown = [Runtime](const char* Name)
	{
		for (u16 Part = 0; Runtime && Part < MAX_PARTS; ++Part)
		{
			if (Runtime->PartName(Part) && !xr_strcmp(Runtime->PartName(Part), Name))
			{
				return true;
			}
		}
		return false;
	};
	string512 Preview = {};
	u32 Used = 0;
	for (const shared_str& Part : Parts)
	{
		Used += static_cast<u32>(std::max(std::snprintf(Preview + Used, sizeof(Preview) - Used, "%s%s", Used ? ", " : "", Part.c_str()), 0));
		Used = std::min<u32>(Used, sizeof(Preview) - 1);
	}
	const bool IsEmpty = Runtime && Runtime->IsComplete() && !Runtime->Track(Layer).Mask;
	const char* Toggled = nullptr;
	ImGui::SetNextItemWidth(std::max(Width, 1.0f));
	ImGui::PushStyleColor(ImGuiCol_Text, IsEmpty ? ErrorColor : ImGui::GetStyleColorVec4(ImGuiCol_Text));
	const bool IsOpen = ImGui::BeginCombo("##Parts", IsEmpty && Parts.empty() ? "No body parts" : !Layer && Parts.empty() ? "Whole body" : Preview);
	ImGui::PopStyleColor();
	ImGui::SetItemTooltip("Body parts");
	if (!IsOpen)
	{
		return false;
	}
	for (u16 Part = 0; Runtime && Part < MAX_PARTS; ++Part)
	{
		const char* Name = Runtime->PartName(Part);
		const bool IsOn = Name && std::any_of(Parts.begin(), Parts.end(), [Name](const shared_str& Wanted) { return !xr_strcmp(Wanted.c_str(), Name); });
		if (Name && ImGui::Selectable(Name, IsOn, ImGuiSelectableFlags_NoAutoClosePopups))
		{
			Toggled = Name;
		}
	}
	for (const shared_str& Part : Parts)
	{
		ImGui::PushStyleColor(ImGuiCol_Text, ErrorColor);
		if (!IsKnown(Part.c_str()) && ImGui::Selectable(Part.c_str(), true, ImGuiSelectableFlags_NoAutoClosePopups))
		{
			Toggled = Part.c_str();
		}
		ImGui::PopStyleColor();
	}
	ImGui::EndCombo();
	if (!Toggled)
	{
		return false;
	}
	const shared_str Name = Toggled;
	xr_vector<shared_str>& Edited = RowOf(Edit(Core, Target.Id, "Body parts")->Actor, Layer).Parts;
	const auto Found = std::find(Edited.begin(), Edited.end(), Name);
	if (Found != Edited.end())
	{
		Edited.erase(Found);
		return true;
	}
	if (Edited.size() >= MAX_PARTS)
	{
		std::erase_if(Edited, [&IsKnown](const shared_str& Part) { return !IsKnown(Part.c_str()); });
	}
	if (Edited.size() < MAX_PARTS)
	{
		Edited.push_back(Name);
	}
	return true;
}

void Editor::DrawBlockSection(Player& Core, const Object& Target)
{
	u8 Layer = 0;
	const s32 Found = FocusedBlock(Core, Target, &Layer);
	const ActorRuntime* Runtime = Core.FindActor(Target.Id);
	const xr_vector<ActorBlock>& Lineup = BlocksOf(Target.Actor, Layer);
	const s32 Index = Found < static_cast<s32>(Lineup.size()) ? Found : -1;
	ActorBlock Value = Index >= 0 ? Lineup[Index] : ActorBlock {};
	string256 Title;
	xr_sprintf(Title, "%.128s###Block", Index >= 0 && Value.Name.size() ? Value.Name.c_str() : "Block");
	if (!ImGui::CollapsingHeader(Title, ImGuiTreeNodeFlags_DefaultOpen))
	{
		return;
	}
	const float Rate = static_cast<float>(Core.Data.Header.FrameRate), Spacing = ImGui::GetStyle().ItemInnerSpacing.x;
	const float Length = Runtime ? Runtime->MotionLength(Value.Source, Value.Name) : 0.0f;
	const float Last = Length > 0.0f ? Length : MaxTime;
	const auto Frame = [Rate](float Seconds) { return std::round(Seconds * Rate) / Rate; };
	shared_str Source, Name;
	const LayerRuntime* Track = Runtime ? &Runtime->Track(Layer) : nullptr;
	const bool IsPlaced = Index >= 0 && Track && Runtime->IsComplete() && Track->Starts.size() == Lineup.size();
	const auto Span = IsPlaced ? std::find_if(Track->Spans.begin(), Track->Spans.end(), [Index](const BlockSpan& Item) { return static_cast<s32>(Item.Block) == Index; }) : xr_vector<BlockSpan>::const_iterator();
	float Begin = Target.Span.Start + Value.Start, Fade = IsPlaced && Span != Track->Spans.end() ? Span->Overlap : 0.0f;
	ImGui::BeginDisabled(Index < 0 || Target.Actor.IsRelative);
	BeginRow("Start", false, false);
	const bool IsMoved = ImGui::DragFloat("##Start", &Begin, 0.01f, 0.0f, MaxTime, "%.3f s", ImGuiSliderFlags_AlwaysClamp);
	EndRow(KeyState::None);
	ChooserOwners.assign(1, Target.Id);
	BeginRow("Animation", false, false);
	if (Chooser(Core, ChooserKind::Animation, "##Animation", Value.Name.size() ? Value.Name.c_str() : "None", ChooserOwners, Source, Name))
	{
		Value.Source = Source;
		Value.Name = Name;
		Value.In = Value.Out = 0.0f;
	}
	EndRow(KeyState::None);
	float In = Value.In, Out = Value.Out > 0.0f ? Value.Out : Last;
	RowAction Action = BeginRow("Range", Value.In > 0.0f || (Value.Out > 0.0f && Value.Out < Length), false);
	const float Half = (RowWidth - Spacing) * 0.5f;
	ImGui::SetNextItemWidth(Half);
	if (ImGui::DragFloat("##In", &In, 0.01f, 0.0f, Out, "%.3f", ImGuiSliderFlags_AlwaysClamp))
	{
		Value.In = Frame(In);
	}
	ImGui::SameLine(0.0f, Spacing);
	ImGui::SetNextItemWidth(Half);
	if (ImGui::DragFloat("##Out", &Out, 0.01f, In, Last, "%.3f", ImGuiSliderFlags_AlwaysClamp))
	{
		Value.Out = Length > 0.0f && Out >= Length - 0.5f / Rate ? 0.0f : Frame(Out);
	}
	if (Action == RowAction::Reset)
	{
		Value.In = Value.Out = 0.0f;
	}
	EndRow(KeyState::None);
	float Speed = Value.Speed;
	Action = BeginRow("Speed", Value.Speed != 1.0f, false);
	if (ImGui::DragFloat("##Speed", &Speed, 0.01f, -4.0f, 4.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp))
	{
		Value.Speed = std::fabs(Speed) >= 0.05f ? Speed : Speed < Value.Speed ? -0.05f : 0.05f;
	}
	Value.Speed = Action == RowAction::Reset ? 1.0f : Value.Speed;
	EndRow(KeyState::None);
	Action = BeginRow("Loops", Value.Loops != 1.0f, false);
	ImGui::DragFloat("##Loops", &Value.Loops, 0.01f, 1.0f, MaxLoops, "%.2f", ImGuiSliderFlags_AlwaysClamp);
	Value.Loops = Action == RowAction::Reset ? 1.0f : Value.Loops;
	EndRow(KeyState::None);
	Action = BeginRow("Hold", Value.Hold > 0.0f, false);
	ImGui::DragFloat("##Hold", &Value.Hold, 0.01f, 0.0f, MaxTime, "%.2f s", ImGuiSliderFlags_AlwaysClamp);
	Value.Hold = Action == RowAction::Reset ? 0.0f : Value.Hold;
	EndRow(KeyState::None);
	Action = BeginRow("Fade", Value.FadeIn > 0.0f || Value.FadeOut > 0.0f, false);
	ImGui::SetNextItemWidth(Half);
	ImGui::DragFloat("##FadeIn", &Value.FadeIn, 0.01f, 0.0f, MaxTime, "%.2f s", ImGuiSliderFlags_AlwaysClamp);
	ImGui::SetItemTooltip("Fade in");
	ImGui::SameLine(0.0f, Spacing);
	ImGui::SetNextItemWidth(Half);
	ImGui::DragFloat("##FadeOut", &Value.FadeOut, 0.01f, 0.0f, MaxTime, "%.2f s", ImGuiSliderFlags_AlwaysClamp);
	ImGui::SetItemTooltip("Fade out");
	Value.FadeIn = Action == RowAction::Reset ? 0.0f : Value.FadeIn;
	Value.FadeOut = Action == RowAction::Reset ? 0.0f : Value.FadeOut;
	EndRow(KeyState::None);
	Action = BeginRow("Crossfade", Value.BlendEase != Interp::EaseInOut, false);
	ImGui::SetNextItemWidth(Half);
	ImGui::BeginDisabled();
	ImGui::DragFloat("##Fade", &Fade, 0.0f, 0.0f, 0.0f, "%.3f s");
	ImGui::EndDisabled();
	ImGui::SameLine(0.0f, Spacing);
	ImGui::SetNextItemWidth(Half);
	s32 Ease = static_cast<s32>(Value.BlendEase);
	if (ImGui::Combo("##Ease", &Ease, InspectorUi::Modes, 4))
	{
		Value.BlendEase = static_cast<Interp>(Ease);
	}
	Value.BlendEase = Action == RowAction::Reset ? Interp::EaseInOut : Value.BlendEase;
	EndRow(KeyState::None);
	ImGui::EndDisabled();
	Value.Start = IsMoved ? std::clamp(Frame(Begin) - Target.Span.Start, 0.0f, MaxTime) : Value.Start;
	if (Index < 0 || Value == Lineup[Index])
	{
		return;
	}
	CutBlocks = Lineup;
	CutBlocks[Index] = Value;
	std::stable_sort(CutBlocks.begin(), CutBlocks.end(), [](const ActorBlock& A, const ActorBlock& B) { return A.Start < B.Start; });
	if (!Runtime || !IsTrackValid(Target, *Runtime, CutBlocks))
	{
		SetStatus(true, "Clips may overlap only two at a time and never lie inside another.");
		return;
	}
	Object* Item = Edit(Core, Target.Id, IsMoved ? "Move block" : "Edit block");
	BlocksOf(Item->Actor, Layer) = CutBlocks;
	FocusBlock = static_cast<s32>(std::find(CutBlocks.begin(), CutBlocks.end(), Value) - CutBlocks.begin());
	Refit(Core, *Item);
}

void Editor::DrawLightSection(Player& Core, const xr_vector<u32>& Ids, bool HasPin)
{
	const Object& Primary = *Find(Core.Data, Ids[0]);
	const LightData& Setup = Primary.Light;
	shared_str Source, Name;
	ImGui::PushID("Light");
	DrawHeader(Core, Ids, HasPin);
	RowAction Action = BeginRow("Type", Setup.Kind != LightKind::Point, false);
	s32 Kind = static_cast<s32>(Setup.Kind);
	if (ImGui::Combo("##Kind", &Kind, "Point\0" "Spot\0") || Action == RowAction::Reset)
	{
		for (u32 Id : Ids)
		{
			Object* Item = Edit(Core, Id, "Type");
			Item->Light.Kind = Action == RowAction::Reset ? LightKind::Point : static_cast<LightKind>(Kind);
			Item->Name = Item->IsNameLocked ? Item->Name : DefaultName(*Item);
		}
	}
	EndRow(KeyState::None);
	ClipRow(Core, Ids);
	if (ImGui::CollapsingHeader("Transform", ImGuiTreeNodeFlags_DefaultOpen))
	{
		DrawGizmoRow(true, true, IsParamShown(Primary, Param::Rotation));
		ParamRow(Core, Ids, -1, Param::Position, 0, 0.01f, "%.2f", 0, 0.0f);
		if (IsParamShown(Primary, Param::Rotation))
		{
			ParamRow(Core, Ids, -1, Param::Rotation, 0, 0.5f, "%.1f", 6, 0.0f);
		}
		KeyRow(Core, Ids);
	}
	DrawAttach(Core, Ids, -1);
	if (ImGui::CollapsingHeader("Light", ImGuiTreeNodeFlags_DefaultOpen))
	{
		ParamRow(Core, Ids, -1, Param::Color, 0, 0.01f, "%.2f", 7, 1.0f);
		ParamRow(Core, Ids, -1, Param::Intensity, 0, 0.1f, "%.2f", 1, 1.0f);
		ParamRow(Core, Ids, -1, Param::Range, 0, 0.1f, "%.2f", 1, 8.0f);
		if (IsParamShown(Primary, Param::Cone))
		{
			ParamRow(Core, Ids, -1, Param::Cone, 0, 0.5f, "%.1f", 1, 60.0f);
		}
	}
	if (GroupHeader(Core, Ids, "Shadow", "Cast shadows", &LightData::HasShadow))
	{
		if (Setup.Kind == LightKind::Spot)
		{
			Action = BeginRow("Texture", Setup.Texture.size() != 0, false);
			if (Chooser(Core, ChooserKind::Texture, "##Texture", Setup.Texture.size() ? Setup.Texture.c_str() : "None", Ids, Source, Name) || Action == RowAction::Reset)
			{
				for (u32 Id : Ids)
				{
					Edit(Core, Id, "Texture")->Light.Texture = Action == RowAction::Reset ? shared_str() : Source;
				}
			}
			EndRow(KeyState::None);
		}
		ParamRow(Core, Ids, -1, Param::VirtualSize, 0, 0.01f, "%.2f", 1, 0.1f);
	}
	if (GroupHeader(Core, Ids, "Volumetric", "Volumetric light", &LightData::IsVolumetric))
	{
		ParamRow(Core, Ids, -1, Param::VolumeIntensity, 0, 0.01f, "%.2f", 1, 1.0f);
		ParamRow(Core, Ids, -1, Param::VolumeDistance, 0, 0.01f, "%.2f", 1, 1.0f);
		ParamRow(Core, Ids, -1, Param::VolumeQuality, 0, 0.01f, "%.2f", 1, 1.0f);
	}
	ImGui::PopID();
}

void Editor::DrawParticleSection(Player& Core, const xr_vector<u32>& Ids, bool HasPin)
{
	const Object& Primary = *Find(Core.Data, Ids[0]);
	const ParticleData& Setup = Primary.Particle;
	shared_str Source, Name;
	ImGui::PushID("Particle");
	DrawHeader(Core, Ids, HasPin);
	BeginRow("Effect", false, false);
	if (Chooser(Core, ChooserKind::Effect, "##Effect", Setup.Effect.size() ? Setup.Effect.c_str() : "None", Ids, Source, Name))
	{
		for (u32 Id : Ids)
		{
			Object* Item = Edit(Core, Id, "Effect");
			Item->Particle.Effect = Source;
			Item->Name = Item->IsNameLocked ? Item->Name : DefaultName(*Item);
		}
	}
	EndRow(KeyState::None);
	ClipRow(Core, Ids);
	if (ImGui::CollapsingHeader("Transform", ImGuiTreeNodeFlags_DefaultOpen))
	{
		DrawGizmoRow(true);
		ParamRow(Core, Ids, -1, Param::Position, 0, 0.01f, "%.2f", 0, 0.0f);
		ParamRow(Core, Ids, -1, Param::Rotation, 0, 0.5f, "%.1f", 6, 0.0f);
		ParamRow(Core, Ids, -1, Param::Size, 0, 0.01f, "%.2f", 1, 1.0f);
		KeyRow(Core, Ids);
	}
	DrawAttach(Core, Ids, -1);
	if (ImGui::CollapsingHeader("Playback", ImGuiTreeNodeFlags_DefaultOpen))
	{
		ParamRow(Core, Ids, -1, Param::Speed, 0, 0.01f, "%.2f", 1, 1.0f);
		const RowAction Action = BeginRow("Retrigger", Setup.ShouldRetrigger, false);
		bool ShouldRetrigger = Setup.ShouldRetrigger;
		if (ImGui::Checkbox("##Retrigger", &ShouldRetrigger) || Action == RowAction::Reset)
		{
			for (u32 Id : Ids)
			{
				Edit(Core, Id, "Retrigger")->Particle.ShouldRetrigger = ShouldRetrigger && Action != RowAction::Reset;
			}
		}
		EndRow(KeyState::None);
	}
	ImGui::PopID();
}

void Editor::ClipRow(Player& Core, const xr_vector<u32>& Ids)
{
	const Object& Primary = *Find(Core.Data, Ids[0]);
	const bool IsActor = Primary.Type == ObjectType::Actor;
	const u32 Rate = Core.Data.Header.FrameRate;
	const float Spacing = ImGui::GetStyle().ItemInnerSpacing.x;
	const bool IsDerived = IsActor && !Primary.Actor.IsRelative && HasClips(Primary.Actor);
	const RowAction Action = BeginRow("Clip", IsActor && !IsDerived && Primary.Span.IsLengthLocked, false);
	const float Half = (RowWidth - Spacing) * 0.5f;
	float Start = Primary.Span.Start, Length = Primary.Span.Length;
	ImGui::SetNextItemWidth(Half);
	if (TimeField("##Start", Start, 0.0f, MaxTime, Rate))
	{
		for (u32 Id : Ids)
		{
			Object* Item = Edit(Core, Id, "Move clip");
			Item->Span.Start = std::clamp(Start, 0.0f, MaxTime - Item->Span.Length);
		}
	}
	ImGui::SameLine(0.0f, Spacing);
	ImGui::SetNextItemWidth(Half);
	ImGui::BeginDisabled(IsDerived);
	const bool IsResized = TimeField("##Length", Length, MinLength, MaxTime, Rate);
	ImGui::EndDisabled();
	if (IsResized)
	{
		for (u32 Id : Ids)
		{
			Object* Item = Edit(Core, Id, "Resize clip");
			Item->Span.Length = std::clamp(Length, MinLength, std::max(MaxTime - Item->Span.Start, MinLength));
			Item->Span.IsLengthLocked = Item->Span.IsLengthLocked || IsActor;
		}
	}
	for (u32 Id : Ids)
	{
		if (Action == RowAction::Reset)
		{
			Edit(Core, Id, "Reset length")->Span.IsLengthLocked = false;
			Core.RequestFit(Id);
		}
	}
	EndRow(KeyState::None);
}

bool Editor::GroupHeader(Player& Core, const xr_vector<u32>& Ids, const char* Title, const char* Label, bool LightData::*Field)
{
	if (!ImGui::CollapsingHeader(Title, ImGuiTreeNodeFlags_DefaultOpen))
	{
		return false;
	}
	bool IsOn = Find(Core.Data, Ids[0])->Light.*Field;
	const bool IsReset = BeginRow(Label, IsOn, false) == RowAction::Reset;
	IsOn = IsOn && !IsReset;
	if (ImGui::Checkbox("##On", &IsOn) || IsReset)
	{
		for (u32 Id : Ids)
		{
			LightData& Light = Edit(Core, Id, Title)->Light;
			Light.*Field = IsOn;
			Light.IsVolumetric = Light.IsVolumetric && (Light.HasShadow || Field == &LightData::IsVolumetric);
			Light.HasShadow = Light.HasShadow || Light.IsVolumetric;
		}
	}
	EndRow(KeyState::None);
	return true;
}

void Editor::PickColor(Player& Core)
{
	if (!IsPickingColor)
	{
		return;
	}
	ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
	IsPickingColor = false;
	const bool IsSampling = ImGui::IsMouseClicked(ImGuiMouseButton_Left) && IsViewportFree();
	IsPickingColor = !IsSampling && !ImGui::IsKeyPressed(ImGuiKey_Escape, false) && !ImGui::IsMouseClicked(ImGuiMouseButton_Right);
	if (!IsSampling)
	{
		return;
	}
	const ImGuiViewport* Main = ImGui::GetMainViewport();
	const ImVec2 Mouse = ImGui::GetIO().MousePos;
	const float X = (Mouse.x - Main->Pos.x) / Main->Size.x, Y = (Mouse.y - Main->Pos.y) / Main->Size.y;
	IRHIDevice* Rhi = GRHI ? GRHI->DevicePtr : nullptr;
	const u64 Capacity = (static_cast<u64>(Device.TargetWidth) + 64) * (static_cast<u64>(Device.TargetHeight) + 64) * 4;
	const bool CanRead = Rhi && Rhi->SwapChainRTV && Capacity <= u32(-1) && X >= 0.0f && X < 1.0f && Y >= 0.0f && Y < 1.0f;
	xr_vector<u8> Pixels(CanRead ? static_cast<size_t>(Capacity) : 0);
	u32 Width = 0, Height = 0, Pitch = 0;
	const bool IsRead = CanRead && Rhi->ReadRenderTargetPixels(Rhi->SwapChainRTV, Pixels.data(), static_cast<u32>(Pixels.size()), Width, Height, Pitch) && Width && Height;
	const size_t Offset = IsRead ? static_cast<size_t>(std::min(static_cast<u32>(Y * Height), Height - 1)) * Pitch + static_cast<size_t>(std::min(static_cast<u32>(X * Width), Width - 1)) * 4 : 0;
	if (!IsRead || Offset + 3 >= Pixels.size())
	{
		SetStatus(true, "The colour under the cursor could not be read.");
		return;
	}
	const float Picked[3] = { Pixels[Offset + 2] / 255.0f, Pixels[Offset + 1] / 255.0f, Pixels[Offset] / 255.0f };
	for (u32 Id : PickColorIds)
	{
		const Object* Item = Find(Core.Data, Id);
		for (u8 Component = 0; Item && Item->Type == ObjectType::Light && Component < 3; ++Component)
		{
			WriteParam(Core, *Edit(Core, Id, "Pick colour"), -1, Param::Color, 0, Component, Picked[Component]);
		}
	}
}

void Editor::DrawCameraSection(Player& Core, const xr_vector<u32>& Ids, bool HasPin)
{
	const Object& Rig = *Find(Core.Data, Ids[0]);
	const u32 Rate = Core.Data.Header.FrameRate;
	const float Spacing = ImGui::GetStyle().ItemInnerSpacing.x;
	ImGui::PushID("Camera");
	DrawHeader(Core, Ids, HasPin);
	if (ImGui::CollapsingHeader("Transform", ImGuiTreeNodeFlags_DefaultOpen))
	{
		DrawGizmoRow(false);
	}
	if (FocusObject == Rig.Id && FocusBlock >= ShakeBase && HasOwner(Rig, FocusBlock))
	{
		DrawShake(Core, Ids, FocusBlock);
	}
	const s32 Block = FocusedBlock(Core, Rig);
	string64 Title;
	xr_sprintf(Title, "Clip %d###CameraBlock", Block + 1);
	if (Block < 0)
	{
		ImGui::TextDisabled("No camera clip at the playhead");
		ImGui::BeginDisabled(Core.View() == ViewSource::Global);
		if (ImGui::Button("Key from view", ImVec2(-FLT_MIN, 0.0f)))
		{
			KeyFromView(Core, Core.Time, false);
		}
		ImGui::EndDisabled();
		ImGui::PopID();
		return;
	}
	if (!ImGui::CollapsingHeader(Title, ImGuiTreeNodeFlags_DefaultOpen))
	{
		ImGui::PopID();
		return;
	}
	const CameraBlock& Shot = Rig.Camera.Blocks[Block];
	BeginRow("Start", false, false);
	float Start = Shot.Start;
	if (TimeField("##Start", Start, 0.0f, MaxTime, Rate))
	{
		const float Picked = FitClip(Rig.Camera, Block, Start, Start + Shot.Length, 0);
		if (!Undo.IsOpen())
		{
			PinShakes(Rig);
		}
		Object& Item = *Edit(Core, Rig.Id, "Move clip");
		Item.Camera.Blocks[Block].Start = Picked < 0.0f ? Shot.Start : Picked;
		FocusBlock = SortShot(Item, Block);
		FollowShakes(Item);
	}
	EndRow(KeyState::None);
	RowAction Action = BeginRow("Length", Shot.Length != 5.0f, false);
	float Length = Shot.Length;
	if (TimeField("##Length", Length, MinLength, MaxTime, Rate) || Action == RowAction::Reset)
	{
		const float Picked = FitClip(Rig.Camera, Block, Shot.Start, Shot.Start + (Action == RowAction::Reset ? 5.0f : Length), 2);
		if (!Undo.IsOpen())
		{
			PinShakes(Rig);
		}
		Object& Item = *Edit(Core, Rig.Id, "Clip length");
		Item.Camera.Blocks[Block].Length = Picked < 0.0f ? Shot.Length : Picked - Shot.Start;
		FollowShakes(Item);
	}
	EndRow(KeyState::None);
	const CameraBlock* Previous = Block > 0 ? &Rig.Camera.Blocks[Block - 1] : nullptr;
	const float Overlap = Previous ? Previous->Start + Previous->Length - Shot.Start : 0.0f;
	if (Overlap > 0.0f)
	{
		Action = BeginRow("Crossfade", Shot.BlendEase != Interp::EaseInOut, false);
		const float Half = (RowWidth - Spacing) * 0.5f;
		s32 Ease = static_cast<s32>(Shot.BlendEase);
		ImGui::SetNextItemWidth(Half);
		ImGui::Text("%.3f s", Overlap);
		ImGui::SameLine(0.0f, Spacing);
		ImGui::SetNextItemWidth(Half);
		if (ImGui::Combo("##Ease", &Ease, InspectorUi::Modes, 4) || Action == RowAction::Reset)
		{
			Edit(Core, Rig.Id, "Crossfade ease")->Camera.Blocks[Block].BlendEase = Action == RowAction::Reset ? Interp::EaseInOut : static_cast<Interp>(Ease);
		}
		EndRow(KeyState::None);
	}
	PoseRow(Core, Ids, Block, Param::Position);
	PoseRow(Core, Ids, Block, Param::Rotation);
	ImGui::BeginDisabled(Core.View() == ViewSource::Global);
	if (ImGui::Button("Key from view", ImVec2(-FLT_MIN, 0.0f)))
	{
		KeyFromView(Core, Core.Time, false);
	}
	ImGui::EndDisabled();
	const BlockCache* Cache = Core.FindCameraBlock(static_cast<u32>(Block));
	const u32 Near = Cache ? PoseKeyAt(Shot, *Cache, Core.Time - TargetStart(Rig, Block), 0.5f / static_cast<float>(Rate)) : u32(-1);
	const u32 At = Near != u32(-1) || TangentBlock != Block || TangentKey < 0 || TangentKey >= static_cast<s32>(Shot.Keys.size()) ? Near : static_cast<u32>(TangentKey);
	const PoseKey Pose = At == u32(-1) ? PoseKey {} : Shot.Keys[At];
	const auto Write = [this, &Core, &Rig, Block, At](const char* Label) -> PoseKey& { return Edit(Core, Rig.Id, Label)->Camera.Blocks[Block].Keys[At]; };
	ImGui::SeparatorText("Pose key");
	ImGui::BeginDisabled(At == u32(-1));
	BeginRow("Interpolation", false, false);
	s32 Mode = static_cast<s32>(Pose.Mode);
	if (ImGui::Combo("##Mode", &Mode, InspectorUi::Modes, 6))
	{
		Write("Interpolation").Mode = static_cast<Interp>(Mode);
	}
	EndRow(KeyState::None);
	Action = BeginRow("Tangent", Pose.Tangent != TangentMode::Auto, false);
	s32 Tangent = static_cast<s32>(Pose.Tangent);
	if (ImGui::Combo("##Tangent", &Tangent, InspectorUi::Tangents, 3) || Action == RowAction::Reset)
	{
		PoseKey& Item = Write("Tangent");
		Fvector In, Out;
		if (Item.Tangent == TangentMode::Auto && Cache && Cache->Path.Handles(At, In, Out))
		{
			Item.InHandle = In;
			Item.OutHandle = Out;
		}
		Item.Tangent = Action == RowAction::Reset ? TangentMode::Auto : static_cast<TangentMode>(Tangent);
	}
	EndRow(KeyState::None);
	for (u32 Index = 0; Index < 3; ++Index)
	{
		ImGui::PushID(static_cast<s32>(Index));
		float Shape = Pose.*InspectorUi::ShapeFields[Index];
		Action = BeginRow(InspectorUi::ShapeNames[Index], Shape != 0.0f, false);
		if (ImGui::DragFloat("##Shape", &Shape, 0.01f, -1.0f, 1.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp) || Action == RowAction::Reset)
		{
			Write(InspectorUi::ShapeNames[Index]).*InspectorUi::ShapeFields[Index] = Action == RowAction::Reset ? 0.0f : std::clamp(Shape, -1.0f, 1.0f);
		}
		EndRow(KeyState::None);
		ImGui::PopID();
	}
	ImGui::BeginDisabled(!Pose.HasTangent());
	if (ImGui::Button("Reset tangents", ImVec2(-FLT_MIN, 0.0f)))
	{
		PoseKey& Item = Write("Reset tangents");
		Item.Tangent = TangentMode::Auto;
		Item.Tension = Item.Continuity = Item.Bias = 0.0f;
		Item.InHandle.set(0.0f, 0.0f, 0.0f);
		Item.OutHandle.set(0.0f, 0.0f, 0.0f);
	}
	ImGui::EndDisabled();
	ImGui::EndDisabled();
	ParamRow(Core, Ids, Block, Param::Fov, 0, 0.1f, "%.1f", 1, 0.0f);
	ParamRow(Core, Ids, Block, Param::TimeScale, 0, 0.01f, "%.2f", 1, 1.0f);
	if (ImGui::CollapsingHeader("Depth of field", ImGuiTreeNodeFlags_DefaultOpen))
	{
		Action = BeginRow("Mode", Shot.Dof != DofMode::Off, false);
		s32 Lens = static_cast<s32>(Shot.Dof);
		if (ImGui::Combo("##Dof", &Lens, InspectorUi::DofModes, 3) || Action == RowAction::Reset)
		{
			Edit(Core, Rig.Id, "Depth of field")->Camera.Blocks[Block].Dof = Action == RowAction::Reset ? DofMode::Off : static_cast<DofMode>(Lens);
		}
		EndRow(KeyState::None);
		const bool IsTracked = Shot.Dof == DofMode::Object;
		if (IsTracked)
		{
			AttachLink Target = Shot.FocusTarget;
			Action = BeginRow("Target", Target.ObjectId != 0, false);
			AttachCombo(Core, "##Focus", Rig.Id, Target);
			Target = Action == RowAction::Reset ? AttachLink {} : Target;
			EndRow(KeyState::None);
			Action = BeginRow("Focus bone", Target.Bone.size() != 0, false);
			BoneCombo(Core, "##FocusBone", Target);
			Target.Bone = Action == RowAction::Reset ? shared_str() : Target.Bone;
			EndRow(KeyState::None);
			if (!(Target == Shot.FocusTarget))
			{
				Edit(Core, Rig.Id, "Focus target")->Camera.Blocks[Block].FocusTarget = Target;
			}
			const u8 Problems = Cache ? Cache->Frames.Problems : 0;
			ImGui::PushStyleColor(ImGuiCol_Text, ErrorColor);
			if (Shot.FocusTarget.ObjectId && (Problems & MissingFocus))
			{
				ImGui::TextWrapped("Focus target is missing: Focus is used");
			}
			if (Problems & MissingFocusBone)
			{
				ImGui::TextWrapped("Focus bone is missing: the origin is used");
			}
			ImGui::PopStyleColor();
		}
		ParamRow(Core, Ids, Block, IsTracked ? Param::DofOffset : Param::DofFocus, 0, 0.01f, "%.2f m", 1, IsTracked ? 0.0f : 5.0f);
		ParamRow(Core, Ids, Block, Param::DofNear, 0, 0.01f, "%.2f m", 1, 2.0f);
		ParamRow(Core, Ids, Block, Param::DofFar, 0, 0.05f, "%.2f m", 1, 20.0f);
		ParamRow(Core, Ids, Block, Param::DofBlur, 0, 0.01f, "%.2f", 1, 5.0f);
		ImGui::BeginDisabled(IsTracked);
		ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyle().Colors[IsPickingFocus ? ImGuiCol_ButtonActive : ImGuiCol_FrameBg]);
		if (ImGui::Button("Pick focus", ImVec2(-FLT_MIN, 0.0f)))
		{
			IsPickingFocus = !IsPickingFocus;
			PickFocusBlock = Block;
		}
		ImGui::PopStyleColor();
		ImGui::EndDisabled();
	}
	DrawAttach(Core, Ids, Block);
	ImGui::PopID();
}

void Editor::DrawShake(Player& Core, const xr_vector<u32>& Ids, s32 Owner)
{
	const u32 Id = Ids[0], Index = static_cast<u32>(Owner - ShakeBase), Rate = Core.Data.Header.FrameRate;
	const Object& Rig = *Find(Core.Data, Id);
	const ShakeClip& Live = Rig.Camera.Shakes[Index];
	const Shake Value = Live.Setup;
	const float Start = Rig.Span.Start + Live.Start, Length = Live.Length, FadeIn = Live.FadeIn, FadeOut = Live.FadeOut;
	const u8 Row = Live.Row;
	const auto Clip = [this, &Core, Id, Index](const char* Label) -> ShakeClip& { return Edit(Core, Id, Label)->Camera.Shakes[Index]; };
	ImGui::PushID("Shake");
	const float Right = ImGui::GetCursorPosX() + ImGui::GetContentRegionAvail().x;
	string128 Title;
	xr_sprintf(Title, "Shake - %s###Shake", ShakePresets[std::min<u32>(Value.Preset, ShakePresetCount - 1)].Name);
	const bool IsOpen = ImGui::CollapsingHeader(Title, ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_AllowOverlap);
	bool ShouldRemove = false;
	if (ImGui::BeginPopupContextItem("##ShakeMenu"))
	{
		ShouldRemove = ImGui::MenuItem("Remove");
		ImGui::EndPopup();
	}
	ImGui::SameLine(Right - ImGui::GetFrameHeight());
	bool IsEnabled = Value.IsEnabled;
	if (ImGui::Checkbox("##Enabled", &IsEnabled))
	{
		Clip("Shake").Setup.IsEnabled = IsEnabled;
	}
	ImGui::SetItemTooltip("Enabled");
	if (ShouldRemove)
	{
		RemoveBlock(Core, Id, Owner);
	}
	if (ShouldRemove || !IsOpen)
	{
		ImGui::PopID();
		return;
	}
	RowAction Action = BeginRow("Preset", Value.Preset != 0, false);
	s32 Preset = Value.Preset;
	if (ImGui::Combo("##Preset", &Preset, [](void*, s32 Item) { return ShakePresets[Item].Name; }, nullptr, static_cast<s32>(ShakePresetCount)) || Action == RowAction::Reset)
	{
		Clip("Shake").Setup.Preset = static_cast<u8>(Action == RowAction::Reset ? 0 : Preset);
	}
	EndRow(KeyState::None);
	ParamRow(Core, Ids, Owner, Param::ShakeInfluence, Row, 0.01f, "%.2f", 1, 1.0f);
	ParamRow(Core, Ids, Owner, Param::ShakePositionScale, Row, 0.01f, "%.2f", 1, 1.0f);
	ParamRow(Core, Ids, Owner, Param::ShakeSpeed, Row, 0.01f, "%.2f", 1, 1.0f);
	Action = BeginRow("Time offset", Value.Offset != 0.0f, false);
	float Offset = Value.Offset;
	if (ImGui::DragFloat("##Offset", &Offset, 0.01f, -MaxTime, MaxTime, "%.2f s", ImGuiSliderFlags_AlwaysClamp) || Action == RowAction::Reset)
	{
		Clip("Shake").Setup.Offset = Action == RowAction::Reset ? 0.0f : Offset;
	}
	EndRow(KeyState::None);
	Action = BeginRow("Stretch", Value.IsStretched, false);
	bool IsStretched = Value.IsStretched;
	if (ImGui::Checkbox("##Stretch", &IsStretched) || Action == RowAction::Reset)
	{
		Clip("Shake").Setup.IsStretched = Action != RowAction::Reset && IsStretched;
	}
	EndRow(KeyState::None);
	float Fades[2] = { FadeIn, FadeOut };
	const char* const FadeNames[2] = { "Fade in", "Fade out" };
	for (u32 Side = 0; Side < 2; ++Side)
	{
		ImGui::PushID(static_cast<s32>(Side));
		Action = BeginRow(FadeNames[Side], Fades[Side] > 0.0f, false);
		if (TimeField("##Fade", Fades[Side], 0.0f, Length, Rate) || Action == RowAction::Reset)
		{
			(Side ? Clip(FadeNames[Side]).FadeOut : Clip(FadeNames[Side]).FadeIn) = Action == RowAction::Reset ? 0.0f : std::clamp(Fades[Side], 0.0f, Length);
		}
		EndRow(KeyState::None);
		ImGui::PopID();
	}
	float Place[2] = { Start, Length };
	const char* const PlaceNames[2] = { "Start", "Length" };
	for (u32 Field = 0; Field < 2; ++Field)
	{
		ImGui::PushID(PlaceNames[Field]);
		BeginRow(PlaceNames[Field], false, false);
		const bool IsChanged = TimeField("##Time", Place[Field], Field ? MinLength : Rig.Span.Start, MaxTime, Rate);
		const float NewStart = std::max(Place[0] - Rig.Span.Start, 0.0f), NewLength = std::clamp(Place[1], MinLength, std::max(MaxTime - Place[0], MinLength));
		if (IsChanged && !Cameras::IsShakeFree(Rig.Camera, Index, Row, NewStart, NewLength, MaxShakeClips))
		{
			SetStatus(true, "Another shake is on that row there.");
		}
		else if (IsChanged)
		{
			ShakeClip& Item = Clip(PlaceNames[Field]);
			Item.Start = NewStart;
			Item.Length = NewLength;
			Item.FadeIn = std::min(Item.FadeIn, NewLength);
			Item.FadeOut = std::min(Item.FadeOut, NewLength);
		}
		EndRow(KeyState::None);
		ImGui::PopID();
	}
	ImGui::PopID();
}

void Editor::PoseRow(Player& Core, const xr_vector<u32>& Ids, s32 Block, Param Id)
{
	const ParamInfo& Meta = Info(Id);
	const Object& Rig = *Find(Core.Data, Ids[0]);
	const CameraBlock& Shot = Rig.Camera.Blocks[Block];
	PoseKey Pose;
	u32 At = u32(-1);
	const BlockCache* Cache = PoseAt(Core, Shot, Block, Core.Time - TargetStart(Rig, Block), Pose, At);
	float Values[3] = {};
	for (u8 Component = 0; Component < 3; ++Component)
	{
		Values[Component] = InspectorUi::PoseField(Pose, Id, Component);
	}
	BeginRow(Meta.Label, false, false);
	ImGui::BeginDisabled(!Cache);
	if (ImGui::DragScalarN("##Value", ImGuiDataType_Float, Values, 3, Id == Param::Rotation ? 0.5f : 0.01f, &Meta.Min, &Meta.Max, Id == Param::Rotation ? "%.1f" : "%.2f", ImGuiSliderFlags_AlwaysClamp))
	{
		for (u8 Component = 0; Component < 3; ++Component)
		{
			if (Values[Component] != InspectorUi::PoseField(Pose, Id, Component))
			{
				WriteParam(Core, *Edit(Core, Rig.Id, Meta.Label), Block, Id, 0, Component, Values[Component]);
			}
		}
	}
	ImGui::EndDisabled();
	if (EndRow(Shot.Keys.empty() ? KeyState::Static : At != u32(-1) ? KeyState::OnKey : KeyState::Animated))
	{
		ToggleKey(Core, Ids, Block, Id, 0);
	}
}

void Editor::ParamRow(Player& Core, const xr_vector<u32>& Ids, s32 Block, Param Id, u8 Layer, float Speed, const char* Format, u8 ResetMask, float Default)
{
	const ParamInfo& Meta = Info(Id);
	const Object& Primary = *Find(Core.Data, Ids[0]);
	float Shown[3] = {}, Values[3] = {};
	bool IsOverridden = false;
	for (u8 Component = 0; Component < Meta.Components; ++Component)
	{
		Shown[Component] = Values[Component] = InspectorUi::ValueAt(Primary, Block, Id, Layer, Component, Core.Time);
		IsOverridden |= ((ResetMask >> Component) & 1) && Shown[Component] != Default;
	}
	const bool IsGame = Id == Param::Fov && !(Shown[0] > 0.0f);
	Shown[0] = Values[0] = IsGame ? GameFov() : Shown[0];
	const RowAction Action = BeginRow(Meta.Label, IsOverridden, Curves::FindLane(LaneList(Primary, Block), Id, Layer) != nullptr);
	const float Spacing = ImGui::GetStyle().ItemInnerSpacing.x, PickWidth = ImGui::CalcTextSize("Pick").x + 2.0f * ImGui::GetStyle().FramePadding.x;
	if (Id == Param::Color)
	{
		ImGui::SetNextItemWidth(std::max(RowWidth - PickWidth - Spacing, 1.0f));
	}
	const bool IsChanged = (Id == Param::Color ? ImGui::ColorEdit3("##Value", Values, ImGuiColorEditFlags_Float | ImGuiColorEditFlags_HDR | ImGuiColorEditFlags_NoTooltip) : ImGui::DragScalarN("##Value", ImGuiDataType_Float, Values, Meta.Components, Speed, &Meta.Min, &Meta.Max, IsGame ? "Game" : Format, ImGuiSliderFlags_AlwaysClamp | (Id == Param::Intensity ? ImGuiSliderFlags_Logarithmic : 0))) || Action == RowAction::Reset;
	if (Id == Param::Color)
	{
		ImGui::SameLine(0.0f, Spacing);
		ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyle().Colors[IsPickingColor ? ImGuiCol_ButtonActive : ImGuiCol_FrameBg]);
		if (ImGui::Button("Pick"))
		{
			IsPickingColor = !IsPickingColor;
			PickColorIds = Ids;
		}
		ImGui::PopStyleColor();
	}
	for (u8 Component = 0; Action == RowAction::Reset && Component < Meta.Components; ++Component)
	{
		Values[Component] = ((ResetMask >> Component) & 1) ? Default : Values[Component];
	}
	for (u32 TargetId : Ids)
	{
		for (u8 Component = 0; IsChanged && Component < Meta.Components; ++Component)
		{
			if (Values[Component] != Shown[Component] && Id == Param::Fov && Values[Component] < Meta.Min)
			{
				*StaticField(*Edit(Core, TargetId, Meta.Label), Block, Id, Layer, Component) = Values[Component];
			}
			else if (Values[Component] != Shown[Component])
			{
				WriteParam(Core, *Edit(Core, TargetId, Meta.Label), Block, Id, Layer, Component, Values[Component]);
			}
		}
		if (Action == RowAction::RemoveAnimation)
		{
			Object& Item = *Edit(Core, TargetId, "Remove animation");
			RemoveLane(Item, Block, Id, Layer, Core.Time - TargetStart(Item, Block));
		}
		if ((IsChanged || Action != RowAction::None) && Id == Param::TimeScale)
		{
			Refit(Core, *Find(Core.Data, TargetId));
		}
	}
	if (EndRow(InspectorUi::StateAt(Primary, Block, Id, Layer, Core.Time, 0.5f / static_cast<float>(Core.Data.Header.FrameRate))))
	{
		ToggleKey(Core, Ids, Block, Id, Layer);
	}
}

void Editor::KeyRow(Player& Core, const xr_vector<u32>& Ids)
{
	const float HalfFrame = 0.5f / static_cast<float>(Core.Data.Header.FrameRate);
	const auto ForKeys = [&Core, HalfFrame](Object& Item, const auto& Visit)
	{
		for (Lane& Track : Item.Lanes)
		{
			for (u8 Component = 0; (Track.Id <= Param::Scale || Track.Id == Param::Size) && IsParamShown(Item, Track.Id) && Component < Info(Track.Id).Components; ++Component)
			{
				const u32 At = Curves::KeyAt(Track.Curves[Component], Core.Time - Item.Span.Start, HalfFrame);
				if (At != u32(-1))
				{
					Visit(Track.Curves[Component], At);
				}
			}
		}
	};
	s32 Mode = -1;
	ForKeys(*Find(Core.Data, Ids[0]), [&Mode](Curve& Part, u32 At) { Mode = Mode < 0 ? static_cast<s32>(Part.Keys[At].Mode) : Mode; });
	BeginRow("Key", false, false);
	ImGui::BeginDisabled(Mode < 0);
	s32 Choice = Mode < 0 ? static_cast<s32>(Interp::EaseInOut) : Mode;
	if (ImGui::Combo("##Mode", &Choice, InspectorUi::Modes, 6))
	{
		for (u32 Id : Ids)
		{
			ForKeys(*Edit(Core, Id, "Interpolation"), [Choice](Curve& Part, u32 At) { Curves::SetMode(Part, At, static_cast<Interp>(Choice)); });
		}
	}
	ImGui::EndDisabled();
	EndRow(KeyState::None);
}

void Editor::DrawAttach(Player& Core, const xr_vector<u32>& Ids, s32 Block)
{
	if (!ImGui::CollapsingHeader("Attach", ImGuiTreeNodeFlags_DefaultOpen))
	{
		return;
	}
	const Object& Primary = *Find(Core.Data, Ids[0]);
	const bool IsObject = Block < 0, IsActor = IsObject && Primary.Type == ObjectType::Actor;
	const Attachment& Links = IsObject ? Primary.Attach : Primary.Camera.Blocks[Block].Attach;
	const ObjectState* State = IsObject ? Core.FindState(Primary.Id) : nullptr;
	const BlockCache* Cache = IsObject ? nullptr : Core.FindCameraBlock(static_cast<u32>(Block));
	const u8 Problems = State ? State->Frames.Problems : Cache ? Cache->Frames.Problems : 0;
	Attachment Value = Links;
	RowAction Action = BeginRow("Parent", Links.Parent.ObjectId != 0, false);
	AttachCombo(Core, "##Target", Primary.Id, Value.Parent);
	Value.Parent = Action == RowAction::Reset ? AttachLink {} : Value.Parent;
	EndRow(KeyState::None);
	Action = BeginRow("Bone", Links.Parent.Bone.size() != 0, false);
	BoneCombo(Core, "##Bone", Value.Parent);
	Value.Parent.Bone = Action == RowAction::Reset ? shared_str() : Value.Parent.Bone;
	EndRow(KeyState::None);
	Action = BeginRow("Lock", Links.IsRollLocked || Links.IsPitchLocked, false);
	ImGui::BeginDisabled(!Value.Parent.ObjectId);
	ImGui::Checkbox("Roll", &Value.IsRollLocked);
	ImGui::SameLine();
	ImGui::Checkbox("Pitch", &Value.IsPitchLocked);
	ImGui::EndDisabled();
	Value.IsRollLocked = Value.IsRollLocked && Action != RowAction::Reset;
	Value.IsPitchLocked = Value.IsPitchLocked && Action != RowAction::Reset;
	EndRow(KeyState::None);
	Action = BeginRow("Look at", Links.LookAt.ObjectId != 0, false);
	AttachCombo(Core, "##Target", Primary.Id, Value.LookAt);
	Value.LookAt = Action == RowAction::Reset ? AttachLink {} : Value.LookAt;
	EndRow(KeyState::None);
	Action = BeginRow("Look bone", Links.LookAt.Bone.size() != 0, false);
	BoneCombo(Core, "##Bone", Value.LookAt);
	Value.LookAt.Bone = Action == RowAction::Reset ? shared_str() : Value.LookAt.Bone;
	EndRow(KeyState::None);
	Action = BeginRow("Yaw only", !IsActor && Links.IsYawOnly, false);
	bool IsYawOnly = IsActor || Value.IsYawOnly;
	ImGui::BeginDisabled(IsActor);
	ImGui::Checkbox("##Yaw", &IsYawOnly);
	ImGui::EndDisabled();
	Value.IsYawOnly = IsActor ? Value.IsYawOnly : IsYawOnly && Action != RowAction::Reset;
	EndRow(KeyState::None);
	for (u32 Bit = 0; Bit < 4; ++Bit)
	{
		if (Problems & (1u << Bit))
		{
			ImGui::PushStyleColor(ImGuiCol_Text, ErrorColor);
			ImGui::TextWrapped("%s", InspectorUi::LinkProblems[Bit == 0 && !IsObject ? 4 : Bit]);
			ImGui::PopStyleColor();
		}
	}
	if (Value == Links)
	{
		return;
	}
	const auto Take = [](auto& Out, const auto& Was, const auto& Now)
	{
		Out = Was == Now ? Out : Now;
	};
	for (u32 Id : Ids)
	{
		const Object& Item = *Find(Core.Data, Id);
		const Attachment& Own = IsObject ? Item.Attach : Item.Camera.Blocks[Block].Attach;
		Attachment Next = Own;
		Take(Next.Parent, Links.Parent, Value.Parent);
		Take(Next.IsRollLocked, Links.IsRollLocked, Value.IsRollLocked);
		Take(Next.IsPitchLocked, Links.IsPitchLocked, Value.IsPitchLocked);
		Take(Next.LookAt, Links.LookAt, Value.LookAt);
		Take(Next.IsYawOnly, Links.IsYawOnly, Value.IsYawOnly);
		const bool IsLegal = (Next.Parent.ObjectId == Own.Parent.ObjectId || CanAttach(Core.Data, Id, Next.Parent.ObjectId)) && (Next.LookAt.ObjectId == Own.LookAt.ObjectId || CanAttach(Core.Data, Id, Next.LookAt.ObjectId));
		if (!IsLegal)
		{
			SetStatus(true, "\"%s\" cannot link there: it would form a loop or a chain deeper than %u.", DisplayName(&Item), MaxAttachDepth);
		}
		if (Next == Own || !IsLegal)
		{
			continue;
		}
		const bool HasGenerator = std::any_of(Item.Lanes.begin(), Item.Lanes.end(), [](const Lane& Track) { return Track.Id <= Param::Rotation && std::any_of(std::begin(Track.Curves), std::end(Track.Curves), [](const Curve& Part) { return Part.Gen.Type != Wave::None; }); });
		if (HasGenerator)
		{
			SetStatus(true, "\"%s\": a generator on Position or Rotation stays as it is in the new space.", DisplayName(&Item));
		}
		Edit(Core, Id, "Attach");
		Core.RequestAttach(Id, Block, Next);
	}
}

bool Editor::AttachCombo(const Player& Core, const char* Label, u32 Owner, AttachLink& Link)
{
	if (!ImGui::BeginCombo(Label, Link.ObjectId ? DisplayName(Find(Core.Data, Link.ObjectId)) : "None", ImGuiComboFlags_HeightLarge))
	{
		return false;
	}
	u32 Picked = ImGui::Selectable("None", !Link.ObjectId) ? 0 : Link.ObjectId;
	AttachChoices.clear();
	for (const Object& Item : Core.Data.Objects)
	{
		AttachChoices.push_back(&Item);
	}
	std::sort(AttachChoices.begin(), AttachChoices.end(), [](const Object* A, const Object* B)
	{
		const bool IsCameraA = A->Type == ObjectType::Camera, IsCameraB = B->Type == ObjectType::Camera;
		return IsCameraA != IsCameraB ? IsCameraA : xr_strcmp(A->Name.c_str(), B->Name.c_str()) < 0;
	});
	AttachCheck.Measure(Core.Data, Owner);
	for (const Object* Item : AttachChoices)
	{
		ImGui::PushID(static_cast<s32>(Item->Id));
		ImGui::BeginDisabled(!AttachCheck.Allows(Item->Id));
		Picked = ImGui::Selectable(DisplayName(Item), Item->Id == Link.ObjectId) ? Item->Id : Picked;
		ImGui::EndDisabled();
		ImGui::PopID();
	}
	ImGui::EndCombo();
	if (Picked == Link.ObjectId)
	{
		return false;
	}
	Link = AttachLink { Picked, shared_str() };
	return true;
}

bool Editor::BoneCombo(const Player& Core, const char* Label, AttachLink& Link)
{
	const ActorRuntime* Runtime = Core.FindActor(Link.ObjectId);
	const u16 Count = Runtime ? Runtime->BoneCount() : 0;
	bool IsPicked = false;
	ImGui::BeginDisabled(!Count);
	if (ImGui::BeginCombo(Label, Link.Bone.size() ? Link.Bone.c_str() : "Origin", ImGuiComboFlags_HeightLarge))
	{
		if (ImGui::IsWindowAppearing())
		{
			BoneFilter[0] = 0;
			ImGui::SetKeyboardFocusHere();
		}
		ImGui::InputTextWithHint("##Filter", "Filter", BoneFilter, sizeof(BoneFilter));
		if (ImGui::Selectable("Origin", !Link.Bone.size()))
		{
			Link.Bone = nullptr;
			IsPicked = true;
		}
		for (u16 Bone = 0; Bone < Count; ++Bone)
		{
			const char* Name = Runtime->BoneName(Bone);
			if (!Name || (BoneFilter[0] && !ImStristr(Name, nullptr, BoneFilter, nullptr)))
			{
				continue;
			}
			ImGui::BeginDisabled(!Io::IniSafe(Name));
			if (ImGui::Selectable(Name, Link.Bone.size() && !xr_strcmp(Link.Bone.c_str(), Name)))
			{
				Link.Bone = Name;
				IsPicked = true;
			}
			ImGui::EndDisabled();
			if (ImGui::IsItemHovered())
			{
				HighlightObject = Link.ObjectId;
				HighlightBone = Name;
			}
		}
		ImGui::EndCombo();
	}
	ImGui::EndDisabled();
	return IsPicked;
}

bool Editor::CanAttach(const Scene& Data, u32 From, u32 To)
{
	return !To || (From != To && AttachCheck.Build(Data, From, To));
}

void Editor::DrawGizmoRow(bool HasScale, bool IsRange, bool HasRotate)
{
	const ImGuiStyle& Style = ImGui::GetStyle();
	const u32 Count = HasScale ? 3 : 2;
	const u32 Shown = static_cast<u32>(GizmoShown);
	const float Size = 1.5f * ImGui::GetFrameHeight(), Spacing = Style.ItemInnerSpacing.x;
	bool& IsLocal = GizmoItems.size() > 1 ? IsGizmoLocalMulti : IsGizmoLocalSingle;
	BeginRow("Gizmo", false, false);
	for (u32 Index = 0; Index <= Count; ++Index)
	{
		const u32 Mode = Index ? Index - 1 : static_cast<u32>(GizmoMode::Universal);
		ImGui::BeginDisabled(Mode == 1 && !HasRotate);
		ImGui::PushStyleColor(ImGuiCol_Button, Style.Colors[Shown == Mode ? ImGuiCol_ButtonActive : ImGuiCol_FrameBg]);
		if (ImGui::Button(InspectorUi::GizmoLabels[Mode], ImVec2(Index ? Size : 1.5f * Size, 0.0f)))
		{
			Gizmo = static_cast<GizmoMode>(Mode);
		}
		ImGui::PopStyleColor();
		ImGui::SetItemTooltip("%s", InspectorUi::GizmoNames[Mode == 2 && IsRange ? 3u : Mode]);
		ImGui::EndDisabled();
		ImGui::SameLine(0.0f, Spacing);
	}
	if (ImGui::Button(IsLocal ? "Local" : "World", ImVec2(std::max(RowWidth - (Count + 1.5f) * (Size + Spacing), 1.0f), 0.0f)))
	{
		IsLocal = !IsLocal;
	}
	EndRow(KeyState::None);
	float* const Steps[4] = { &Settings.MoveStep, &Settings.RotateStep, &Settings.ScaleStep, &Settings.RangeStep };
	BeginRow("Steps", false, false);
	for (u32 Index = 0; Index < Count; ++Index)
	{
		const u32 Slot = Index == 2 && IsRange ? 3u : Index;
		if (Index)
		{
			ImGui::SameLine(0.0f, Spacing);
		}
		ImGui::PushID(static_cast<s32>(Slot));
		ImGui::SetNextItemWidth((RowWidth - (Count - 1) * Spacing) / Count);
		ImGui::DragFloat("##Step", Steps[Slot], InspectorUi::StepLo[Slot], InspectorUi::StepLo[Slot], InspectorUi::StepHi[Slot], InspectorUi::StepFormats[Slot], ImGuiSliderFlags_AlwaysClamp);
		ImGui::SetItemTooltip("%s", InspectorUi::StepNames[Slot]);
		ImGui::PopID();
	}
	EndRow(KeyState::None);
}

RowAction Editor::BeginRow(const char* Name, bool IsOverridden, bool HasLane)
{
	ImGui::PushID(Name);
	const float Start = ImGui::GetCursorPosX(), Width = ImGui::GetContentRegionAvail().x, Size = ImGui::GetFrameHeight();
	const float Left = std::max(90.0f, 0.4f * Width);
	RowKeyX = Start + Width - Size;
	RowWidth = std::max(Width - Left - Size - ImGui::GetStyle().ItemSpacing.x, 1.0f);
	const ImVec2 At = ImGui::GetCursorScreenPos();
	const float Right = At.x + Left - ImGui::GetStyle().ItemSpacing.x;
	ImGui::Dummy(ImVec2(std::max(Right - At.x, 1.0f), Size));
	ImGui::PushStyleColor(ImGuiCol_Text, IsOverridden ? OverrideColor : ImGui::GetStyleColorVec4(ImGuiCol_Text));
	ImGui::RenderTextEllipsis(ImGui::GetWindowDrawList(), ImVec2(At.x, At.y + ImGui::GetStyle().FramePadding.y), ImVec2(Right, At.y + Size), Right, Name, nullptr, nullptr);
	ImGui::PopStyleColor();
	if (ImGui::IsItemHovered() && ImGui::CalcTextSize(Name).x > Right - At.x)
	{
		ImGui::SetTooltip("%s", Name);
	}
	if ((IsOverridden || HasLane) && ImGui::IsItemHovered() && ImGui::IsMouseReleased(ImGuiMouseButton_Right))
	{
		ImGui::OpenPopup("##Row");
	}
	RowAction Action = RowAction::None;
	if (ImGui::BeginPopup("##Row"))
	{
		Action = ImGui::MenuItem("Reset", nullptr, false, IsOverridden) ? RowAction::Reset : Action;
		Action = HasLane && ImGui::MenuItem("Remove animation") ? RowAction::RemoveAnimation : Action;
		ImGui::EndPopup();
	}
	ImGui::SameLine(Start + Left);
	ImGui::SetNextItemWidth(RowWidth);
	return Action;
}

bool Editor::EndRow(KeyState Mark)
{
	bool IsClicked = false;
	if (Mark != KeyState::None)
	{
		const float Size = ImGui::GetFrameHeight();
		ImGui::SameLine(RowKeyX);
		IsClicked = ImGui::InvisibleButton("##Key", ImVec2(Size, Size));
		const ImRect Box(ImGui::GetItemRectMin(), ImGui::GetItemRectMax());
		InspectorUi::Diamond(ImGui::GetWindowDrawList(), Box.GetCenter(), Size * 0.3f, ImGui::GetColorU32(Mark == KeyState::Static ? ImGuiCol_TextDisabled : ImGuiCol_Text), Mark == KeyState::OnKey);
	}
	ImGui::PopID();
	return IsClicked;
}

bool Editor::TimeField(const char* Label, float& Seconds, float Lo, float Hi, u32 Rate)
{
	string32 Code;
	Timecode(Code, Seconds, Rate);
	const float Scale = static_cast<float>(Rate);
	s32 Frames = static_cast<s32>(std::round(Seconds * Scale));
	if (!ImGui::DragInt(Label, &Frames, 0.25f, static_cast<s32>(std::ceil(Lo * Scale)), static_cast<s32>(Hi * Scale), Code, ImGuiSliderFlags_AlwaysClamp))
	{
		return false;
	}
	Seconds = static_cast<float>(Frames) / Scale;
	return true;
}

bool Editor::OffsetField(const char* Label, float& Seconds, u32 Rate)
{
	string32 Code;
	string64 Format;
	Timecode(Code, std::fabs(Seconds), Rate);
	xr_sprintf(Format, "%s%s", Seconds < 0.0f ? "-" : "+", Code);
	const s32 Limit = static_cast<s32>(MaxTime) * static_cast<s32>(Rate);
	s32 Frames = static_cast<s32>(std::round(Seconds * static_cast<float>(Rate)));
	if (!ImGui::DragInt(Label, &Frames, 0.25f, -Limit, Limit, Format, ImGuiSliderFlags_AlwaysClamp))
	{
		return false;
	}
	Seconds = static_cast<float>(Frames) / static_cast<float>(Rate);
	return true;
}

s32 Editor::FocusedBlock(const Player& Core, const Object& Target, u8* Layer) const
{
	const bool IsCamera = Target.Type == ObjectType::Camera;
	const u8 Focused = IsCamera ? 0 : FocusLayer;
	const bool IsLayered = IsCamera || Focused <= Target.Actor.Layers.size();
	const s32 Count = IsLayered ? static_cast<s32>(IsCamera ? Target.Camera.Blocks.size() : BlocksOf(Target.Actor, Focused).size()) : 0;
	if (FocusObject == Target.Id && FocusBlock >= 0 && FocusBlock < Count)
	{
		if (Layer)
		{
			*Layer = Focused;
		}
		return FocusBlock;
	}
	const ActorRuntime* Runtime = IsCamera ? nullptr : Core.FindActor(Target.Id);
	double Rep = 0.0;
	float Sequence = 0.0f;
	const s32 Span = Runtime ? SpanAt(Target, *Runtime, Core.Time - Target.Span.Start, Rep, Sequence) : -1;
	const s32 Shot = IsCamera ? Cameras::BlockAt(Target.Camera, Core.Time - Target.Span.Start) : -1;
	const s32 Base = static_cast<s32>(IsCamera ? Target.Camera.Blocks.size() : Target.Actor.Base.Blocks.size());
	return Span >= 0 ? static_cast<s32>(Runtime->Track(0).Spans[Span].Block) : IsCamera ? Shot : Base - 1;
}

void Editor::BuildLibrary()
{
	if (IsLibraryBuilt)
	{
		return;
	}
	IsLibraryBuilt = IsFilterDirty = true;
	ModelFiles.clear();
	ItemSections.clear();
	OmfFiles.clear();
	EffectNames.clear();
	InspectorUi::ListFiles(ModelFiles, _game_meshes_, "*.ogf", FS_ListFiles, MaxIdentifier);
	InspectorUi::ListFiles(OmfFiles, _game_meshes_, "*.omf", FS_ListFiles, MaxIdentifier);
	InspectorUi::ListFiles(OmfFiles, "$level$", "*.omf", FS_ListFiles, MaxIdentifier);
	for (const CInifile::Sect& Section : pSettings->sections())
	{
		if (Actors::ItemValid(Section.Name))
		{
			ItemSections.push_back(Section.Name);
		}
	}
	Effects::ListEffects(EffectNames);
	std::erase_if(EffectNames, [](const shared_str& Effect) { return !Io::IniSafe(Effect.c_str()); });
	for (xr_vector<shared_str>* Names : { &ModelFiles, &ItemSections, &OmfFiles, &EffectNames })
	{
		InspectorUi::SortNames(*Names);
	}
}

bool Editor::Chooser(Player& Core, ChooserKind Kind, const char* Label, const char* Preview, const xr_vector<u32>& Owners, shared_str& Source, shared_str& Name)
{
	if (!ImGui::BeginCombo(Label, Preview, ImGuiComboFlags_HeightLargest))
	{
		return false;
	}
	const bool IsPicked = ChooserList(Core, Kind, Owners, Source, Name);
	if (IsPicked)
	{
		ImGui::CloseCurrentPopup();
	}
	ImGui::EndCombo();
	return IsPicked;
}

bool Editor::ChooserList(Player& Core, ChooserKind Kind, const xr_vector<u32>& Owners, shared_str& Source, shared_str& Name)
{
	BuildLibrary();
	if (Kind == ChooserKind::Texture && !IsTexturesBuilt)
	{
		IsTexturesBuilt = IsFilterDirty = true;
		InspectorUi::ListFiles(TextureFiles, _game_textures_, "*.dds", FS_ListFiles | FS_ClampExt, MaxTextureBytes);
		InspectorUi::SortNames(TextureFiles);
	}
	const u32 Owner = Owners.empty() ? 0 : Owners[0];
	const ActorRuntime* Runtime = Owner ? Core.FindActor(Owner) : nullptr;
	if (ImGui::IsWindowAppearing() || IsChooserFresh)
	{
		IsBrowsingPacks = IsChooserFresh = false;
		IsFilterDirty = true;
		ImGui::SetKeyboardFocusHere();
	}
	const ChooserKind Shown = Kind == ChooserKind::Animation && IsBrowsingPacks ? ChooserKind::Pack : Kind;
	if (Shown == ChooserKind::Preset)
	{
		LoadPresets();
	}
	const xr_vector<shared_str>* const Lists[7] = { &ModelFiles, &ItemSections, &OmfFiles, &OmfFiles, &PresetNames, &EffectNames, &TextureFiles };
	const xr_vector<shared_str>& Files = *Lists[static_cast<u32>(Shown)];
	const size_t Sources = Shown == ChooserKind::Animation && Runtime ? Runtime->Sources.size() : 0;
	char* Filter = Filters[static_cast<u32>(Shown)];
	ImGui::SetNextItemWidth(InspectorUi::ListWidth);
	const bool IsEnter = ImGui::InputTextWithHint("##Filter", "Filter", Filter, sizeof(Filters[0]), ImGuiInputTextFlags_EnterReturnsTrue | ImGuiInputTextFlags_AutoSelectAll | ImGuiInputTextFlags_CallbackHistory, [](ImGuiInputTextCallbackData*) { return 0; }) || ImGui::IsKeyPressed(ImGuiKey_Enter, false);
	IsFilterDirty |= ImGui::IsItemEdited();
	if (ImGui::IsKeyPressed(ImGuiKey_Escape, false))
	{
		ImGui::CloseCurrentPopup();
	}
	if (IsFilterDirty || FilteredKind != Shown || FilteredObject != Owner || FilteredSources != Sources)
	{
		IsFilterDirty = false;
		FilteredKind = Shown;
		FilteredObject = Owner;
		FilteredSources = Sources;
		ChooserCursor = 0;
		Ranked.clear();
		const auto Add = [this, Filter](u32 Code, const char* Text, const char* Pack)
		{
			const u32 Rank = InspectorUi::MatchRank(Filter, Text, Pack);
			if (Rank != u32(-1))
			{
				Ranked.push_back((static_cast<u64>(Rank) << 32) | Code);
			}
		};
		for (u32 Index = 0; Shown != ChooserKind::Animation && Index < Files.size(); ++Index)
		{
			Add(Index, Files[Index].c_str(), nullptr);
		}
		for (u32 Group = 0; Shown == ChooserKind::Animation && Group < Sources; ++Group)
		{
			const AnimationSource& Item = Runtime->Sources[Group];
			for (u32 Clip = 0; Clip < Item.Clips.size(); ++Clip)
			{
				if (!Item.Clips[Clip].IsFx)
				{
					Add((Group << 16) | Clip, Item.Clips[Clip].Name.c_str(), Item.Id.c_str());
				}
			}
		}
		std::sort(Ranked.begin(), Ranked.end());
		Filtered.clear();
		for (u64 Item : Ranked)
		{
			Filtered.push_back(static_cast<u32>(Item));
		}
	}
	const s32 Count = static_cast<s32>(Filtered.size());
	const s32 Move = (ImGui::IsKeyPressed(ImGuiKey_DownArrow) ? 1 : 0) - (ImGui::IsKeyPressed(ImGuiKey_UpArrow) ? 1 : 0);
	ChooserCursor = std::clamp(ChooserCursor + Move, 0, std::max(Count - 1, 0));
	const auto SetPack = [&](const shared_str& Path, bool IsAdding)
	{
		for (u32 Id : Owners)
		{
			const Object* Item = Find(Core.Data, Id);
			const bool HasPack = Item && std::find(Item->Actor.Packs.begin(), Item->Actor.Packs.end(), Path) != Item->Actor.Packs.end();
			if (!Item || HasPack == IsAdding || (IsAdding && Item->Actor.Packs.size() >= MaxPacks))
			{
				continue;
			}
			xr_vector<shared_str>& Packs = Edit(Core, Id, IsAdding ? "Add pack" : "Remove pack")->Actor.Packs;
			IsAdding ? Packs.push_back(Path) : (void)std::erase(Packs, Path);
			Refit(Core, *Item);
		}
	};
	const auto ClipOf = [&](u32 Code) { return Shown == ChooserKind::Animation && (Code >> 16) < Sources && (Code & 0xFFFFu) < Runtime->Sources[Code >> 16].Clips.size() ? &Runtime->Sources[Code >> 16].Clips[Code & 0xFFFFu].Name : nullptr; };
	bool IsPicked = false;
	if ((Shown == ChooserKind::Item || Shown == ChooserKind::Texture) && ImGui::Selectable("None"))
	{
		Source = Name = shared_str();
		IsPicked = true;
	}
	IsChooserOpen = true;
	bool ShouldOpenPack = false;
	s32 Chosen = IsEnter && ChooserCursor < Count ? ChooserCursor : -1;
	const float Split = InspectorUi::ListWidth * (Shown == ChooserKind::Animation ? 0.55f : Shown == ChooserKind::Preset ? 0.8f : 1.0f);
	ImGui::BeginChild("##List", ImVec2(InspectorUi::ListWidth, 16.0f * ImGui::GetTextLineHeightWithSpacing()));
	ImGuiListClipper Clipper;
	Clipper.Begin(Count);
	if (Move && Count)
	{
		Clipper.IncludeItemByIndex(ChooserCursor);
	}
	while (Clipper.Step())
	{
		for (s32 Row = Clipper.DisplayStart; Row < Clipper.DisplayEnd; ++Row)
		{
			const u32 Code = Filtered[Row];
			const shared_str* Animation = ClipOf(Code);
			const bool IsCursor = Row == ChooserCursor, IsListed = Animation || (Shown != ChooserKind::Animation && Code < Files.size());
			ImGui::PushID(Row);
			if (IsCursor && Move)
			{
				ImGui::SetScrollHereY();
			}
			Chosen = ImGui::Selectable("##Row", IsCursor, Shown == ChooserKind::Pack ? ImGuiSelectableFlags_NoAutoClosePopups : ImGuiSelectableFlags_None) && IsListed ? Row : Chosen;
			const ImRect Box(ImGui::GetItemRectMin(), ImGui::GetItemRectMax());
			const float Middle = Box.Min.x + Split - ImGui::GetStyle().ItemSpacing.x;
			const bool IsHovered = ImGui::IsItemHovered(Shown == ChooserKind::Model ? ImGuiHoveredFlags_DelayShort | ImGuiHoveredFlags_NoSharedDelay : ImGuiHoveredFlags_None) || (IsCursor && Move);
			if (Animation)
			{
				const shared_str& Pack = Runtime->Sources[Code >> 16].Id;
				const char* Slash = strrchr(Pack.c_str(), '\\');
				InspectorUi::Ellipsis(ImRect(Box.Min, ImVec2(Middle, Box.Max.y)), Animation->c_str(), ImGui::GetColorU32(ImGuiCol_Text));
				InspectorUi::Ellipsis(ImRect(Box.Min.x + Split, Box.Min.y, Box.Max.x, Box.Max.y), Slash ? Slash + 1 : Pack.c_str(), ImGui::GetColorU32(ImGuiCol_TextDisabled));
				if (IsHovered && !Core.IsPlaying)
				{
					Core.SetAudition(Owner, Pack, *Animation);
				}
				if (ImGui::IsItemClicked(ImGuiMouseButton_Right) && Omf::IsOmfPath(Pack))
				{
					MenuPack = Pack;
					ShouldOpenPack = true;
				}
			}
			else if (IsListed)
			{
				const bool IsOld = Shown == ChooserKind::Preset && Code < Presets.size() && Presets[Code].IsOld;
				InspectorUi::Ellipsis(ImRect(Box.Min, ImVec2(IsOld ? Middle : Box.Max.x, Box.Max.y)), Files[Code].c_str(), ImGui::GetColorU32(ImGuiCol_Text));
				InspectorUi::Ellipsis(ImRect(Box.Min.x + Split, Box.Min.y, Box.Max.x, Box.Max.y), IsOld ? "(old)" : nullptr, ImGui::GetColorU32(ImGuiCol_TextDisabled));
			}
			if (Shown == ChooserKind::Model && Owner && IsListed && IsHovered && !Core.IsPlaying)
			{
				Core.SetAudition(Owner, shared_str(), shared_str(), Files[Code]);
			}
			ImGui::PopID();
		}
	}
	if (ShouldOpenPack)
	{
		ImGui::OpenPopup("##Pack");
	}
	if (ImGui::BeginPopup("##Pack"))
	{
		if (ImGui::MenuItem("Remove pack"))
		{
			SetPack(MenuPack, false);
		}
		ImGui::EndPopup();
	}
	ImGui::EndChild();
	if (const shared_str* Animation = Chosen >= 0 ? ClipOf(Filtered[Chosen]) : nullptr)
	{
		Source = Runtime->Sources[Filtered[Chosen] >> 16].Id;
		Name = *Animation;
		IsPicked = true;
	}
	else if (Chosen >= 0 && Shown != ChooserKind::Animation)
	{
		Source = Files[Filtered[Chosen]];
		IsPicked = Shown != ChooserKind::Pack;
		IsBrowsingPacks = IsPicked && IsBrowsingPacks;
		if (!IsPicked)
		{
			SetPack(Source, true);
		}
	}
	ImGui::TextDisabled("%d found", Count);
	if (Shown == ChooserKind::Animation)
	{
		ImGui::SameLine();
		if (ImGui::Selectable("Browse OMF...", false, ImGuiSelectableFlags_NoAutoClosePopups))
		{
			IsBrowsingPacks = IsFilterDirty = true;
		}
	}
	return IsPicked;
}

void Editor::ToggleKey(Player& Core, const xr_vector<u32>& Ids, s32 Block, Param Id, u8 Layer, bool IsAdding)
{
	const float HalfFrame = 0.5f / static_cast<float>(Core.Data.Header.FrameRate);
	const Object& Primary = *Find(Core.Data, Ids[0]);
	if (Block >= 0 && Id <= Param::Rotation)
	{
		const float Local = Core.Time - TargetStart(Primary, Block);
		PoseKey Pose;
		u32 At = u32(-1);
		const BlockCache* Cache = PoseAt(Core, Primary.Camera.Blocks[Block], Block, Local, Pose, At);
		if (At != u32(-1) && !IsAdding)
		{
			RemovePose(Core, Block, At);
		}
		else if (Cache)
		{
			PutPose(Edit(Core, Primary.Id, "Add pose key")->Camera.Blocks[Block], *Cache, Local, HalfFrame, Pose);
		}
		return;
	}
	const bool IsRemoving = !IsAdding && InspectorUi::StateAt(Primary, Block, Id, Layer, Core.Time, HalfFrame) == KeyState::OnKey;

	for (u32 TargetId : Ids)
	{
		Object& Item = *Edit(Core, TargetId, IsRemoving ? "Remove key" : "Add key");
		if (!HasOwner(Item, Block) || !StaticField(Item, Block, Id, Layer, 0))
		{
			continue;
		}
		xr_vector<Lane>& Lanes = LaneList(Item, Block);
		if (IsRemoving && !Curves::FindLane(Lanes, Id, Layer))
		{
			continue;
		}
		if (!IsRemoving)
		{
			UseGameFov(Item, Block, Id);
		}
		Lane& Track = Curves::EnsureLane(Lanes, Id, Layer);
		const float Local = Core.Time - TargetStart(Item, Block);
		bool IsEmpty = true;
		for (u8 Component = 0; Component < Info(Id).Components; ++Component)
		{
			float* Static = StaticField(Item, Block, Id, Layer, Component);
			Curve& Source = Track.Curves[Component];
			const float Current = Curves::Value(Source, Local, *Static);
			const u32 At = Curves::KeyAt(Source, Local, HalfFrame);
			if (!IsRemoving)
			{
				Curves::SetKey(Source, Local, Current, HalfFrame);
			}
			else if (At != u32(-1))
			{
				Source.Keys.erase(Source.Keys.begin() + At);
				*Static = Source.Keys.empty() ? Current : *Static;
			}
			IsEmpty = IsEmpty && Source.Keys.empty() && Source.Gen.Type == Wave::None;
		}
		if (IsEmpty)
		{
			Lanes.erase(Lanes.begin() + (&Track - Lanes.data()));
		}
		if (Id == Param::TimeScale)
		{
			Refit(Core, Item);
		}
	}
}

void Editor::WriteParam(Player& Core, Object& Target, s32 Block, Param Id, u8 Layer, u8 Component, float NewValue, bool IsTotal)
{
	const ParamInfo& Meta = Info(Id);
	const float HalfFrame = 0.5f / static_cast<float>(Core.Data.Header.FrameRate), Local = Core.Time - TargetStart(Target, Block);
	const bool IsPose = Block >= 0 && Id <= Param::Rotation;
	if (IsAutoKey)
	{
		UseGameFov(Target, Block, Id);
	}
	PoseKey Pose;
	u32 KeyIndex = u32(-1);
	const BlockCache* Cache = IsPose ? PoseAt(Core, Target.Camera.Blocks[Block], Block, Local, Pose, KeyIndex) : nullptr;
	float* Static = IsPose ? &InspectorUi::PoseField(Pose, Id, Component) : StaticField(Target, Block, Id, Layer, Component);
	if (!Static || (IsPose && !Cache))
	{
		return;
	}
	const Lane* Found = IsPose ? nullptr : Curves::FindLane(LaneList(Target, Block), Id, Layer);
	const Curve* Existing = Found ? &Found->Curves[Component] : nullptr;
	KeyIndex = Existing ? Curves::KeyAt(*Existing, Local, HalfFrame) : KeyIndex;
	const bool HasKeys = IsPose ? !Target.Camera.Blocks[Block].Keys.empty() : Existing && !Existing->Keys.empty();
	auto Latch = std::find_if(Latches.begin(), Latches.end(), [&](const WriteLatch& Item) { return Item.ObjectId == Target.Id && Item.Block == Block && Item.Id == Id && Item.Layer == Layer && Item.Component == Component; });
	if (Latch == Latches.end())
	{
		const WriteKind Kind = IsAutoKey ? (Core.IsPlaying ? WriteKind::Record : WriteKind::Insert) : !HasKeys ? (IsPose ? WriteKind::Insert : WriteKind::Static) : KeyIndex != u32(-1) ? WriteKind::OnKey : WriteKind::Offset;
		const float Current = Existing ? Curves::Value(*Existing, Local, *Static) : *Static;
		Latch = Latches.insert(Latches.end(), WriteLatch { Target.Id, static_cast<s16>(Block), Id, Layer, Component, Kind, KeyIndex, Local, Local - 1.0f, Current, Current });
	}
	const float Change = IsTotal ? NewValue - (Latch->Value - Latch->Start) : NewValue - (Existing ? Curves::Result(*Existing, Local, *Static, Meta) : *Static);
	const float Delta = IsTotal && Id == Param::Rotation ? std::remainder(Change, 360.0f) : Change;
	if (!std::isfinite(Delta))
	{
		return;
	}
	const auto Shift = [&Meta, Delta](float Value) { return std::clamp(Value + Delta, Meta.Min, Meta.Max); };
	Latch->Value = Shift(Latch->Value);
	if (Latch->Kind == WriteKind::Static)
	{
		*Static = Shift(*Static);
		return;
	}
	if (IsPose)
	{
		CameraBlock& Shot = Target.Camera.Blocks[Block];
		for (u32 Index = 0; Index < Shot.Keys.size(); ++Index)
		{
			float& Field = InspectorUi::PoseField(Shot.Keys[Index], Id, Component);
			Field = Latch->Kind == WriteKind::Offset || (Latch->Kind == WriteKind::OnKey && Index == Latch->KeyIndex) ? Shift(Field) : Field;
		}
		if (Latch->Kind == WriteKind::Insert)
		{
			*Static = Shift(*Static);
			PutPose(Shot, *Cache, Local, HalfFrame, Pose);
		}
	}
	else
	{
		Curve& Source = Curves::EnsureLane(LaneList(Target, Block), Id, Layer).Curves[Component];
		if (Latch->Kind == WriteKind::OnKey && Latch->KeyIndex < Source.Keys.size())
		{
			Source.Keys[Latch->KeyIndex].Value = Shift(Source.Keys[Latch->KeyIndex].Value);
		}
		for (u32 Index = 0; Latch->Kind == WriteKind::Offset && Index < Source.Keys.size(); ++Index)
		{
			Source.Keys[Index].Value = Shift(Source.Keys[Index].Value);
		}
		if (Latch->Kind == WriteKind::Insert)
		{
			Curves::SetKey(Source, Local, Shift(Curves::Value(Source, Local, *Static)), HalfFrame);
		}
	}
	Record(Core, *Latch);
}

void Editor::Record(Player& Core, WriteLatch& Latch)
{
	Object* Target = Find(Core.Data, Latch.ObjectId);
	if (Latch.Kind != WriteKind::Record || !Target || !HasOwner(*Target, Latch.Block))
	{
		return;
	}
	const float Local = Core.Time - TargetStart(*Target, Latch.Block);
	const float Length = OwnerLength(*Target, Latch.Block);
	if (!(Local >= 0.0f && Local <= Length))
	{
		return;
	}
	const float From = Local < Latch.Last ? Local : std::max(Latch.Begin, Latch.Last), HalfFrame = 0.5f / static_cast<float>(Core.Data.Header.FrameRate);
	const bool IsDue = Local < Latch.Last || Local >= Latch.Last + 0.1f || !Core.IsPlaying;
	Latch.Last = IsDue ? Local : Latch.Last;
	if (Latch.Block < 0 || Latch.Id > Param::Rotation)
	{
		Curve& Source = Curves::EnsureLane(LaneList(*Target, Latch.Block), Latch.Id, Latch.Layer).Curves[Latch.Component];
		std::erase_if(Source.Keys, [From, Local](const Key& Item) { return Item.Time > From && Item.Time < Local; });
		if (IsDue)
		{
			Curves::SetKey(Source, Local, Latch.Value, HalfFrame);
		}
		if (Latch.Id == Param::TimeScale)
		{
			Refit(Core, *Target);
		}
		return;
	}
	CameraBlock& Shot = Target->Camera.Blocks[Latch.Block];
	PoseKey Pose;
	u32 At = u32(-1);
	const BlockCache* Cache = PoseAt(Core, Shot, Latch.Block, Local, Pose, At);
	if (!Cache)
	{
		return;
	}
	const float Lo = Cameras::PathTime(Shot, *Cache, From), Hi = Cameras::PathTime(Shot, *Cache, Local);
	std::erase_if(Shot.Keys, [Lo, Hi](const PoseKey& Item) { return Item.Time > Lo && Item.Time < Hi; });
	if (IsDue)
	{
		InspectorUi::PoseField(Pose, Latch.Id, Latch.Component) = Latch.Value;
		PutPose(Shot, *Cache, Local, HalfFrame, Pose);
	}
}
}
