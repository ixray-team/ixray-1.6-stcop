#include "StdAfx.h"
#include "CinemaEditor.h"
#include "../../Cinema/CinemaSceneIo.h"
#include "../../Cinema/CinemaShake.h"
#include "../../../xrRHI/RHI.h"
#include "../../../xrEngine/Environment.h"
#include "../../../xrEngine/IGame_Persistent.h"

namespace Cinema::InspectorUi
{
constexpr const char* Modes[6] = { "Linear", "Ease in", "Ease out", "Ease in-out", "Bezier", "Hold" };
constexpr const char* DofModes[3] = { "Off", "Manual", "Focus on object" };
constexpr u32 Rates[4] = { 24, 25, 30, 60 };
constexpr float ListWidth = 360.0f;
constexpr const char* LibraryFile = "cinema_editor\\cinema_library.ltx";
constexpr const char* LibraryKinds[5] = { "model", "item", "animation", "effect", "texture" };
constexpr s32 LibrarySlots[8] = { 0, 1, 2, -1, -1, 3, 4, -1 };
constexpr u32 MaxFavourites = 1024, MaxFolders = 64, MaxFolderItems = 256;
constexpr size_t MaxLibrarySize = 256u << 10;
constexpr const char* LinkProblems[5] = { "Parent is missing: hidden", "Bone is missing: the origin is used", "Look at target is missing: ignored", "Look bone is missing: the origin is used", "Parent is missing: the block passes through" };

float ValueAt(const Object& Target, s32 Block, Param Id, u8 Layer, u8 Component, float Time)
{
	const float Static = *StaticField(const_cast<Object&>(Target), Block, Id, Layer, Component);
	const Lane* Found = Curves::FindLane(LaneList(Target, Block), Id, Layer);
	return Found ? Curves::Result(Found->Curves[Component], LaneLocal(Target, Block, Id, Time), Static, Info(Id)) : Static;
}

KeyState StateAt(const Object& Target, s32 Block, Param Id, u8 Layer, float Time, float HalfFrame)
{
	const Lane* Found = Curves::FindLane(LaneList(Target, Block), Id, Layer);
	for (u8 Component = 0; Found && Component < Info(Id).Components; ++Component)
	{
		if (Curves::KeyAt(Found->Curves[Component], LaneLocal(Target, Block, Id, Time), HalfFrame) != u32(-1))
		{
			return KeyState::OnKey;
		}
	}
	return Found ? KeyState::Animated : KeyState::Static;
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
	ImGui::SetNextWindowPos(ImVec2(View->Pos.x + View->Size.x * 0.76f, View->WorkPos.y), LayoutCond());
	ImGui::SetNextWindowSize(ImVec2(View->Size.x * 0.24f, View->Pos.y + View->Size.y * (ShowAnimator ? 0.6f : 0.72f) - View->WorkPos.y), LayoutCond());
	if (LayoutReset)
	{
		ImGui::SetNextWindowDockID(0, ImGuiCond_Always);
	}
	const bool IsVisible = ImGui::Begin(ShowAnimator ? "Inspector###CinemaInspectorAnimation" : "Inspector###CinemaInspector", &Settings.ShowInspector, ImGuiWindowFlags_NoNavInputs);
	IsOwnWindowFocused |= ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows);
	ViewRight = ImGui::GetWindowPos().x;
	std::erase_if(PinnedIds, [&Core](u32 Id) { return !Find(Core.Data, Id); });
	IsPinned = IsPinned && !PinnedIds.empty();
	xr_vector<u32>* const Lists[7] = { &InspectActors, &InspectCameras, &InspectLights, &InspectParticles, &InspectDecals, &InspectWeathers, &InspectPosts };
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
	if (ShowAnimator)
	{
		const Object* Posed = Find(Core.Data, AnimObject);
		if (Posed && Posed->Type == ObjectType::Actor)
		{
			ImGui::TextUnformatted(DisplayName(Posed));
			DrawPoseSection(Core, *Posed);
		}
		else
		{
			ImGui::TextDisabled("Select an actor to animate.");
		}
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
	const bool HasLightPin = InspectActors.empty(), HasParticlePin = HasLightPin && InspectLights.empty(), HasDecalPin = HasParticlePin && InspectParticles.empty(), HasWeatherPin = HasDecalPin && InspectDecals.empty(), HasPostPin = HasWeatherPin && InspectWeathers.empty(), HasCameraPin = HasPostPin && InspectPosts.empty();
	if (HasCameraPin && InspectCameras.empty())
	{
		DrawSceneSection(Core);
	}
	if (!InspectActors.empty())
	{
		DrawActorSection(Core, InspectActors);
	}
	if (const Object* Posed = InspectActors.size() == 1 ? Find(Core.Data, InspectActors[0]) : nullptr)
	{
		DrawPoseSection(Core, *Posed);
	}
	if (!InspectLights.empty())
	{
		DrawLightSection(Core, InspectLights, HasLightPin);
	}
	if (!InspectParticles.empty())
	{
		DrawParticleSection(Core, InspectParticles, HasParticlePin);
	}
	if (!InspectDecals.empty())
	{
		DrawDecalSection(Core, InspectDecals, HasDecalPin);
	}
	if (!InspectWeathers.empty())
	{
		DrawWeatherSection(Core, InspectWeathers, HasWeatherPin);
	}
	if (!InspectPosts.empty())
	{
		DrawPostSection(Core, InspectPosts, HasPostPin);
	}
	if (!InspectCameras.empty())
	{
		DrawCameraSection(Core, InspectCameras, HasCameraPin);
	}
	ImGui::End();
}

void Editor::DrawRigSection(const ActorRuntime& Runtime)
{
	if (RigObject != PoseObject || !ImGui::TreeNode(Rig.HasOverride ? "Rig (override)" : "Rig (auto)"))
	{
		return;
	}
	const auto Pick = [&](const char* Label, u16& Bone)
	{
		ImGui::SetNextItemWidth(-ImGui::GetFontSize() * 5.0f);
		if (ImGui::BeginCombo(Label, Bone != BI_NONE ? Runtime.BoneName(Bone) : "None", ImGuiComboFlags_HeightLarge))
		{
			for (u16 Id = 0; Id < Runtime.BoneCount(); ++Id)
			{
				if (Runtime.BoneName(Id) && ImGui::Selectable(Runtime.BoneName(Id), Id == Bone))
				{
					Bone = Id;
				}
			}
			ImGui::EndCombo();
		}
	};
	for (u32 Control = 0; Control < RigControlCount; ++Control)
	{
		Pick(RigControlKeys[Control], Rig.Controls[Control]);
	}
	for (u32 Chain = 0; Chain < RigChainCount; ++Chain)
	{
		ImGui::PushID(static_cast<s32>(Chain));
		ImGui::TextUnformatted(RigChainKeys[Chain]);
		Pick("##Root", Rig.Chains[Chain][0]);
		Pick("##Middle", Rig.Chains[Chain][1]);
		Pick("##End", Rig.Chains[Chain][2]);
		ImGui::PopID();
	}
	if (ImGui::Button("Save rig"))
	{
		SaveRig();
	}
	ImGui::SameLine();
	if (ImGui::Button("Auto rig"))
	{
		string_path Path;
		RigPath(Path);
		if (FS.exist(Path))
		{
			FS.file_delete(Path);
		}
		BuildRig(Runtime, RigVisual);
	}
	ImGui::TreePop();
}

void Editor::DrawPoseSection(Player& Core, const Object& Target)
{
	if (!IsPosing || !ImGui::CollapsingHeader("Posing", ImGuiTreeNodeFlags_DefaultOpen))
	{
		return;
	}
	ImGui::PushID("Posing");
	PoseIds.assign(1, Target.Id);
	const xr_vector<u32>& Ids = PoseIds;
	const ActorRuntime* Runtime = Core.FindActor(Target.Id);
	const xr_vector<PoseRow>& Rows = Target.Actor.Poses;
	ActivePose = Rows.empty() ? 0 : std::min<u32>(ActivePose, static_cast<u32>(Rows.size() - 1));
	const bool IsCompact = ShowAnimator;
	if (IsCompact)
	{
		DrawBoneValues(Core, Target);
		ImGui::SeparatorText("IK");
		for (u32 Limb = 0; Limb < MaxIkLimbs; ++Limb)
		{
			const u16* Chain = RigObject == Target.Id ? Rig.Chains[Limb] : nullptr;
			const bool HasChain = Chain && std::find(Chain, Chain + 3, BI_NONE) == Chain + 3;
			bool IsOn = IkOf(Target, IkBlock(Target))[Limb].IsOn;
			ImGui::PushID(static_cast<s32>(Limb));
			if (Limb & 1)
			{
				ImGui::SameLine(ImGui::GetContentRegionAvail().x * 0.5f);
			}
			ImGui::BeginDisabled(!HasChain && !IsOn);
			if (ImGui::Checkbox(RigChainKeys[Limb], &IsOn))
			{
				SetIk(Core, Target, Limb, IsOn);
			}
			ImGui::EndDisabled();
			ImGui::SetItemTooltip(HasChain ? "On: the limb follows its target and pole handles in the game view. Off: its bones are posed one by one" : "The rig of this model has no such limb (Rig and tools -> Rig)");
			ImGui::PopID();
		}
		ImGui::SeparatorText("Select");
		constexpr const char* SetNames[7] = { "Arm L", "Arm R", "Leg L", "Leg R", "Spine", "Fingers L", "Fingers R" };
		for (u32 Set = 0; Set < 7; ++Set)
		{
			if (Set % 4)
			{
				ImGui::SameLine();
			}
			if (ImGui::SmallButton(SetNames[Set]))
			{
				SelectSet(Set, ImGui::GetIO().KeyCtrl || ImGui::GetIO().KeyShift);
			}
		}
		ImGui::SetItemTooltip("Selects every control of that part; Ctrl or Shift adds to the selection");
		ImGui::Checkbox("Bone lines", &ShowBoneLines);
		ImGui::SetItemTooltip("Off: only the rig's controls are drawn and picked. On: every bone is drawn as a line and can be picked");
		if (!ImGui::TreeNode("Rig and tools"))
		{
			ImGui::PopID();
			return;
		}
	}
	for (u32 Row = 0; !IsCompact && Row < Rows.size(); ++Row)
	{
		ImGui::PushID(static_cast<s32>(Row));
		string32 Title;
		xr_sprintf(Title, "Pose %u", Row + 1u);
		if (ImGui::RadioButton(Title, ActivePose == Row))
		{
			ActivePose = Row;
		}
		ImGui::SetItemTooltip("Active pose row");
		ImGui::SameLine();
		const bool IsAdd = Rows[Row].Mode == RowMode::Add;
		if (ImGui::Button(IsAdd ? "Add" : "Override"))
		{
			Edit(Core, Target.Id, "Pose row mode")->Actor.Poses[Row].Mode = IsAdd ? RowMode::Override : RowMode::Add;
		}
		ImGui::SetItemTooltip("Mode");
		ImGui::SameLine();
		bool IsMuted = Rows[Row].IsMuted;
		if (ImGui::Checkbox("M", &IsMuted))
		{
			Edit(Core, Target.Id, "Mute pose row")->Actor.Poses[Row].IsMuted = IsMuted;
		}
		ImGui::SetItemTooltip("Mute");
		ImGui::SameLine();
		const bool IsRemoved = ImGui::Button("x");
		ImGui::SetItemTooltip("Remove pose row");
		ParamRow(Core, Ids, -1, Param::PoseWeight, static_cast<u8>(Row), 0.01f, "%.2f", 1, 1.0f);
		ImGui::PopID();
		if (IsRemoved)
		{
			RemovePoseRow(Core, Target.Id, Row);
			break;
		}
	}
	if (!IsCompact && Rows.size() < MaxPoseRows && ImGui::Button("+ Pose row"))
	{
		Object* Item = Edit(Core, Target.Id, "Add pose row");
		Item->Actor.Poses.emplace_back();
		ActivePose = static_cast<u32>(Item->Actor.Poses.size() - 1);
	}
	if (!Runtime || !Runtime->BoneCount())
	{
		if (IsCompact)
		{
			ImGui::TreePop();
		}
		ImGui::PopID();
		return;
	}
	ImGui::SetNextItemWidth(-1.0f);
	ImGui::InputTextWithHint("##PoseBoneFilter", "Filter bones", PoseBoneFilter, sizeof(PoseBoneFilter));
	ImGui::BeginChild("##Bones", ImVec2(0.0f, ImGui::GetTextLineHeightWithSpacing() * 10.0f), ImGuiChildFlags_Borders);
	for (u16 Bone = 0; Bone < Runtime->BoneCount(); ++Bone)
	{
		const char* Name = Runtime->BoneName(Bone);
		if (!Name || (PoseBoneFilter[0] && !ImStristr(Name, nullptr, PoseBoneFilter, nullptr)))
		{
			continue;
		}
		u32 Depth = 0;
		for (u16 Parent = Runtime->BoneParent(Bone); Parent < Runtime->BoneCount() && Depth < 64; Parent = Runtime->BoneParent(Parent))
		{
			++Depth;
		}
		const auto Found = std::find(SelectedBones.begin(), SelectedBones.end(), Bone);
		ImGui::PushID(Bone);
		ImGui::SetCursorPosX(ImGui::GetCursorPosX() + (PoseBoneFilter[0] ? 0.0f : static_cast<float>(Depth) * ImGui::GetFontSize() * 0.5f));
		if (ImGui::Selectable(Name, Found != SelectedBones.end()))
		{
			ImGui::GetIO().KeyCtrl ? (Found == SelectedBones.end() ? SelectedBones.push_back(Bone) : (void)SelectedBones.erase(Found)) : SelectedBones.assign(1, Bone);
		}
		ImGui::PopID();
	}
	ImGui::EndChild();
	DrawRigSection(*Runtime);
	if (ImGui::TreeNodeEx("IK", ImGuiTreeNodeFlags_DefaultOpen))
	{
		for (u32 Limb = 0; Limb < MaxIkLimbs; ++Limb)
		{
			const IkLimb& Setup = IkOf(Target, IkBlock(Target))[Limb];
			const u16* Chain = RigObject == Target.Id ? Rig.Chains[Limb] : nullptr;
			const bool HasChain = Chain && std::find(Chain, Chain + 3, BI_NONE) == Chain + 3;
			const bool HasLanes = std::any_of(LaneList(Target, IkBlock(Target)).begin(), LaneList(Target, IkBlock(Target)).end(), [Limb](const Lane& Track) { return Track.Id >= Param::IkTarget && Track.Id <= Param::IkPole && Track.Layer == Limb; });
			bool IsOn = Setup.IsOn;
			ImGui::PushID(static_cast<s32>(Limb));
			ImGui::BeginDisabled(!HasChain && !IsOn);
			if (ImGui::Checkbox(RigChainKeys[Limb], &IsOn))
			{
				SetIk(Core, Target, Limb, IsOn);
			}
			ImGui::EndDisabled();
			ImGui::SetItemTooltip("IK");
			if (Setup.IsOn)
			{
				ImGui::SameLine();
				ImGui::BeginDisabled(HasLanes);
				if (ImGui::Button(Setup.Space == IkSpace::World ? "World" : "Actor"))
				{
					SwitchIkSpace(Core, Target, Limb);
				}
				ImGui::EndDisabled();
				ImGui::SetItemTooltip("Space");
				ParamRow(Core, Ids, IkBlock(Target), Param::IkWeight, static_cast<u8>(Limb), 0.01f, "%.2f", 1, 1.0f);
				ParamRow(Core, Ids, IkBlock(Target), Param::IkTarget, static_cast<u8>(Limb), 0.005f, "%.3f", 0, 0.0f);
				ParamRow(Core, Ids, IkBlock(Target), Param::IkTurn, static_cast<u8>(Limb), 0.5f, "%.1f", 0, 0.0f);
				ParamRow(Core, Ids, IkBlock(Target), Param::IkPole, static_cast<u8>(Limb), 0.005f, "%.3f", 0, 0.0f);
			}
			ImGui::PopID();
		}
		ImGui::TreePop();
	}
	DrawConstraints(Core, Target);
	const s32 Block = PoseBlock(Target);
	if (Block >= 0 && !IsCompact)
	{
		ImGui::TextDisabled("Bones go to the clip \"%s\" (Animator)", Target.Actor.Clips[ClipIndex(Block)].Name.c_str());
	}
	if (!IsCompact)
	{
		DrawBoneValues(Core, Target);
	}
	ImGui::BeginDisabled(SelectedBones.empty());
	if (ImGui::Button("Key pose"))
	{
		KeyBones(Core);
	}
	ImGui::SetItemTooltip("Key pose (F)");
	ImGui::SameLine();
	if (ImGui::Button("Reset"))
	{
		ResetBones(Core);
	}
	ImGui::SetItemTooltip("Reset bones");
	ImGui::EndDisabled();
	ImGui::BeginDisabled((Block < 0 && ActivePose >= Rows.size()) || RigObject != Target.Id);
	if (ImGui::Button("Mirror"))
	{
		MirrorBones(Core, true);
	}
	ImGui::SetItemTooltip("Mirror pose");
	ImGui::SameLine();
	if (ImGui::Button("Paste opposite"))
	{
		MirrorBones(Core, false);
	}
	ImGui::SetItemTooltip("Paste opposite");
	ImGui::EndDisabled();
	DrawPoseLibrary(Core);
	if (IsCompact)
	{
		ImGui::TreePop();
	}
	ImGui::PopID();
}

void Editor::SelectSet(u32 Set, bool IsAdding)
{
	constexpr s8 Sets[7][6] = { { 14, 3, -1, -1, -1, -1 }, { 15, 4, -1, -1, -1, -1 }, { 5, 16, -1, -1, -1, -1 }, { 6, 17, -1, -1, -1, -1 }, { 0, 11, 12, 1, 13, 2 }, { 18, 19, 20, 21, 22, -1 }, { 23, 24, 25, 26, 27, -1 } };
	if (RigObject != PoseObject || Set >= 7)
	{
		return;
	}
	if (!IsAdding)
	{
		SelectedBones.clear();
	}
	const auto Add = [this](u16 Bone)
	{
		if (Bone != BI_NONE && !IsBoneShared(Bone) && std::find(SelectedBones.begin(), SelectedBones.end(), Bone) == SelectedBones.end())
		{
			SelectedBones.push_back(Bone);
		}
	};
	for (s8 Control : Sets[Set])
	{
		Add(Control >= 0 ? Rig.Controls[Control] : BI_NONE);
	}
	for (u32 Link = 0; Set < RigChainCount && Link < 3; ++Link)
	{
		Add(Rig.Chains[Set][Link]);
	}
}

bool Editor::DrawAttachSwitches(Player& Core, const Object& Target)
{
	const float Local = std::clamp(Core.Time - Target.Span.Start, 0.0f, Target.Span.Length);
	const xr_vector<AttachSwitch>& Switches = Target.Attach.Switches;
	s32 Removed = -1;
	ImGui::PushID("AttachSwitches");
	ImGui::SeparatorText("Bone over time");
	for (u32 Index = 0; Index < Switches.size(); ++Index)
	{
		ImGui::PushID(static_cast<s32>(Index));
		if (ImGui::SmallButton("Go"))
		{
			Core.Seek(Target.Span.Start + Switches[Index].Time);
		}
		ImGui::SameLine();
		Removed = ImGui::SmallButton("x") ? static_cast<s32>(Index) : Removed;
		ImGui::SameLine();
		ImGui::Text("%.2f s: %s", Switches[Index].Time, Switches[Index].Bone.size() ? Switches[Index].Bone.c_str() : "origin");
		ImGui::PopID();
	}
	AttachLink Pick { Target.Attach.Parent.ObjectId, AttachSwitchBone };
	ImGui::SetNextItemWidth(ImGui::GetFontSize() * 9.0f);
	BoneCombo(Core, "##SwitchBone", Pick);
	AttachSwitchBone = Pick.Bone;
	ImGui::SameLine();
	ImGui::Checkbox("Keep place", &IsSwitchKeeping);
	ImGui::SetItemTooltip("Keys the position and rotation at the switch so the object stays where it is when it changes bone");
	const bool IsAdded = Switches.size() < MaxAttachSwitches && ImGui::Button("Switch to this bone at the playhead");
	ImGui::PopID();
	if (Removed >= 0)
	{
		xr_vector<AttachSwitch>& Edited = Edit(Core, Target.Id, "Remove attach switch")->Attach.Switches;
		Edited.erase(Edited.begin() + Removed);
	}
	if (IsAdded)
	{
		AddAttachSwitch(Core, Target.Id, Local);
	}
	return Removed >= 0 || IsAdded;
}

void Editor::AddAttachSwitch(Player& Core, u32 Id, float Local)
{
	const ObjectState* State = Core.FindState(Id);
	Object& Item = *Edit(Core, Id, "Switch attach bone");
	const float Frame = 1.0f / static_cast<float>(Core.Data.Header.FrameRate), Half = 0.5f * Frame;
	Fmatrix Hook;
	Fvector* const Places[2] = { PlacementOf(Item, 0), PlacementOf(Item, 1) };
	if (IsSwitchKeeping && State && Places[0] && Places[1] && Core.LinkFrame({ Item.Attach.Parent.ObjectId, AttachSwitchBone }, Hook))
	{
		Fmatrix Back, Seat;
		Fvector Angles;
		Back.invert(Hook);
		Seat.mul_43(Back, State->World);
		Seat.i.normalize_safe();
		Seat.j.normalize_safe();
		Seat.k.normalize_safe();
		Seat.getHPB(Angles);
		for (u32 Kind = 0; Kind < 2; ++Kind)
		{
			for (u8 Axis = 0; Axis < 3; ++Axis)
			{
				Curve& Part = Curves::EnsureLane(Item.Lanes, Kind ? Param::Rotation : Param::Position, 0).Curves[Axis];
				const float Old = Curves::Value(Part, Local, (*Places[Kind])[Axis]), Degrees = rad2deg(Angles[Axis]);
				const float New = Kind ? Degrees + 360.0f * std::round((Old - Degrees) / 360.0f) : Seat.c[Axis];
				if (Local > Frame)
				{
					Curves::SetKey(Part, Local - Frame, Old, Half);
					const u32 Before = Curves::KeyAt(Part, Local - Frame, Half);
					Before != u32(-1) ? Curves::SetMode(Part, Before, Interp::Hold) : (void)0;
				}
				Curves::SetKey(Part, Local, New, Half);
			}
		}
	}
	xr_vector<AttachSwitch>& Switches = Item.Attach.Switches;
	std::erase_if(Switches, [Local, Half](const AttachSwitch& Other) { return std::fabs(Other.Time - Local) <= Half; });
	Switches.push_back({ Local, AttachSwitchBone });
	std::stable_sort(Switches.begin(), Switches.end(), [](const AttachSwitch& A, const AttachSwitch& B) { return A.Time < B.Time; });
}

void Editor::DrawBoneValues(Player& Core, const Object& Target)
{
	const ActorRuntime* Runtime = Core.FindActor(Target.Id);
	const xr_vector<PoseRow>& Rows = Target.Actor.Poses;
	const s32 Block = PoseBlock(Target);
	if (!Runtime || SelectedBones.size() != 1 || (Block < 0 && ActivePose >= Rows.size()))
	{
		if (ShowAnimator)
		{
			ImGui::TextDisabled(SelectedBones.size() > 1 ? "Several bones are selected." : "Select a bone to see its values.");
		}
		return;
	}
	const xr_vector<PoseBone>& Bones = Block < 0 ? Rows[ActivePose].Bones : TrackOf(Target, Block).Bones;
	const char* Name = Runtime->BoneName(SelectedBones[0]);
	const auto Entry = std::find_if(Bones.begin(), Bones.end(), [Name](const PoseBone& Item) { return !xr_strcmp(Item.Name, Name); });
	if (ShowAnimator)
	{
		ImGui::TextUnformatted(Name ? Name : "");
	}
	if (Entry == Bones.end())
	{
		if (ShowAnimator)
		{
			ImGui::TextDisabled("Not posed yet: move it in the game view.");
		}
		return;
	}
	const u32 Index = static_cast<u32>(Entry - Bones.begin());
	const u8 Layer = Block < 0 ? PoseLayer(ActivePose, Index) : static_cast<u8>(Index);
	ParamRow(Core, PoseIds, Block, Param::BoneRotation, Layer, 0.5f, "%.1f", 7, 0.0f);
	ParamRow(Core, PoseIds, Block, Param::BonePosition, Layer, 0.005f, "%.3f", 7, 0.0f);
	ParamRow(Core, PoseIds, Block, Param::BoneScale, Layer, 0.005f, "%.3f", 7, 1.0f);
}

bool Editor::ReadPoseValue(const Object& Item, u16 Bone, PoseValue& Out) const
{
	const Player* Core = FindPlayer();
	const ActorRuntime* Runtime = Core ? Core->FindActor(Item.Id) : nullptr;
	const char* Name = Runtime ? Runtime->BoneName(Bone) : nullptr;
	const s32 Block = PoseBlock(Item);
	if (!Name || (Block < 0 && ActivePose >= Item.Actor.Poses.size()))
	{
		return false;
	}
	Out = PoseValue { Name };
	const xr_vector<PoseBone>& Bones = Block < 0 ? Item.Actor.Poses[ActivePose].Bones : TrackOf(Item, Block).Bones;
	const auto Entry = std::find_if(Bones.begin(), Bones.end(), [&Out](const PoseBone& Other) { return Other.Name == Out.Bone; });
	if (Entry == Bones.end())
	{
		return true;
	}
	const float Local = PoseLocal(*Core, Item);
	const u32 Index = static_cast<u32>(Entry - Bones.begin());
	const u8 Layer = Block < 0 ? PoseLayer(ActivePose, Index) : static_cast<u8>(Index);
	const xr_vector<Lane>& Tracks = LaneList(Item, Block);
	Out.Turn = Curves::LaneVector(Tracks, Param::BoneRotation, Local, Entry->Rotation, Layer);
	Out.Shift = Curves::LaneVector(Tracks, Param::BonePosition, Local, Entry->Offset, Layer);
	Out.Size = Curves::LaneVector(Tracks, Param::BoneScale, Local, Entry->Scale, Layer);
	return true;
}

void Editor::WritePoses(Player& Core, const char* Label, const xr_vector<PoseValue>& Values, bool IsMirrored)
{
	const ActorRuntime* Runtime = Core.FindActor(PoseObject);
	Object* Item = Runtime && !Values.empty() ? Edit(Core, PoseObject, Label) : nullptr;
	if (!Item)
	{
		return;
	}
	u32 Missing = 0, Side = 0;
	for (u16 Bone = 0; IsMirrored && Bone < Rig.Mirror.size(); ++Bone)
	{
		Fmatrix A, B;
		if (Rig.Mirror[Bone] != BI_NONE && Runtime->RestModel(Bone, A) && Runtime->RestModel(Rig.Mirror[Bone], B))
		{
			const Fvector Gap = Fvector().sub(A.c, B.c);
			Side = std::fabs(Gap[2]) > std::fabs(Gap[Side]) ? 2 : std::fabs(Gap[1]) > std::fabs(Gap[Side]) ? 1 : Side;
		}
	}
	for (const PoseValue& Value : Values)
	{
		const u16 From = Runtime->FindBone(Value.Bone);
		const u16 To = IsMirrored && From < Rig.Mirror.size() && Rig.Mirror[From] != BI_NONE ? Rig.Mirror[From] : From;
		Fmatrix FromRest, ToRest;
		const bool HasFrames = !IsMirrored || (Runtime->RestModel(From, FromRest) && Runtime->RestModel(To, ToRest));
		const s32 Layer = To != BI_NONE && HasFrames ? EnsurePoseBone(*Item, Runtime->BoneName(To)) : -1;
		Missing += Layer < 0;
		if (Layer < 0)
		{
			continue;
		}
		PoseValue Result = Value;
		if (IsMirrored)
		{
			Fmatrix Flip = Fidentity, Map, Back, Delta;
			Flip.i.x = Side == 0 ? -1.0f : 1.0f;
			Flip.j.y = Side == 1 ? -1.0f : 1.0f;
			Flip.k.z = Side == 2 ? -1.0f : 1.0f;
			FromRest.c.set(0.0f, 0.0f, 0.0f);
			ToRest.c.set(0.0f, 0.0f, 0.0f);
			Back.invert(ToRest);
			Map.mul_43(Back, Flip);
			Map.mulB_43(FromRest);
			Back.invert(Map);
			Delta.setHPB(deg2rad(Value.Turn.x), deg2rad(Value.Turn.y), deg2rad(Value.Turn.z));
			Delta.c = Value.Shift;
			Delta.mulA_43(Map);
			Delta.mulB_43(Back);
			Fvector Angles;
			Delta.getHPB(Angles);
			Result.Turn.set(rad2deg(Angles.x), rad2deg(Angles.y), rad2deg(Angles.z));
			Result.Shift = Delta.c;
			const Fvector* Axes[3] = { &Map.i, &Map.j, &Map.k };
			for (u32 Axis = 0; Axis < 3; ++Axis)
			{
				const Fvector& Column = *Axes[Axis];
				const u32 Target = std::fabs(Column.z) > std::fabs(Column.x) && std::fabs(Column.z) > std::fabs(Column.y) ? 2 : std::fabs(Column.y) > std::fabs(Column.x) ? 1 : 0;
				Result.Size[Target] = Value.Size[Axis];
			}
		}
		for (u8 Axis = 0; Axis < 3; ++Axis)
		{
			WriteParam(Core, *Item, PoseBlock(*Item), Param::BoneRotation, static_cast<u8>(Layer), Axis, Result.Turn[Axis]);
			WriteParam(Core, *Item, PoseBlock(*Item), Param::BonePosition, static_cast<u8>(Layer), Axis, std::clamp(Result.Shift[Axis], -10.0f, 10.0f));
			WriteParam(Core, *Item, PoseBlock(*Item), Param::BoneScale, static_cast<u8>(Layer), Axis, std::clamp(Result.Size[Axis], 0.01f, 100.0f));
		}
	}
	if (Missing)
	{
		SetStatus(true, "%u bones were skipped: missing on this model, without rest frames, or past 64 per pose row.", Missing);
	}
}

void Editor::MirrorBones(Player& Core, bool IsSwap)
{
	const Object* Item = Find(Core.Data, PoseObject);
	const ActorRuntime* Runtime = Item ? Core.FindActor(PoseObject) : nullptr;
	const s32 Block = Item ? PoseBlock(*Item) : -1;
	if (!Runtime || (Block < 0 && ActivePose >= Item->Actor.Poses.size()) || RigObject != PoseObject)
	{
		return;
	}
	const xr_vector<PoseBone>& Posed = Block < 0 ? Item->Actor.Poses[ActivePose].Bones : TrackOf(*Item, Block).Bones;
	xr_vector<u16> Picked = SelectedBones;
	for (u32 Index = 0; SelectedBones.empty() && Index < Posed.size(); ++Index)
	{
		const u16 Bone = Runtime->FindBone(Posed[Index].Name);
		Bone != BI_NONE ? Picked.push_back(Bone) : (void)0;
	}
	PoseScratch.clear();
	for (u16 Bone : Picked)
	{
		const u16 Other = Bone < Rig.Mirror.size() ? Rig.Mirror[Bone] : BI_NONE;
		PoseValue Value;
		if ((IsSwap || Other != BI_NONE) && ReadPoseValue(*Item, Bone, Value))
		{
			PoseScratch.push_back(Value);
		}
		const bool IsListed = std::find(Picked.begin(), Picked.end(), Other) != Picked.end();
		if (IsSwap && Other != BI_NONE && !IsListed && ReadPoseValue(*Item, Other, Value))
		{
			PoseScratch.push_back(Value);
		}
	}
	WritePoses(Core, IsSwap ? "Mirror pose" : "Paste opposite", PoseScratch, true);
}

void Editor::LoadPoseLibrary()
{
	IsPoseLibraryRead = true;
	PoseLibrary.clear();
	string_path Path;
	FS.update_path(Path, "$app_data_root$", "cinema\\poses.ltx");
	const CReaderGuarded File(FS.exist(Path) ? FS.r_open(Path) : nullptr);
	xr_string Error = "the file is larger than 1 MiB";
	if (!File)
	{
		return;
	}
	if (static_cast<size_t>(File->length()) > 1048576 || !Io::PreScan(*File, Error))
	{
		Msg("! Cinema editor: %s is ignored: %s", Path, Error.c_str());
		return;
	}
	File->seek(0);
	const CInifile Ini(&*File, nullptr);
	for (const CInifile::Sect& Section : Ini.sections())
	{
		if (PoseLibrary.size() >= MaxNamedPoses)
		{
			break;
		}
		NamedPose& Pose = PoseLibrary.emplace_back();
		Pose.Name = Section.Name.c_str();
		Pose.Bones.clear();
		for (const CInifile::Item& Line : Section.Data)
		{
			float Values[9] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 1.0f, 1.0f };
			const u32 Count = Line.second.size() ? static_cast<u32>(_GetItemCount(Line.second.c_str())) : 0;
			string64 Part;
			for (u32 Index = 0; Count == 9 && Index < 9; ++Index)
			{
				Values[Index] = static_cast<float>(atof(_GetItem(Line.second.c_str(), static_cast<int>(Index), Part)));
			}
			if (Count != 9 || Pose.Bones.size() >= MaxPoseBones || !std::all_of(std::begin(Values), std::end(Values), [](float Value) { return std::isfinite(Value); }))
			{
				Msg("! Cinema editor: %s: [%s] %s is not 9 numbers or the pose has more than 64 bones, skipped", Path, Pose.Name.c_str(), Line.first.c_str());
				continue;
			}
			PoseValue& Bone = Pose.Bones.emplace_back();
			Bone.Bone = Line.first;
			Bone.Turn.set(std::clamp(Values[0], -3600.0f, 3600.0f), std::clamp(Values[1], -3600.0f, 3600.0f), std::clamp(Values[2], -3600.0f, 3600.0f));
			Bone.Shift.set(std::clamp(Values[3], -10.0f, 10.0f), std::clamp(Values[4], -10.0f, 10.0f), std::clamp(Values[5], -10.0f, 10.0f));
			Bone.Size.set(std::clamp(Values[6], 0.01f, 100.0f), std::clamp(Values[7], 0.01f, 100.0f), std::clamp(Values[8], 0.01f, 100.0f));
		}
	}
	std::sort(PoseLibrary.begin(), PoseLibrary.end(), [](const NamedPose& A, const NamedPose& B) { return A.Name < B.Name; });
}

void Editor::SavePoseLibrary()
{
	CInifile Ini(nullptr, false, false, false);
	for (const NamedPose& Pose : PoseLibrary)
	{
		for (const PoseValue& Bone : Pose.Bones)
		{
			string512 Text;
			xr_sprintf(Text, "%g, %g, %g, %g, %g, %g, %g, %g, %g", Bone.Turn.x, Bone.Turn.y, Bone.Turn.z, Bone.Shift.x, Bone.Shift.y, Bone.Shift.z, Bone.Size.x, Bone.Size.y, Bone.Size.z);
			Ini.w_string(Pose.Name.c_str(), Bone.Bone.c_str(), Text);
		}
	}
	string_path Path;
	FS.update_path(Path, "$app_data_root$", "cinema\\poses.ltx");
	xr_string Error;
	if (!Io::SaveIni(Ini, Path, Error))
	{
		SetStatus(true, "%s", Error.c_str());
	}
}

void Editor::DrawPoseLibrary(Player& Core)
{
	if (!ImGui::TreeNodeEx("Pose library"))
	{
		return;
	}
	if (!IsPoseLibraryRead)
	{
		LoadPoseLibrary();
	}
	const Object* Item = Find(Core.Data, PoseObject);
	const ActorRuntime* Runtime = Item ? Core.FindActor(PoseObject) : nullptr;
	ImGui::SetNextItemWidth(std::max(ImGui::GetContentRegionAvail().x - ImGui::CalcTextSize("Save").x - 3.0f * ImGui::GetStyle().FramePadding.x, 1.0f));
	ImGui::InputTextWithHint("##PoseName", "Pose name", PoseName, sizeof(PoseName));
	ImGui::SameLine();
	const bool IsNamed = PoseName[0] && Io::IniSafe(PoseName);
	ImGui::BeginDisabled(!IsNamed || !Runtime || ActivePose >= Item->Actor.Poses.size() || PoseLibrary.size() >= MaxNamedPoses);
	if (ImGui::Button("Save"))
	{
		const auto Found = std::find_if(PoseLibrary.begin(), PoseLibrary.end(), [this](const NamedPose& Pose) { return Pose.Name == PoseName; });
		NamedPose& Pose = Found == PoseLibrary.end() ? PoseLibrary.emplace_back() : *Found;
		Pose = NamedPose { PoseName };
		for (const PoseBone& Entry : Item->Actor.Poses[ActivePose].Bones)
		{
			const u16 Bone = Runtime->FindBone(Entry.Name);
			const bool IsPicked = SelectedBones.empty() || std::find(SelectedBones.begin(), SelectedBones.end(), Bone) != SelectedBones.end();
			PoseValue Value;
			if (IsPicked && Bone != BI_NONE && ReadPoseValue(*Item, Bone, Value))
			{
				Pose.Bones.push_back(Value);
			}
		}
		std::sort(PoseLibrary.begin(), PoseLibrary.end(), [](const NamedPose& A, const NamedPose& B) { return A.Name < B.Name; });
		SavePoseLibrary();
		SetStatus(false, "Pose saved");
	}
	ImGui::EndDisabled();
	ImGui::SetItemTooltip("Save pose");
	s32 Removed = -1;
	for (u32 Index = 0; Index < PoseLibrary.size(); ++Index)
	{
		const NamedPose& Pose = PoseLibrary[Index];
		ImGui::PushID(static_cast<s32>(Index));
		ImGui::AlignTextToFramePadding();
		ImGui::Text("%s (%u)", Pose.Name.c_str(), static_cast<u32>(Pose.Bones.size()));
		ImGui::SameLine();
		ImGui::BeginDisabled(!Runtime);
		if (ImGui::Button("Apply"))
		{
			WritePoses(Core, "Apply pose", Pose.Bones, false);
		}
		ImGui::SetItemTooltip("Apply");
		ImGui::SameLine();
		ImGui::BeginDisabled(RigObject != PoseObject);
		if (ImGui::Button("Mirrored"))
		{
			WritePoses(Core, "Apply pose mirrored", Pose.Bones, true);
		}
		ImGui::EndDisabled();
		ImGui::SetItemTooltip("Apply mirrored");
		ImGui::EndDisabled();
		ImGui::SameLine();
		Removed = ImGui::Button("x") ? static_cast<s32>(Index) : Removed;
		ImGui::SetItemTooltip("Delete pose");
		ImGui::PopID();
	}
	if (Removed >= 0)
	{
		PoseLibrary.erase(PoseLibrary.begin() + Removed);
		SavePoseLibrary();
	}
	ImGui::TreePop();
}

void Editor::SetIk(Player& Core, const Object& Target, u32 Limb, bool IsOn)
{
	const ActorRuntime* Runtime = Core.FindActor(Target.Id);
	const u16* Chain = Rig.Chains[Limb];
	Fmatrix Root, Joint, End;
	const bool HasPose = Runtime && RigObject == Target.Id && Runtime->BoneWorld(Chain[0], Fidentity, Root) && Runtime->BoneWorld(Chain[1], Fidentity, Joint) && Runtime->BoneWorld(Chain[2], Fidentity, End);
	if (IsOn && !HasPose)
	{
		return;
	}
	IkLimb& Item = IkOf(*Edit(Core, Target.Id, IsOn ? "IK on" : "IK off"), IkBlock(Target))[Limb];
	Item.IsOn = IsOn;
	IkHandle = IsOn ? IkHandle : -1;
	if (!IsOn)
	{
		return;
	}
	Fvector Angles, Line, Bend;
	End.getHPB(Angles);
	Line.sub(End.c, Root.c).normalize_safe();
	Bend.sub(Joint.c, Root.c);
	Bend.mad(Line, -Bend.dotproduct(Line)).normalize_safe();
	for (u32 Link = 0; Link < 3; ++Link)
	{
		Item.Bones[Link] = Runtime->BoneName(Chain[Link]);
	}
	Item.Space = IkSpace::Actor;
	Item.Weight = 1.0f;
	Item.Target = End.c;
	Item.Turn.set(rad2deg(Angles.x), rad2deg(Angles.y), rad2deg(Angles.z));
	Item.Pole.mad(Joint.c, Bend, 0.5f);
}

void Editor::SwitchIkSpace(Player& Core, const Object& Target, u32 Limb)
{
	const ObjectState* State = Core.FindState(Target.Id);
	if (!State)
	{
		return;
	}
	IkLimb& Item = IkOf(*Edit(Core, Target.Id, "IK space"), IkBlock(Target))[Limb];
	const bool IsWorld = Item.Space == IkSpace::World;
	Fmatrix Change, Goal;
	IsWorld ? Change.invert(State->World) : Change.set(State->World);
	Goal.setHPB(deg2rad(Item.Turn.x), deg2rad(Item.Turn.y), deg2rad(Item.Turn.z));
	Goal.c = Item.Target;
	Goal.mulA_43(Change);
	Goal.i.normalize_safe();
	Goal.j.normalize_safe();
	Goal.k.normalize_safe();
	Fvector Angles;
	Goal.getHPB(Angles);
	Item.Target = Goal.c;
	Item.Turn.set(rad2deg(Angles.x), rad2deg(Angles.y), rad2deg(Angles.z));
	Change.transform_tiny(Item.Pole);
	Item.Space = IsWorld ? IkSpace::Actor : IkSpace::World;
}

void Editor::DrawConstraints(Player& Core, const Object& Target)
{
	if (!ImGui::TreeNodeEx("Constraints", ImGuiTreeNodeFlags_DefaultOpen))
	{
		return;
	}
	const ActorRuntime* Runtime = Core.FindActor(Target.Id);
	const xr_vector<BoneConstraint>& Rules = Target.Actor.Constraints;
	s32 Removed = -1;
	for (u32 Slot = 0; Slot < Rules.size(); ++Slot)
	{
		const BoneConstraint& Rule = Rules[Slot];
		const bool IsMissing = (Rule.Target.ObjectId && !Find(Core.Data, Rule.Target.ObjectId)) || (Runtime && Runtime->FindBone(Rule.Bone) == BI_NONE);
		ImGui::PushID(static_cast<s32>(Slot));
		ImGui::AlignTextToFramePadding();
		ImGui::TextColored(IsMissing ? ErrorColor : ImGui::GetStyleColorVec4(ImGuiCol_Text), "%s", Rule.Bone.size() ? Rule.Bone.c_str() : "(no bone)");
		ImGui::SameLine();
		bool IsMuted = Rule.IsMuted;
		if (ImGui::Checkbox("M", &IsMuted))
		{
			Edit(Core, Target.Id, IsMuted ? "Mute constraint" : "Unmute constraint")->Actor.Constraints[Slot].IsMuted = IsMuted;
		}
		ImGui::SetItemTooltip("Mute");
		ImGui::SameLine();
		Removed = ImGui::Button("x") ? static_cast<s32>(Slot) : Removed;
		ImGui::SetItemTooltip("Remove constraint");
		s32 Kind = static_cast<s32>(Rule.Kind);
		BeginRow("Kind", false, false);
		const bool IsKindChanged = ImGui::Combo("##Kind", &Kind, "Point\0" "Orient\0" "Parent\0" "Aim\0");
		EndRow(KeyState::None);
		AttachLink Link = Rule.Target;
		BeginRow("Target", false, false);
		bool IsPicked = AttachCombo(Core, "##Target", Target.Id, Link);
		EndRow(KeyState::None);
		BeginRow("Target bone", false, false);
		IsPicked = BoneCombo(Core, "##Bone", Link) || IsPicked;
		EndRow(KeyState::None);
		if (IsKindChanged)
		{
			Edit(Core, Target.Id, "Constraint kind")->Actor.Constraints[Slot].Kind = static_cast<ConstraintKind>(Kind);
			CaptureConstraint(Core, Target.Id, Slot, false);
		}
		if (IsPicked && !(Link == Rule.Target) && Link.ObjectId != Target.Id && (Link.ObjectId == Rule.Target.ObjectId || CanAttach(Core.Data, Target.Id, Link.ObjectId)))
		{
			Edit(Core, Target.Id, "Constraint target")->Actor.Constraints[Slot].Target = Link;
			CaptureConstraint(Core, Target.Id, Slot, false);
		}
		else if (IsPicked && !(Link == Rule.Target))
		{
			SetStatus(true, "The constraint cannot point there: it would form a loop or a chain deeper than %u.", MaxAttachDepth);
		}
		ParamRow(Core, PoseIds, -1, Param::ConstraintWeight, static_cast<u8>(Slot), 0.01f, "%.2f", 1, 1.0f);
		BeginRow("Offset", false, false);
		if (ImGui::Button("Keep pose"))
		{
			CaptureConstraint(Core, Target.Id, Slot, false);
		}
		ImGui::SetItemTooltip("Keep pose");
		ImGui::SameLine();
		ImGui::BeginDisabled(Rule.Kind == ConstraintKind::Aim);
		if (ImGui::Button("Snap"))
		{
			CaptureConstraint(Core, Target.Id, Slot, true);
		}
		ImGui::EndDisabled();
		ImGui::SetItemTooltip("Snap");
		EndRow(KeyState::None);
		ImGui::PopID();
	}
	if (Removed >= 0)
	{
		Object& Item = *Edit(Core, Target.Id, "Remove constraint");
		const u8 Slot = static_cast<u8>(Removed);
		std::erase_if(Item.Lanes, [Slot](const Lane& Track) { return Track.Id == Param::ConstraintWeight && Track.Layer == Slot; });
		for (Lane& Track : Item.Lanes)
		{
			Track.Layer = static_cast<u8>(Track.Id == Param::ConstraintWeight && Track.Layer > Slot ? Track.Layer - 1 : Track.Layer);
		}
		Item.Actor.Constraints.erase(Item.Actor.Constraints.begin() + Removed);
		SelectedKeys.clear();
		Slots.clear();
	}
	ImGui::BeginDisabled(SelectedBones.size() != 1 || !Runtime || Rules.size() >= MaxConstraints);
	if (ImGui::Button("+ Constraint", ImVec2(-FLT_MIN, 0.0f)))
	{
		BoneConstraint& Rule = Edit(Core, Target.Id, "Add constraint")->Actor.Constraints.emplace_back();
		Rule.Bone = Runtime->BoneName(SelectedBones[0]);
	}
	ImGui::EndDisabled();
	ImGui::SetItemTooltip("Add constraint");
	ImGui::TreePop();
}

void Editor::CaptureConstraint(Player& Core, u32 Id, u32 Slot, bool IsSnap)
{
	const ActorRuntime* Runtime = Core.FindActor(Id);
	const ObjectState* State = Core.FindState(Id);
	BoneConstraint& Rule = Edit(Core, Id, IsSnap ? "Snap constraint" : "Keep pose")->Actor.Constraints[Slot];
	Fmatrix Goal, Bone, Back;
	Rule.Shift.set(0.0f, 0.0f, 0.0f);
	Rule.Turn.set(0.0f, 0.0f, 0.0f);
	if (IsSnap || !Runtime || !State || !Core.LinkFrame(Rule.Target, Goal) || !Runtime->BoneWorld(Runtime->FindBone(Rule.Bone), State->World, Bone))
	{
		return;
	}
	Bone.i.normalize_safe();
	Bone.j.normalize_safe();
	Bone.k.normalize_safe();
	if (Rule.Kind == ConstraintKind::Aim)
	{
		Fvector Direction;
		Direction.sub(Goal.c, Bone.c).normalize_safe();
		Back.invert(Bone);
		Back.transform_dir(Rule.Shift, Direction);
		Rule.Shift.normalize_safe();
		return;
	}
	Fmatrix Offset;
	Back.invert(Goal);
	Offset.mul_43(Back, Bone);
	Fvector Angles;
	Offset.getHPB(Angles);
	Rule.Shift = Offset.c;
	Rule.Turn.set(rad2deg(Angles.x), rad2deg(Angles.y), rad2deg(Angles.z));
}

void Editor::KeyBones(Player& Core)
{
	const ActorRuntime* Runtime = Core.FindActor(PoseObject);
	Object* Item = Runtime && !SelectedBones.empty() ? Edit(Core, PoseObject, "Key pose") : nullptr;
	const bool WasAutoKey = IsAutoKey;
	IsAutoKey = true;
	for (u32 Index = 0; Item && Index < SelectedBones.size(); ++Index)
	{
		const s32 Layer = EnsurePoseBone(*Item, Runtime->BoneName(SelectedBones[Index]));
		if (Layer < 0)
		{
			continue;
		}
		const float Local = PoseLocal(Core, *Item);
		const s32 Block = PoseBlock(*Item);
		const xr_vector<Lane>& Tracks = LaneList(*Item, Block);
		const PoseBone& Entry = *BoneEntry(*Item, Block, static_cast<u8>(Layer));
		const Fvector Turn = Curves::LaneVector(Tracks, Param::BoneRotation, Local, Entry.Rotation, static_cast<u8>(Layer));
		const Fvector Shift = Curves::LaneVector(Tracks, Param::BonePosition, Local, Entry.Offset, static_cast<u8>(Layer));
		const Fvector Size = Curves::LaneVector(Tracks, Param::BoneScale, Local, Entry.Scale, static_cast<u8>(Layer));
		for (u8 Axis = 0; Axis < 3; ++Axis)
		{
			WriteParam(Core, *Item, Block, Param::BoneRotation, static_cast<u8>(Layer), Axis, Turn[Axis]);
			WriteParam(Core, *Item, Block, Param::BonePosition, static_cast<u8>(Layer), Axis, Shift[Axis]);
			WriteParam(Core, *Item, Block, Param::BoneScale, static_cast<u8>(Layer), Axis, Size[Axis]);
		}
	}
	IsAutoKey = WasAutoKey;
}

void Editor::ResetBones(Player& Core)
{
	const ActorRuntime* Runtime = Core.FindActor(PoseObject);
	Object* Item = Runtime && !SelectedBones.empty() ? Edit(Core, PoseObject, "Reset bones") : nullptr;
	if (const s32 Block = Item ? PoseBlock(*Item) : -1; Block >= 0)
	{
		ClipTrack& Clip = TrackOf(*Item, Block);
		for (u32 Bone = 0; Bone < Clip.Bones.size(); ++Bone)
		{
			if (std::any_of(SelectedBones.begin(), SelectedBones.end(), [&](u16 Picked) { return !xr_strcmp(Clip.Bones[Bone].Name, Runtime->BoneName(Picked)); }))
			{
				Clip.Bones[Bone] = PoseBone { Clip.Bones[Bone].Name };
				std::erase_if(Clip.Lanes, [Bone](const Lane& Track) { return Track.Layer == Bone; });
			}
		}
		AnimKeys.clear();
		return;
	}
	for (u32 Row = 0; Item && Row < Item->Actor.Poses.size(); ++Row)
	{
		xr_vector<PoseBone>& Bones = Item->Actor.Poses[Row].Bones;
		for (u32 Bone = 0; Bone < Bones.size(); ++Bone)
		{
			const bool IsPicked = std::any_of(SelectedBones.begin(), SelectedBones.end(), [&](u16 Picked) { return !xr_strcmp(Bones[Bone].Name, Runtime->BoneName(Picked)); });
			if (!IsPicked)
			{
				continue;
			}
			Bones[Bone].Rotation.set(0.0f, 0.0f, 0.0f);
			Bones[Bone].Offset.set(0.0f, 0.0f, 0.0f);
			Bones[Bone].Scale.set(1.0f, 1.0f, 1.0f);
			const u8 Layer = PoseLayer(Row, Bone);
			std::erase_if(Item->Lanes, [Layer](const Lane& Track) { return IsBoneParam(Track.Id) && Track.Layer == Layer; });
		}
	}
	SelectedKeys.clear();
}

void Editor::RemovePoseRow(Player& Core, u32 Id, u32 Row)
{
	Object* Item = Edit(Core, Id, "Remove pose row");
	if (!Item || Row >= Item->Actor.Poses.size())
	{
		return;
	}
	std::erase_if(Item->Lanes, [Row](const Lane& Track) { return (Track.Id == Param::PoseWeight && Track.Layer == Row) || (IsBoneParam(Track.Id) && Track.Layer / MaxPoseBones == Row); });
	for (Lane& Track : Item->Lanes)
	{
		const bool IsBone = IsBoneParam(Track.Id);
		Track.Layer = static_cast<u8>(Track.Id == Param::PoseWeight && Track.Layer > Row ? Track.Layer - 1 : IsBone && Track.Layer / MaxPoseBones > Row ? Track.Layer - MaxPoseBones : Track.Layer);
	}
	Item->Actor.Poses.erase(Item->Actor.Poses.begin() + Row);
	ActivePose = Item->Actor.Poses.empty() ? 0 : std::min<u32>(ActivePose, static_cast<u32>(Item->Actor.Poses.size() - 1));
	SelectedKeys.clear();
	Slots.clear();
}

void Editor::DrawShots(Player& Core)
{
	if (!Settings.ShowShots)
	{
		return;
	}
	const ImGuiViewport* View = ImGui::GetMainViewport();
	ImGui::SetNextWindowPos(ImVec2(View->Pos.x + View->Size.x * 0.48f, View->WorkPos.y), ImGuiCond_FirstUseEver);
	ImGui::SetNextWindowSize(ImVec2(View->Size.x * 0.14f, View->Size.y * 0.4f), ImGuiCond_FirstUseEver);
	if (!ImGui::Begin("Shots & Markers###CinemaShots", &Settings.ShowShots, ImGuiWindowFlags_NoNavInputs))
	{
		ImGui::End();
		return;
	}
	const Object* Rig = FindCamera(Core.Data);
	xr_vector<Marker>& Markers = Core.Data.Header.Markers;
	const u32 Rate = Core.Data.Header.FrameRate;
	const bool IsFocused = ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows);
	const auto Row = [&](s32 Index, bool IsMarker, const char* Name, float Time)
	{
		ImGui::TableNextRow();
		ImGui::TableNextColumn();
		ImGui::PushID(Index * 2 + IsMarker);
		const bool IsRenaming = ShotRename == Index && IsShotMarker == IsMarker;
		if (IsRenaming)
		{
			ImGui::SetNextItemWidth(-FLT_MIN);
			if (!ImGui::IsAnyItemActive())
			{
				ImGui::SetKeyboardFocusHere();
			}
			ImGui::InputText("##Name", ShotBuffer, sizeof(ShotBuffer), ImGuiInputTextFlags_AutoSelectAll);
			const bool IsDone = ImGui::IsItemDeactivated();
			if (IsDone && !ImGui::IsKeyPressed(ImGuiKey_Escape, false))
			{
				if (IsMarker)
				{
					Undo.Begin("Rename marker");
					Undo.TouchHeader(Core.Data);
					Markers[Index].Name = ShotBuffer;
				}
				else
				{
					Edit(Core, Rig->Id, "Rename shot")->Camera.Blocks[Index].Name = ShotBuffer;
				}
			}
			ShotRename = IsDone ? -1 : ShotRename;
		}
		else if (ImGui::Selectable(Name, ShotRow == Index && IsShotMarker == IsMarker, ImGuiSelectableFlags_SpanAllColumns | ImGuiSelectableFlags_AllowDoubleClick))
		{
			ShotRow = Index;
			IsShotMarker = IsMarker;
			Core.Seek(Time);
			if (!IsMarker)
			{
				Selected.assign(1, Rig->Id);
				FocusObject = Rig->Id;
				FocusBlock = Index;
				Focus = FocusKind::Block;
			}
			if (ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
			{
				ShotRename = Index;
				xr_strcpy(ShotBuffer, Name);
			}
		}
		ImGui::PopID();
	};
	string32 At, Span;
	if (Rig && ImGui::BeginTable("##Shots", 5, ImGuiTableFlags_RowBg | ImGuiTableFlags_BordersInnerV | ImGuiTableFlags_SizingStretchProp))
	{
		ImGui::TableSetupColumn("Name");
		ImGui::TableSetupColumn("Start");
		ImGui::TableSetupColumn("Length");
		ImGui::TableSetupColumn("FOV");
		ImGui::TableSetupColumn("DOF");
		ImGui::TableHeadersRow();
		for (u32 Index = 0; Index < Rig->Camera.Blocks.size(); ++Index)
		{
			const CameraBlock& Shot = Rig->Camera.Blocks[Index];
			string64 Label;
			xr_sprintf(Label, "%s", Shot.Name.size() ? Shot.Name.c_str() : "");
			if (!Label[0])
			{
				xr_sprintf(Label, "Shot %u", Index + 1);
			}
			Row(static_cast<s32>(Index), false, Label, Rig->Span.Start + Shot.Start);
			Timecode(At, Rig->Span.Start + Shot.Start, Rate);
			Timecode(Span, Shot.Length, Rate);
			ImGui::TableNextColumn();
			ImGui::TextUnformatted(At);
			ImGui::TableNextColumn();
			ImGui::TextUnformatted(Span);
			ImGui::TableNextColumn();
			Shot.Fov > 0.0f ? ImGui::Text("%.0f", Shot.Fov) : ImGui::TextDisabled("Game");
			ImGui::TableNextColumn();
			ImGui::TextUnformatted(InspectorUi::DofModes[static_cast<u32>(Shot.Dof)]);
		}
		ImGui::EndTable();
	}
	ImGui::Spacing();
	if (ImGui::BeginTable("##Markers", 2, ImGuiTableFlags_RowBg | ImGuiTableFlags_BordersInnerV | ImGuiTableFlags_SizingStretchProp))
	{
		ImGui::TableSetupColumn("Marker");
		ImGui::TableSetupColumn("Time");
		ImGui::TableHeadersRow();
		for (u32 Index = 0; Index < Markers.size(); ++Index)
		{
			Row(static_cast<s32>(Index), true, Markers[Index].Name.c_str(), Markers[Index].Time);
			Timecode(At, Markers[Index].Time, Rate);
			ImGui::TableNextColumn();
			ImGui::TextUnformatted(At);
		}
		ImGui::EndTable();
	}
	if (IsFocused && ShotRename < 0 && ShotRow >= 0 && ImGui::IsKeyPressed(ImGuiKey_Delete, false) && !ImGui::IsAnyItemActive())
	{
		if (IsShotMarker)
		{
			RemoveMarker(Core, ShotRow);
		}
		else if (Rig && ShotRow < static_cast<s32>(Rig->Camera.Blocks.size()))
		{
			RemoveBlock(Core, Rig->Id, ShotRow);
		}
		ShotRow = -1;
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
	float* const Jumps[2] = { &Header.CutDistance, &Header.CutAngle };
	for (u32 Index = 0; Index < 2; ++Index)
	{
		const float Usual = Index ? 45.0f : 5.0f;
		const RowAction Reset = BeginRow(Index ? "Cut angle" : "Cut distance", *Jumps[Index] != Usual, false);
		float Value = *Jumps[Index];
		if (ImGui::DragFloat(Index ? "##CutAngle" : "##CutDistance", &Value, Index ? 1.0f : 0.1f, 0.0f, Index ? 180.0f : 1000.0f, Index ? "%.0f deg" : "%.1f m", ImGuiSliderFlags_AlwaysClamp) || Reset == RowAction::Reset)
		{
			Undo.Begin("Cut threshold");
			Undo.TouchHeader(Core.Data);
			*Jumps[Index] = Reset == RowAction::Reset ? Usual : Value;
		}
		EndRow(KeyState::None);
	}
	const RowAction World = BeginRow("World follows camera", Header.IsWorldFollowing, false);
	bool IsFollowing = Header.IsWorldFollowing;
	if (ImGui::Checkbox("##WorldFollows", &IsFollowing) || World == RowAction::Reset)
	{
		Undo.Begin("World follows camera");
		Undo.TouchHeader(Core.Data);
		Header.IsWorldFollowing = World != RowAction::Reset && IsFollowing;
	}
	ImGui::SetItemTooltip("While a shot is on screen the game brings NPCs and objects to life around the camera instead of the player; the player's surroundings sleep meanwhile");
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
		PresetDraft.Setup.Constraints.clear();
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
		ParamRow(Core, Ids, -1, Param::Position, 0, 0.01f, "%.2f", 0, 0.0f);
		ParamRow(Core, Ids, -1, Param::Rotation, 0, 0.5f, "%.1f", 6, 0.0f);
		ParamRow(Core, Ids, -1, Param::Scale, 0, 0.01f, "%.2f", 7, 1.0f);
		KeyRow(Core, Ids);
	}
	DrawAttach(Core, Ids, -1);
	if (ImGui::CollapsingHeader("Animation", ImGuiTreeNodeFlags_DefaultOpen))
	{
		ParamRow(Core, Ids, -1, Param::TimeScale, 0, 0.01f, "%.2f", 1, 1.0f);
		const Object* Held = Ids.size() == 1 ? Find(Core.Data, Ids[0]) : nullptr;
		if (Held && Held->Actor.Hud.size() && Held->Attach.Parent.ObjectId)
		{
			bool IsLinked = Held->Actor.IsHudLinked;
			if (ImGui::Checkbox("Follow hands animation", &IsLinked))
			{
				Edit(Core, Ids[0], IsLinked ? "Link item animation" : "Unlink item animation")->Actor.IsHudLinked = IsLinked;
			}
			ImGui::SetItemTooltip("Linked: the animation rows of this item copy the rows of the hands (same times, trims and speed, the item animation of the same name). Untick to unlink and edit the rows of the item on their own");
		}
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
	if (Ids.size() == 1 && ImGui::CollapsingHeader("Shaders"))
	{
		DrawLookSection(Core, Primary);
	}
	DrawBlockSection(Core, Primary);
}

void Editor::DrawLookSection(Player& Core, const Object& Target)
{
	const ActorRuntime* Runtime = Core.FindActor(Target.Id);
	const xr_vector<ActorLook>& Looks = Target.Actor.Looks;
	const u32 Count = std::max(static_cast<u32>(Looks.size()), 1u);
	const float Size = ImGui::GetFrameHeight(), Spacing = ImGui::GetStyle().ItemInnerSpacing.x;
	const float Local = std::clamp(Core.Time - Target.Span.Start, 0.0f, Target.Span.Length);
	const u32 Shown = std::min(Curves::LookAt(Target.Lanes, Local), Count - 1);
	const auto LookCombo = [Count](const char* Id, u32 Current, u32& Picked)
	{
		string32 Label;
		const auto Name = [&Label](u32 Look) { xr_sprintf(Label, Look ? "Look %u" : "Look %u (default)", Look + 1); return Label; };
		bool IsPicked = false;
		if (ImGui::BeginCombo(Id, Name(Current)))
		{
			for (u32 Look = 0; Look < Count; ++Look)
			{
				IsPicked = ImGui::Selectable(Name(Look), Look == Current) ? (Picked = Look, true) : IsPicked;
			}
			ImGui::EndCombo();
		}
		return IsPicked;
	};
	LookEdit = std::min(LookEdit, Count - 1);
	BeginRow("Edit", false, false);
	ImGui::SetNextItemWidth(std::max(RowWidth - 2.0f * (Size + Spacing), 1.0f));
	LookCombo("##Look", LookEdit, LookEdit);
	ImGui::SameLine(0.0f, Spacing);
	ImGui::BeginDisabled(Count >= MaxLooks);
	const bool ShouldAdd = ImGui::Button("+", ImVec2(Size, 0.0f));
	ImGui::EndDisabled();
	ImGui::SetItemTooltip("Add look");
	ImGui::SameLine(0.0f, Spacing);
	ImGui::BeginDisabled(Looks.empty());
	const bool ShouldRemove = ImGui::Button("x", ImVec2(Size, 0.0f));
	ImGui::EndDisabled();
	ImGui::SetItemTooltip("Remove look");
	EndRow(KeyState::None);
	if (ShouldAdd)
	{
		Edit(Core, Target.Id, "Add look")->Actor.Looks.resize(Count + 1);
		LookEdit = Count;
	}
	if (ShouldRemove)
	{
		Object& Item = *Edit(Core, Target.Id, "Remove look");
		Item.Actor.Looks.erase(Item.Actor.Looks.begin() + LookEdit);
		for (Lane& Track : Item.Lanes)
		{
			for (u32 Index = 0; Track.Id == Param::Look && Index < Track.Curves[0].Keys.size(); ++Index)
			{
				float& Value = Track.Curves[0].Keys[Index].Value;
				const float Look = std::round(Value);
				Value = Look == static_cast<float>(LookEdit) ? 0.0f : Look > static_cast<float>(LookEdit) ? Look - 1.0f : Look;
			}
		}
		return;
	}
	u32 Picked = Shown;
	BeginRow("At playhead", false, false);
	ImGui::BeginDisabled(Count < 2);
	if (LookCombo("##Shown", Shown, Picked))
	{
		const float HalfFrame = 0.5f / static_cast<float>(Core.Data.Header.FrameRate);
		Curve& Part = Curves::EnsureLane(Edit(Core, Target.Id, "Key look")->Lanes, Param::Look, 0).Curves[0];
		if (Part.Keys.empty() && Local > HalfFrame)
		{
			Curves::SetKey(Part, 0.0f, 0.0f, HalfFrame);
		}
		Curves::SetKey(Part, Local, static_cast<float>(Picked), HalfFrame);
		for (Key& Entry : Part.Keys)
		{
			Entry.Mode = Interp::Hold;
		}
	}
	ImGui::EndDisabled();
	EndRow(KeyState::None);
	const auto SetSurface = [&](u32 Surface, bool IsShader, const shared_str& Value)
	{
		xr_vector<ActorLook>& All = Edit(Core, Target.Id, IsShader ? "Surface shader" : "Surface texture")->Actor.Looks;
		All.resize(std::max<size_t>(All.size(), LookEdit + 1));
		xr_vector<SurfaceShader>& List = All[LookEdit].Surfaces;
		auto Found = std::find_if(List.begin(), List.end(), [Surface](const SurfaceShader& Item) { return Item.Surface == Surface; });
		Found = Found == List.end() ? List.insert(List.end(), SurfaceShader { static_cast<u8>(Surface) }) : Found;
		(IsShader ? Found->Shader : Found->Texture) = Value;
		if (!Found->Shader.size() && !Found->Texture.size())
		{
			List.erase(Found);
		}
	};
	ChooserOwners.assign(1, Target.Id);
	shared_str Shader, Texture, Source, Name;
	for (u32 Surface = 0; Runtime && Surface < ItemSurfaceBase + MaxLookSurfaces; ++Surface)
	{
		if (!Runtime->SurfaceNames(Surface, Shader, Texture))
		{
			Surface = Surface < ItemSurfaceBase ? ItemSurfaceBase - 1 : ItemSurfaceBase + MaxLookSurfaces;
			continue;
		}
		const SurfaceShader* Entry = LookEdit < Looks.size() ? FindSurface(Looks[LookEdit], Surface) : nullptr;
		const bool IsShader = Entry && Entry->Shader.size(), IsTexture = Entry && Entry->Texture.size();
		const shared_str ShownShader = IsShader ? Entry->Shader : Shader, ShownTexture = IsTexture ? Entry->Texture : Texture;
		const bool IsKnown = !IsShader || std::binary_search(Core.Shaders().begin(), Core.Shaders().end(), ShownShader, ShaderLess);
		string32 Caption;
		xr_sprintf(Caption, Surface < ItemSurfaceBase ? "Surface %u" : "Item surface %u", Surface % ItemSurfaceBase + 1);
		ImGui::PushID(static_cast<s32>(Surface));
		RowAction Action = BeginRow(Caption, IsShader, false);
		ImGui::PushStyleColor(ImGuiCol_Text, IsKnown ? ImGui::GetStyleColorVec4(ImGuiCol_Text) : ErrorColor);
		const bool IsShaderPicked = Chooser(Core, ChooserKind::Shader, "##Shader", ShownShader.c_str(), ChooserOwners, Source, Name);
		ImGui::PopStyleColor();
		ImGui::SetItemTooltip(IsKnown ? "Shader" : "Shader (not in shaders.xr, the original is used)");
		EndRow(KeyState::None);
		if (IsShaderPicked || Action == RowAction::Reset)
		{
			SetSurface(Surface, true, Action == RowAction::Reset ? shared_str() : Source);
		}
		Action = BeginRow("Texture", IsTexture, false);
		const bool IsTexturePicked = Chooser(Core, ChooserKind::Texture, "##Texture", ShownTexture.c_str(), ChooserOwners, Source, Name);
		ImGui::SetItemTooltip("Texture");
		EndRow(KeyState::None);
		if (IsTexturePicked || Action == RowAction::Reset)
		{
			SetSurface(Surface, false, Action == RowAction::Reset ? shared_str() : Source);
		}
		ImGui::PopID();
	}
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
	if (Index >= 0 && RowOf(Target.Actor, Layer).Mode == RowMode::Add && (Value.In > 0.0f || Value.Speed < 0.0f))
	{
		ImGui::PushStyleColor(ImGuiCol_Text, ErrorColor);
		ImGui::TextWrapped("Adds from the animation's first frame");
		ImGui::PopStyleColor();
	}
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

void Editor::DrawDecalSection(Player& Core, const xr_vector<u32>& Ids, bool HasPin)
{
	const Object& Primary = *Find(Core.Data, Ids[0]);
	const DecalData& Setup = Primary.Decal;
	shared_str Source, Name;
	ImGui::PushID("Decal");
	DrawHeader(Core, Ids, HasPin);
	const bool IsKnown = !Setup.Shader.size() || std::binary_search(Core.Shaders().begin(), Core.Shaders().end(), Setup.Shader, ShaderLess);
	BeginRow("Texture", false, false);
	if (Chooser(Core, ChooserKind::Texture, "##Texture", Setup.Texture.size() ? Setup.Texture.c_str() : "None", Ids, Source, Name))
	{
		for (u32 Id : Ids)
		{
			Object* Item = Edit(Core, Id, "Decal texture");
			Item->Decal.Texture = Source;
			Item->Name = Item->IsNameLocked ? Item->Name : DefaultName(*Item);
		}
	}
	EndRow(KeyState::None);
	RowAction Action = BeginRow("Shader", Setup.Shader.size() != 0, false);
	ImGui::PushStyleColor(ImGuiCol_Text, IsKnown ? ImGui::GetStyleColorVec4(ImGuiCol_Text) : ErrorColor);
	const bool IsShaderPicked = Chooser(Core, ChooserKind::Shader, "##Shader", Setup.Shader.size() ? Setup.Shader.c_str() : "effects\\wallmark", Ids, Source, Name);
	ImGui::PopStyleColor();
	EndRow(KeyState::None);
	if (IsShaderPicked || Action == RowAction::Reset)
	{
		for (u32 Id : Ids)
		{
			Edit(Core, Id, "Decal shader")->Decal.Shader = Action == RowAction::Reset ? shared_str() : Source;
		}
	}
	Action = BeginRow("Stamp", Setup.IsStamp, false);
	bool IsStamp = Setup.IsStamp;
	if (ImGui::Checkbox("##Stamp", &IsStamp) || Action == RowAction::Reset)
	{
		for (u32 Id : Ids)
		{
			Edit(Core, Id, "Decal stamp")->Decal.IsStamp = IsStamp && Action != RowAction::Reset;
		}
	}
	ImGui::SetItemTooltip("Stamp");
	EndRow(KeyState::None);
	ImGui::BeginDisabled(!g_pGameLevel);
	if (Setup.IsStamp && ImGui::Button("Clear stamps", ImVec2(-FLT_MIN, 0.0f)))
	{
		::Render->clear_static_wallmarks();
	}
	ImGui::EndDisabled();
	ClipRow(Core, Ids);
	if (ImGui::CollapsingHeader("Transform", ImGuiTreeNodeFlags_DefaultOpen))
	{
		ParamRow(Core, Ids, -1, Param::Position, 0, 0.01f, "%.2f", 0, 0.0f);
		ParamRow(Core, Ids, -1, Param::Rotation, 0, 0.5f, "%.1f", 6, 0.0f);
		ParamRow(Core, Ids, -1, Param::DecalWidth, 0, 0.01f, "%.2f", 0, 0.5f);
		ParamRow(Core, Ids, -1, Param::DecalHeight, 0, 0.01f, "%.2f", 0, 0.5f);
		KeyRow(Core, Ids);
	}
	DrawAttach(Core, Ids, -1);
	ImGui::PopID();
}

void Editor::DrawWeatherSection(Player& Core, const xr_vector<u32>& Ids, bool HasPin)
{
	struct Row
	{
		Param Id;
		const char* Group;
		float Speed;
		const char* Format;
	};
	static constexpr Row Rows[16] = {
		{ Param::WeatherTime, "Time", 0.05f, "%.2f h" }, { Param::SunHeading, "Sun", 0.5f, "%.1f" }, { Param::SunHeight, nullptr, 0.5f, "%.1f" }, { Param::SunColor, nullptr, 0.01f, "%.2f" },
		{ Param::SunBrightness, nullptr, 0.01f, "%.2f" }, { Param::SunShafts, nullptr, 0.01f, "%.2f" }, { Param::AmbientColor, "Light", 0.01f, "%.2f" }, { Param::HemiColor, nullptr, 0.01f, "%.2f" },
		{ Param::FogColor, "Fog", 0.01f, "%.2f" }, { Param::FogDensity, nullptr, 0.01f, "%.2f" }, { Param::FogDistance, nullptr, 1.0f, "%.0f m" }, { Param::FarPlane, nullptr, 1.0f, "%.0f m" },
		{ Param::RainDensity, "Rain and wind", 0.01f, "%.2f" }, { Param::WindSpeed, nullptr, 1.0f, "%.0f" }, { Param::WindDirection, nullptr, 1.0f, "%.0f" }, { Param::SkyRotation, "Sky", 1.0f, "%.0f" }
	};
	const Object& Primary = *Find(Core.Data, Ids[0]);
	const WeatherData& Setup = Primary.Weather;
	ImGui::PushID("Weather");
	DrawHeader(Core, Ids, HasPin);
	BeginRow("Weather", false, false);
	const shared_str* Picked = nullptr;
	const shared_str Game;
	if (ImGui::BeginCombo("##Cycle", Setup.Cycle.size() ? Setup.Cycle.c_str() : "Game weather"))
	{
		if (ImGui::Selectable("Game weather", !Setup.Cycle.size()))
		{
			Picked = &Game;
		}
		for (const auto& Entry : g_pGamePersistent->Environment().WeatherCycles)
		{
			if (ImGui::Selectable(Entry.first.c_str(), Entry.first == Setup.Cycle))
			{
				Picked = &Entry.first;
			}
		}
		ImGui::EndCombo();
	}
	EndRow(KeyState::None);
	for (u32 Index = 0; Picked && Index < Ids.size(); ++Index)
	{
		Object* Item = Edit(Core, Ids[Index], "Weather");
		Item->Weather.Cycle = *Picked;
		Item->Name = Item->IsNameLocked ? Item->Name : DefaultName(*Item);
	}
	BeginRow("Fade in, out", Setup.FadeIn != 0.0f || Setup.FadeOut != 0.0f, false);
	float Fades[2] = { Setup.FadeIn, Setup.FadeOut };
	if (ImGui::DragFloat2("##Fades", Fades, 0.05f, 0.0f, 600.0f, "%.2f s", ImGuiSliderFlags_AlwaysClamp))
	{
		for (u32 Id : Ids)
		{
			WeatherData& Edited = Edit(Core, Id, "Weather fade")->Weather;
			Edited.FadeIn = Fades[0];
			Edited.FadeOut = Fades[1];
		}
	}
	EndRow(KeyState::None);
	ClipRow(Core, Ids);
	bool IsOpen = false;
	for (const Row& Line : Rows)
	{
		IsOpen = Line.Group ? ImGui::CollapsingHeader(Line.Group, ImGuiTreeNodeFlags_DefaultOpen) : IsOpen;
		if (!IsOpen)
		{
			continue;
		}
		const u32 Bit = 1u << (static_cast<u32>(Line.Id) - static_cast<u32>(Param::WeatherTime));
		bool IsOn = (Setup.Mask & Bit) != 0;
		ImGui::PushID(static_cast<s32>(Line.Id));
		if (ImGui::Checkbox("##On", &IsOn))
		{
			for (u32 Id : Ids)
			{
				u32& Mask = Edit(Core, Id, "Weather override")->Weather.Mask;
				Mask = IsOn ? Mask | Bit : Mask & ~Bit;
			}
		}
		ImGui::SetItemTooltip("Ticked: the scene sets this value. Unticked: it comes from the weather.");
		ImGui::SameLine();
		ImGui::BeginDisabled(!IsWeatherSet(Setup, Primary.Lanes, Line.Id));
		ParamRow(Core, Ids, -1, Line.Id, 0, Line.Speed, Line.Format, 0, *WeatherField(const_cast<WeatherData&>(Setup), Line.Id, 0));
		ImGui::EndDisabled();
		ImGui::PopID();
	}
	ImGui::PopID();
}

void Editor::DrawPostSection(Player& Core, const xr_vector<u32>& Ids, bool HasPin)
{
	struct Row
	{
		Param Id;
		const char* Group;
		float Speed;
		const char* Format;
	};
	static constexpr Row Rows[16] = {
		{ Param::PostFade, "Fade", 0.01f, "%.2f" }, { Param::PostFadeColor, nullptr, 0.01f, "%.2f" },
		{ Param::PostBase, "Color", 0.005f, "%.3f" }, { Param::PostAdd, nullptr, 0.005f, "%.3f" }, { Param::PostGray, nullptr, 0.01f, "%.2f" }, { Param::PostGrayColor, nullptr, 0.005f, "%.3f" },
		{ Param::PostBlur, "Image", 0.01f, "%.2f" }, { Param::PostDualityH, nullptr, 0.001f, "%.3f" }, { Param::PostDualityV, nullptr, 0.001f, "%.3f" },
		{ Param::PostNoise, "Noise", 0.01f, "%.2f" }, { Param::PostGrain, nullptr, 0.01f, "%.2f" }, { Param::PostNoiseFps, nullptr, 1.0f, "%.0f" },
		{ Param::PostExposure, "Grade", 0.02f, "%.2f" }, { Param::PostBloom, nullptr, 0.005f, "%.3f" }, { Param::PostTonemap, nullptr, 0.01f, "%.2f" }, { Param::PostMiddleGray, nullptr, 0.01f, "%.2f" }
	};
	const Object& Primary = *Find(Core.Data, Ids[0]);
	const PostData& Setup = Primary.Post;
	const u32 FadeBit = 1u << (static_cast<u32>(Param::PostFade) - static_cast<u32>(Param::PostBase)), GrayBit = 1u << (static_cast<u32>(Param::PostGray) - static_cast<u32>(Param::PostBase));
	ImGui::PushID("Post");
	DrawHeader(Core, Ids, HasPin);
	BeginRow("Preset", false, false);
	const float Width = (RowWidth - 2.0f * ImGui::GetStyle().ItemInnerSpacing.x) / 3.0f;
	const s32 Picked = ImGui::Button("Fade out", ImVec2(Width, 0.0f)) ? 0 : (ImGui::SameLine(0.0f, ImGui::GetStyle().ItemInnerSpacing.x), ImGui::Button("Fade in", ImVec2(Width, 0.0f))) ? 1 : (ImGui::SameLine(0.0f, ImGui::GetStyle().ItemInnerSpacing.x), ImGui::Button("Black & white", ImVec2(Width, 0.0f))) ? 2 : -1;
	EndRow(KeyState::None);
	for (u32 Index = 0; Picked >= 0 && Index < Ids.size(); ++Index)
	{
		Object& Item = *Edit(Core, Ids[Index], "Post preset");
		Item.Post.Mask |= Picked == 2 ? GrayBit : FadeBit;
		Item.Post.Gray = Picked == 2 ? 1.0f : Item.Post.Gray;
		if (Picked < 2)
		{
			Curve& Ramp = Curves::EnsureLane(Item.Lanes, Param::PostFade, 0).Curves[0];
			Ramp.Keys.clear();
			Curves::SetKey(Ramp, 0.0f, Picked ? 1.0f : 0.0f, 1e-4f);
			Curves::SetKey(Ramp, Item.Span.Length, Picked ? 0.0f : 1.0f, 1e-4f);
		}
	}
	BeginRow("Fade in, out", Setup.FadeIn != 0.0f || Setup.FadeOut != 0.0f, false);
	float Fades[2] = { Setup.FadeIn, Setup.FadeOut };
	if (ImGui::DragFloat2("##Fades", Fades, 0.05f, 0.0f, 600.0f, "%.2f s", ImGuiSliderFlags_AlwaysClamp))
	{
		for (u32 Id : Ids)
		{
			PostData& Edited = Edit(Core, Id, "Post fade")->Post;
			Edited.FadeIn = Fades[0];
			Edited.FadeOut = Fades[1];
		}
	}
	EndRow(KeyState::None);
	ClipRow(Core, Ids);
	bool IsOpen = false;
	for (const Row& Line : Rows)
	{
		IsOpen = Line.Group ? ImGui::CollapsingHeader(Line.Group, ImGuiTreeNodeFlags_DefaultOpen) : IsOpen;
		if (!IsOpen)
		{
			continue;
		}
		const u32 Bit = 1u << (static_cast<u32>(Line.Id) - static_cast<u32>(Param::PostBase));
		bool IsOn = (Setup.Mask & Bit) != 0;
		ImGui::PushID(static_cast<s32>(Line.Id));
		if (ImGui::Checkbox("##On", &IsOn))
		{
			for (u32 Id : Ids)
			{
				u32& Mask = Edit(Core, Id, "Post override")->Post.Mask;
				Mask = IsOn ? Mask | Bit : Mask & ~Bit;
			}
		}
		ImGui::SetItemTooltip("Ticked: the scene sets this value. Unticked: the game's own value shows.");
		ImGui::SameLine();
		ImGui::BeginDisabled(!IsPostSet(Setup, Primary.Lanes, Line.Id));
		ParamRow(Core, Ids, -1, Line.Id, 0, Line.Speed, Line.Format, 0, *PostField(const_cast<PostData&>(Setup), Line.Id, 0));
		ImGui::EndDisabled();
		ImGui::PopID();
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
	ParamRow(Core, Ids, Block, Param::Position, 0, 0.01f, "%.2f", 0, 0.0f);
	ParamRow(Core, Ids, Block, Param::Rotation, 0, 0.5f, "%.1f", 6, 0.0f);
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
	ImGui::SeparatorText("Pose key");
	ImGui::BeginDisabled(At == u32(-1));
	BeginRow("Interpolation", false, false);
	s32 Mode = static_cast<s32>(Pose.Mode);
	if (ImGui::Combo("##Mode", &Mode, InspectorUi::Modes, 6))
	{
		Cameras::ModeAt(Edit(Core, Rig.Id, "Interpolation")->Camera.Blocks[Block], Pose.Time, static_cast<Interp>(Mode), 1e-4f);
	}
	EndRow(KeyState::None);
	ImGui::EndDisabled();
	ParamRow(Core, Ids, Block, Param::Fov, 0, 0.1f, "%.1f", 1, 0.0f);
	HorizontalFovRow(Core, Ids, Block);
	ParamRow(Core, Ids, Block, Param::TimeScale, 0, 0.01f, "%.2f", 1, 1.0f);
	ParamRow(Core, Ids, Block, Param::CameraSmooth, 0, 0.01f, "%.2f s", 1, 0.0f);
	Action = BeginRow("Path tension", Shot.Tension != 1.0f, false);
	float Tension = Shot.Tension;
	if (ImGui::DragFloat("##Tension", &Tension, 0.01f, 0.1f, 3.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp) || Action == RowAction::Reset)
	{
		Edit(Core, Rig.Id, "Path tension")->Camera.Blocks[Block].Tension = Action == RowAction::Reset ? 1.0f : Tension;
	}
	EndRow(KeyState::None);
	Action = BeginRow("Shortest turn", Shot.IsShortTurn, false);
	bool IsShortTurn = Shot.IsShortTurn;
	if (ImGui::Checkbox("##ShortTurn", &IsShortTurn) || Action == RowAction::Reset)
	{
		Edit(Core, Rig.Id, "Shortest turn")->Camera.Blocks[Block].IsShortTurn = Action != RowAction::Reset && IsShortTurn;
	}
	EndRow(KeyState::None);
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

void Editor::HorizontalFovRow(Player& Core, const xr_vector<u32>& Ids, s32 Block)
{
	const ParamInfo& Meta = Info(Param::Fov);
	const float Stored = InspectorUi::ValueAt(*Find(Core.Data, Ids[0]), Block, Param::Fov, 0, 0, Core.Time), Aspect = FrameAspect();
	const float Shown = rad2deg(2.0f * std::atan(std::tan(deg2rad(0.5f * (Stored > 0.0f ? Stored : GameFov()))) * Aspect));
	float Value = Shown;
	BeginRow("Horizontal FOV", false, false);
	const bool IsChanged = ImGui::DragFloat("##Value", &Value, 0.1f, 1.0f, 179.0f, Stored > 0.0f ? "%.1f" : "Game", ImGuiSliderFlags_AlwaysClamp) && Value != Shown;
	const float Vertical = std::clamp(rad2deg(2.0f * std::atan(std::tan(deg2rad(0.5f * Value)) / Aspect)), Meta.Min, Meta.Max);
	for (u32 Index = 0; IsChanged && Index < Ids.size(); ++Index)
	{
		WriteParam(Core, *Edit(Core, Ids[Index], Meta.Label), Block, Param::Fov, 0, 0, Vertical);
	}
	EndRow(KeyState::None);
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
			RemoveLane(Item, Block, Id, Layer, LaneLocal(Item, Block, Id, Core.Time));
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
	if (Links.Parent.ObjectId)
	{
		ParamRow(Core, Ids, Block, Param::AttachWeight, 0, 0.01f, "%.2f", 1, 1.0f);
		Value.Weight = Links.Weight;
	}
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
	if (IsObject && Links.Parent.ObjectId && Ids.size() == 1 && DrawAttachSwitches(Core, Primary))
	{
		return;
	}
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
	ReadLibrary();
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

void Editor::ReadLibrary()
{
	string_path Path;
	FS.update_path(Path, Io::LegacyAlias, InspectorUi::LibraryFile);
	const CReaderGuarded File(FS.exist(Path) ? FS.r_open(Path) : nullptr);
	xr_string Error = "the file is larger than 256 KiB";
	if (!File)
	{
		return;
	}
	if (static_cast<size_t>(File->length()) > InspectorUi::MaxLibrarySize || !Io::PreScan(*File, Error))
	{
		Msg("! Cinema editor: %s is ignored: %s", Path, Error.c_str());
		return;
	}
	File->seek(0);
	const CInifile Ini(&*File, nullptr);
	string64 Section, Line;
	const auto ReadItems = [&](xr_vector<xr_string>& Out, u32 Cap)
	{
		for (u32 Index = 0; Index < Cap; ++Index)
		{
			xr_sprintf(Line, "item_%04u", Index);
			const char* Text = Ini.read_if_exists<str_c>(Section, Line, nullptr);
			if (!Text)
			{
				break;
			}
			if (Io::IniSafe(Text) && std::find(Out.begin(), Out.end(), Text) == Out.end())
			{
				Out.push_back(Text);
			}
		}
	};
	for (u32 Slot = 0; Slot < 5; ++Slot)
	{
		xr_sprintf(Section, "fav_%s", InspectorUi::LibraryKinds[Slot]);
		ReadItems(Library[Slot].Favourites, InspectorUi::MaxFavourites);
		for (u32 Folder = 0; Folder < InspectorUi::MaxFolders; ++Folder)
		{
			xr_sprintf(Section, "folder_%s_%03u", InspectorUi::LibraryKinds[Slot], Folder);
			const char* Name = Ini.section_exist(Section) ? Ini.read_if_exists<str_c>(Section, "name", nullptr) : nullptr;
			if (!Name || !Io::IniSafe(Name))
			{
				break;
			}
			LibraryFolder& Item = Library[Slot].Folders.emplace_back();
			Item.Name = Name;
			ReadItems(Item.Items, InspectorUi::MaxFolderItems);
		}
	}
}

void Editor::WriteLibrary()
{
	CInifile Ini(nullptr, false, false, false);
	string64 Section, Line;
	for (u32 Slot = 0; Slot < 5; ++Slot)
	{
		xr_sprintf(Section, "fav_%s", InspectorUi::LibraryKinds[Slot]);
		for (u32 Index = 0; Index < Library[Slot].Favourites.size(); ++Index)
		{
			xr_sprintf(Line, "item_%04u", Index);
			Ini.w_string(Section, Line, Library[Slot].Favourites[Index].c_str());
		}
		for (u32 Folder = 0; Folder < Library[Slot].Folders.size(); ++Folder)
		{
			const LibraryFolder& Item = Library[Slot].Folders[Folder];
			xr_sprintf(Section, "folder_%s_%03u", InspectorUi::LibraryKinds[Slot], Folder);
			Ini.w_string(Section, "name", Item.Name.c_str());
			for (u32 Index = 0; Index < Item.Items.size(); ++Index)
			{
				xr_sprintf(Line, "item_%04u", Index);
				Ini.w_string(Section, Line, Item.Items[Index].c_str());
			}
		}
	}
	string_path Path;
	xr_string Error;
	FS.update_path(Path, Io::LegacyAlias, InspectorUi::LibraryFile);
	if (!Io::SaveIni(Ini, Path, Error))
	{
		SetStatus(true, "%s", Error.c_str());
	}
}

void Editor::ReleasePreview()
{
	if (PreviewView)
	{
		PreviewView->Release();
	}
	if (PreviewSurface)
	{
		PreviewSurface->Release();
	}
	PreviewView = nullptr;
	PreviewSurface = nullptr;
	PreviewName = shared_str();
}

void Editor::PreviewTexture(const shared_str& Texture)
{
	if (Texture == PreviewName || !GRHI || GRHI->APILevel != D3D11)
	{
		return;
	}
	ReleasePreview();
	PreviewName = Texture;
	string_path File;
	xr_sprintf(File, "%s.dds", Texture.c_str());
	u32 Size = 0;
	PreviewSurface = ::Render->load_texture(File, Size);
	if (!PreviewSurface)
	{
		return;
	}
	RHIShaderResourceViewDesc Desc;
	Desc.MipLevels = 1;
	Desc.Format = PreviewSurface->GetFormat();
	Desc.MostDetailedMip = 0;
	Desc.ViewDimension = ERHI_SRV_DIMENSION::TEXTURE2D;
	Desc.FirstArraySlice = 0;
	Desc.ArraySize = 1;
	Desc.ElementWidth = 0;
	PreviewView = GRHI->CreateShaderResourceView(PreviewSurface, &Desc);
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
	const xr_vector<shared_str>* const Lists[8] = { &ModelFiles, &ItemSections, &OmfFiles, &OmfFiles, &PresetNames, &EffectNames, &TextureFiles, &Core.Shaders() };
	const xr_vector<shared_str>& Files = *Lists[static_cast<u32>(Shown)];
	const size_t Sources = Shown == ChooserKind::Animation && Runtime ? Runtime->Sources.size() : 0;
	char* Filter = Filters[static_cast<u32>(Shown)];
	const s32 Slot = InspectorUi::LibrarySlots[static_cast<u32>(Shown)];
	LibraryShelf* Shelf = Slot >= 0 ? &Library[Slot] : nullptr;
	u8 Scope = Shelf ? Scopes[Slot] : 0;
	Scope = Shelf && Scope < Shelf->Folders.size() + 2 ? Scope : 0;
	const auto ScopeName = [&](u32 Index) { return Index == 0 ? "All" : Index == 1 ? "Favourites" : Shelf->Folders[Index - 2].Name.c_str(); };
	if (Shelf)
	{
		ImGui::SetNextItemWidth(InspectorUi::ListWidth);
		if (ImGui::BeginCombo("##Scope", ScopeName(Scope)))
		{
			for (u32 Index = 0; Index < Shelf->Folders.size() + 2; ++Index)
			{
				if (ImGui::Selectable(ScopeName(Index), Index == Scope))
				{
					Scope = static_cast<u8>(Index);
					IsFilterDirty = true;
				}
			}
			if (ImGui::Selectable("New folder...") && Shelf->Folders.size() < InspectorUi::MaxFolders)
			{
				FolderAction = 1;
				FolderEdited = -1;
				MenuKey.clear();
			}
			ImGui::EndCombo();
		}
		ImGui::SetItemTooltip("Shelf");
		if (Scope >= 2 && ImGui::IsItemClicked(ImGuiMouseButton_Right))
		{
			FolderAction = 2;
		}
		Scopes[Slot] = Scope;
	}
	if (FolderAction == 1)
	{
		xr_strcpy(FolderName, FolderEdited >= 0 && Shelf && FolderEdited < static_cast<s32>(Shelf->Folders.size()) ? Shelf->Folders[FolderEdited].Name.c_str() : "");
		ImGui::OpenPopup("##FolderName");
	}
	if (FolderAction == 2)
	{
		ImGui::OpenPopup("##FolderMenu");
	}
	FolderAction = 0;
	if (ImGui::BeginPopup("##FolderMenu"))
	{
		if (ImGui::MenuItem("Rename...") && Shelf && Scope >= 2)
		{
			FolderAction = 1;
			FolderEdited = Scope - 2;
		}
		if (ImGui::MenuItem("Delete") && Shelf && Scope >= 2)
		{
			Shelf->Folders.erase(Shelf->Folders.begin() + (Scope - 2));
			Scopes[Slot] = 0;
			IsFilterDirty = true;
			WriteLibrary();
		}
		ImGui::EndPopup();
	}
	if (ImGui::BeginPopup("##FolderName"))
	{
		if (ImGui::IsWindowAppearing())
		{
			ImGui::SetKeyboardFocusHere();
		}
		const bool IsDone = ImGui::InputTextWithHint("##Folder", "Folder name", FolderName, sizeof(FolderName), ImGuiInputTextFlags_EnterReturnsTrue);
		if (IsDone && Shelf && FolderName[0] && Io::IniSafe(FolderName))
		{
			const bool IsNew = FolderEdited < 0 || FolderEdited >= static_cast<s32>(Shelf->Folders.size());
			LibraryFolder& Item = IsNew ? Shelf->Folders.emplace_back() : Shelf->Folders[FolderEdited];
			Item.Name = FolderName;
			if (IsNew && !MenuKey.empty())
			{
				Item.Items.push_back(MenuKey);
			}
			WriteLibrary();
			IsFilterDirty = true;
			ImGui::CloseCurrentPopup();
		}
		ImGui::EndPopup();
	}
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
		const xr_vector<xr_string>* Inside = Shelf && Scope >= 2 ? &Shelf->Folders[Scope - 2].Items : Shelf && Scope == 1 ? &Shelf->Favourites : nullptr;
		const auto Add = [&](u32 Code, const char* Text, const char* Pack)
		{
			const u32 Rank = InspectorUi::MatchRank(Filter, Text, Pack);
			LibraryKey = Pack ? xr_string(Pack) + "|" + Text : xr_string(Text);
			const bool IsFavourite = Shelf && std::find(Shelf->Favourites.begin(), Shelf->Favourites.end(), LibraryKey) != Shelf->Favourites.end();
			if (Rank != u32(-1) && (!Inside || std::find(Inside->begin(), Inside->end(), LibraryKey) != Inside->end()))
			{
				Ranked.push_back((static_cast<u64>(IsFavourite ? 0u : 1u) << 63) | (static_cast<u64>(Rank & 0x3FFFFFFFu) << 32) | Code);
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
	if ((Shown == ChooserKind::Item || Shown == ChooserKind::Texture || Shown == ChooserKind::Shader) && ImGui::Selectable(Shown != ChooserKind::Item && Runtime ? "Original" : "None"))
	{
		Source = Name = shared_str();
		IsPicked = true;
	}
	IsChooserOpen = true;
	bool ShouldOpenRow = false;
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
			const bool IsPressed = ImGui::Selectable("##Row", IsCursor, ImGuiSelectableFlags_NoAutoClosePopups);
			const float Star = Shelf ? ImGui::GetFontSize() : 0.0f;
			const ImRect Box(ImVec2(ImGui::GetItemRectMin().x + ImGui::GetStyle().ItemSpacing.x + Star, ImGui::GetItemRectMin().y), ImGui::GetItemRectMax());
			const bool IsStar = IsPressed && ImGui::GetIO().MousePos.x < Box.Min.x;
			Chosen = IsPressed && !IsStar && IsListed ? Row : Chosen;
			const float Middle = Box.Min.x + Split - Star - ImGui::GetStyle().ItemSpacing.x;
			const bool IsHovered = ImGui::IsItemHovered(Shown == ChooserKind::Model || Shown == ChooserKind::Texture ? ImGuiHoveredFlags_DelayShort | ImGuiHoveredFlags_NoSharedDelay : ImGuiHoveredFlags_None) || (IsCursor && Move);
			if (Shelf && IsListed)
			{
				LibraryKey = Animation ? xr_string(Runtime->Sources[Code >> 16].Id.c_str()) + "|" + Animation->c_str() : xr_string(Files[Code].c_str());
				const auto Found = std::find(Shelf->Favourites.begin(), Shelf->Favourites.end(), LibraryKey);
				const bool IsFavourite = Found != Shelf->Favourites.end();
				ImVec4 color = IsFavourite ? ImVec4(1.0f, 0.8f, 0.2f, 1.0f) : ImGui::GetStyleColorVec4(ImGuiCol_TextDisabled);
				// , IsFavourite || ImGui::IsItemHovered() ? 1.0f : 0.0f
				ImGui::GetWindowDrawList()->AddText(ImVec2(Box.Min.x - Star, Box.Min.y), ImGui::GetColorU32(color), "*");
				if (IsStar && (IsFavourite || Shelf->Favourites.size() < InspectorUi::MaxFavourites))
				{
					IsFavourite ? (void)Shelf->Favourites.erase(Found) : Shelf->Favourites.push_back(LibraryKey);
					WriteLibrary();
					IsFilterDirty = true;
				}
				if (ImGui::IsItemClicked(ImGuiMouseButton_Right))
				{
					MenuKey = LibraryKey;
					MenuPack = shared_str();
					ShouldOpenRow = true;
				}
			}
			if (Shown == ChooserKind::Texture && IsListed && IsHovered)
			{
				PreviewTexture(Files[Code]);
			}
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
				if (ImGui::IsItemClicked(ImGuiMouseButton_Right))
				{
					MenuPack = Omf::IsOmfPath(Pack) ? Pack : shared_str();
					ShouldOpenRow = true;
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
	if (ShouldOpenRow)
	{
		ImGui::OpenPopup("##Row");
	}
	if (ImGui::BeginPopup("##Row"))
	{
		if (Shelf && ImGui::BeginMenu("Add to folder"))
		{
			for (LibraryFolder& Folder : Shelf->Folders)
			{
				const bool HasItem = std::find(Folder.Items.begin(), Folder.Items.end(), MenuKey) != Folder.Items.end();
				if (ImGui::MenuItem(Folder.Name.c_str(), nullptr, HasItem, !HasItem && Folder.Items.size() < InspectorUi::MaxFolderItems))
				{
					Folder.Items.push_back(MenuKey);
					WriteLibrary();
				}
			}
			if (ImGui::MenuItem("New folder...", nullptr, false, Shelf->Folders.size() < InspectorUi::MaxFolders))
			{
				FolderAction = 1;
				FolderEdited = -1;
			}
			ImGui::EndMenu();
		}
		if (Shelf && Scope >= 2 && ImGui::MenuItem("Remove from folder"))
		{
			std::erase(Shelf->Folders[Scope - 2].Items, MenuKey);
			WriteLibrary();
			IsFilterDirty = true;
		}
		if (MenuPack.size() && ImGui::MenuItem("Remove pack"))
		{
			SetPack(MenuPack, false);
		}
		ImGui::EndPopup();
	}
	ImGui::EndChild();
	if (Shown == ChooserKind::Texture)
	{
		const bool HasPreview = PreviewView && PreviewSurface;
		const float Side = 128.0f, Width = HasPreview ? static_cast<float>(PreviewSurface->GetWidth()) : 1.0f, Height = HasPreview ? static_cast<float>(PreviewSurface->GetHeight()) : 1.0f, Scale = Side / std::max({ Width, Height, 1.0f });
		const ImVec2 Corner = ImGui::GetCursorScreenPos(), Size(Width * Scale, Height * Scale), Offset((Side - Width * Scale) * 0.5f, (Side - Height * Scale) * 0.5f);
		ImGui::Dummy(ImVec2(Side, Side));
		ImDrawList* List = ImGui::GetWindowDrawList();
		List->AddRectFilled(Corner, ImVec2(Corner.x + Side, Corner.y + Side), ImGui::GetColorU32(ImGuiCol_FrameBg));
		if (HasPreview)
		{
			List->AddImage(PreviewView->GetRawSRV(), ImVec2(Corner.x + Offset.x, Corner.y + Offset.y), ImVec2(Corner.x + Offset.x + Size.x, Corner.y + Offset.y + Size.y));
			ImGui::SameLine();
			ImGui::TextDisabled("%s\n%ux%u", PreviewName.c_str(), PreviewSurface->GetWidth(), PreviewSurface->GetHeight());
		}
		ImGui::Dummy(ImVec2(0.0f, ImGui::GetStyle().ItemSpacing.y));
	}
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
		const float Local = LaneLocal(Item, Block, Id, Core.Time);
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
	const float HalfFrame = 0.5f / static_cast<float>(Core.Data.Header.FrameRate), Local = LaneLocal(Target, Block, Id, Core.Time);
	if (IsAutoKey)
	{
		UseGameFov(Target, Block, Id);
	}
	float* Static = StaticField(Target, Block, Id, Layer, Component);
	if (!Static)
	{
		return;
	}
	const Lane* Found = Curves::FindLane(LaneList(Target, Block), Id, Layer);
	const Curve* Existing = Found ? &Found->Curves[Component] : nullptr;
	const u32 KeyIndex = Existing ? Curves::KeyAt(*Existing, Local, HalfFrame) : u32(-1);
	const bool HasKeys = Existing && !Existing->Keys.empty();
	auto Latch = std::find_if(Latches.begin(), Latches.end(), [&](const WriteLatch& Item) { return Item.ObjectId == Target.Id && Item.Block == Block && Item.Id == Id && Item.Layer == Layer && Item.Component == Component; });
	if (Latch == Latches.end())
	{
		const WriteKind Kind = IsAutoKey ? (Core.IsPlaying ? WriteKind::Record : WriteKind::Insert) : !HasKeys ? WriteKind::Static : KeyIndex != u32(-1) ? WriteKind::OnKey : WriteKind::Offset;
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
	if (Block >= 0 && Block < ShakeBase)
	{
		Cameras::Rebuild(Target.Camera.Blocks[Block]);
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
	const float Moment = Core.Time - TargetStart(*Target, Latch.Block);
	if (!(Moment >= 0.0f && Moment <= OwnerLength(*Target, Latch.Block)))
	{
		return;
	}
	const float Local = LaneLocal(*Target, Latch.Block, Latch.Id, Core.Time);
	const float From = Local < Latch.Last ? Local : std::max(Latch.Begin, Latch.Last), HalfFrame = 0.5f / static_cast<float>(Core.Data.Header.FrameRate);
	const bool IsDue = Local < Latch.Last || Local >= Latch.Last + 0.1f || !Core.IsPlaying;
	Latch.Last = IsDue ? Local : Latch.Last;
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
	if (Latch.Block >= 0 && Latch.Block < ShakeBase)
	{
		Cameras::Rebuild(Target->Camera.Blocks[Latch.Block]);
	}
}
}
