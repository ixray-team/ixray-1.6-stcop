#include "StdAfx.h"
#include "CinemaEditor.h"
#include "../../Cinema/CinemaOmf.h"
#include "../../Cinema/CinemaSceneIo.h"

extern ENGINE_API float psHUD_FOV;

namespace Cinema::HudUi
{
constexpr u32 MaxItems = 4096;

const char* HudOf(const char* Item)
{
	const char* Hud = Item && pSettings->section_exist(Item) ? pSettings->read_if_exists<str_c>(Item, "hud", nullptr) : nullptr;
	return Hud && pSettings->section_exist(Hud) && (pSettings->line_exist(Hud, "item_visual") || pSettings->line_exist(Hud, "visual")) ? Hud : nullptr;
}

Fmatrix Offset(const char* Hud, const char* Position, const char* Rotation)
{
	string64 Wide[2];
	xr_sprintf(Wide[0], "%s_16x9", Position);
	xr_sprintf(Wide[1], "%s_16x9", Rotation);
	const bool IsWide = Device.fASPECT < 0.7f && pSettings->line_exist(Hud, Wide[0]) && pSettings->line_exist(Hud, Wide[1]);
	const Fvector Zero = Fvector().set(0.0f, 0.0f, 0.0f);
	const Fvector Shift = pSettings->read_if_exists<Fvector3>(Hud, IsWide ? Wide[0] : Position, Zero), Turn = pSettings->read_if_exists<Fvector3>(Hud, IsWide ? Wide[1] : Rotation, Zero);
	Fmatrix Out;
	Out.setHPB(deg2rad(Turn.x), deg2rad(Turn.y), deg2rad(Turn.z));
	Out.c = Shift;
	return Out;
}

bool FindSource(const ActorRuntime& Runtime, const char* Name, shared_str& Source, shared_str& Found)
{
	for (const AnimationSource& Pack : Runtime.Sources)
	{
		for (const AnimationClip& Clip : Pack.Clips)
		{
			if (!Clip.IsFx && !xr_stricmp(Clip.Name.c_str(), Name))
			{
				Source = Pack.Id;
				Found = Clip.Name;
				return true;
			}
		}
	}
	return false;
}
}

namespace Cinema
{
const Object* Editor::HudHands(const Player& Core) const
{
	const Object* Fallback = nullptr;
	for (const Object& Item : Core.Data.Objects)
	{
		if (Item.Type != ObjectType::Actor || !Item.Actor.Hud.size() || Item.Attach.Parent.ObjectId || !pSettings->section_exist(Item.Actor.Hud))
		{
			continue;
		}
		const bool IsChosen = IsSelected(Item.Id) || std::any_of(Core.Data.Objects.begin(), Core.Data.Objects.end(), [&](const Object& Other) { return Other.Attach.Parent.ObjectId == Item.Id && IsSelected(Other.Id); });
		if (IsChosen)
		{
			return &Item;
		}
		Fallback = Fallback ? Fallback : &Item;
	}
	return Fallback;
}

void Editor::AddHud(Player& Core, const char* Item, float Start)
{
	const char* Hud = HudUi::HudOf(Item);
	const bool IsCombined = Hud && !pSettings->line_exist(Hud, "item_visual");
	const char* Hands = !Hud || IsCombined ? nullptr : pSettings->section_exist(HudHandsSection) ? pSettings->read_if_exists<str_c>(HudHandsSection, "visual", nullptr) : nullptr;
	const char* Visual = Hud ? pSettings->r_string(Hud, IsCombined ? "visual" : "item_visual") : nullptr;
	if (!Hud || !Io::IniSafe(Hud) || !Omf::VisualExists(Visual) || (!IsCombined && !Omf::VisualExists(Hands)))
	{
		SetStatus(true, "%s has no usable HUD: its HUD section, the item model or the hands model \"%s\" is missing.", Item, HudHandsSection);
		return;
	}
	Fvector Position, Rotation;
	SpawnPose(Position, Rotation);
	Position.y += 1.6f;
	Object* Body = AddObject(Core, ObjectType::Actor, IsCombined ? Visual : Hands, Start, Position, Fvector().set(Rotation.x, 0.0f, 0.0f));
	if (!Body)
	{
		return;
	}
	const u32 BodyId = Body->Id;
	Body->Actor.Hud = Hud;
	Body->Name = IsCombined ? xr_string(Item) : xr_string("Hands ") + Item;
	shared_str Anchor;
	u32 Place = pSettings->read_if_exists<u16>(Hud, "attach_place_idx", 0);
	if (!IsCombined)
	{
		for (const auto& Line : pSettings->r_section(HudHandsSection).Data)
		{
			if (strstr(Line.first.c_str(), "ancor_") == Line.first.c_str() && !Place--)
			{
				Anchor = Line.second;
			}
		}
	}
	Object* Thing = IsCombined ? nullptr : AddObject(Core, ObjectType::Actor, Visual, Start, pSettings->read_if_exists<Fvector3>(Hud, "item_position", Fvector().set(0.0f, 0.0f, 0.0f)), pSettings->read_if_exists<Fvector3>(Hud, "item_orientation", Fvector().set(0.0f, 0.0f, 0.0f)));
	if (Thing)
	{
		Thing->Name = Item;
		Thing->Actor.Hud = Hud;
		Thing->Actor.IsHudLinked = true;
		Thing->Attach.Parent = { BodyId, Anchor };
	}
	Selected.assign(1, BodyId);
	SetStatus(!IsCombined && !Anchor.size(), !IsCombined && !Anchor.size() ? "Added the HUD of %s, but the hands section names no attach bone: the item sits at the hands' origin." : "Added the HUD of %s.", Item);
}

void Editor::AddHudMotion(Player& Core, u32 Id, const char* Alias)
{
	const Object* Hands = Find(Core.Data, Id);
	const char* Hud = Hands && Hands->Type == ObjectType::Actor ? Hands->Actor.Hud.c_str() : nullptr;
	const char* Value = Hud && pSettings->section_exist(Hud) ? pSettings->read_if_exists<str_c>(Hud, Alias, nullptr) : nullptr;
	if (!Value)
	{
		return;
	}
	string256 Names[3] = {};
	for (int Part = 0; Part < 3 && Part < _GetItemCount(Value); ++Part)
	{
		_GetItem(Value, Part, Names[Part]);
	}
	const float Speed = Names[2][0] ? std::clamp(static_cast<float>(atof(Names[2])), 0.05f, 20.0f) : 1.0f;
	u32 Targets[2] = { Id, 0 }, Missing = 0, Added = 0;
	for (const Object& Item : Core.Data.Objects)
	{
		Targets[1] = !Targets[1] && Item.Type == ObjectType::Actor && Item.Attach.Parent.ObjectId == Id && !Item.Actor.IsHudLinked ? Item.Id : Targets[1];
	}
	const float Time = Core.Time;
	for (u32 Side = 0; Side < 2; ++Side)
	{
		const char* Name = Side && Names[1][0] ? Names[1] : Names[0];
		const ActorRuntime* Runtime = Targets[Side] ? Core.FindActor(Targets[Side]) : nullptr;
		shared_str Source, Found;
		if (!Runtime || !(HudUi::FindSource(*Runtime, Name, Source, Found) || (Side && HudUi::FindSource(*Runtime, "idle", Source, Found))))
		{
			Missing += Targets[Side] != 0;
			continue;
		}
		AddAnimationAt(Core, Targets[Side], 0, Time, Source, Found);
		Object* Item = Find(Core.Data, Targets[Side]);
		xr_vector<ActorBlock>* Blocks = Item && FocusObject == Targets[Side] && FocusLayer <= Item->Actor.Layers.size() ? &BlocksOf(Item->Actor, FocusLayer) : nullptr;
		if (Blocks && FocusBlock >= 0 && FocusBlock < static_cast<s32>(Blocks->size()) && (*Blocks)[FocusBlock].Name == Found)
		{
			(*Blocks)[FocusBlock].Speed = Speed;
			Refit(Core, *Item);
			++Added;
		}
	}
	SetStatus(Missing != 0, Missing ? "%s: placed on %u row(s); %u model(s) do not have the animation it names." : "%s: placed on %u row(s).", Alias, Added, Missing);
}

void Editor::SyncHudLinks(Player& Core)
{
	for (u32 Index = 0; Index < Core.Data.Objects.size(); ++Index)
	{
		const Object& Item = Core.Data.Objects[Index];
		const Object* Hands = Item.Type == ObjectType::Actor && Item.Actor.IsHudLinked && !Item.Actor.IsRelative ? Find(Core.Data, Item.Attach.Parent.ObjectId) : nullptr;
		const ActorRuntime* Runtime = Hands && Hands->Type == ObjectType::Actor && !Hands->Actor.IsRelative ? Core.FindActor(Item.Id) : nullptr;
		if (!Runtime || Runtime->Sources.empty())
		{
			continue;
		}
		const u8 Rows = static_cast<u8>(Hands->Actor.Layers.size() + 1);
		const auto Fill = [&](u8 Row)
		{
			HudBlocks.clear();
			for (const ActorBlock& Played : BlocksOf(Hands->Actor, Row))
			{
				ActorBlock Copy = Played;
				if (HudUi::FindSource(*Runtime, Played.Name.c_str(), Copy.Source, Copy.Name) || HudUi::FindSource(*Runtime, "idle", Copy.Source, Copy.Name))
				{
					HudBlocks.push_back(Copy);
				}
			}
		};
		bool IsSame = Item.Actor.Layers.size() + 1 == Rows && Item.Span.Start == Hands->Span.Start;
		for (u8 Row = 0; IsSame && Row < Rows; ++Row)
		{
			Fill(Row);
			IsSame = BlocksOf(Item.Actor, Row) == HudBlocks;
		}
		if (IsSame)
		{
			continue;
		}
		Object& Edited = *Edit(Core, Item.Id, "Follow hands animation");
		Edited.Span.Start = Hands->Span.Start;
		Edited.Actor.Layers.resize(Rows - 1, NewRow(Runtime));
		for (u8 Row = 0; Row < Rows; ++Row)
		{
			Fill(Row);
			BlocksOf(Edited.Actor, Row) = HudBlocks;
		}
		Refit(Core, Edited);
	}
}

void Editor::UpdateHudView(Player& Core)
{
	const Object* Hands = IsHudView && !Job.IsActive ? HudHands(Core) : nullptr;
	const ObjectState* State = Hands ? Core.FindState(Hands->Id) : nullptr;
	if (!State || !State->IsReady)
	{
		if (HudNear >= 0.0f)
		{
			Device.fViewportNear = HudNear;
			HudNear = -1.0f;
		}
		return;
	}
	HudNear = HudNear < 0.0f ? Device.fViewportNear : HudNear;
	Device.fViewportNear = std::min(HudNear, Device.fHUDViewportNear);
	Fmatrix Back, View;
	Back.invert(HudUi::Offset(Hands->Actor.Hud.c_str(), "hands_position", "hands_orientation"));
	View.mul_43(State->World, Back);
	const Fvector Seat = View.c;
	View.c.set(0.0f, 0.0f, 0.0f);
	Core.SetEditorView(true, true, Seat, View, psHUD_FOV);
}

void Editor::BuildHudItems()
{
	if (!HudHandsSection[0])
	{
		xr_strcpy(HudHandsSection, pSettings->read_if_exists<str_c>("actor", "player_hud_default", "actor_hud"));
	}
	if (!HudItems.empty())
	{
		return;
	}
	for (CInifile::Sect& Section : pSettings->sections())
	{
		if (HudItems.size() < HudUi::MaxItems && Io::IniSafe(Section.Name.c_str()) && HudUi::HudOf(Section.Name.c_str()))
		{
			HudItems.push_back(Section.Name);
		}
	}
	std::sort(HudItems.begin(), HudItems.end(), [](const shared_str& Left, const shared_str& Right) { return xr_strcmp(Left, Right) < 0; });
}

void Editor::DrawActorPicker(Player& Core)
{
	BuildHudItems();
	LoadPresets();
	if (!IsActorModelsListed)
	{
		IsActorModelsListed = true;
		FS_FileSet Found;
		FS.file_list(Found, _game_meshes_, FS_ListFiles, "*.ogf");
		for (const FS_File& File : Found)
		{
			xr_string Path = File.name;
			std::replace(Path.begin(), Path.end(), '/', '\\');
			if (Path.size() > 11 && Path.size() < 200 && !Path.compare(0, 7, "actors\\") && Io::IniSafe(Path.c_str()) && ActorModels.size() < HudUi::MaxItems)
			{
				ActorModels.emplace_back(Path.substr(0, Path.size() - 4).c_str());
			}
		}
		std::sort(ActorModels.begin(), ActorModels.end(), [](const shared_str& Left, const shared_str& Right) { return xr_strcmp(Left, Right) < 0; });
	}
	const auto IsShown = [this](const char* Name) { return !HudFilter[0] || ImStristr(Name, nullptr, HudFilter, nullptr); };
	shared_str Preset, Model, Item;
	ImGui::TextDisabled("Add an actor to animate:");
	ImGui::SameLine();
	ImGui::SetNextItemWidth(ImGui::GetFontSize() * 14.0f);
	ImGui::InputTextWithHint("##ActorFilter", "Filter", HudFilter, sizeof(HudFilter));
	if (!ImGui::BeginTable("##ActorPicker", 2, ImGuiTableFlags_SizingStretchSame))
	{
		return;
	}
	ImGui::TableNextColumn();
	ImGui::TextUnformatted("Character (third person)");
	ImGui::BeginChild("##Characters", ImVec2(0.0f, 0.0f), ImGuiChildFlags_Borders);
	for (const ActorPreset& Entry : Presets)
	{
		ImGui::PushID(&Entry);
		Preset = IsShown(Entry.Name.c_str()) && ImGui::Selectable(Entry.Name.c_str()) ? shared_str(Entry.Name.c_str()) : Preset;
		ImGui::SetItemTooltip("Preset");
		ImGui::PopID();
	}
	for (const shared_str& Entry : ActorModels)
	{
		Model = IsShown(Entry.c_str()) && ImGui::Selectable(Entry.c_str()) ? Entry : Model;
	}
	ImGui::EndChild();
	ImGui::TableNextColumn();
	ImGui::TextUnformatted("Hands and item (first person)");
	ImGui::BeginChild("##HudItems", ImVec2(0.0f, 0.0f), ImGuiChildFlags_Borders);
	for (const shared_str& Entry : HudItems)
	{
		Item = IsShown(Entry.c_str()) && ImGui::Selectable(Entry.c_str()) ? Entry : Item;
	}
	ImGui::EndChild();
	ImGui::EndTable();
	Fvector Position, Rotation;
	SpawnPose(Position, Rotation);
	const u32 Before = Core.Data.NextId;
	if (Item.size())
	{
		AddHud(Core, Item.c_str(), Core.Time);
		IsHudView = Core.Data.NextId != Before;
	}
	else if (Preset.size())
	{
		AddActorFromPreset(Core, Preset.c_str(), Core.Time, Position, Rotation);
	}
	else if (Model.size() && AddObject(Core, ObjectType::Actor, Model, Core.Time, Position, Rotation))
	{
		Refit(Core, *Find(Core.Data, Before));
	}
	if (Core.Data.NextId != Before && Find(Core.Data, Before))
	{
		Selected.assign(1, Before);
		SelectedBones.clear();
		AnimObject = PoseObject = Before;
		AnimClipIndex = AnimStockBlock = -1;
		IsAnimFresh = false;
	}
}
}
