#include "StdAfx.h"
#include "pch_script.h"
#include "WeaponCustomizeScript.h"

#include "Actor.h"
#include "Level.h"
#include "inventory_item.h"
#include "Inventory.h"
#include "script_game_object.h"
#include "ai_space.h"
#include "ui/UIWeaponViewport.h"
#include "../xrScripts/script_engine.h"
#include "../xrEngine/string_table.h"

using namespace luabind;

void OpenWeaponCustomize(u32 id);

namespace WeaponCustomizeLua
{
constexpr ALife::_OBJECT_ID NoId = ALife::INVALID_OBJECT_ID;

struct SPlacement
{
	PIItem Item = nullptr;
	PIItem Owner = nullptr;
	shared_str Bone;
};

lua_State* Lua()
{
	return ai().script_engine().lua();
}

PIItem FindItem(ALife::_OBJECT_ID Id)
{
	if (!g_pGameLevel || Id == NoId)
	{
		return nullptr;
	}
	CObject* Object = Level().Objects.net_Find(Id);
	return Object ? Object->cast_inventory_item() : nullptr;
}

bool IsActorRoot(PIItem Item)
{
	CActor* Owner = Actor();
	return Owner && Item && !Item->m_attach_parent && Item->object().H_Parent() == Owner;
}

PIItem FindAttached(PIItem Owner, const shared_str& Bone)
{
	for (const auto& [Id, Child] : Owner->m_children)
	{
		if (Child->m_attach_bone == Bone)
		{
			return Child;
		}
	}
	return nullptr;
}

bool IsAllowed(PIItem Owner, const shared_str& Bone, const shared_str& Section)
{
	const auto& Bones = Owner->ItemDesc().m_customize_bones;
	const auto It = Bones.find(Bone);
	return It != Bones.end() && std::find(It->second.begin(), It->second.end(), Section) != It->second.end();
}

void SortedBones(PIItem Item, xr_vector<shared_str>& Result)
{
	Result.clear();
	for (const auto& [Bone, Sections] : Item->ItemDesc().m_customize_bones)
	{
		Result.push_back(Bone);
	}
	std::sort(Result.begin(), Result.end(), [](const shared_str& Left, const shared_str& Right)
	{
		return xr_strcmp(Left, Right) < 0;
	});
}

void CollectPlacements(PIItem Item, xr_vector<SPlacement>& Result)
{
	xr_vector<shared_str> Bones;
	SortedBones(Item, Bones);
	for (const shared_str& Bone : Bones)
	{
		if (PIItem Child = FindAttached(Item, Bone))
		{
			Result.push_back({ Child, Item, Bone });
			CollectPlacements(Child, Result);
		}
	}
}

bool IsInSubtree(PIItem Item, PIItem Root)
{
	for (PIItem It = Item; It; It = It->m_attach_parent)
	{
		if (It == Root)
		{
			return true;
		}
	}
	return false;
}

void SendMove(ALife::_OBJECT_ID From, ALife::_OBJECT_ID To, ALife::_OBJECT_ID What)
{
	NET_Packet P;
	CGameObject::u_EventGen(P, GE_OWNERSHIP_REJECT, From);
	P << What;
	CGameObject::u_EventSend(P);

	CGameObject::u_EventGen(P, GE_OWNERSHIP_TAKE, To);
	P << What;
	CGameObject::u_EventSend(P);
}

ALife::_OBJECT_ID ReadId(luabind::object Table, const char* Key)
{
	luabind::object Value = Table[Key];
	return Value.type() == LUA_TNUMBER ? ALife::_OBJECT_ID(luabind::object_cast<double>(Value)) : NoId;
}

shared_str ReadString(luabind::object Table, const char* Key)
{
	luabind::object Value = Table[Key];
	return Value.type() == LUA_TSTRING ? shared_str(luabind::object_cast<const char*>(Value)) : shared_str();
}

void PushSlots(PIItem Item, int Depth, int ParentIndex, luabind::object& Result, int& Count)
{
	xr_vector<shared_str> Bones;
	SortedBones(Item, Bones);
	for (const shared_str& Bone : Bones)
	{
		const RStringVec& Sections = Item->ItemDesc().m_customize_bones.at(Bone);
		PIItem Attached = FindAttached(Item, Bone);

		luabind::object Slot = luabind::newtable(Lua());
		Slot["owner"] = Item->object_id();
		Slot["owner_section"] = Item->object().cNameSect().c_str();
		Slot["bone"] = Bone.c_str();
		Slot["depth"] = Depth;
		Slot["parent"] = ParentIndex;

		luabind::object Allowed = luabind::newtable(Lua());
		int AllowedCount = 0;
		for (const shared_str& Section : Sections)
		{
			if (Section.size() && pSettings->section_exist(Section))
			{
				Allowed[++AllowedCount] = Section.c_str();
			}
		}
		Slot["allowed"] = Allowed;

		if (Attached)
		{
			Slot["attached"] = Attached->object_id();
			Slot["attached_section"] = Attached->object().cNameSect().c_str();
		}

		Result[++Count] = Slot;
		if (Attached)
		{
			PushSlots(Attached, Depth + 1, Count, Result, Count);
		}
	}
}

luabind::object Slots(ALife::_OBJECT_ID RootId)
{
	luabind::object Result = luabind::newtable(Lua());
	if (PIItem Root = FindItem(RootId))
	{
		int Count = 0;
		PushSlots(Root, 0, 0, Result, Count);
	}
	return Result;
}

luabind::object Layout(ALife::_OBJECT_ID RootId)
{
	luabind::object Result = luabind::newtable(Lua());
	PIItem Root = FindItem(RootId);
	if (!Root)
	{
		return Result;
	}

	xr_vector<SPlacement> Placements;
	CollectPlacements(Root, Placements);
	int Count = 0;
	for (const SPlacement& Placement : Placements)
	{
		luabind::object Entry = luabind::newtable(Lua());
		Entry["id"] = Placement.Item->object_id();
		Entry["owner"] = Placement.Owner->object_id();
		Entry["bone"] = Placement.Bone.c_str();
		Entry["section"] = Placement.Item->object().cNameSect().c_str();
		Result[++Count] = Entry;
	}
	return Result;
}

u32 Signature(ALife::_OBJECT_ID RootId)
{
	PIItem Root = FindItem(RootId);
	if (!Root)
	{
		return 0;
	}

	xr_vector<SPlacement> Placements;
	CollectPlacements(Root, Placements);
	u32 Hash = 2166136261u;
	auto Mix = [&Hash](u32 Value)
	{
		Hash = (Hash ^ Value) * 16777619u;
	};
	for (const SPlacement& Placement : Placements)
	{
		Mix(Placement.Item->object_id());
		Mix(Placement.Owner->object_id());
		for (const char* It = Placement.Bone.c_str(); *It; ++It)
		{
			Mix(u32(*It));
		}
	}
	Mix(u32(Placements.size()));
	return Hash;
}

luabind::object Conflicts(ALife::_OBJECT_ID RootId, ALife::_OBJECT_ID PartId, ALife::_OBJECT_ID OwnerId, const char* BoneName)
{
	luabind::object Result = luabind::newtable(Lua());
	PIItem Root = FindItem(RootId);
	PIItem Part = FindItem(PartId);
	PIItem Owner = FindItem(OwnerId);
	if (!Root || !Part || !Owner || !BoneName)
	{
		return Result;
	}

	const shared_str Bone = BoneName;
	xr_vector<PIItem> Found;
	if (PIItem Occupant = FindAttached(Owner, Bone); Occupant && Occupant != Part)
	{
		Found.push_back(Occupant);
	}

	xr_vector<PIItem> Collected;
	Root->CollectConflicts(Part, Collected);
	for (PIItem Conflict : Collected)
	{
		if (Conflict == Part || IsInSubtree(Owner, Conflict) || IsInSubtree(Conflict, Part))
		{
			continue;
		}
		if (std::find(Found.begin(), Found.end(), Conflict) == Found.end())
		{
			Found.push_back(Conflict);
		}
	}

	int Count = 0;
	for (PIItem Item : Found)
	{
		Result[++Count] = Item->object_id();
	}
	return Result;
}

bool ApplyLayout(ALife::_OBJECT_ID RootId, luabind::object Target)
{
	PIItem Root = FindItem(RootId);
	CActor* Owner = Actor();
	if (!IsActorRoot(Root) || Target.type() != LUA_TTABLE)
	{
		return false;
	}

	xr_vector<SPlacement> Wanted;
	for (luabind::object::iterator It = Target.begin(); It != Target.end(); ++It)
	{
		luabind::object Entry = *It;
		if (Entry.type() != LUA_TTABLE)
		{
			continue;
		}

		SPlacement Placement;
		Placement.Item = FindItem(ReadId(Entry, "id"));
		Placement.Owner = FindItem(ReadId(Entry, "owner"));
		Placement.Bone = ReadString(Entry, "bone");
		if (!Placement.Item || !Placement.Owner || !Placement.Bone.size() || Placement.Item == Root || Placement.Item == Placement.Owner)
		{
			continue;
		}
		if (!IsAllowed(Placement.Owner, Placement.Bone, Placement.Item->object().cNameSect()))
		{
			continue;
		}
		Wanted.push_back(Placement);
	}

	xr_vector<SPlacement> Current;
	CollectPlacements(Root, Current);

	auto FindWanted = [&Wanted](PIItem Item) -> const SPlacement*
	{
		for (const SPlacement& Placement : Wanted)
		{
			if (Placement.Item == Item)
			{
				return &Placement;
			}
		}
		return nullptr;
	};

	xr_vector<PIItem> Available;
	for (const SPlacement& Placement : Current)
	{
		const SPlacement* Want = FindWanted(Placement.Item);
		if (Want && Want->Owner == Placement.Owner && Want->Bone == Placement.Bone)
		{
			continue;
		}

		bool ParentLeaves = false;
		for (PIItem It = Placement.Owner; It && It != Root; It = It->m_attach_parent)
		{
			if (std::find(Available.begin(), Available.end(), It) != Available.end())
			{
				ParentLeaves = true;
				break;
			}
		}

		if (!ParentLeaves || Want)
		{
			SendMove(Placement.Owner->object_id(), Owner->ID(), Placement.Item->object_id());
		}
		Available.push_back(Placement.Item);
	}

	xr_vector<PIItem> Placed;
	Placed.push_back(Root);
	for (const SPlacement& Placement : Current)
	{
		if (std::find(Available.begin(), Available.end(), Placement.Item) == Available.end())
		{
			Placed.push_back(Placement.Item);
		}
	}

	bool Progress = true;
	xr_vector<const SPlacement*> Pending;
	for (const SPlacement& Placement : Wanted)
	{
		if (std::find(Placed.begin(), Placed.end(), Placement.Item) == Placed.end())
		{
			Pending.push_back(&Placement);
		}
	}

	xr_vector<shared_str> UsedSlots;
	auto SlotKey = [](PIItem Item, const shared_str& Bone)
	{
		string256 Key;
		xr_sprintf(Key, "%u:%s", u32(Item->object_id()), Bone.c_str());
		return shared_str(Key);
	};
	for (const SPlacement& Placement : Current)
	{
		if (std::find(Available.begin(), Available.end(), Placement.Item) == Available.end())
		{
			UsedSlots.push_back(SlotKey(Placement.Owner, Placement.Bone));
		}
	}

	bool Changed = !Available.empty();
	while (Progress && !Pending.empty())
	{
		Progress = false;
		for (auto It = Pending.begin(); It != Pending.end();)
		{
			const SPlacement& Placement = **It;
			if (std::find(Placed.begin(), Placed.end(), Placement.Owner) == Placed.end())
			{
				++It;
				continue;
			}

			const bool Free = std::find(Available.begin(), Available.end(), Placement.Item) != Available.end() || IsActorRoot(Placement.Item);
			const shared_str Key = SlotKey(Placement.Owner, Placement.Bone);
			if (!Free || std::find(UsedSlots.begin(), UsedSlots.end(), Key) != UsedSlots.end())
			{
				It = Pending.erase(It);
				continue;
			}

			Placement.Item->m_attach_request = Placement.Bone;
			SendMove(Owner->ID(), Placement.Owner->object_id(), Placement.Item->object_id());
			Placed.push_back(Placement.Item);
			UsedSlots.push_back(Key);
			It = Pending.erase(It);
			Progress = true;
			Changed = true;
		}
	}

	return Changed;
}

bool Attach(ALife::_OBJECT_ID RootId, ALife::_OBJECT_ID PartId, ALife::_OBJECT_ID OwnerId, const char* BoneName)
{
	PIItem Root = FindItem(RootId);
	PIItem Part = FindItem(PartId);
	PIItem Owner = FindItem(OwnerId);
	CActor* Holder = Actor();
	if (!IsActorRoot(Root) || !Part || !Owner || !BoneName || !Holder)
	{
		return false;
	}

	const shared_str Bone = BoneName;
	if (Owner->AttachRoot() != Root || !IsAllowed(Owner, Bone, Part->object().cNameSect()))
	{
		return false;
	}

	if (FindAttached(Owner, Bone) == Part)
	{
		return false;
	}

	if (!IsActorRoot(Part))
	{
		if (Part->AttachRoot() != Root || IsInSubtree(Owner, Part))
		{
			return false;
		}
		SendMove(Part->m_attach_parent->object_id(), Holder->ID(), Part->object_id());
	}

	if (PIItem Occupant = FindAttached(Owner, Bone))
	{
		SendMove(Owner->object_id(), Holder->ID(), Occupant->object_id());
	}

	Part->m_attach_request = Bone;
	SendMove(Holder->ID(), Owner->object_id(), Part->object_id());
	return true;
}

bool Detach(ALife::_OBJECT_ID PartId)
{
	PIItem Part = FindItem(PartId);
	CActor* Holder = Actor();
	if (!Part || !Holder || !Part->m_attach_parent || !IsActorRoot(Part->AttachRoot()))
	{
		return false;
	}

	SendMove(Part->m_attach_parent->object_id(), Holder->ID(), Part->object_id());
	return true;
}

ALife::_OBJECT_ID AttachParent(ALife::_OBJECT_ID Id)
{
	PIItem Item = FindItem(Id);
	return Item && Item->m_attach_parent ? Item->m_attach_parent->object_id() : NoId;
}

ALife::_OBJECT_ID AttachRootId(ALife::_OBJECT_ID Id)
{
	PIItem Item = FindItem(Id);
	return Item ? Item->AttachRoot()->object_id() : NoId;
}

bool IsCustomizable(ALife::_OBJECT_ID Id)
{
	PIItem Item = FindItem(Id);
	return Item && !Item->ItemDesc().m_customize_bones.empty();
}

luabind::object ActorParts()
{
	luabind::object Result = luabind::newtable(Lua());
	CActor* Holder = Actor();
	if (!Holder)
	{
		return Result;
	}

	int Count = 0;
	for (PIItem Item : Holder->inventory().m_all)
	{
		if (!IsActorRoot(Item))
		{
			continue;
		}
		luabind::object Entry = luabind::newtable(Lua());
		Entry["id"] = Item->object_id();
		Entry["section"] = Item->object().cNameSect().c_str();
		Entry["condition"] = Item->GetCondition();
		Result[++Count] = Entry;
	}
	return Result;
}

void OpenDebug(ALife::_OBJECT_ID Id)
{
	OpenWeaponCustomize(Id);
}
}

bool WeaponCustomizeScript::TryOpen(CInventoryItem* Item)
{
	if (!Item || !pSettings->section_exist("weapon_customize"))
	{
		return false;
	}

	const char* Name = pSettings->read_if_exists<str_c>("weapon_customize", "ui_functor", "");
	if (!Name || !Name[0])
	{
		return false;
	}

	luabind::functor<bool> Open;
	if (!ai().script_engine().functor(Name, Open))
	{
		Msg("! [weapon_customize] functor [%s] not found", Name);
		return false;
	}

	try
	{
		return Open(Item->object().lua_game_object());
	}
	catch (...)
	{
		Msg("! [weapon_customize] functor [%s] failed", Name);
		return false;
	}
}

void weapon_customize_registrator::script_register(lua_State* L)
{
	module(L, "weapon_customize")
	[
		def("slots", &WeaponCustomizeLua::Slots),
		def("layout", &WeaponCustomizeLua::Layout),
		def("signature", &WeaponCustomizeLua::Signature),
		def("conflicts", &WeaponCustomizeLua::Conflicts),
		def("apply_layout", &WeaponCustomizeLua::ApplyLayout),
		def("attach", &WeaponCustomizeLua::Attach),
		def("detach", &WeaponCustomizeLua::Detach),
		def("parent", &WeaponCustomizeLua::AttachParent),
		def("root", &WeaponCustomizeLua::AttachRootId),
		def("is_customizable", &WeaponCustomizeLua::IsCustomizable),
		def("actor_parts", &WeaponCustomizeLua::ActorParts),
		def("open_debug", &WeaponCustomizeLua::OpenDebug)
	];

	CUIWeaponViewport::script_register(L);
}
