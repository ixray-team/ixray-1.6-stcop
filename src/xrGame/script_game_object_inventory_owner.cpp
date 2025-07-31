////////////////////////////////////////////////////////////////////////////
// script_game_object_inventory_owner.сpp :	функции для inventory owner
//////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "script_game_object.h"
#include "InventoryOwner.h"
#include "PDA.h"
#include "xrMessages.h"
#include "character_info.h"
#include "GameTask.h"
#include "Actor.h"
#include "Level.h"
#include "../xrEngine/date_time.h"
#include "UIGameSP.h"
#include "restricted_object.h"
#include "../xrScripts/script_engine.h"
#include "attachable_item.h"
#include "script_entity.h"
#include "../xrEngine/string_table.h"
#include "alife_registry_wrappers.h"
#include "relation_registry.h"
#include "CustomMonster.h"
#include "ActorCondition.h"
#include "level_graph.h"
#include "HudItem.h"
#include "ui/UITalkWnd.h"
#include "Inventory.h"
#include "InfoPortion.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "WeaponMagazined.h"
#include "ai/stalker/ai_stalker.h"
#include "agent_manager.h"
#include "agent_member_manager.h"
#include "stalker_animation_manager.h"
#include "CameraFirstEye.h"
#include "stalker_movement_manager_smart_cover.h"
#include "memory_manager.h"
#include "enemy_manager.h"
#include "ai/stalker/ai_stalker_impl.h"
#include "smart_cover_object.h"
#include "smart_cover.h"
#include "CustomDetector.h"
#include "doors_manager.h"
#include "doors_door.h"
#include "Torch.h"
#include "PhysicObject.h"
#include "inventory_upgrade_manager.h"
#include "GametaskManager.h"
#include "player_hud.h"
#include "CustomOutfit.h"

bool CScriptGameObject::GiveInfoPortion(LPCSTR info_id)
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		pInventoryOwner->TransferInfo(info_id, true);
		return true;
	}

	return false;
}

bool CScriptGameObject::DisableInfoPortion(LPCSTR info_id)
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		pInventoryOwner->TransferInfo(info_id, false);
		return true;
	}

	return false;
}

void _AddIconedTalkMessage(LPCSTR caption, LPCSTR text, LPCSTR texture_name, LPCSTR templ_name);

void CScriptGameObject::AddIconedTalkMessage(LPCSTR caption, LPCSTR text, LPCSTR texture_name, LPCSTR templ_name)
{
	_AddIconedTalkMessage(caption, text, texture_name, templ_name);
}

void _AddIconedTalkMessage(LPCSTR caption, LPCSTR text, LPCSTR texture_name, LPCSTR templ_name)
{
	if (CUIGameCustom* current_ui = CurrentGameUI())
	{
		if (current_ui->TalkMenu->IsShown())
		{
			current_ui->TalkMenu->AddIconedMessage(caption, text, texture_name, templ_name ? templ_name : "iconed_answer_item");
		}
	}
}

void _give_news(LPCSTR caption, LPCSTR news, LPCSTR texture_name, int delay, int show_time, int type);

void CScriptGameObject::GiveGameNews(LPCSTR caption, LPCSTR news, LPCSTR texture_name, int delay, int show_time)
{
	GiveGameNews(caption, news,	texture_name, delay, show_time, 0);
}

void CScriptGameObject::GiveGameNews(LPCSTR caption, LPCSTR news, LPCSTR texture_name, int delay, int show_time, int type)
{
	_give_news(caption, news, texture_name, delay, show_time, type);	
}

void _give_news(LPCSTR caption, LPCSTR text, LPCSTR texture_name, int delay, int show_time, int type)
{
	GAME_NEWS_DATA news_data;
	news_data.m_type = (GAME_NEWS_DATA::eNewsType)type;
	news_data.news_caption = caption;
	news_data.news_text = text;
	if (show_time != 0)
	{
		news_data.show_time = show_time;// override default
	}

	VERIFY(xr_strlen(texture_name) > 0);

	news_data.texture_name = texture_name;

	if (delay == 0)
	{
		Actor()->AddGameNews(news_data);
	}
	else
	{
		Actor()->AddGameNews_deffered(news_data, delay);
	}
}

bool CScriptGameObject::HasInfo(LPCSTR info_id)
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		return pInventoryOwner->HasInfo(info_id);
	}

	return false;

}
bool CScriptGameObject::DontHasInfo(LPCSTR info_id)
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		return !pInventoryOwner->HasInfo(info_id);
	}

	return false;
}

bool CScriptGameObject::IsTalking()
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		return pInventoryOwner->IsTalking();
	}

	return false;
}

void CScriptGameObject::StopTalk()
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		pInventoryOwner->StopTalk();
	}
}

void CScriptGameObject::EnableTalk()
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		pInventoryOwner->EnableTalk();
	}
}

void CScriptGameObject::DisableTalk()
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		pInventoryOwner->DisableTalk();
	}
}

bool CScriptGameObject::IsTalkEnabled()
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		return pInventoryOwner->IsTalkEnabled();
	}

	return false;
}

void CScriptGameObject::EnableTrade()
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		pInventoryOwner->EnableTrade();
	}
}

void CScriptGameObject::DisableTrade()
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		pInventoryOwner->DisableTrade();
	}
}

bool CScriptGameObject::IsTradeEnabled()
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		return pInventoryOwner->IsTradeEnabled();
	}

	return false;
}

void CScriptGameObject::EnableInvUpgrade()
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		pInventoryOwner->EnableInvUpgrade();
	}
}

void CScriptGameObject::DisableInvUpgrade()
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		pInventoryOwner->DisableInvUpgrade();
	}
}

bool CScriptGameObject::IsInvUpgradeEnabled()
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		return pInventoryOwner->IsInvUpgradeEnabled();
	}

	return false;
}

void CScriptGameObject::ForEachInventoryItems(const luabind::functor<void> &functor)
{
	if (CInventoryOwner* owner = object().cast_inventory_owner())
	{
		TIItemContainer item_list = {};
		owner->inventory().AddAvailableItems(item_list, true);

		for (auto item : item_list)
		{
			if (CGameObject* inv_go = item->cast_game_object())
			{
				functor(inv_go->lua_game_object(), this);
			}
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CScriptGameObject::ForEachInventoryItems non-CInventoryOwner object !!!");
	}
}

void CScriptGameObject::IterateInventory(luabind::functor<bool> functor, luabind::object object)
{
	if (CInventoryOwner* inventory_owner = this->object().cast_inventory_owner())
	{
		for (PIItem item : inventory_owner->inventory().m_all)
		{
			if (functor(object, item->object().lua_game_object()) == true)
			{
				return;
			}
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject::IterateInventory non-CInventoryOwner object !!!");
	}
}

void CScriptGameObject::IterateInventoryBox(luabind::functor<bool> functor, luabind::object object)
{
	if (CInventoryBox* inventory_box = this->object().cast_inventory_box())
	{
		for (const u16& id : inventory_box->m_items)
		{
			if (CGameObject* GO = Level().Objects.net_Find(id)->cast_game_object())
			{
				if (functor(object, GO->lua_game_object()) == true)
				{
					return;
				}
			}
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject::IterateInventoryBox non-CInventoryBox object !!!");
	}
}

void CScriptGameObject::MarkItemDropped(CScriptGameObject *item)
{
	CInventoryOwner* inventory_owner = object().cast_inventory_owner();
	if (inventory_owner == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CScriptGameObject::MarkItemDropped non-CInventoryOwner object !!!");
		return;
	}

	CInventoryItem* inventory_item = item->object().cast_inventory_item();
	if (inventory_item == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CScriptGameObject::MarkItemDropped non-CInventoryItem object !!!");
		return;
	}

	inventory_item->SetDropManual(TRUE);
}

bool CScriptGameObject::MarkedDropped(CScriptGameObject *item)
{
	CInventoryOwner* inventory_owner = object().cast_inventory_owner();
	if (inventory_owner == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CScriptGameObject::MarkedDropped non-CInventoryOwner object !!!");
		return false;
	}

	CInventoryItem* inventory_item = item->object().cast_inventory_item();
	if (inventory_item == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CScriptGameObject::MarkedDropped non-CInventoryItem object !!!");
		return false;
	}

	return !!inventory_item->GetDropManual();
}

void CScriptGameObject::UnloadMagazine()
{
	CWeaponMagazined* weapon_magazined = object().cast_weapon_magazined();
	if (weapon_magazined == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CScriptGameObject::UnloadMagazine non-CWeaponMagazined object !!!");
		return;
	}

	CObject* parent = weapon_magazined->H_Parent();
	CAI_Stalker* stalker = parent != nullptr ? parent->cast_stalker() : nullptr;
	if (stalker != nullptr && stalker->hammer_is_clutched())
	{
		return;
	}

	weapon_magazined->UnloadMagazine(false);
}
//

void CScriptGameObject::DropItem(CScriptGameObject* pItem)
{
	CInventoryOwner* owner = object().cast_inventory_owner();
	CInventoryItem* item = pItem->object().cast_inventory_item();
	if (owner == nullptr || item == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CScriptGameObject::DropItem non-CInventoryOwner object !!!");
		return;
	}

	NET_Packet P;
	CGameObject::u_EventGen(P,GE_OWNERSHIP_REJECT, object().ID());
	P.w_u16(pItem->object().ID());
	CGameObject::u_EventSend(P);
}

void CScriptGameObject::DropItemAndTeleport(CScriptGameObject* pItem, Fvector position)
{
	DropItem(pItem);

	NET_Packet PP;
	CGameObject::u_EventGen(PP,GE_CHANGE_POS, pItem->object().ID());
	PP.w_vec3(position);
	CGameObject::u_EventSend(PP);
}

void CScriptGameObject::MakeItemActive(CScriptGameObject* pItem)
{
	if (pItem == nullptr)
	{
		return;
	}

	CInventoryOwner* owner = object().cast_inventory_owner();
	CInventoryItem* item = pItem->object().cast_inventory_item();
	u16 slot = item->BaseSlot();

	NET_Packet P;
	if (CInventoryItem* item_in_slot = owner->inventory().ItemFromSlot(slot))
	{
		CGameObject::u_EventGen(P, GEG_PLAYER_ITEM2RUCK, owner->object_id());
		P.w_u16(item_in_slot->object().ID());
		CGameObject::u_EventSend(P);
	}
	CGameObject::u_EventGen(P, GEG_PLAYER_ITEM2SLOT, owner->object_id());
	P.w_u16(item->object().ID());
	P.w_u16(slot);
	CGameObject::u_EventSend(P);

	CGameObject::u_EventGen(P, GEG_PLAYER_ACTIVATE_SLOT, owner->object_id());
	P.w_u16(slot);
	CGameObject::u_EventSend(P);

}

float CScriptGameObject::GetActorMaxWeight() const
{
	if (CActor* pActor = object().cast_actor())
	{
		return pActor->inventory().GetMaxWeight();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member GetActorMaxWeight!");
	return false;
}

void CScriptGameObject::SetActorMaxWeight(float max_weight)
{
	if (CActor* pActor = object().cast_actor())
	{
		pActor->inventory().SetMaxWeight(max_weight);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member SetActorMaxWeight!");
	}
}

// получить и задать максимальный вес при котором можно ходить
float CScriptGameObject::GetActorMaxWalkWeight() const
{
	if (CActor* pActor = object().cast_actor())
	{
		return (pActor->conditions().m_MaxWalkWeight);
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member GetActorMaxWalkWeight!");
	return false;
}

void CScriptGameObject::SetActorMaxWalkWeight(float max_walk_weight)
{
	if (CActor* pActor = object().cast_actor())
	{
		pActor->conditions().m_MaxWalkWeight = max_walk_weight;
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member SetActorMaxWalkWeight!");
	}
}

//передаче вещи из своего инвентаря в инвентарь партнера
void CScriptGameObject::TransferItem(CScriptGameObject* pItem, CScriptGameObject* pForWho)
{
	if (pItem == nullptr || pForWho == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"cannot transfer nullptr item");
		return;
	}

	CInventoryItem* pIItem = pItem->object().cast_inventory_item();

	if (pIItem == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"Cannot transfer not CInventoryItem item");
		return;
	}

	// выбросить у себя 
	NET_Packet P;
	CGameObject::u_EventGen(P, GE_TRADE_SELL, object().ID());
	P.w_u16(pIItem->object().ID());
	CGameObject::u_EventSend(P);

	// отдать партнеру
	CGameObject::u_EventGen(P, GE_TRADE_BUY, pForWho->object().ID());
	P.w_u16(pIItem->object().ID());
	CGameObject::u_EventSend(P);
}

u32 CScriptGameObject::Money()
{
	CInventoryOwner* pOurOwner = object().cast_inventory_owner();
	VERIFY(pOurOwner);
	return pOurOwner->get_money();
}

void CScriptGameObject::TransferMoney(int money, CScriptGameObject* pForWho)
{
	if (pForWho == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"cannot transfer money for nullptr object");
		return;
	}

	CInventoryOwner* pOurOwner = object().cast_inventory_owner();
	VERIFY(pOurOwner);

	CInventoryOwner* pOtherOwner = pForWho->object().cast_inventory_owner();
	VERIFY(pOtherOwner);

	if (pOurOwner->get_money() - money < 0)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"Character does not have enought money");
		return;
	}

	pOurOwner->set_money(pOurOwner->get_money() - money, true);
	pOtherOwner->set_money(pOtherOwner->get_money() + money, true);
}

void CScriptGameObject::GiveMoney(int money)
{
	CInventoryOwner* pOurOwner = object().cast_inventory_owner();
	VERIFY(pOurOwner);

	pOurOwner->set_money(pOurOwner->get_money() + money, true);
}
//////////////////////////////////////////////////////////////////////////

int	CScriptGameObject::GetGoodwill(CScriptGameObject* pToWho)
{
	CInventoryOwner* pInventoryOwner = object().cast_inventory_owner();

	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		return RELATION_REGISTRY().GetGoodwill(pInventoryOwner->object_id(), pToWho->object().ID());
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"GetGoodwill available only for InventoryOwner");
	return 0;
}

void CScriptGameObject::SetGoodwill(int goodwill, CScriptGameObject* pWhoToSet)
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		RELATION_REGISTRY().SetGoodwill(pInventoryOwner->object_id(), pWhoToSet->object().ID(), goodwill);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"SetGoodwill available only for InventoryOwner");
	}
}

void CScriptGameObject::ForceSetGoodwill(int goodwill, CScriptGameObject* pWhoToSet)
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		RELATION_REGISTRY().ForceSetGoodwill(pInventoryOwner->object_id(), pWhoToSet->object().ID(), goodwill);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "ForceSetGoodwill available only for InventoryOwner");
	}
}

void CScriptGameObject::ChangeGoodwill(int delta_goodwill, CScriptGameObject* pWhoToSet)
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		RELATION_REGISTRY().ChangeGoodwill(pInventoryOwner->object_id(), pWhoToSet->object().ID(), delta_goodwill);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"ChangeGoodwill available only for InventoryOwner");
	}
}

//////////////////////////////////////////////////////////////////////////

void CScriptGameObject::SetRelation(ALife::ERelationType relation, CScriptGameObject* pWhoToSet)
{
	CInventoryOwner* pInventoryOwner = object().cast_inventory_owner();

	if (pInventoryOwner == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"SetRelation available only for InventoryOwner");
		return;
	}

	CInventoryOwner* pOthersInventoryOwner = pWhoToSet->object().cast_inventory_owner();
	VERIFY(pOthersInventoryOwner);

	if (pOthersInventoryOwner == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"SetRelation available only for InventoryOwner");
		return;
	}

	RELATION_REGISTRY().SetRelationType(pInventoryOwner, pOthersInventoryOwner, relation);
}

float CScriptGameObject::GetSympathy()
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		return pInventoryOwner->Sympathy();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "GetSympathy available only for InventoryOwner");
	return 0.0f;
}

void CScriptGameObject::SetSympathy(float sympathy)
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		pInventoryOwner->CharacterInfo().SetSympathy(sympathy);
	}
	else
	{
		ai().script_engine().script_log	(ScriptStorage::eLuaMessageTypeError,"SetSympathy available only for InventoryOwner");
	}
}

int CScriptGameObject::GetCommunityGoodwill_obj(LPCSTR community)
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		CHARACTER_COMMUNITY c;
		c.set(community);

		return RELATION_REGISTRY().GetCommunityGoodwill(c.index(), pInventoryOwner->object_id());
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "GetCommunityGoodwill available only for InventoryOwner");
	return 0;
}

void CScriptGameObject::SetCommunityGoodwill_obj(LPCSTR community, int goodwill)
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		CHARACTER_COMMUNITY c;
		c.set(community);

		RELATION_REGISTRY().SetCommunityGoodwill(c.index(), pInventoryOwner->object_id(), goodwill);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "SetCommunityGoodwill available only for InventoryOwner");
	}
}

int	CScriptGameObject::GetAttitude(CScriptGameObject* pToWho)
{
	CInventoryOwner* pInventoryOwner = object().cast_inventory_owner();
	VERIFY(pInventoryOwner);

	CInventoryOwner* pOthersInventoryOwner = pToWho->object().cast_inventory_owner();
	VERIFY(pOthersInventoryOwner);

	return RELATION_REGISTRY().GetAttitude(pInventoryOwner, pOthersInventoryOwner);
}

LPCSTR CScriptGameObject::ProfileName()
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		shared_str profile_id = pInventoryOwner->CharacterInfo().Profile();
		if (profile_id != nullptr && profile_id.size())
		{
			return *profile_id;
		}
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "ProfileName available only for InventoryOwner");
	return 0;
}

LPCSTR CScriptGameObject::CharacterName()
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		return pInventoryOwner->Name();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CharacterName available only for InventoryOwner");
	return 0;
}

LPCSTR CScriptGameObject::CharacterIcon()
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		return pInventoryOwner->IconName();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CharacterIconName available only for InventoryOwner");
	return 0;
}

int CScriptGameObject::CharacterRank()
{
	// rank support for monster
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		return pInventoryOwner->Rank();
	}
	else if (CBaseMonster* monster = object().cast_base_monster())
	{
		return monster->Rank();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CharacterRank available only for InventoryOwner and BaseMonster");
	return 0;
}

void CScriptGameObject::SetCharacterRank(int char_rank)
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		pInventoryOwner->SetRank(char_rank);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"SetCharacterRank available only for InventoryOwner");
	}
}

void CScriptGameObject::ChangeCharacterRank(int char_rank)
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		pInventoryOwner->ChangeRank(char_rank);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"ChangeCharacterRank available only for InventoryOwner");
	}
}

int CScriptGameObject::CharacterReputation()
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		return pInventoryOwner->Reputation();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CharacterReputation available only for InventoryOwner");
	return 0;
}

void CScriptGameObject::ChangeCharacterReputation(int char_rep)
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		pInventoryOwner->ChangeReputation(char_rep);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"ChangeCharacterReputation available only for InventoryOwner");
	}
}

void CScriptGameObject::SetCharacterReputation(int char_rep)
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		pInventoryOwner->SetReputation(char_rep);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "SetCharacterReputation available only for InventoryOwner");
	}
}

LPCSTR CScriptGameObject::CharacterCommunity()
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		return *pInventoryOwner->CharacterInfo().Community().id();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CharacterCommunity available only for InventoryOwner");
	return 0;
}

void CScriptGameObject::SetCharacterCommunity(LPCSTR comm, int squad, int group)
{
	CInventoryOwner* pInventoryOwner = object().cast_inventory_owner();
	CEntity* entity	= object().cast_entity();

	if (pInventoryOwner == nullptr || entity == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"SetCharacterCommunity available only for InventoryOwner");
		return;
	}

	CHARACTER_COMMUNITY	community;
	community.set(comm);

	if (community.index() >= 0)
	{
		pInventoryOwner->SetCommunity(community.index());
		entity->ChangeTeam(community.team(), squad, group);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeInfo, "SetCharacterCommunity can't set %s for %s", comm, Name());
	}
}

LPCSTR CScriptGameObject::sound_voice_prefix() const
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		return pInventoryOwner->SpecificCharacter().sound_voice_prefix();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "sound_voice_prefix available only for InventoryOwner");
	return 0;
}

ETaskState CScriptGameObject::GetGameTaskState(LPCSTR task_id)
{
	shared_str shared_name = task_id;
	
	if (CGameTask* t = Level().GameTaskManager()->HasGameTask(shared_name, true))
	{
		return t->GetTaskState();
	}

	return eTaskStateDummy;
}

void CScriptGameObject::SetGameTaskState(ETaskState state, LPCSTR task_id)
{
	shared_str shared_name = task_id;
	Level().GameTaskManager()->SetTaskState(shared_name, state);
}

void CScriptGameObject::SwitchToTrade()
{
	CActor* pActor = object().cast_actor();
	if (pActor == nullptr)
	{
		return;
	}

	if (CUIGameCustom* current_ui = CurrentGameUI())
	{
		if (current_ui->TalkMenu->IsShown())
		{
			current_ui->TalkMenu->SwitchToTrade();
		}
	}
}

void CScriptGameObject::SwitchToUpgrade()
{
	CActor* pActor = object().cast_actor();
	if (pActor == nullptr)
	{
		return;
	}

	if (CUIGameCustom* current_ui = CurrentGameUI())
	{
		if (current_ui->TalkMenu->IsShown())
		{
			current_ui->TalkMenu->SwitchToUpgrade();
		}
	}
}

void CScriptGameObject::SwitchToTalk()
{
	//Omg bro wtf hell no
	R_ASSERT("switch_to_talk called ;)");
}

void CScriptGameObject::AllowBreakTalkDialog(bool b)
{
	if (EngineExternal().CallOfPripyatMode())
	{
		CInventoryOwner* inv_owner = object().cast_inventory_owner();
		VERIFY(inv_owner);
		inv_owner->bDisableBreakDialog = !b;
	}
	else
	{
		CUIGameSP* pGameSP = smart_cast<CUIGameSP*>(CurrentGameUI());
		if (!pGameSP) return;
		pGameSP->TalkMenu->b_disable_break = !b;
	}
}

void CScriptGameObject::RunTalkDialog(CScriptGameObject* pToWho, bool disable_break)
{
	if (CActor* pActor = object().cast_actor())
	{
		CInventoryOwner* pPartner = pToWho->object().cast_inventory_owner();
		VERIFY(pPartner);

		pActor->RunTalkDialog(pPartner, disable_break);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"RunTalkDialog applicable only for actor");
	}
}

void CScriptGameObject::ActorLookAtPoint(Fvector point)
{
	CCameraBase* c = Actor()->cam_FirstEye();
	CCameraFirstEye* cf = smart_cast<CCameraFirstEye*>(c);
	cf->LookAtPoint(point);
}

void construct_restriction_vector(shared_str restrictions, xr_vector<ALife::_OBJECT_ID> &result)
{
	result.clear();
	string64 temp = {};
	u32	n = _GetItemCount(*restrictions);
	for (u32 i =0 ; i < n; ++i)
	{
		CObject	*object = Level().Objects.FindObjectByName(_GetItem(*restrictions, i, temp));
		if (object == nullptr)
		{
			continue;
		}

		result.push_back(object->ID());
	}
}

void CScriptGameObject::add_restrictions(LPCSTR out, LPCSTR in)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->movement().restrictions().add_restrictions(out, in);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CRestrictedObject : cannot access class member add_restrictions!");
	}
}

void CScriptGameObject::remove_restrictions(LPCSTR out, LPCSTR in)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->movement().restrictions().remove_restrictions(out, in);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CRestrictedObject : cannot access class member remove_restrictions!");
	}
}

void CScriptGameObject::remove_all_restrictions()
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->movement().restrictions().remove_all_restrictions();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CRestrictedObject : cannot access class member remove_all_restrictions!");
	}
}

LPCSTR CScriptGameObject::in_restrictions()
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		return (*monster->movement().restrictions().in_restrictions());
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CRestrictedObject : cannot access class member in_restrictions!");
	return "";
}

LPCSTR CScriptGameObject::out_restrictions()
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		return (*monster->movement().restrictions().out_restrictions());
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CRestrictedObject : cannot access class member out_restrictions!");
	return "";
}

LPCSTR CScriptGameObject::base_in_restrictions()
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		return (*monster->movement().restrictions().base_in_restrictions());
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CRestrictedObject : cannot access class member base_in_restrictions!");
	return "";
}

LPCSTR CScriptGameObject::base_out_restrictions()
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		return (*monster->movement().restrictions().base_out_restrictions());
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CRestrictedObject : cannot access class member base_out_restrictions!");
	return "";
}

bool CScriptGameObject::accessible_position(const Fvector &position)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		return monster->movement().restrictions().accessible(position);
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CRestrictedObject : cannot access class member accessible!");
	return false;
}

bool CScriptGameObject::accessible_vertex_id(u32 level_vertex_id)
{
	if (!ai().level_graph().valid_vertex_id(level_vertex_id))
	{
		return false;
	}

	CCustomMonster* monster = object().cast_custom_monster();

	if (monster == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CRestrictedObject : cannot access class member accessible!");
		return false;
	}

	THROW2(ai().level_graph().valid_vertex_id(level_vertex_id),"Cannot check if level vertex id is accessible, because it is invalid");
	return monster->movement().restrictions().accessible(level_vertex_id);
}

u32 CScriptGameObject::accessible_nearest(const Fvector &position, Fvector &result)
{
	CCustomMonster* monster = object().cast_custom_monster();
	if (!monster)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CRestrictedObject : cannot access class member accessible!");
		return (u32(-1));
	}

	if (monster->movement().restrictions().accessible(position))
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CRestrictedObject : you use accessible_nearest when position is already accessible!");
		return (u32(-1));
	}

	return (monster->movement().restrictions().accessible_nearest(position,result));
}

void CScriptGameObject::enable_attachable_item(bool value)
{
	if (CAttachableItem* attachable_item = object().cast_attachable_item())
	{
		attachable_item->enable(value);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CAttachableItem : cannot access class member enable_attachable_item!");
	}
}

bool CScriptGameObject::attachable_item_enabled() const
{
	if (CAttachableItem* attachable_item = object().cast_attachable_item())
	{
		return attachable_item->enabled();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAttachableItem : cannot access class member attachable_item_enabled!");
	return false;
}

void CScriptGameObject::night_vision_allowed(bool value)
{
	if (CTorch* torch = object().cast_torch())
    {
		// TODO: St4lker0k765: Implement this?
		// No.
		//torch->SetNightVisionAllowed(value);
    }
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CTorch : cannot access class member enable_night_vision!");
	}
}

void CScriptGameObject::enable_night_vision(bool value)
{
	if (CActor* actor = object().cast_actor())
	{
		if (actor->GetNightVisionEffector())
		{
			actor->GetNightVisionEffector()->SwitchNightVision(value);
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member enable_night_vision!");
	}
}

bool CScriptGameObject::night_vision_enabled() const
{
	if (CActor* actor = object().cast_actor())
	{
		return actor->GetNightVisionEffector() && actor->GetNightVisionEffector()->GetStatus();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member enable_night_vision!");
	return false;
}

void CScriptGameObject::enable_torch(bool value)
{
	if (CTorch* torch = object().cast_torch())
	{
		torch->Switch(value);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CTorch : cannot access class member enable_torch!");
	}
}

bool CScriptGameObject::torch_enabled() const
{
	if (CTorch* torch = object().cast_torch())
	{
		return torch->torch_active();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CTorch : cannot access class member torch_enabled!");
	return false;
}

void CScriptGameObject::attachable_item_load_attach(LPCSTR section)
{
	CAttachableItem* attachable_item = object().cast_attachable_item();
	if (CAttachableItem* attachable_item = object().cast_attachable_item())
	{
		attachable_item->load_attach_position(section);

		if (CObject* parent = attachable_item->object().H_Parent())
		{
			if (CAttachmentOwner* AO = parent->cast_attachment_owner())
			{
				AO->reattach_items();
			}
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAttachableItem : cannot access class member attachable_item_load_attach!");
	}
}

void CScriptGameObject::RestoreWeapon()
{
#ifdef DEBUG
	ai().script_engine().script_log(eLuaMessageTypeMessage,"CScriptGameObject::RestoreWeapon called!!!");
	ai().script_engine().print_stack();
#endif //#ifdef DEBUG

	Actor()->SetWeaponHideState(INV_STATE_BLOCK_ALL, false);
}

void CScriptGameObject::HideWeapon()
{
#ifdef DEBUG
	ai().script_engine().script_log(eLuaMessageTypeMessage,"CScriptGameObject::HideWeapon called!!!");
	ai().script_engine().print_stack();
#endif //#ifdef DEBUG

	Actor()->SetWeaponHideState(INV_STATE_BLOCK_ALL, true);
}

void CScriptGameObject::HideDetector()
{
	CActor* pActor = object().cast_actor();

	if (pActor == nullptr)
	{
		return;
	}

	if (g_player_hud->attached_item(1) == nullptr)
	{
		return;
	}

	g_player_hud->detach_item(g_player_hud->attached_item(1)->m_parent_hud_item);
}

int CScriptGameObject::Weapon_GrenadeLauncher_Status()
{
	if (CWeapon* weapon = object().cast_weapon())
	{
		return (int)weapon->get_GrenadeLauncherStatus();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, make_string<const char*>("CWeapon [%s] : cannot access class member Weapon_GrenadeLauncher_Status!", object().cNameSect().c_str()));
	return false;
}

int CScriptGameObject::Weapon_Scope_Status()
{
	if (CWeapon* weapon = object().cast_weapon())
	{
		return (int)weapon->get_ScopeStatus();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, make_string<const char*>("CWeapon [%s] : cannot access class member Weapon_Scope_Status!", object().cNameSect().c_str()));
	return false;
}

int CScriptGameObject::Weapon_Silencer_Status()
{
	if (CWeapon* weapon = object().cast_weapon())
	{
		return (int)weapon->get_SilencerStatus();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, make_string<const char*>("CWeapon [%s] : cannot access class member Weapon_Silencer_Status!", object().cNameSect().c_str()));
	return false;
}

bool CScriptGameObject::Weapon_IsGrenadeLauncherAttached()
{
	if (CWeapon* weapon = object().cast_weapon())
	{
		return weapon->IsGrenadeLauncherAttached();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, make_string<const char*>("CWeapon [%s] : cannot access class member Weapon_IsGrenadeLauncherAttached!", object().cNameSect().c_str()));
	return false;
}

bool CScriptGameObject::Weapon_IsScopeAttached()
{
	if (CWeapon* weapon = object().cast_weapon())
	{
		return weapon->IsScopeAttached();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, make_string<const char*>("CWeapon [%s] : cannot access class member Weapon_IsScopeAttached!", object().cNameSect().c_str()));
	return false;
}

bool CScriptGameObject::Weapon_IsSilencerAttached()
{
	if (CWeapon* weapon = object().cast_weapon())
	{
		return weapon->IsSilencerAttached();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, make_string<const char*>("CWeapon [%s] : cannot access class member Weapon_IsSilencerAttached!", object().cNameSect().c_str()));
	return false;
}

void  CScriptGameObject::AllowSprint(bool b)
{
	Actor()->SetCantRunState(!b);
}

int	CScriptGameObject::animation_slot() const
{
	CHudItem* hud_item = object().cast_hud_item();
	if (CHudItem* hud_item = object().cast_hud_item())
	{
		return hud_item->animation_slot();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CHudItem : cannot access class member animation_slot!");
	return (u32(-1));
}

CScriptGameObject* CScriptGameObject::active_detector() const
{
	CInventoryOwner* inventory_owner = object().cast_inventory_owner();
	if (inventory_owner == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CInventoryOwner : cannot access class member active_detector!");
		return 0;
	}

	if (CInventoryItem* result = inventory_owner->inventory().ItemFromSlot(DETECTOR_SLOT))
	{
		CCustomDetector* detector = result->cast_custom_detector();
		VERIFY(detector);

		return detector->IsWorking() ? result->object().lua_game_object() : 0;
	}

	return 0;
}

CScriptGameObject* CScriptGameObject::item_in_slot(u32 slot_id) const
{
	if (CInventoryOwner* inventory_owner = object().cast_inventory_owner())
	{
		CInventoryItem* result = inventory_owner->inventory().ItemFromSlot((u16)slot_id);
		return result != nullptr ? result->object().lua_game_object() : 0;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryOwner : cannot access class member item_in_slot!");
	return 0;
}

void CScriptGameObject::GiveTaskToActor(CGameTask* t, u32 dt, bool bCheckExisting, u32 t_timer)
{
	Level().GameTaskManager()->GiveGameTaskToActor(t, dt, bCheckExisting, t_timer);
}

CGameTask* CScriptGameObject::GetTask(LPCSTR id, bool only_inprocess)
{
	return Level().GameTaskManager()->HasGameTask(id, only_inprocess);
}

void CScriptGameObject::SetActiveTask(CGameTask* t)
{
	VERIFY(t);
	Level().GameTaskManager()->SetActiveTask(t);
}

bool CScriptGameObject::IsActiveTask(CGameTask* t)
{
	VERIFY(t);

	const auto t1 = Level().GameTaskManager()->ActiveTask(eTaskTypeStoryline);
	const auto t2 = Level().GameTaskManager()->ActiveTask(eTaskTypeAdditional);
	const auto t3 = Level().GameTaskManager()->ActiveTask(eTaskTypeInsignificant);

	return t == t1 || t == t2 || t == t3;
}

u32	CScriptGameObject::active_slot()
{
	if (CInventoryOwner* inventory_owner = object().cast_inventory_owner())
	{
		return inventory_owner->inventory().GetActiveSlot();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryOwner : cannot access class member active_slot!");
	return 0;
}

void CScriptGameObject::activate_slot(u32 slot_id)
{
	if (CInventoryOwner* inventory_owner = object().cast_inventory_owner())
	{
		inventory_owner->inventory().Activate((u16)slot_id);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryOwner : cannot access class member activate_slot!");
	}
}

bool CScriptGameObject::IsBoosterInfluence(EBoostParams param)
{
	CActor* pActor = object().cast_actor();
	if (pActor == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member IsBoosterInfluence!");
		return false;
	}

	for (auto& booster : pActor->conditions().GetCurBoosterInfluences())
	{
		if (booster.second.m_type == param)
		{
			return booster.second.fBoostTime > 0.0f;
		}
	}

	return false;
}

float CScriptGameObject::GetBoosterInfluenceTime(EBoostParams param)
{
	if (CActor* pActor = object().cast_actor())
	{
		for (auto& booster : pActor->conditions().GetCurBoosterInfluences())
		{
			if (booster.second.m_type == param)
			{
				return booster.second.fBoostTime;
			}
		}
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member GetBoosterInfluenceTime!");
	return 0.0f;
}

void CScriptGameObject::ApplyBooster(LPCSTR sect)
{
	CActor* pActor = object().cast_actor();
	if (CActor* pActor = object().cast_actor())
	{
		for (u8 i = 0; i < (u8)eBoostMaxCount; i++)
		{
			if (pSettings->line_exist(sect, ef_boosters_section_names[i]))
			{
				SBooster B;
				B.Load(sect, (EBoostParams)i);
				pActor->conditions().ApplyBooster(B, sect);
			}
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member ApplyBooster!");
	}
}

void CScriptGameObject::SetBoosterTime(float time, EBoostParams param)
{
	if (CActor* pActor = object().cast_actor())
	{
		auto cur_boost = pActor->conditions().GetCurBoosterInfluences().find(param);
		if (cur_boost != pActor->conditions().GetCurBoosterInfluences().end())
		{
			if (cur_boost->second.fBoostTime > 0.0f)
			{
				cur_boost->second.fBoostTime = time;
			}
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member SetBoosterTime!");
	}
}

bool CScriptGameObject::GetActorMovementState(ACTOR_DEFS::EMovementStates state, ACTOR_DEFS::EMoveCommand mask)
{
	if (CActor* pActor = object().cast_actor())
	{
		return !!((pActor->GetMovementState(state) & mask) > 0);
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member GetActorMovementState!");
	return 0;
}

void CScriptGameObject::SetActorMovementState(ACTOR_DEFS::EMovementStates state, ACTOR_DEFS::EMoveCommand mask, bool status)
{
	if (CActor* pActor = object().cast_actor())
	{
		pActor->SetMovementState(state, mask, status);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member SetActorMovementState!");
	}
}

void CScriptGameObject::enable_movement(bool enable)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->movement().enable_movement(enable);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster : cannot access class member movement_enabled!");
	}
}

bool CScriptGameObject::movement_enabled()
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		return monster->movement().enabled();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster : cannot access class member movement_enabled!");
	return false;
}

bool CScriptGameObject::can_throw_grenades() const
{
	CAI_Stalker* stalker = object().cast_stalker();
	if (!stalker)
	{
		return stalker->can_throw_grenades();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member can_throw_grenades!");
	return false;
}

void CScriptGameObject::can_throw_grenades(bool can_throw_grenades)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->can_throw_grenades(can_throw_grenades);
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member can_throw_grenades!");
}

u32 CScriptGameObject::throw_time_interval() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->throw_time_interval();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member throw_time_interval!");
	return 0;
}

void CScriptGameObject::throw_time_interval(u32 throw_time_interval)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->throw_time_interval(throw_time_interval);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member throw_time_interval!");
	}
}

u32 CScriptGameObject::group_throw_time_interval() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->agent_manager().member().throw_time_interval();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member group_throw_time_interval!");
	return 0;
}

void CScriptGameObject::group_throw_time_interval(u32 throw_time_interval)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->agent_manager().member().throw_time_interval(throw_time_interval);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member group_throw_time_interval!");
	}
}

void CScriptGameObject::aim_time(CScriptGameObject *weapon, u32 aim_time)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		if (CWeapon* weapon_ = weapon->object().cast_weapon())
		{
			stalker->aim_time(*weapon_, aim_time);
		}
		else
		{
			ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member aim_time (not a weapon passed)!");
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member aim_time!");
	}
}

u32 CScriptGameObject::aim_time(CScriptGameObject *weapon)
{
	CAI_Stalker* stalker = object().cast_stalker();
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		if (CWeapon* weapon_ = weapon->object().cast_weapon())
		{
			return stalker->aim_time(*weapon_);
		}
		else
		{
			ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member aim_time (not a weapon passed)!");
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CAI_Stalker : cannot access class member aim_time!");
	}

	return (u32(-1));
}

void CScriptGameObject::special_danger_move(bool value)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->animation().special_danger_move(value);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member special_danger_move!");
	}
}

bool CScriptGameObject::special_danger_move()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->animation().special_danger_move();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member special_danger_move!");
	}

	return false;
}

void CScriptGameObject::sniper_update_rate(bool value)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->sniper_update_rate(value);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member sniper_update_rate!");
	}
}

bool CScriptGameObject::sniper_update_rate() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->sniper_update_rate();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member sniper_update_rate!");
	return false;
}

void CScriptGameObject::sniper_fire_mode(bool value)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->sniper_fire_mode(value);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member sniper_fire_mode!");
	}
}

bool CScriptGameObject::sniper_fire_mode() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->sniper_fire_mode();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member sniper_fire_mode!");
	return false;
}

void CScriptGameObject::aim_bone_id(LPCSTR bone_id)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->aim_bone_id(bone_id);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member aim_bone_id!");
	}
}

LPCSTR CScriptGameObject::aim_bone_id() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return *stalker->aim_bone_id();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member aim_bone_id!");
	return 0;
}

void CScriptGameObject::register_in_combat()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->agent_manager().member().register_in_combat(stalker);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member register_in_combat!");
	}
}

void CScriptGameObject::unregister_in_combat()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->agent_manager().member().unregister_in_combat(stalker);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member unregister_in_combat!");
	}
}

CCoverPoint const* CScriptGameObject::find_best_cover(Fvector position_to_cover_from)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->find_best_cover(position_to_cover_from);
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member find_best_cover!");
	return 0;
}

bool CScriptGameObject::suitable_smart_cover(CScriptGameObject* object)
{
	if (!object)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker::suitable_smart_cover null smart cover specified!");
		return false;
	}

	CAI_Stalker* stalker = this->object().cast_stalker();
	if (stalker == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member suitable_smart_cover!");
		return false;
	}

	smart_cover::object const* const smart_object = smart_cast<smart_cover::object const*>(&object->object());
	if (smart_object == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : suitable_smart_cover: passed non-smart_cover object!");
		return false;
	}

	smart_cover::cover const& cover = smart_object->cover();
	if (!cover.can_fire())
	{
		return true;
	}

	CInventoryItem const* inventory_item = stalker->inventory().ActiveItem();
	if (inventory_item != nullptr)
	{
		return inventory_item->BaseSlot() == INV_SLOT_3;
	}

	CInventoryItem const* best_weapon = stalker->best_weapon();
	if (best_weapon == nullptr)
	{
		return false;
	}

	return !!(best_weapon->BaseSlot() == INV_SLOT_3);
}

void CScriptGameObject::take_items_enabled(bool const value)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->take_items_enabled(value);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member take_items_enabled!");
	}
}

bool CScriptGameObject::take_items_enabled() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->take_items_enabled();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member take_items_enabled!");
	return false;
}

void CScriptGameObject::SetPlayShHdRldSounds(bool val)
{
	if (CInventoryOwner* owner = object().cast_inventory_owner())
	{
		owner->SetPlayShHdRldSounds(val);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryOwner : cannot access class member SetPlayShHdRldSounds!");
	}
}

void CScriptGameObject::death_sound_enabled(bool const value)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->death_sound_enabled(value);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member death_sound_enabled!");
	}
}

bool CScriptGameObject::death_sound_enabled() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->death_sound_enabled();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member death_sound_enabled!");
	return false;
}

void CScriptGameObject::register_door()
{
	VERIFY2(!m_door, make_string<const char*>("object %s has been registered as a door already", m_game_object->cName().c_str()));
	m_door = ai().doors().register_door(*smart_cast<CPhysicObject*>(m_game_object));
}

void CScriptGameObject::unregister_door()
{
	VERIFY2(m_door, make_string<const char*>("object %s is not a door", m_game_object->cName().c_str()));
	ai().doors().unregister_door(m_door);
	m_door = 0;
}

void CScriptGameObject::on_door_is_open()
{
	VERIFY2(m_door, make_string<const char*>("object %s hasn't been registered as a door already", m_game_object->cName().c_str()));
	ai().doors().on_door_is_open(m_door);
}

void CScriptGameObject::on_door_is_closed()
{
	VERIFY2(m_door, make_string<const char*>("object %s hasn't been registered as a door already", m_game_object->cName().c_str()));
	ai().doors().on_door_is_closed(m_door);
}

bool CScriptGameObject::is_door_locked_for_npc() const
{
	VERIFY2(m_door, make_string<const char*>("object %s hasn't been registered as a door already", m_game_object->cName().c_str()));
	return ai().doors().is_door_locked(m_door);
}

void CScriptGameObject::lock_door_for_npc()
{
	VERIFY2(m_door, make_string<const char*>("object %s hasn't been registered as a door already", m_game_object->cName().c_str()));
	ai().doors().lock_door(m_door);
}

void CScriptGameObject::unlock_door_for_npc()
{
	VERIFY2(m_door, make_string<const char*>("object %s hasn't been registered as a door already", m_game_object->cName().c_str()));
	ai().doors().unlock_door(m_door);
}

bool CScriptGameObject::is_door_blocked_by_npc() const
{
	VERIFY2(m_door, make_string<const char*>("object %s hasn't been registered as a door already", m_game_object->cName().c_str()));
	return ai().doors().is_door_blocked(m_door);
}

//Alundaio: Methods for exporting the ability to detach/attach addons for magazined weapons
void CScriptGameObject::Weapon_AddonAttach(CScriptGameObject* item)
{
	CWeaponMagazined* weapon = object().cast_weapon_magazined();
	if (!weapon)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CWeaponMagazined : cannot access class member Weapon_AddonAttach!");
		return;
	}

	CInventoryItem* pItm = item->object().cast_inventory_item();
	if (!pItm)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CWeaponMagazined : trying to attach non-CInventoryItem!");
		return;
	}

	if (weapon->CanAttach(pItm))
	{
		weapon->Attach(pItm, true);
	}
}

void CScriptGameObject::Weapon_AddonDetach(LPCSTR item_section, bool b_spawn_item = true)
{
	if (CWeaponMagazined* weapon = object().cast_weapon_magazined())
	{
		if (weapon->CanDetach(item_section))
		{
			weapon->Detach(item_section, b_spawn_item);
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CWeaponMagazined : cannot access class member Weapon_AddonDetach!");
	}
}

void CScriptGameObject::Weapon_SetCurrentScope(u8 type)
{
	CWeaponMagazined* weapon = object().cast_weapon_magazined();
	if (!weapon)
	{
		weapon->m_cur_scope = type;
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CWeaponMagazined : cannot access class member Weapon_SetCurrentScope!");
	}
}

u8 CScriptGameObject::Weapon_GetCurrentScope()
{
	CWeaponMagazined* weapon = object().cast_weapon_magazined();
	if (weapon == nullptr)
	{
		return weapon->m_cur_scope;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CWeaponMagazined : cannot access class member Weapon_GetCurrentScope!");
	return 255;
}

LPCSTR CScriptGameObject::Weapon_GetAmmoSection(u8 ammo_type)
{
	CWeaponMagazined* weapon = object().cast_weapon_magazined();
	if (weapon == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CWeaponMagazined : cannot access class member Weapon_GetAmmoSection!");
		return "";
	}

	if (weapon->m_ammoTypes.empty() || ammo_type + 1 > weapon->m_ammoTypes.size())
	{
		return "";
	}

	return weapon->m_ammoTypes[ammo_type].c_str();
}

void CScriptGameObject::IterateInstalledUpgrades(const luabind::functor<bool> &functor)
{
	if (CInventoryItem* Item = object().cast_inventory_item())
	{
		for (const shared_str& upgrade : Item->get_upgrades())
		{
			if (functor(*upgrade, object().lua_game_object()) == true)
			{
				return;
			}
		}
	}
}

CScriptGameObject *CScriptGameObject::ItemOnBelt(u32 item_id) const
{
	CInventoryOwner	*inventory_owner = object().cast_inventory_owner();
	if (!inventory_owner)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CInventoryOwner : cannot access class member item_on_belt!");
		return 0;
	}

	TIItemContainer& belt = inventory_owner->inventory().m_belt;
	if (belt.size() < item_id)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"item_on_belt: item id outside belt!");
		return 0;
	}

	CInventoryItem* result = belt.at(item_id);
	return result ? result->object().lua_game_object() : 0;
}

bool CScriptGameObject::IsOnBelt(CScriptGameObject *obj) const
{
	CInventoryOwner* inventory_owner = object().cast_inventory_owner();
	if (inventory_owner == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CInventoryOwner : cannot access class member is_on_belt!");
		return 0;
	}

	CInventoryItem* inventory_item = obj->object().cast_inventory_item();
	if (inventory_item == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryItem : cannot access class member is_on_belt!");
		return 0;
	}

	return inventory_owner->inventory().InBelt(inventory_item);
}

u32 CScriptGameObject::BeltSize() const
{
	if (CInventoryOwner* inventory_owner = object().cast_inventory_owner())
	{
		return inventory_owner->inventory().m_belt.size();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryOwner : cannot access class member move_to_belt!");
	return 0;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////
// получить и задать доп. вес для костюма

float CScriptGameObject::GetAdditionalMaxWeight() const
{
	if (CCustomOutfit* outfit = object().cast_outfit())
	{
		return outfit->m_additional_weight2;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomOutfit : cannot access class member GetAdditionalMaxWeight!");
	return 0.0f;
}

float CScriptGameObject::GetAdditionalMaxWalkWeight() const
{
	if (CCustomOutfit* outfit = object().cast_outfit())
	{
		return outfit->m_additional_weight;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomOutfit : cannot access class member GetAdditionalMaxWalkWeight!");
	return 0.0f;
}

void CScriptGameObject::SetAdditionalMaxWeight(float add_max_weight)
{
	if (CCustomOutfit* outfit = object().cast_outfit())
	{
		outfit->m_additional_weight2 = add_max_weight;
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomOutfit : cannot access class member SetAdditionalMaxWeight!");
	}
}

void CScriptGameObject::SetAdditionalMaxWalkWeight(float add_max_walk_weight)
{
	if (CCustomOutfit* outfit = object().cast_outfit())
	{
		outfit->m_additional_weight = add_max_walk_weight;
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomOutfit : cannot access class member SetAdditionalMaxWalkWeight!");
	}
}

////////////////////////////////////////////////////////////////////////////////////////////////////////
// получить суммарный вес инвентаря
float CScriptGameObject::GetTotalWeight() const
{
	if (CInventoryOwner* inventory_owner = object().cast_inventory_owner())
	{
		return inventory_owner->inventory().TotalWeight();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryOwner : cannot access class member GetTotalWeight!");
	return 0.0f;
}

// получить вес предмета
float CScriptGameObject::Weight() const
{
	if (CInventoryItem* inventory_item = object().cast_inventory_item())
	{
		return inventory_item->Weight();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSciptEntity : cannot access class member Weight!");
	return 0.0f;
}

void CScriptGameObject::SetWeight(float w)
{
	if (CInventoryItem* inventory_item = object().cast_inventory_item())
	{
		inventory_item->setWeight(w);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSciptEntity : cannot access class member SetWeight!");
	}
}

float CScriptGameObject::GetActorJumpSpeed() const
{
	if (CActor* pActor = object().cast_actor())
	{
		return pActor->m_fJumpSpeed;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member GetActorJumpSpeed!");
	return false;
}

void CScriptGameObject::SetActorJumpSpeed(float jump_speed)
{
	if (CActor* pActor = object().cast_actor())
	{
		pActor->m_fJumpSpeed = jump_speed;
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member SetActorJumpSpeed!");
	}
}

float CScriptGameObject::GetActorSprintKoef() const
{
	if (CActor* pActor = object().cast_actor())
	{
		return pActor->m_fSprintFactor;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member GetActorSprintKoef!");
	return 0.0f;
}

void CScriptGameObject::SetActorSprintKoef(float sprint_koef)
{
	if (CActor* pActor = object().cast_actor())
	{
		pActor->m_fSprintFactor = sprint_koef;
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member SetActorSprintKoef!");
	}
}

float CScriptGameObject::GetActorRunCoef() const
{
	if (CActor* pActor = object().cast_actor())
	{
		return pActor->m_fRunFactor;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member GetActorRunCoef!");
	return false;
}

void CScriptGameObject::SetActorRunCoef(float run_coef)
{
	if (CActor* pActor = object().cast_actor())
	{
		pActor->m_fRunFactor = run_coef;
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member SetActorRunCoef!");
	}
}

float CScriptGameObject::GetActorRunBackCoef() const
{
	if (CActor* pActor = object().cast_actor())
	{
		return pActor->m_fRunBackFactor;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member GetActorRunBackCoef!");
	return false;
}

void CScriptGameObject::SetActorRunBackCoef(float run_back_coef)
{
	if (CActor* pActor = object().cast_actor())
	{
		pActor->m_fRunBackFactor = run_back_coef;
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member SetActorRunBackCoef!");
	}
}

void CScriptGameObject::SetCharacterIcon(LPCSTR iconName)
{
	if (CInventoryOwner* pInventoryOwner = object().cast_inventory_owner())
	{
		pInventoryOwner->SetIcon(iconName);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "SetCharacterIcon available only for InventoryOwner");
	}
}

void CScriptGameObject::StartActorAnimator(LPCSTR section)
{
	if (CActor* pActor = object().cast_actor())
	{
		if (pActor->HudAnimator())
		{
			pActor->HudAnimator()->StartAnimator(section);
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member StartActorAnimator!");
	}
}

void CScriptGameObject::StopActorAnimator()
{
	if (CActor* pActor = object().cast_actor())
	{
		if (pActor->HudAnimator())
		{
			pActor->HudAnimator()->StopAnimator();
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member StopActorAnimator!");
	}
}

LPCSTR CScriptGameObject::GetActorAnimatorSection()
{
	if (CActor* pActor = object().cast_actor())
	{
		return pActor->HudAnimator() ? *pActor->HudAnimator()->GetSection() : "null";
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member GetActorAnimatorSection!");
	return "null";
}

bool CScriptGameObject::IsAnimatorActive()
{
	if (CActor* pActor = object().cast_actor())
	{
		return pActor->HudAnimator() && pActor->HudAnimator()->IsActive();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member IsAnimatorActive!");
	return false;
}

u8 CScriptGameObject::GetActorAnimatorRestoredSlot()
{
	if (CActor* pActor = object().cast_actor())
	{
		return pActor->HudAnimator() ? pActor->HudAnimator()->GetSlotToRestore() : 0;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member GetActorAnimatorRestoredSlot!");
	return 0;
}

bool CScriptGameObject::GetAnimatorForceHideItems()
{
	if (CActor* pActor = object().cast_actor())
	{
		return pActor->HudAnimator() && pActor->HudAnimator()->IsForceHideItems();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member GetAnimatorForceHideItems!");
	return false;
}

void CScriptGameObject::SetAnimatorForceHideItems(bool status)
{
	if (CActor* pActor = object().cast_actor())
	{
		if (pActor->HudAnimator())
		{
			pActor->HudAnimator()->SetForceHideItems(status);
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member SetAnimatorForceHideItems!");
	}
}

float CScriptGameObject::GetActorPowerBoostTime()
{
	if (CActor* pActor = object().cast_actor())
	{
		for (auto& booster : pActor->conditions().GetCurBoosterInfluences())
		{
			if (booster.second.m_type == EBoostParams::eBoostPowerRestore)
			{
				return booster.second.fBoostTime;
			}
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CActor : cannot access class member GetActorPowerBoostTime!");
	}

	return 0.0f;
}