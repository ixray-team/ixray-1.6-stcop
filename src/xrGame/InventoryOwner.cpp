#include "StdAfx.h"
#include "pch_script.h"
#include "InventoryOwner.h"
#include "PDA.h"
#include "pda_communication.h"
#include "Actor.h"
#include "trade.h"
#include "Inventory.h"
#include "InventoryWeaponSlotLayout.h"
#include "character_info.h"
#include "script_game_object.h"
#include "../xrScripts/script_engine.h"
#include "AI_PhraseDialogManager.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "alife_registry_wrappers.h"
#include "relation_registry.h"
#include "ai_object_location.h"
#include "trade_parameters.h"
#include "purchase_list.h"
#include "alife_object_registry.h"
#include "ActorBackpack.h"
#include "map_manager.h"
#include "UIGameSP.h"
#include "ui/UITalkWnd.h"
#include "../xrScripts/script_callback_ex.h"
#include "ActorHelmet.h"
#include "antigas_filter.h"

CInventoryOwner::CInventoryOwner()
{
	m_inventory = new CInventory();
	m_pCharacterInfo = new CCharacterInfo();
	m_known_info_registry = new CInfoPortionWrapper();
}

DLL_Pure* CInventoryOwner::_construct()
{
	return smart_cast<DLL_Pure*>(this);
}

CInventoryOwner::~CInventoryOwner()
{
	xr_delete(m_inventory);
	xr_delete(m_pTrade);
	xr_delete(m_pCharacterInfo);
	xr_delete(m_known_info_registry);
	xr_delete(m_trade_parameters);
	xr_delete(m_purchase_list);
}

CHelmet* CScriptGameObject::cast_CHelmet()
{
	return object().cast_helmet();
}

AntigasFilter* CScriptGameObject::cast_AntigasFilter()
{
	if (AntigasFilter* oAntigasFilter = smart_cast<AntigasFilter*>(object().cast_inventory_item()))
	{
		return oAntigasFilter;
	}

	return nullptr;
}

void CInventoryOwner::Load(const char* section)
{
	if (pSettings->line_exist(section, "inv_max_weight"))
	{
		m_inventory->SetMaxWeight(pSettings->r_float(section, "inv_max_weight"));
	}

	m_isFocusingOnNpc = READ_IF_EXISTS(pSettings, r_bool, section, "focus_on_npc", true);
	m_need_osoznanie_mode = READ_IF_EXISTS(pSettings, r_bool, section, "need_osoznanie_mode", false);
}

void CInventoryOwner::reload(const char* section)
{
	inventory().Clear();
	inventory().m_pOwner = this;
	inventory().SetSlotsUseful(true);

	m_money = 0;
	m_bTrading = false;
	m_bTalking = false;
	m_pTalkPartner = nullptr;

	CAttachmentOwner::reload(section);
}

void CInventoryOwner::reinit()
{
	CAttachmentOwner::reinit();
	m_item_to_spawn = shared_str();
	m_ammo_in_box_to_spawn = 0;
}

//call this after CGameObject::net_Spawn
bool CInventoryOwner::net_Spawn(CSE_Abstract* DC)
{
	if (m_pTrade == nullptr)
	{
		m_pTrade = new CTrade(this);
	}

	if (m_trade_parameters)
	{
		xr_delete(m_trade_parameters);
	}

	m_trade_parameters = new CTradeParameters(trade_section());

	//получить указатель на объект, InventoryOwner
	//m_inventory->setSlotsBlocked(false);
	CGameObject* pThis = cast_game_object();
	if (pThis == nullptr)
	{
		return false;
	}

	CSE_Abstract* E = (CSE_Abstract*)(DC);

	if (IsGameTypeSingleCompatible() || !smart_cast<CSE_ALifeCreatureActor*>(E))
	{
		CSE_ALifeTraderAbstract* pTrader = nullptr;
		if (E) pTrader = smart_cast<CSE_ALifeTraderAbstract*>(E);
		if (!pTrader) return false;

		R_ASSERT(pTrader->character_profile().size());

		//синхронизируем параметры персонажа с серверным объектом
		CharacterInfo().Init(pTrader);

		//-------------------------------------
		m_known_info_registry->registry().init(E->ID);
		//-------------------------------------


		CAI_PhraseDialogManager* dialog_manager = cast_ai_phrase_dialog_manager();
		if (dialog_manager && !dialog_manager->GetStartDialog().size())
		{
			dialog_manager->SetStartDialog(CharacterInfo().StartDialog());
			dialog_manager->SetDefaultStartDialog(CharacterInfo().StartDialog());
		}

		m_game_name_str = pTrader->m_character_name_raw;
		m_game_name = pTrader->m_character_name;

		m_deadbody_can_take = pTrader->m_deadbody_can_take;
		m_deadbody_closed = pTrader->m_deadbody_closed;
	}
	else
	{
		CharacterInfo().m_SpecificCharacter.Load("mp_actor");
		CharacterInfo().InitSpecificCharacter("mp_actor");
		CharacterInfo().m_SpecificCharacter.data()->m_sGameName = (E->name_replace()[0]) ? E->name_replace() : *pThis->cName();
		m_game_name = (E->name_replace()[0]) ? E->name_replace() : *pThis->cName();
	}

	CharacterInfo().m_SpecificCharacter.updateMechanic(READ_IF_EXISTS(pSettings, r_bool, cast_game_object()->cNameSect(), "mechanic", SpecificCharacter().upgrade_mechanic()));

	if (!pThis->Local())
	{
		return true;
	}

	return true;
}

void CInventoryOwner::net_Destroy()
{
    if (PdaCommunication().GetSessionNpc() == this)
    {
        if (CUIGameCustom* ui = CurrentGameUI())
        {
            if (ui->TalkMenu)
            {
                ui->TalkMenu->StopPdaDialog();
            }
            else
            {
                PdaCommunication_Stop();
            }
        }
        else
        {
            PdaCommunication_Stop();
        }
    }

	CAttachmentOwner::net_Destroy();

	inventory().Clear();
	inventory().SetActiveSlot(NO_ACTIVE_SLOT);

	Level().MapManager().RemoveRelationLocation(this);
}

void CInventoryOwner::save(NET_Packet& output_packet)
{
	if (inventory().GetActiveSlot() == NO_ACTIVE_SLOT)
	{
		output_packet.w_u8((u8)NO_ACTIVE_SLOT);
	}
	else
	{
		output_packet.w_u8((u8)inventory().GetActiveSlot());
	}

	CharacterInfo().save(output_packet);
	save_data(m_game_name_str, output_packet);
	save_data(m_money, output_packet);
}

void CInventoryOwner::load(IReader& input_packet)
{
	u8 active_slot = input_packet.r_u8();
	if (active_slot == NO_ACTIVE_SLOT)
	{
		inventory().SetActiveSlot(NO_ACTIVE_SLOT);
	}

	m_tmp_active_slot_num = active_slot;

	CharacterInfo().load(input_packet);
	load_data(m_game_name_str, input_packet);
	load_data(m_money, input_packet);
	if (g_actor != nullptr && this->object_id() != Actor()->object_id())
	{
		m_game_name = TranslateName(m_game_name_str.c_str());
	}
}

void CInventoryOwner::UpdateInventoryOwner(u32 deltaT)
{
	PROF_EVENT("UpdateInvOwner");
	inventory().Update();

	if (m_pTrade)
	{
		m_pTrade->UpdateTrade();
	}

	if (IsTrading())
	{
		//если мы умерли, то нет "trade"
		if (!is_alive())
		{
			StopTrading();
		}
	}

	if (IsTalking())
	{
		//если наш собеседник перестал говорить с нами,
		//то и нам нечего ждать.
		if (!m_pTalkPartner->IsTalking())
		{
			StopTalk();
		}

		//если мы умерли, то тоже не говорить
		if (!is_alive())
		{
			StopTalk();
		}
	}
}

void CInventoryOwner::RefreshNamesNPC()
{
	m_game_name = TranslateName(m_game_name_str.c_str());
}

//достать PDA из специального слота инвентаря
CPda* CInventoryOwner::GetPDA() const
{
	return (CPda*)(m_inventory->ItemFromSlot(PDA_SLOT));
}

CTrade* CInventoryOwner::GetTrade()
{
	R_ASSERT2(m_pTrade, "trade for object does not init yet");
	return m_pTrade;
}

//состояние диалога

//нам предлагают поговорить,
//проверяем наше отношение 
//и если не враг начинаем разговор
bool CInventoryOwner::OfferTalk(CInventoryOwner* talk_partner)
{
	if (!IsTalkEnabled())
	{
		return false;
	}

	//проверить отношение к собеседнику
	CEntityAlive* pPartnerEntityAlive = talk_partner->cast_entity_alive();
	R_ASSERT(pPartnerEntityAlive);

	//	ALife::ERelationType relation = RELATION_REGISTRY().GetRelationType(this, talk_partner);
	//	if(relation == ALife::eRelationTypeEnemy) return false;

	if (!IsGameTypeSingle())
	{
		const ALife::ERelationType relation = RELATION_REGISTRY().GetRelationType(this, talk_partner);
		if (relation == ALife::eRelationTypeEnemy || relation == ALife::eRelationTypeWorstEnemy)
		{
			return false;
		}
	}

	if (!is_alive() || !pPartnerEntityAlive->g_Alive())
	{
		return false;
	}

	StartTalk(talk_partner);

	return true;
}

void CInventoryOwner::StartTalk(CInventoryOwner* talk_partner, bool start_trade)
{
	m_bTalking = true;
	m_pTalkPartner = talk_partner;

}

void CInventoryOwner::SetTalkPartner(CInventoryOwner* talk_partner)
{
	m_pTalkPartner = talk_partner;
}

void CInventoryOwner::SetTalking(bool talking)
{
	m_bTalking = talking;
}

void CInventoryOwner::StopTalk()
{
	m_pTalkPartner = nullptr;
	m_bTalking = false;

	if (CUIGameCustom* ui = CurrentGameUI())
	{
		if (ui->TalkMenu->IsActiveTalkUi())
		{
			ui->TalkMenu->Stop();
		}
	}
}

void CInventoryOwner::StopTrading()
{
	m_bTrading = false;

	if (CUIGameCustom* ui = CurrentGameUI())
	{
		ui->HideActorMenu();
	}
}

void CInventoryOwner::renderable_Render()
{
	PIItem active_item = inventory().ActiveItem();
	if (active_item != nullptr)
	{
		active_item->renderable_Render();
	}

	if (CEntityAlive* CurrEntity = cast_entity_alive(); CurrEntity == Actor())
	{
		PIItem rWeapon = inventory().ItemFromSlot(INV_SLOT_3);
		bool rValid = rWeapon != nullptr ? rWeapon->BaseSlot() == INV_SLOT_3 : false;
		if (rWeapon != nullptr && rValid && rWeapon != active_item)
		{
			rWeapon->renderable_Render();
		}

		PIItem lWeapon = inventory().ItemFromSlot(INV_SLOT_2);
		bool lValid = lWeapon != nullptr ? lWeapon->BaseSlot() == INV_SLOT_3 : false;
		if (lWeapon != nullptr && lValid && lWeapon != active_item)
		{
			lWeapon->renderable_Render();
		}

		PIItem lWeapon2 = inventory().ItemFromSlot(PISTOL_SLOT_NEW);
		bool lValid2 = lWeapon2 != nullptr ? IsSidearmPhysicalSlot(lWeapon2->BaseSlot()) : false;
		if (lWeapon2 != nullptr && lValid2 && lWeapon2 != active_item)
		{
			lWeapon2->renderable_Render();
		}
	}

	CAttachmentOwner::renderable_Render();
}

void CInventoryOwner::OnItemTake(CInventoryItem* inventory_item)
{
	CGameObject* object = cast_game_object();
	VERIFY(object);

	object->callback(GameObject::eOnItemTake)(inventory_item->object().lua_game_object(), inventory_item->m_last_dropped_owner_id);
	inventory_item->m_last_dropped_owner_id = 65535;
	inventory_item->ClearPreferredSlotAfterPickup();

	attach(inventory_item);

	if (m_tmp_active_slot_num != NO_ACTIVE_SLOT && inventory_item->CurrPlace() == eItemPlaceSlot && inventory_item->CurrSlot() == m_tmp_active_slot_num)
	{
		if (inventory().ItemFromSlot(m_tmp_active_slot_num))
		{
			inventory().Activate(m_tmp_active_slot_num);
			m_tmp_active_slot_num = NO_ACTIVE_SLOT;
		}
	}
}

//максимальный переносимы вес
float CInventoryOwner::MaxCarryWeight() const
{
	float ret = inventory().GetMaxWeight();

	const CCustomOutfit* outfit = GetOutfit();
	if (outfit)
	{
		ret += outfit->m_additional_weight2;
	}

	const CBackpack* backpack = GetBackpack();
	if (backpack)
	{
		ret += backpack->m_additional_weight2;
	}

	return ret;
}

void CInventoryOwner::spawn_supplies()
{
	if (cast_base_monster() != nullptr)
	{
		return;
	}

	CGameObject* game_object = cast_game_object();
	VERIFY(game_object);

	if (use_bolts())
	{
		Level().spawn_item(pGameGlobals->r_string("actor_item", "bolt_item"), game_object->Position(), game_object->ai_location().level_vertex_id(), game_object->ID());
	}

	if (ai().get_alife() == nullptr && IsGameTypeSingle())
	{
		CSE_Abstract* abstract = Level().spawn_item(pGameGlobals->r_string("actor_item", "pda_item"), game_object->Position(), game_object->ai_location().level_vertex_id(), game_object->ID(), true);
		CSE_ALifeItemPDA* pda = abstract->cast_item_pda();
		R_ASSERT(pda);

		pda->m_original_owner = (u16)game_object->ID();

		NET_Packet P;
		abstract->Spawn_Write(P, true);
		Level().Send(P, net_flags(true));
		F_entity_Destroy(abstract);
	}
}

//////////////////////////////////////////////////////////////////////////
//для работы с relation system
u16 CInventoryOwner::object_id() const
{
	CInventoryOwner* This = const_cast<CInventoryOwner*>(this);
	return This->cast_game_object()->ID();
}

//////////////////////////////////////////////////////////////////////////
//установка группировки на клиентском и серверном объкте

void CInventoryOwner::SetCommunity(s32 new_community)
{
	CEntityAlive* EA = cast_entity_alive();
	VERIFY(EA);

	CharacterInfo().SetCommunity(new_community);
	if (EA->g_Alive())
	{
		EA->ChangeTeam(CharacterInfo().Community().team(), EA->g_Squad(), EA->g_Group());
	}

	CSE_Abstract* e_entity = nullptr;
	if (IsGameTypeSingle())
	{
		e_entity = ai().alife().objects().object(EA->ID(), false);
	}
	else
	{
		e_entity = smart_cast<CSE_Abstract*>(Level().Objects.net_Find(EA->ID()));
	}

	if (e_entity == nullptr)
	{
		return;
	}

	CSE_ALifeTraderAbstract* trader = e_entity->cast_trader_abstract();
	if (trader == nullptr)
	{
		return;
	}

	trader->m_community_index = new_community;
}

void CInventoryOwner::SetRank			(s32 rank)
{
	CEntityAlive* EA = cast_entity_alive();
	VERIFY(EA);

	CSE_Abstract* e_entity = ai().alife().objects().object(EA->ID(), false);
	if (e_entity == nullptr)
	{
		return;
	}

	CSE_ALifeTraderAbstract* trader = e_entity->cast_trader_abstract();
	if (trader == nullptr)
	{
		return;
	}

	CharacterInfo().m_CurrentRank.set(rank);
	trader->m_rank = rank;
}

void CInventoryOwner::ChangeRank			(s32 delta)
{
	SetRank(Rank() + delta);
}

void CInventoryOwner::SetReputation		(s32 reputation)
{
	CEntityAlive* EA = cast_entity_alive();
	VERIFY(EA);

	CSE_Abstract* e_entity = ai().alife().objects().object(EA->ID(), false);
	if (e_entity == nullptr)
	{
		return;
	}

	CSE_ALifeTraderAbstract* trader = e_entity->cast_trader_abstract();
	if (trader == nullptr)
	{
		return;
	}

	CharacterInfo().m_CurrentReputation.set(reputation);
	trader->m_reputation = reputation;
}

void CInventoryOwner::ChangeReputation	(s32 delta)
{
	SetReputation(Reputation() + delta);
}

void CInventoryOwner::SetIcon(const shared_str& iconName, bool is_outfit_icon)
{
	if (!is_outfit_icon)
	{
		CharacterInfo().m_SpecificCharacter.data()->m_prev_icon_name = iconName;
	}

	const shared_str& prev = CharacterInfo().m_SpecificCharacter.data()->m_prev_icon_name;
	const shared_str& saved = CharacterInfo().m_SpecificCharacter.data()->m_saved_icon_name;
	const shared_str& cur = CharacterInfo().m_SpecificCharacter.data()->m_icon_name;

	if (CCustomOutfit* outfit = GetOutfit())
	{
		if (cur == outfit->GetPortrait())
		{
			return;
		}
	}

	CharacterInfo().m_SpecificCharacter.data()->m_icon_name = iconName.size() > 0 ? iconName : prev.size() > 0 ? prev : saved;
}

void CInventoryOwner::OnItemDrop(CInventoryItem* inventory_item, bool just_before_destroy)
{
	if (CNVG* nvg = smart_cast<CNVG*>(inventory_item))
	{
		nvg->OnItemDrop();
	}

	CGameObject* object = cast_game_object();
	VERIFY(object);
	object->callback(GameObject::eOnItemDrop)(inventory_item->object().lua_game_object());

	detach(inventory_item);
}

void CInventoryOwner::OnItemBelt(CInventoryItem* inventory_item, const SInvItemPlace& previous_place)
{
	CGameObject* object = cast_game_object();
	VERIFY(object);
	object->callback(GameObject::eItemToBelt)(inventory_item->object().lua_game_object());
}

void CInventoryOwner::OnItemRuck(CInventoryItem* inventory_item, const SInvItemPlace& previous_place)
{
	if (CNVG* nvg = smart_cast<CNVG*>(inventory_item))
	{
		nvg->OnItemRuck();
	}

	CGameObject* object = cast_game_object();
	VERIFY(object);
	object->callback(GameObject::eItemToRuck)(inventory_item->object().lua_game_object());

	detach(inventory_item);
}

void CInventoryOwner::OnItemSlot(CInventoryItem* inventory_item, const SInvItemPlace& previous_place)
{
	if (CNVG* nvg = smart_cast<CNVG*>(inventory_item))
	{
		nvg->OnItemToSlot();
	}

	CGameObject* object = cast_game_object();
	VERIFY(object);
	object->callback(GameObject::eItemToSlot)(inventory_item->object().lua_game_object());

	attach(inventory_item);
}

CCustomOutfit* CInventoryOwner::GetOutfit() const
{
	PIItem item_from_slot = inventory().ItemFromSlot(OUTFIT_SLOT);
	return item_from_slot != nullptr ? item_from_slot->cast_outfit() : nullptr;
}

CHelmet* CInventoryOwner::GetHelmet() const
{
	PIItem item_from_slot = inventory().ItemFromSlot(HELMET_SLOT);
	return item_from_slot != nullptr ? item_from_slot->cast_helmet() : nullptr;
}

CBackpack* CInventoryOwner::GetBackpack() const
{
	PIItem item_from_slot = inventory().ItemFromSlot(BACKPACK_SLOT);
	return item_from_slot != nullptr ? item_from_slot->cast_backpack() : nullptr;
}


const char* CInventoryOwner::trade_section() const
{
	const CGameObject* game_object = smart_cast<const CGameObject*>(this);
	VERIFY(game_object);
	return READ_IF_EXISTS(pSettings, r_string, game_object->cNameSect(), "trade_section", "trade");
}

float CInventoryOwner::deficit_factor(const shared_str& section) const
{
	if (m_purchase_list == nullptr)
	{
		return 1.0f;
	}

	return m_purchase_list->deficit(section);
}

void CInventoryOwner::buy_supplies(CInifile& ini_file, const char* section)
{
	if (m_purchase_list == nullptr)
	{
		m_purchase_list = new CPurchaseList();
	}

	m_purchase_list->process(ini_file, section, *this);
}

void CInventoryOwner::sell_useless_items()
{
	CGameObject* object = cast_game_object();

	for (PIItem item : inventory().m_all)
	{
		if (item->cast_bolt())
		{
			continue;
		}

		if (item->CurrSlot() && item->CurrPlace() == eItemPlaceSlot && item->cast_weapon())
			continue;

		if (CPda* pda = item->cast_pda())
		{
			if (pda->GetOriginalOwnerID() == object->ID())
			{
				continue;
			}
		}

		item->SetDropManual(false);
		item->object().DestroyObject();
	}
}

bool CInventoryOwner::AllowItemToTrade(CInventoryItem const* item, const SInvItemPlace& place) const
{
	return (trade_parameters().enabled(CTradeParameters::action_sell(0), item->object().cNameSect()));
}

void CInventoryOwner::set_money(u32 amount, bool bSendEvent)
{
	const u32 previousMoney = m_money;

	if (InfinitiveMoney())
	{
		m_money = std::max(m_money, amount);
	}
	else
	{
		m_money = amount;
	}

	if (CActor* actor = cast_actor())
	{
		actor->OnMoneyChanged(previousMoney, m_money);
	}

	if (bSendEvent)
	{
		CGameObject* object = cast_game_object();
		NET_Packet packet;
		object->u_EventGen(packet, GE_MONEY, object->ID());
		packet.w_u32(m_money);
		object->u_EventSend(packet);
	}
}

bool CInventoryOwner::is_alive()
{
	CEntityAlive* pEntityAlive = cast_entity_alive();
	R_ASSERT(pEntityAlive);

	return (!!pEntityAlive->g_Alive());
}

void CInventoryOwner::deadbody_can_take(bool status)
{
	if (is_alive())
	{
		return;
	}

	m_deadbody_can_take = status;

	NET_Packet P;
	CGameObject::u_EventGen(P, GE_INV_OWNER_STATUS, object_id());
	P.w_u8((m_deadbody_can_take) ? 1 : 0);
	P.w_u8((m_deadbody_closed) ? 1 : 0);
	CGameObject::u_EventSend(P);
}

void CInventoryOwner::deadbody_closed(bool status)
{
	if (is_alive())
	{
		return;
	}

	m_deadbody_closed = status;

	NET_Packet P;
	CGameObject::u_EventGen(P, GE_INV_OWNER_STATUS, object_id());
	P.w_u8((m_deadbody_can_take) ? 1 : 0);
	P.w_u8((m_deadbody_closed) ? 1 : 0);
	CGameObject::u_EventSend(P);
}