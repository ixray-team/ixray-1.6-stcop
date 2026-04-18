/////////////////////////////////////////////////////
// Для персонажей, имеющих инвентарь
// InventoryOwner.h
//////////////////////////////////////////////////////

#pragma once
#include "InfoPortionDefs.h"
#include "pda_space.h"
#include "attachment_owner.h"
#include "../xrScripts/script_space_forward.h"
#include "character_info.h"
#include "inventory_space.h"
#include "../xrScripts/script_export_space.h"

extern xr_string TranslateName(const char* nameStr);

class CSE_Abstract;
class CInventory;
class CInventoryItem;
class CTrade;
class CPda;
class CGameObject;
class CEntityAlive;
class CInfoPortionWrapper;
class NET_Packet;
class CCharacterInfo;
class CSpecificCharacter;
class CTradeParameters;
class CPurchaseList;
class CWeapon;
class CCustomOutfit;
class CHelmet;
class CActor;
class CAI_Stalker;
class CEntity;
class CBaseMonster;
class CCar;
class CAI_Trader;
class CPhraseDialogManager;
class CAI_PhraseDialogManager;

class CInventoryOwner : public CAttachmentOwner
{
public:
	CInventoryOwner();
	virtual	~CInventoryOwner();

public:
	virtual CInventoryOwner* cast_inventory_owner() { return this; }
	virtual CAttachmentOwner* cast_attachment_owner() override { return this; }
	virtual CActor* cast_actor() { return nullptr; }
	virtual CEntityAlive* cast_entity_alive() { return nullptr; }
	virtual CEntity* cast_entity() { return nullptr; }
	virtual CAI_Stalker* cast_stalker() { return nullptr; }
	virtual CGameObject* cast_game_object() override { return nullptr; }
	virtual CBaseMonster* cast_base_monster() { return nullptr; }
	virtual CCar* cast_car() { return nullptr; }
	virtual CAI_Trader* cast_trader() { return nullptr; }
	virtual CPhraseDialogManager* cast_phrase_dialog_manager() { return nullptr; }
	virtual CAI_PhraseDialogManager* cast_ai_phrase_dialog_manager() { return nullptr; }

public:

	virtual DLL_Pure* _construct();
	virtual bool net_Spawn(CSE_Abstract* DC);
	virtual void net_Destroy();
	void Init();
	virtual void Load(const char* section);
	virtual void reinit();
	virtual void reload(const char* section);
	virtual void OnEvent(NET_Packet& P, u16 type);

	//serialization
	virtual void save(NET_Packet& output_packet);
	virtual void load(IReader& input_packet);

	void RefreshNamesNPC();

	//обновление
	virtual void UpdateInventoryOwner(u32 deltaT);
	virtual bool CanPutInSlot(PIItem item, u32 slot) { return true; };


	CPda* GetPDA() const;

	void ChangeName(const char* name)
	{
		m_game_name_str = name;
		m_game_name = TranslateName(name);
	}

	// инвентарь
	CInventory* m_inventory = nullptr;

	////////////////////////////////////
	//торговля и общение с персонажем

	virtual bool AllowItemToTrade(CInventoryItem const* item, const SInvItemPlace& place) const;
	virtual void OnFollowerCmd(int cmd) {};//redefine for CAI_Stalkker
	bool bDisableBreakDialog = false;
	//инициализация объекта торговли
	CTrade* GetTrade();

	//для включения разговора
	virtual bool OfferTalk(CInventoryOwner* talk_partner);
	virtual void StartTalk(CInventoryOwner* talk_partner, bool start_trade = true);
	virtual void StopTalk();
	virtual bool IsTalking() const { return m_bTalking; }

	void StartTrading() { m_bTrading = true; }
	void StopTrading();
	bool IsTrading() const { return m_bTrading; }

	virtual void EnableTalk() { m_bAllowTalk = true; }
	virtual void DisableTalk() { m_bAllowTalk = false; }
	virtual bool IsTalkEnabled() { return m_bAllowTalk; }

	void EnableTrade() { m_bAllowTrade = true; }
	void DisableTrade() { m_bAllowTrade = false; }
	bool IsTradeEnabled() { return m_bAllowTrade; }

	void EnableInvUpgrade() { m_bAllowInvUpgrade = true; }
	void DisableInvUpgrade() { m_bAllowInvUpgrade = false; }
	bool IsInvUpgradeEnabled() { return m_bAllowInvUpgrade; }

	CInventoryOwner* GetTalkPartner() { return m_pTalkPartner; }
	virtual void NewPdaContact(CInventoryOwner*) {}
	virtual void LostPdaContact(CInventoryOwner*) {}

	//игровое имя 
	virtual const char* Name() const { return m_game_name.c_str(); }
	const char* NameReal() const { return m_game_name_str.c_str(); }
	void SetName(const char* name) { m_game_name = name; }
	const char* IconName() const { return CharacterInfo().IconName().c_str(); }
	u32 get_money() const { return m_money; }
	void set_money(u32 amount, bool bSendEvent);
	bool is_alive();

protected:
	u32 m_money = 0;
	// торговля
	CTrade* m_pTrade = nullptr;
	bool m_bTrading = false;
	bool m_bTalking = false;
	CInventoryOwner* m_pTalkPartner = nullptr;

	bool m_bAllowTalk = true;
	bool m_bAllowTrade = true;
	bool m_bAllowInvUpgrade;
	bool m_bHeadRotate = true;

	u16	m_tmp_active_slot_num = NO_ACTIVE_SLOT;

	bool m_play_show_hide_reload_sounds = true;
	//////////////////////////////////////////////////////////////////////////
	// сюжетная информация
public:
	//персонаж получил новую порцию информации
	virtual bool OnReceiveInfo(shared_str info_id) const;
	//убрать информацию
	virtual void OnDisableInfo(shared_str info_id) const;
	//передать/удалить информацию через сервер
	virtual void TransferInfo(shared_str info_id, bool add_info) const;
	//есть ли информация у персонажа
	virtual bool HasInfo(shared_str info_id) const;
	virtual bool GetInfo(shared_str info_id, INFO_DATA&) const;

#ifdef DEBUG
	void DumpInfo() const;
#endif

	CInfoPortionWrapper* m_known_info_registry = nullptr;

	//////////////////////////////////////////////////////////////////////////
	// инвентарь 
public:
	const CInventory& inventory() const { VERIFY(m_inventory); return(*m_inventory); }
	CInventory& inventory() { VERIFY(m_inventory); return(*m_inventory); }

	//возвращает текуший разброс стрельбы (в радианах) с учетом движения (в радианах)
	virtual float GetWeaponAccuracy() const { return 0.0f; }
	//максимальный переносимы вес
	virtual float MaxCarryWeight() const;

	CCustomOutfit* GetOutfit() const;
	CHelmet* GetHelmet() const;
	CBackpack* GetBackpack() const;

	bool CanPlayShHdRldSounds() const { return m_play_show_hide_reload_sounds; };
	void SetPlayShHdRldSounds(bool play) { m_play_show_hide_reload_sounds = play; };
	//////////////////////////////////////////////////////////////////////////
		//игровые характеристики персонажа
public:
	CCharacterInfo& CharacterInfo() const { VERIFY(m_pCharacterInfo); return *m_pCharacterInfo; }
	IC const CSpecificCharacter& SpecificCharacter() const { return CharacterInfo().m_SpecificCharacter; };
	bool InfinitiveMoney() { return CharacterInfo().m_SpecificCharacter.MoneyDef().inf_money; }

	//установка группировки на клиентском и серверном объкте
	virtual void			SetCommunity	(s32);
	virtual void			SetRank			(s32);
	virtual void			ChangeRank		(s32);
	virtual void			SetReputation	(s32);
	virtual void			ChangeReputation(s32);
			void			SetHeadRotate	(bool value) { m_bHeadRotate = value; }

	virtual void SetIcon(const shared_str& iconName, bool is_outfit_icon = false);

	//для работы с relation system
	u16								object_id	() const;
	s32		Community	() const {return CharacterInfo().Community().index();};
	s32			Rank		() const {return CharacterInfo().Rank().value();};
	s32		Reputation	() const {return CharacterInfo().Reputation().value();};
	float							Sympathy	() const {return CharacterInfo().Sympathy(); }

protected:
	CCharacterInfo* m_pCharacterInfo = nullptr;
	xr_string m_game_name;
	xr_string m_game_name_str;

public:
	virtual void renderable_Render();
	virtual void OnItemTake(CInventoryItem* inventory_item);

	virtual void OnItemBelt(CInventoryItem* inventory_item, const SInvItemPlace& previous_place);
	virtual void OnItemRuck(CInventoryItem* inventory_item, const SInvItemPlace& previous_place);
	virtual void OnItemSlot(CInventoryItem* inventory_item, const SInvItemPlace& previous_place);

	virtual void OnItemDrop(CInventoryItem* inventory_item, bool just_before_destroy);
	virtual void OnItemDropUpdate() {}
	virtual bool use_bolts() const { return(true); }
	virtual	void spawn_supplies();

protected:
	shared_str m_item_to_spawn;
	u32	m_ammo_in_box_to_spawn = 0;

public:
	IC const shared_str& item_to_spawn() const { return m_item_to_spawn; }
	IC const u32& ammo_in_box_to_spawn() const { return m_ammo_in_box_to_spawn; }

public:
	virtual bool unlimited_ammo() = 0;
	virtual bool infinite_fire() = 0;
	virtual	void on_weapon_shot_start(CWeapon* weapon) {}
	virtual	void on_weapon_shot_update() {}
	virtual	void on_weapon_shot_stop() {}
	virtual	void on_weapon_shot_remove(CWeapon* weapon) {}
	virtual	void on_weapon_hide(CWeapon* weapon) {}

public:
	virtual	bool use_simplified_visual() const { return (false); };

private:
	CTradeParameters* m_trade_parameters = nullptr;
	CPurchaseList* m_purchase_list = nullptr;
	bool m_need_osoznanie_mode = false;
	bool m_isFocusingOnNpc = true;
	bool m_deadbody_can_take = true;
	bool m_deadbody_closed = false;

public:
	IC CTradeParameters& trade_parameters() const
	{
		VERIFY(m_trade_parameters);
		return *m_trade_parameters;
	}

	virtual	const char* trade_section() const;
	float deficit_factor(const shared_str& section) const;
	void buy_supplies(CInifile& ini_file, const char* section);
	void sell_useless_items();
	virtual	void on_before_sell(CInventoryItem* item) {}
	virtual	void on_before_buy(CInventoryItem* item) {}
	virtual bool can_use_dynamic_lights() { return true; }
	virtual	bool use_default_throw_force() { return true; }
	virtual	float missile_throw_force() { return 0.0f; }
	virtual	bool use_throw_randomness() { return true; }
	virtual bool NeedOsoznanieMode() { return m_need_osoznanie_mode; }
	virtual bool GetFocusingOnNpc() { return m_isFocusingOnNpc; }

	void deadbody_can_take(bool status);
	IC bool	deadbody_can_take_status() const { return m_deadbody_can_take; }
	void deadbody_closed(bool status);
	IC bool	deadbody_closed_status() const { return m_deadbody_closed; }
	DECLARE_SCRIPT_REGISTER_FUNCTION
};