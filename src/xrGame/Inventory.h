#pragma once
#include "inventory_item.h"

class CInventoryItem;
class CInventoryOwner;
class CGrenade;

class CInventorySlot final
{
public:
	CInventorySlot() = default;
	~CInventorySlot() = default;

	bool CanBeActivated() const { return m_bAct; }

	PIItem m_pIItem = nullptr;
	bool m_bPersistent = false;
	bool m_bAct = true;
};

class priority_group final
{
public:
	priority_group() = default;
	~priority_group() = default;
	void init_group(shared_str const& game_section, shared_str const& line);
	bool is_item_in_group(shared_str const& section_name) const;
private:
	xr_set<shared_str> m_sections = {};
};

class CInventory final
{
	using TISlotArr = xr_map<u16, CInventorySlot>;

public:
	CInventory();
	virtual ~CInventory() = default;

	float TotalWeight() const;
	float CalcTotalWeight();

	void Take(CGameObject* pObj, bool bNotActivate, bool strict_placement);
	//if just_before_destroy is true, then activate will be forced (because deactivate message will not deliver)
	bool DropItem(CGameObject* pObj, bool just_before_destroy, bool dont_create_shell);
	void Clear();

	IC u16 FirstSlot() const { return KNIFE_SLOT; }
	IC u16 LastSlot() const { return m_last_slot; } // not "end"
	IC bool SlotIsPersistent(u16 slot_id) { return m_slots[slot_id].m_bPersistent; }
	bool Slot(u16 slot_id, PIItem pIItem, bool bNotActivate = false, bool strict_placement = false);
	bool Belt(PIItem pIItem, bool strict_placement = false);
	bool Ruck(PIItem pIItem, bool strict_placement = false);

	bool InSlot(const CInventoryItem* pIItem) const;
	bool InBelt(const CInventoryItem* pIItem) const;
	bool InRuck(const CInventoryItem* pIItem) const;

	bool CanPutInSlot(PIItem pIItem, u16 slot_id, bool bAllowReplacement = false) const;
	bool CanPutInBelt(PIItem pIItem);
	bool CanPutInRuck(PIItem pIItem) const;

	bool CanTakeItem(CInventoryItem* inventory_item) const;

	void Activate(u16 slot, bool bForce = false, bool ForceHide = false);
	void PutGrenade(CGrenade* new_grenade);

	static u32 const qs_priorities_count = 5;
	PIItem GetNextItemInActiveSlot(u8 const priority_value, bool ignore_ammo);
	bool ActivateNextItemInActiveSlot();
	priority_group& GetPriorityGroup(u8 const priority_value, u16 slot);
	void InitPriorityGroupsForQSwitch();

	PIItem ActiveItem() const { return (m_iActiveSlot == NO_ACTIVE_SLOT) ? nullptr : ItemFromSlot(m_iActiveSlot); }
	PIItem ItemFromSlot(u16 slot) const;

	bool Action(u16 cmd, u32 flags);
	void ActiveWeapon(u16 slot);
	void Update();

	// ищет на поясе аналогичный PIItem
	PIItem Same(const PIItem pIItem, bool bSearchRuck) const;

	// ищет на поясе PIItem дл€ указанного слота
	PIItem SameSlot(const u16 slot, PIItem pIItem, bool bSearchRuck) const;

	//ищет на поясе или в рюкзаке PIItem с указанным именем (cName())
	PIItem Get(const char* name, bool bSearchRuck) const;

	// ищет на поясе или в рюкзаке PIItem с указанным именем (id)
	PIItem Get(const u16  id, bool bSearchRuck) const;

	// ищет на поясе или в рюкзаке PIItem с указанным CLS_ID
	PIItem Get(CLASS_ID cls_id, bool bSearchRuck) const;
	PIItem GetAny(const char* name) const;//search both (ruck and belt)
	PIItem item(CLASS_ID cls_id) const;

	// get all the items with the same section name
	virtual u32 dwfGetSameItemCount(const char* caSection, bool SearchAll = false);
	virtual u32	dwfGetGrenadeCount(const char* caSection, bool SearchAll);
	// get all the items with the same object id
	virtual bool bfCheckForObject(ALife::_OBJECT_ID tObjectID);
	PIItem get_object_by_id(ALife::_OBJECT_ID tObjectID);

	u32	dwfGetObjectCount();
	PIItem tpfGetObjectByIndex(int iIndex);
	PIItem GetItemFromInventory(const char* caItemName);

	bool Eat(PIItem pIItem);
	bool ClientEat(PIItem pIItem);

	IC u16 GetActiveSlot() const { return m_iActiveSlot; }

	void SetPrevActiveSlot(u16 ActiveSlot) { m_iPrevActiveSlot = ActiveSlot; }
	u16	GetPrevActiveSlot() const { return m_iPrevActiveSlot; }
	IC u16 GetNextActiveSlot() const { return m_iNextActiveSlot; }

	void SetActiveSlot(u16 ActiveSlot) { m_iActiveSlot = m_iNextActiveSlot = ActiveSlot; }

	IC bool IsSlotsUseful() const { return m_bSlotsUseful; }
	void SetSlotsUseful(bool slots_useful) { m_bSlotsUseful = slots_useful; }
	IC bool IsBeltUseful() const { return m_bBeltUseful; }
	void SetBeltUseful(bool belt_useful) { m_bBeltUseful = belt_useful; }

	void SetSlotsBlocked(u16 mask, bool bBlock);

	void BlockSlot(u16 slot_id);
	void UnblockSlot(u16 slot_id);
	bool IsSlotBlocked(PIItem const iitem) const;

	TIItemContainer	m_all = {};
	TIItemContainer m_ruck = {}, m_belt = {};
	TIItemContainer m_activ_last_items = {};

	TISlotArr m_slots = {};
public:
	//возвращает все кроме PDA в слоте и болта
	void AddAvailableItems(TIItemContainer& items_container, bool for_trade) const;

	float GetMaxWeight() const { return m_fMaxWeight; }
	void SetMaxWeight(float weight) { m_fMaxWeight = weight; }

	u32 BeltWidth() const;

	inline CInventoryOwner* GetOwner() const { return m_pOwner; }

	friend class CInventoryOwner;
	friend class CCar;

	u32	ModifyFrame() const { return m_dwModifyFrame; }
	void InvalidateState() { m_dwModifyFrame = Device.dwFrame; }
	void Items_SetCurrentEntityHud(bool current_entity);
	bool isBeautifulForActiveSlot(CInventoryItem* pIItem);

	// Максимальное кол-во объектов на поясе
	u32 m_iMaxBelt = 0;

protected:
	void UpdateDropTasks();
	void UpdateDropItem(PIItem pIItem);

	// активный слот и слот который станет активным после смены
	// значения совпадают в обычном состоянии (нет смены слотов)
	u16 m_iActiveSlot = NO_ACTIVE_SLOT;
	u16 m_iNextActiveSlot = NO_ACTIVE_SLOT;
	u16 m_iPrevActiveSlot = NO_ACTIVE_SLOT;
	u16 m_last_slot = NO_ACTIVE_SLOT;

	CInventoryOwner* m_pOwner = nullptr;
	CGrenade* m_pNewGrenade = nullptr;

	//флаг, показывающий наличие пояса в инвентаре
	bool m_bBeltUseful = false;

	//флаг, допускающий использование слотов
	bool m_bSlotsUseful = true;

	// максимальный вес инвентаря
	float m_fMaxWeight = 0.0f;

	// текущий вес в инвентаре
	float m_fTotalWeight = 0.0f;

	//кадр на котором произошло последнее изменение в инвентаре
	u32 m_dwModifyFrame = 0;

	bool m_drop_last_frame = false;

	void SendActionEvent(u16 cmd, u32 flags);

private:
	priority_group* m_slot2_priorities[qs_priorities_count] = { nullptr };
	priority_group* m_slot3_priorities[qs_priorities_count] = { nullptr };

	priority_group m_groups[qs_priorities_count] = {};
	priority_group m_null_priority = {};
	using except_next_items_t = xr_set<PIItem>;
	except_next_items_t m_next_items_exceptions = {};
	u32 m_next_item_iteration_time = 0;

	U8Vec m_blocked_slots = {};

	bool IsSlotBlocked(u16 slot_id) const;
	void TryActivatePrevSlot();
	void TryDeactivateActiveSlot(bool Force = false);

	const char* m_onItemAvailableToTrade = {};
	bool m_isItemAvailableToTrade = false;

	const char* m_onInventoryEat = {};
	bool m_isInventoryEat = false;
};
