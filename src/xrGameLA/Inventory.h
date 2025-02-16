#pragma once
#include "inventory_item.h"
#include "customdetector2.h"

class CInventory;
class CInventoryItem;
class CHudItem;
class CInventoryOwner;

class CInventorySlot
{									
public:
							CInventorySlot		();
	virtual					~CInventorySlot		();

	bool					CanBeActivated		() const;
	bool					IsBlocked			() const;

	PIItem					m_pIItem;
	bool					m_bPersistent;
	bool					m_bVisible;
	s8						m_blockCounter; 
};
enum EActivationReason{
	eGeneral,
	eKeyAction,
	eImportUpdate,
};

typedef xr_vector<CInventorySlot> TISlotArr;



class CInventory
{	
//gr1ph:
private:
	struct SBeltItemPred
	{
		private:
			ALife::_OBJECT_ID m_id;
		public:
			SBeltItemPred(ALife::_OBJECT_ID id) : m_id(id)	{ }
			IC	bool operator()	(PIItem &item);
	};

public:
							CInventory			();
	virtual					~CInventory			();

	float 					TotalWeight			() const;
	float 					CalcTotalWeight		();

	void					Take				(CGameObject *pObj, bool bNotActivate, bool strict_placement, bool duringSpawn);
	//void					repackAmmo			(PIItem pObj); //Im refactoring this, perviously it was called by take. ive moved repack calls to belt and ruck

	bool					DropItem			(CGameObject *pObj);
	void					Clear				();
	
	IC TSlotId				FirstSlot			() const {return KNIFE_SLOT;}
	IC TSlotId				LastSlot			() const {return LAST_SLOT;} // not "end"
	IC bool					SlotIsPersistent	(TSlotId slot_id) const {return m_slots[slot_id].m_bPersistent;}
	bool					Slot				(TSlotId slot, PIItem pIItem, bool bNotActivate = false);

	void					RepackBelt			(PIItem pIItem);
	bool					Belt				(PIItem pIItem);
	void					RepackRuck			(PIItem pIItem);
	bool					Ruck				(PIItem pIItem);

	bool 					InSlot				(PIItem pIItem) const;
	bool 					InBelt				(PIItem pIItem) const;
	bool 					InRuck				(PIItem pIItem) const;

	bool 					CanPutInSlot		(PIItem pIItem, TSlotId slot) const;
	bool 					CanPutInBelt		(PIItem pIItem, bool forceRoomCheck = true);
	bool 					CanPutInRuck		(PIItem pIItem) const;

	bool					CanTakeItem			(CInventoryItem *inventory_item) const;
	


	bool					Activate			(TSlotId slot, EActivationReason reason = eGeneral, bool bForce = false);
	bool					TryActivate			(TSlotId slot, EActivationReason reason, bool bForce);
	void					Activate_deffered	(TSlotId slot, u32 _frame);
	PIItem					ActiveItem			()const					{return m_iActiveSlot==NO_ACTIVE_SLOT ? nullptr :m_slots[m_iActiveSlot].m_pIItem;}
	PIItem					ItemFromSlot		(TSlotId slot) const;
	void					ActivateNextItemInActiveSlot();
	bool					Action				(u16 cmd, u32 flags);
	void					Update				();
	// Ищет на поясе аналогичный IItem
	PIItem					Same				(const PIItem pIItem, bool bSearchRuck) const;
	// Ищет на поясе IItem для указанного слота
	PIItem					SameSlot			(const TSlotId slot, PIItem pIItem, bool bSearchRuck) const;
	// Ищет на поясе или в рюкзаке IItem с указанным именем (cName())
	PIItem					Get					(const char *name, bool bSearchRuck) const;
	// Ищет на поясе или в рюкзаке IItem с указанным именем (id)
	PIItem					Get					(const u16  id,	 bool bSearchRuck) const;
	// Ищет на поясе или в рюкзаке IItem с указанным CLS_ID
	PIItem					Get					(CLASS_ID cls_id,  bool bSearchRuck) const;
	PIItem					GetAny				(const char *name) const;//search both (ruck and belt)
	// Ищет на поясе или в рюкзаке пачку патронов с указанным именем (cName())
	PIItem					GetAmmo				(const char *name, bool bSearchRuck) const;
	PIItem					item				(CLASS_ID cls_id) const;
	
	// get all the items with the same section name
	virtual u32				dwfGetSameItemCount	(LPCSTR caSection, bool SearchAll = false);	
	virtual u32				dwfGetGrenadeCount	(LPCSTR caSection, bool SearchAll);	
	// get all the items with the same object id
	virtual bool			bfCheckForObject	(ALife::_OBJECT_ID tObjectID);	
	PIItem					get_object_by_id	(ALife::_OBJECT_ID tObjectID);

	u32						dwfGetObjectCount	();
	PIItem					tpfGetObjectByIndex	(int iIndex);

	CInventoryItem	*		tpfGetBeltObjectByIndex(int iIndex);
	CInventoryItem	*		tpfGetBeltObjectById(int item_id);

	PIItem					GetItemFromInventory(LPCSTR caItemName);

	bool					Eat					(PIItem pIItem);								

	TSlotId					GetActiveSlot		() const			{return m_iActiveSlot;}
	
	void					SetPrevActiveSlot	(TSlotId ActiveSlot)	{m_iPrevActiveSlot = ActiveSlot;}
	TSlotId					GetPrevActiveSlot	() const			{return m_iPrevActiveSlot;}
	TSlotId					GetNextActiveSlot	() const			{return m_iNextActiveSlot;}

	void					SetActiveSlot		(TSlotId ActiveSlot)	{m_iActiveSlot = m_iNextActiveSlot = ActiveSlot; }

	bool 					IsSlotsUseful		() const			{return m_bSlotsUseful;}	 
	void 					SetSlotsUseful		(bool slots_useful) {m_bSlotsUseful = slots_useful;}
	bool 					IsBeltUseful		() const			{return m_bBeltUseful;}
	void 					SetBeltUseful		(bool belt_useful)	{m_bBeltUseful = belt_useful;}
	bool 					IsHandsOnly		() const			{return m_bHandsOnly;}	 
	void 					SetHandsOnly		(bool hands_only) {m_bHandsOnly = hands_only;}

	void					SetSlotsBlocked		(u16 mask, bool bBlock);
	bool					AreSlotsBlocked		();

	void					SetCurrentDetector	(CCustomDetectorR * detector) { m_currentDetectorInHand = detector;}; 	// Для детекторов. Использовал для возвращения детектора в руки гг после смены камеры
	CCustomDetectorR *		CurrentDetector		() const { return m_currentDetectorInHand; };
	// void					SetNeedToActivateWeapon(u32 slot)	{ m_needToActivateWeapon = slot; } //для правильной смены оружия
	// u32						NeedToActivateWeapon()const { return m_needToActivateWeapon; };


	TIItemContainer			m_all;
	TIItemContainer			m_ruck, m_belt;
	TISlotArr				m_slots;

	//возвращает все кроме PDA в слоте и болта
	void				AddAvailableItems			(TIItemContainer& items_container, bool for_trade) const;

	struct SInventorySelectorPredicate
	{
		virtual bool operator() (PIItem item) = 0;
	};

	void				AddAvailableItems(TIItemContainer& items_container, SInventorySelectorPredicate& pred) const;

	float				GetTakeDist					() const				{return m_fTakeDist;}
	void				SetTakeDist					(float dist)			{m_fTakeDist = dist;}
	
	float				GetMaxWeight				() const				{return m_fMaxWeight;}
	void				SetMaxWeight				(float weight)			{m_fMaxWeight = weight;}
	bool				CanBeDragged				()						{return (m_fTotalWeight<30.f);} // skyloader: dont use < m_fMaxWeight because maximum value is 1000.f for stalkers

	u32					BeltWidth					() const;

	inline	CInventoryOwner*GetOwner				() const				{ return m_pOwner; }
	

	// Объект на который наведен прицел
	PIItem				m_pTarget;

	friend class CInventoryOwner;


	u32					ModifyFrame					() const					{ return m_dwModifyFrame; }
	void				InvalidateState				()							{ m_dwModifyFrame = Device.dwFrame; }
	void				Items_SetCurrentEntityHud	(bool current_entity);
	bool				isBeautifulForActiveSlot	(CInventoryItem *pIItem);
protected:
	void					UpdateDropTasks		();
	void					UpdateDropItem		(PIItem pIItem);
	TSlotId				GetSlotByKey(u16 cmd);

	// Активный слот и слот который станет активным после смены
    //значения совпадают в обычном состоянии (нет смены слотов)
	TSlotId 			m_iActiveSlot;
	TSlotId 			m_iNextActiveSlot;
	TSlotId 			m_iPrevActiveSlot;
	TSlotId 			m_iLoadActiveSlot;
	u32 				m_iLoadActiveSlotFrame;
	EActivationReason	m_ActivationSlotReason;

	// Для детекторов. Использовал для возвращения детектора в руки гг после смены камеры
	CCustomDetectorR *	m_currentDetectorInHand;

	u32					m_needToActivateWeapon; //для правильной смены оружия

	CInventoryOwner*	m_pOwner;

	//флаг, показывающий наличие пояса в инвенторе
	bool				m_bBeltUseful;
	//флаг, допускающий использование слотов
	bool				m_bSlotsUseful;
	//if need to block all slots and inventory
	bool				m_bHandsOnly;

	// максимальный вес инвентаря
	float				m_fMaxWeight;
	// текущий вес в инвентаре
	float				m_fTotalWeight;

	// Максимальное кол-во объектов
	//на поясе
	u32					m_iMaxBelt;	
	// Максимальное расстояние на котором можно подобрать объект
	float				 m_fTakeDist;

	//кадр на котором произошло последнее изменение в инвенторе
	u32					m_dwModifyFrame;

	bool				m_drop_last_frame;

	void				SendActionEvent		(u16 cmd, u32 flags);
};