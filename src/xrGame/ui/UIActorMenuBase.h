#pragma once

#include "UIDragDropListEx.h"
#include "ui_drop_amount.h"
#include "../inventory_item.h"
#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "../../xrUI/Widgets/UIWindow.h"
#include "InventorySorter.h"
#include "../../xrUI/Widgets/UIFocusSystem.h"
#include "../InventoryOwner.h"

class CUIPropertiesBox;
class CUIActorMenu;
class CInventory;
class CInventoryBox;
class CTrade;
class CUIDragDropReferenceList;
class CUIProgressBar;
class CUIItemStateDisplay;
class CUIItemInfo;
class CUITabControl;
class CUIInventoryUpgradeWnd;

const u64 INVENTORY_ALL_CODE = 33;
const u64 INVENTORY_AMOUNT_CODE = 77;

enum eActorMenuControllerAuxMode 
{
	eAuxMode_None = 0,
	eAuxMode_Upgrade,
	eAuxMode_QuickSlot,
	eAuxMode_BeltSlot
};

enum EMenuMode 
{
	mmUndefined,
	mmInventory,
	mmTrade,
	mmUpgrade,
	mmDeadBodySearch,
};

enum EDDListType 
{
	iInvalid,
	iActorSlot,
	iActorBag,
	iActorBelt,

	iActorTrade,
	iPartnerTradeBag,
	iPartnerTrade,
	iDeadBodyBag,
	iQuickSlot,
	iTrashSlot,
	iListTypeMax
};

class CUIActorMenuBase : public CUIDialogWnd
{
private:
	typedef CUIDialogWnd inherited;
protected:
	enum eActorMenuSndAction{	eSndOpen	=0,
								eSndClose,
								eItemToSlot,
								eItemToBelt,
								eItemToRuck,
								eProperties,
								eDropItem,
								eAttachAddon,
								eDetachAddon,
								eItemUse,
								eUnloadMagazine,
								eSndMax};

	ref_sound					sounds						[eSndMax];
	void						InitBase					(CUIXml& xml);
	void						InitSlots					(CUIXml& xml);
	void						InitGamepadSelectors		();
	void						PlaySnd						(eActorMenuSndAction a);

								CUIActorMenuBase			();
	virtual						~CUIActorMenuBase			();
	void						PropertiesBoxForWeapon		(CUICellItem* cell_item, PIItem item, bool& b_show);
	void						PropertiesBoxForAddon		(PIItem item, bool& b_show);
	void						PropertiesBoxForUsing		(PIItem item, bool& b_show);
	void						PropertiesBoxForQuickSlots	(CUICellItem* cell_item, PIItem item, bool& b_show);
	void						PropertiesBoxForPlaying		(PIItem item, bool& b_show);
	void						PropertiesBoxForDrop		(CUICellItem* cell_item, PIItem item, bool& b_show);
	void						PropertiesBoxForSlots		(CUICellItem* cell_item, PIItem item, bool& b_show);
	void						PropertiesBoxForParse		(PIItem item, bool& b_show);
	void						PropertiesBoxForDonate		(PIItem item, bool& b_show); //Alundaio
	void						PropertiesBoxForTrade		(CUICellItem* cell_item, PIItem item, bool& b_show);
	void						PropertiesBoxForUpgrade		(PIItem item, bool& b_show);
	void						PropertiesBoxForRepair		(PIItem item, bool& b_show);
	bool						CanMoveToPartner			(PIItem pItem);
	bool						CanSetItemToList			(PIItem item, CUIDragDropListEx* l, u16& ret_slot);

	void						TryHidePropertiesBox		();
	float						CalcItemsWeight				(CUIDragDropListEx* pList);
	u32							CalcItemsPrice				(CUIDragDropListEx* pList, CTrade* pTrade, bool bBuying);
	bool						CanUpgradeItem				(PIItem item);
	void						ActivatePropertiesBox		();
	bool						IsAllowPlaceToInvBox		(CUICellItem* itm); // FFx0001
	bool						IsAllowTakeFromInvBox		(CUICellItem* itm); // FFx0001
	void						ColorizeItem				(CUICellItem* itm, bool colorize);

	CUIDragDropListEx*			GetListByType				(EDDListType t);

	void						AttachAddon					(PIItem item_to_upgrade);
	void						DetachAddon					(const char* addon_name, PIItem itm = nullptr);

	void						UnloadWeapon				(CWeaponMagazined* pWnp);
	void						UnloadWeaponItem			(CWeaponMagazined* pWpn);
	void						UnloadAllWeaponsFromRuck	();

	void						UpdateItemsPlace			();
	void						UpdateConditionProgressBars	();
	virtual void				TryRepairItem				(CUIWindow* w, void* d) {}
	virtual void				UpdateDeadBodyBag			() {}
	virtual void				SetAuxMode					(eActorMenuControllerAuxMode mode) {}
	CUIDragDropListEx*			GetSlotList					(u16 slot_idx);
	CUIDragDropListEx*			GetDisplayListForItem		(PIItem item, const SInvItemPlace& place);
	bool						IsSlotHiddenInUi			(u16 slot_idx) const;
	bool						ShouldDisplayGrenadeInBag	() const;
	CUIDragDropListEx*			GetSidearmDragDropList		() const;
	CUIDragDropListEx*			GetPrimaryDragDropList		() const;
	virtual	void				InfoCurItem					(CUICellItem* cell_item) {}
	void						ClearAllLists				();
	void						InvalidateTransientCellRefsForList(CUIDragDropListEx* list);
	void						InvalidateTransientCellRefsForCell(CUICellItem* cell);
	void						InvalidateAllTransientCellRefs();
	virtual void				InvalidateDerivedCellRefsForList(CUIDragDropListEx* list) {}
	virtual void				InvalidateDerivedCellRefsForCell(CUICellItem* cell) {}
	void						OnDragDropListContentReset	(CUIDragDropListEx* list);
	virtual	void				SetupUpgradeItem			() {}
	virtual void				TrySetCurUpgrade			() {}
	virtual void				UpdateOutfit				() {}
	virtual void				UpdateActor					() {}
	virtual void				TradeShowMessage			(s64 money_actor, s64 money_patner) {}
	void						InitPartnerInventoryContents();

	void						clear_highlight_lists		();
	void						set_highlight_item			(CUICellItem* cell_item);
	void						highlight_item_slot			(CUICellItem* cell_item);
	void						highlight_armament			(PIItem item, CUIDragDropListEx* ddlist);
	void						highlight_ammo_for_weapon	(PIItem weapon_item, CUIDragDropListEx* ddlist);
	void						highlight_weapons_for_ammo	(PIItem ammo_item, CUIDragDropListEx* ddlist);
	bool						highlight_addons_for_weapon	(PIItem weapon_item, CUICellItem* ci);
	void						highlight_weapons_for_addon	(PIItem addon_item, CUIDragDropListEx* ddlist);
	void						highlight_related_config_sections(PIItem item, CUIDragDropListEx* ddlist); // FFx001 ++
	void						highlight_antigas_for_filter(PIItem item, CUIDragDropListEx* ddlist); // FFx001 ++
	void						highlight_power_banks_for_power_cell(PIItem item, CUIDragDropListEx* ddlist); 
	void						highlight_power_manager_for_power_cell(PIItem item, CUIDragDropListEx* ddlist);

	void						InitInventoryContents		(CUIDragDropListEx* pBagList);
	void						InitCellForSlot				(u16 slot_idx);
	void						UpdateDeadBodyBagList		();
	bool						RemoveItemFromList			(CUIDragDropListEx* lst, PIItem pItem);

	EDDListType					GetListType					(CUIDragDropListEx* l);

	void						SendEvent_Item2Slot			(PIItem	pItem, u16 parent, u16 slot_id);
	void						SendEvent_Item2Belt			(PIItem	pItem, u16 parent);
	void						SendEvent_Item2Ruck			(PIItem	pItem, u16 parent);
	void						SendEvent_Item_Drop			(PIItem	pItem, u16 parent);
	void						SendEvent_Item_Eat			(PIItem	pItem, u16 parent);
	void						SendEvent_ActivateSlot		(u16 slot, u16 recipient);
	bool						OnItemDropped				(PIItem itm, CUIDragDropListEx* new_owner, CUIDragDropListEx* old_owner);

	bool						OnItemFocusedUpdate			(CUICellItem* itm);
	bool						OnItemFocusLost				(CUICellItem* itm);
	bool						OnItemStartDrag				(CUICellItem* itm);
	bool						OnItemRButtonClick			(CUICellItem* itm);
	bool						OnItemSelected				(CUICellItem* itm);
	bool						OnItemDrop					(CUICellItem* itm);
	bool						OnItemFocusReceive			(CUICellItem* itm);
	bool						OnItemDbClick				(CUICellItem* itm);

	bool						TryUseItem					(CUICellItem* cell_itm);
	bool						TryActiveSlot				(CUICellItem* itm);
	bool						TryHolsterPistolBagDbClick			(CUICellItem* itm);
	bool						TryHolsterPistolHolsterSlotDbClick	(CUICellItem* itm);
	bool						ToSlot						(CUICellItem* itm, bool force_place, u16 slot_id);
	bool						ToBag						(CUICellItem* itm, bool b_use_cursor_pos);
	bool						ToBelt						(CUICellItem* itm, bool b_use_cursor_pos);
	bool						ToQuickSlot					(CUICellItem* itm);
	bool						ToQuickSlotAt				(CUICellItem* itm, u8 slotIndex);
	void						MoveArtefactsToBag			();

	bool						ToActorTrade				(CUICellItem* itm, bool b_use_cursor_pos);
	bool						ToPartnerTrade				(CUICellItem* itm, bool b_use_cursor_pos);
	bool						ToPartnerTradeBag			(CUICellItem* itm, bool b_use_cursor_pos);
	bool						ToDeadBodyBag				(CUICellItem* itm, bool b_use_cursor_pos);

	void						DonateCurrentItem			(CUICellItem* cell_item); //Alundaio: Donate item via context menu while in trade menu
	
	void						BindDragDropListEvents		(CUIDragDropListEx* lst);

	void						TransferItems				(CUIDragDropListEx* pSellList, CUIDragDropListEx* pBuyList, CTrade* pTrade, bool bBuying);
	void						TransferItemsMp				(CUIDragDropListEx* pSellList, CUIDragDropListEx* pBuyList, CTrade* pTrade, bool bBuying);

	xr_vector<EDDListType>		m_allowed_drops				[iListTypeMax];
	bool						AllowItemDrops				(EDDListType from, EDDListType to);
	void						UpdateGamepadLegend			();

	enum ESortTabsLayoutSlot : u8
	{
		eSortTabsInventory = 0,
		eSortTabsUpgrade,
		eSortTabsTradeActor,
		eSortTabsTradePartner,
		eSortTabsDeadBody,
		eSortTabsLayoutCount
	};

	EInventorySortCategory		GetPlayerSortCategory		() const;
	EInventoryOrderMode			GetPlayerOrderMode			() const;
	ESortTabsLayoutSlot			GetActiveBagListSlot		() const;
	void						PrepareBagItemList			(TIItemContainer& items, ESortTabsLayoutSlot slot) const;
	bool						CycleActiveOrderOption		(ESortTabsLayoutSlot slot);
	void						UpdateOrderTabCaption		(ESortTabsLayoutSlot slot) const;
	void						ApplySortForSlot			(ESortTabsLayoutSlot sortSlot);
	virtual void UpdateActorBagList();
	virtual void				UpdateTradeActorBagList		();
	virtual void				UpdateTradePartnerBagList	();
	virtual bool				ShouldPutArtefactsToBag		() { return false; }

	void						OnPressUserKey				();

	// Controller UI
	bool						MoveAreaSelector			(eUIDirection4 dir);
	void						MoveSelector				(eUIDirection4 dir, bool bAllowAreaExit);
	void						SetAreaSelectionTo			(CUIWindow* pList);
	eUIDirection4				GetNaviDirection			(CUIWindow* pWndFrom, CUIWindow* pWndTo);
	void						UpdateSortTabsLayout		();
	void						ShowSortTabsForCurrentMode	();
	void						ApplySortTabCaptions		(CUITabControl* tabControl) const;
	void						ApplySortTabCaptions		(CUITabControl* tabControl, EInventorySortSystem system) const;
	CUITabControl*				GetActiveSortTabControl		() const;
	bool						ProcessSortTabKeyboardSwitch(int dik, EUIMessages keyboard_action);
	virtual bool				AnyInfoWindowOpen			() const { return false; }
			void				CheckSelectors				();

	u8							GetActiveSortSystemIndex	() const;
	CUITabControl*				GetSortTabControl			(ESortTabsLayoutSlot slot) const;
	CUITabControl*				GetSortTabControl			(EInventorySortSystem system, ESortTabsLayoutSlot slot) const;
	void						HideAllSortTabs			();
	void						SetInventorySortSystem		(EInventorySortSystem system);
	void						SetInventorySortSystemScript(LPCSTR system);
	LPCSTR						GetInventorySortSystemScript() const;

	EMenuMode					m_currMenuMode = mmUndefined;
	CUIItemDropAmountWnd*		m_pItemDropAmountWnd = nullptr;
	CUICellItem*				m_pCurrentCellItem = nullptr;

	bool						m_highlight_clear = true;
	bool						m_item_info_view = false;

	CUICellItem*				_tradeHoverCell = nullptr;
	CUIStatic*					m_pInvSlotHighlight[LAST_SLOT + 1]{};
	CUIProgressBar*				m_pInvSlotProgressLegacy[LAST_SLOT + 1]{};
	CUIItemStateDisplay*		m_pInvSlotProgressPercent[LAST_SLOT + 1]{};
	CUIDragDropListEx*			m_pInvList[LAST_SLOT + 1]{};
	CUIDragDropListEx*			m_pTrashList = nullptr;

	CUIItemInfo*				m_ItemInfo = nullptr;
	
	CUIStatic*					m_QuickSlotsHighlight[4]{};
	xr_vector<CUIStatic*>		m_ArtefactSlotsHighlight;

	int							m_ArtefactSlotsCount = 0;
	xr_vector<CUIStatic*>		m_belt_list_over;
	
	CInventorySorter*			m_pInventorySorter = nullptr;
	// [0] = categories, [1] = ordering
	CUITabControl*				m_sortTabControl[2][eSortTabsLayoutCount] = {};
	Fvector2					m_sortTabsLayoutPos[2][eSortTabsLayoutCount];
	Fvector2					m_sortTabsLayoutSize[2][eSortTabsLayoutCount];
	bool						m_sortTabsLayoutDefined[2][eSortTabsLayoutCount] = {};
	shared_str					m_sortCategoryId[eSortTabsLayoutCount];
	EInventorySortCategory		m_sortCategory[eSortTabsLayoutCount] = {};
	shared_str					m_orderModeId[eSortTabsLayoutCount];
	EInventoryOrderMode			m_orderMode[eSortTabsLayoutCount] = {};
	SInventoryOrderOptions		m_orderOptions[eSortTabsLayoutCount] = {};

	u32							m_trade_partner_inventory_state = 0;
	
	CUIInventoryUpgradeWnd*		m_pUpgradeWnd = nullptr;

	// Controller UI
	xr_map<EMenuMode, xr_vector<WND_SELECTOR_INFO>>	m_ui_navigation_lists;
	CUIWindow*					m_ui_navigation_selection = nullptr;
	CUIFrameWindow*				m_ui_navigation_selector = nullptr;
	bool						m_ui_navigation_selector_shown = false;
	bool						m_bShowInfoWnds = false;

	eActorMenuControllerAuxMode	m_AuxMode= eAuxMode_None;
	CUIFrameWindow*				m_ui_aux_selector = nullptr; // For upgrades, and picking item for a quickslot or belt
	bool						m_ui_aux_selector_shown = false;

	float						m_selectorPadding = 4.0f;

	const char* m_onCanMoveToPartner = {};
	bool m_isCanMoveToPartner = false;

	const char* m_onDonateCurrentItem = {};
	bool m_isDonateCurrentItem = false;

	const char* m_onInvBoxCanTakeItem = {};	// FFx0001
	bool m_isInvBoxCanTakeItem = false;		// FFx0001

	const char* m_onInvBoxCanPlaceItem = {};	// FFx0001
	bool m_isInvBoxCanPlaceItem = false;		// FFx0001

	const char* m_onCanTake = {};
	bool m_isCanTake = false;

	const char* m_onItemDropped = {};
	bool m_isItemDropped = false;

	PIItem m_lastFocusRecivedItem = nullptr;
	CUICellItem* m_cell_lastFocusRecivedItem = nullptr;
	u16 m_lastFocusLostItem_id = u16(0xffff);

	const char* m_onItemFocusLost = {};
	bool m_isItemFocusLost = false;

	const char* m_onItemFocusReceive = {};
	bool m_isItemFocusReceive = false;

	u32 m_cell_item_color_default = 0xFFFFFFFF;
	u32 m_cell_item_color_slot = 0xFFB4FFB4;
	u32 m_cell_item_color_unavaliable = 0xFFFF6464;

public:
	CUIDragDropReferenceList*	m_pQuickSlot = nullptr;
	EMenuMode					GetMenuMode					() {return m_currMenuMode;}
	CUIPropertiesBox*			m_UIPropertiesBox = nullptr;
			CInventory*			GetInventory				() { return GetInventoryOwner() ? &GetInventoryOwner()->inventory() : nullptr; }
	virtual CInventoryOwner*	GetInventoryOwner			() { return nullptr; }
	virtual CInventoryOwner*	GetPartner					() { return nullptr; }
	virtual CInventoryBox*		GetInvBox					() { return nullptr; }

	virtual CUIDragDropListEx*	GetActorList				() { return nullptr; }
	virtual CUIDragDropListEx*	GetPartnerList				() { return nullptr; }
	virtual CUIDragDropListEx*	GetBeltList					() { return nullptr; }
	virtual CUIDragDropListEx*	GetTradeActorList			() { return nullptr; }
	virtual CUIDragDropListEx*	GetTradeActorBagList		() { return nullptr; }
	virtual CUIDragDropListEx*	GetTradePartnerList			() { return nullptr; }
	virtual CUIDragDropListEx*	GetTradePartnerBagList		() { return nullptr; }

	virtual CTrade*				GetActorTrade				() { return nullptr; }
	virtual CTrade*				GetPartnerTrade				() { return nullptr; }

	void						DropAllCurrentItem			(u32 item_amount);
	void						MoveAllCurrentItem			(u32 item_amount);
	void						TakeAllCurrentItem			(u32 item_amount);
	void						ToBagAll					(u32 item_amount);
	void						ToActorTradeAll				(u32 item_amount);
	void						ToPartnerTradeAll			(u32 item_amount);
	void						ToPartnerTradeBagAll		(u32 item_amount);

	void						TakeAllFromPartner			(CUIWindow* w, void* d);
	void						PutAllToPartner				(CUIWindow* w, void* d);
	void						ProcessPropertiesBoxClicked	(CUIWindow* w, void* d);
	void						OnBtnPerformTrade			(CUIWindow* w, void* d);
	void						TakeAllFromInventoryBox		();

	CUICellItem*				CurrentItem					();
	PIItem						CurrentIItem				();
	virtual void				SetCurrentItem				(CUICellItem* itm) = 0;
	void						RefreshCurrentItemCell		();

	void						OnInventoryAction			(PIItem pItem, u16 action_type);
	virtual void				Update						();

	virtual bool				OnMouseAction				(float x, float y, EUIMessages mouse_action);
	virtual bool				OnKeyboardAction			(int dik, EUIMessages keyboard_action);
	virtual bool				OnKeyboardHold				(int dik);
	virtual bool				OnGamepadKeyAction			(int id, EUIMessages gamepad_action);
	virtual bool				OnGamepadKeyHold			(int id);
	virtual bool				StopAnyMove					();
	virtual void				UpdatePrices				() {}
	void						ReloadGamepadLegend			();
	CUIGamepadLegend*			m_gamepad_legend = nullptr;
	bool m_should_show_gamepad_legend = true;
};
