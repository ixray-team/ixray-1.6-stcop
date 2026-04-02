#pragma once

#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "../../xrUI/Widgets/UIWndCallback.h"
#include "../../xrServerEntities/inventory_space.h"
#include "../../xrUI/Widgets/UIHint.h"
#include "../../xrUI/Widgets/UITabControl.h"
#include "../../xrUI/ui_defs.h"
#include "../../xrUI/Widgets/UIFocusSystem.h"
#include "UIActorMenuBase.h"

#include "../script_game_object.h" //Alundaio
#include <WeaponMagazined.h>

class CUICharacterInfo;
class CUIDragDropListEx;
class CUIDragDropReferenceList;
class CUICellItem;
class CUIDragItem;
class ui_actor_state_wnd;
class CUIFrameLineWnd;
class CUIStatic;
class CUI3tButton;
class CInventoryOwner;
class CInventoryBox;
class CUIInventoryUpgradeWnd;
class UIInvUpgradeInfo;
class CUIMessageBoxEx;
class CUIPropertiesBox;
class CTrade;
class CUIProgressBar;
class CUIItemDropAmountWnd;
class CUIGamepadLegend;

namespace inventory { namespace upgrade {
	class Upgrade;
} } // namespace upgrade, inventory

class CUIActorMenu final : 
						public CUIWndCallback,
						public CUIActorMenuBase
{
	typedef CUIActorMenuBase inherited;
	typedef inventory::upgrade::Upgrade 	Upgrade_type;

protected:
	UIHint*						m_hint_wnd;
	CUICellItem*				m_InfoCellItem;
	u32							m_InfoCellItem_timer;
	CUICellItem*				m_upgrade_selected;

	ui_actor_state_wnd*			m_ActorStateInfo;
	CUICharacterInfo*			m_ActorCharacterInfo;
	CUICharacterInfo*			m_PartnerCharacterInfo;

	CUIDragDropListEx*			m_pInventoryBeltList;
	CUIDragDropListEx*			m_pInventoryBagList;

	CUIDragDropListEx*			m_pTradeActorBagList;
	CUIDragDropListEx*			m_pTradeActorList;
	CUIDragDropListEx*			m_pTradePartnerBagList;
	CUIDragDropListEx*			m_pTradePartnerList;
	CUIDragDropListEx*			m_pDeadBodyBagList;

	CUIStatic*					m_HelmetOver;

	CUIInventoryUpgradeWnd*		m_pUpgradeWnd = nullptr;
	
	CUIStatic*					m_LeftBackground = nullptr;

	UIInvUpgradeInfo*			m_upgrade_info = nullptr;
	CUIMessageBoxEx*			m_message_box_yes_no = nullptr;
	CUIMessageBoxEx*			m_message_box_ok = nullptr;

	CInventoryOwner*			m_pActorInvOwner = nullptr;
	CInventoryOwner*			m_pPartnerInvOwner = nullptr;
	CInventoryBox*				m_pInvBox = nullptr;

	CUIStatic*					m_ActorMoney = nullptr;
	CUIStatic*					m_PartnerMoney = nullptr;
	CUIStatic*					m_QuickSlot1= nullptr;
	CUIStatic*					m_QuickSlot2= nullptr;
	CUIStatic*					m_QuickSlot3= nullptr;
	CUIStatic*					m_QuickSlot4= nullptr;

	// bottom ---------------------------------
	CUIStatic*					m_ActorBottomInfo = nullptr;
	CUIStatic*					m_ActorWeight = nullptr;
	CUIStatic*					m_ActorWeightMax = nullptr;
	
	CUIStatic*					m_PartnerBottomInfo = nullptr;
	CUIStatic*					m_PartnerWeight = nullptr;
	float						m_PartnerWeight_end_x;
//*	CUIStatic*					m_PartnerWeightMax;

	// delimiter ------------------------------
	CUIStatic*					m_LeftDelimiter = nullptr;
	CUIStatic*					m_PartnerTradeCaption = nullptr;
	CUIStatic*					m_PartnerTradePrice = nullptr;
	CUIStatic*					m_PartnerTradeWeightMax = nullptr;

	CUIStatic*					m_RightDelimiter = nullptr;
	CUIStatic*					m_ActorTradeCaption = nullptr;
	CUIStatic*					m_ActorTradePrice = nullptr;
	CUIStatic*					m_ActorTradeWeightMax = nullptr;

	CTrade*						m_actor_trade = nullptr;
	CTrade*						m_partner_trade = nullptr;

	CUI3tButton*				m_trade_button = nullptr;
	CUI3tButton*				m_trade_buy_button = nullptr;
	CUI3tButton*				m_trade_sell_button = nullptr;
	CUI3tButton*				m_takeall_button = nullptr;
	CUI3tButton*				m_putall_button = nullptr;
	CUI3tButton*				m_exit_button = nullptr;
	CUIStatic*					m_clock_value = nullptr;

	u32							m_last_time;
	u8							m_repair_mode;
	
	// Controller UI
	xr_map<EMenuMode, xr_vector<WND_SELECTOR_INFO>>	m_ui_navigation_lists;
	CUIWindow*					m_ui_navigation_selection;
	CUIFrameWindow*				m_ui_navigation_selector = nullptr;
	bool						m_ui_navigation_selector_shown = false;
	bool						m_bShowInfoWnds = false;

	eActorMenuControllerAuxMode	m_AuxMode= eAuxMode_None;
	CUIFrameWindow*				m_ui_aux_selector = nullptr; // For upgrades, and picking item for a quickslot or belt
	bool						m_ui_aux_selector_shown = false;

	float						m_selectorPadding = 4.0f;

	CUIGamepadLegend*			m_gamepad_legend = nullptr;
private:
	const char* m_onCanMoveToPartner = {};
	bool m_isCanMoveToPartner = false;

	const char* m_onItemFocusReceive = {};
	bool m_isItemFocusReceive = false;

	const char* m_onItemFocusLost = {};
	bool m_isItemFocusLost = false;

	const char* m_onCanDisassembleItem = {};
	bool m_isCanDisassembleItem = false;

	const char* m_onQuestionDisassembleItem = {};
	bool m_isQuestionDisassembleItem = false;

	const char* m_onEffectDisassemble = {};
	bool m_isEffectDisassemble = false;
public:
	void						SetMenuMode							(EMenuMode mode);
	void						SetActor							(CInventoryOwner* io);
	void						ReloadActorInfo						();
	void						SetPartner							(CInventoryOwner* io);
	void						SetInvBox							(CInventoryBox* box);
	virtual CInventoryBox*		GetInvBox							() {return m_pInvBox;}

	virtual CUIWindow*			ui_cast_window						() { return this; }

	virtual CUIDragDropListEx*	GetActorList						() { return m_pInventoryBagList; }
	virtual CUIDragDropListEx*	GetPartnerList						() { return m_pDeadBodyBagList; }
	virtual CUIDragDropListEx*	GetBeltList							() { return m_pInventoryBeltList; }
	virtual CUIDragDropListEx*	GetTradeActorList					() { return m_pTradeActorList; }
	virtual CUIDragDropListEx*	GetTradeActorBagList				() { return m_pTradeActorBagList; }
	virtual CUIDragDropListEx*	GetTradePartnerList					() { return m_pTradePartnerList; }
	virtual CUIDragDropListEx*	GetTradePartnerBagList				() { return m_pTradePartnerBagList; }

	virtual CTrade*				GetActorTrade						() { return m_actor_trade; }
	virtual CTrade*				GetPartnerTrade						() { return m_partner_trade; }

protected:			
	void						Construct							();
	void						InitCallbacks						();

	void						UpdateActorBagList					();
	void						UpdateTradeActorBagList				();
	void						UpdateTradePartnerBagList			();
	void						UpdateSortTabsLayout				();
	void						ShowSortTabsForCurrentMode			();
	ESortTabsLayoutSlot			GetSortTabsSlotByWindow				(CUIWindow* window) const;
	void						ApplySortForSlot					(ESortTabsLayoutSlot sortSlot);
	void						OnSortTabChanged					(CUIWindow* w, void* pData);
	
	void						OnDragItemOnTrash					(CUIDragItem* item, bool b_receive);

	void						ResetMode							();
	void						InitInventoryMode					();
	void						DeInitInventoryMode					();
	void						InitTradeMode						();
	void						DeInitTradeMode						();
	void						InitUpgradeMode						();
	void						DeInitUpgradeMode					();
	void						InitDeadBodySearchMode				();
	void						DeInitDeadBodySearchMode			();

	void						CurModeToScript						();
	void						RepairEffect_CurItem				();
	void						PerformDisassemble					();

	virtual	void				InfoCurItem							(CUICellItem* cell_item); //on update item
	
	void						CheckDistance						();

	virtual void				SetupUpgradeItem					();
	void						UpdateUpgradeItem					();
	void						TrySetCurUpgrade					();
	void						UpdateButtonsLayout					();

	// inventory
	bool						ToSlotScript						(CScriptGameObject* GO, bool force_place, u16 slot_id);
	bool						ToBeltScript						(CScriptGameObject* GO, bool b_use_cursor_pos);

	void						SetActorInfoMP();
	void						UpdateActorMoneyMP();
	virtual void				UpdateOutfit						();
	virtual void				TryRepairItem						(CUIWindow* w, void* d);
	void						TryDisassembleItem					(CUIWindow* w, void* d);
	
	void						OnPressUserKey						();

	// trade
	virtual void				UpdatePrices						();

	// Controller UI
	bool						MoveAreaSelector					(eUIDirection4 dir);
	void						MoveSelector						(eUIDirection4 dir, bool bAllowAreaExit);
	void						SetAreaSelectionTo					(CUIWindow* pList);
	virtual void				SetAuxMode							(eActorMenuControllerAuxMode mode);
	eUIDirection4				GetNaviDirection					(CUIWindow* pWndFrom, CUIWindow* pWndTo);

public:
								CUIActorMenu						();
	virtual						~CUIActorMenu						();

	virtual CInventory*			GetInventory						();
	virtual CInventoryOwner*	GetInventoryOwner					() { return m_pActorInvOwner; }
	virtual CInventoryOwner*	GetPartner							() { return m_pPartnerInvOwner; }
	virtual bool				ShouldPutArtefactsToBag				() { return true; }
	virtual void				SetCurrentItem						(CUICellItem* itm);
	virtual bool				StopAnyMove							();
	virtual void				SendMessage							(CUIWindow* pWnd, s16 msg, void* pData = NULL);
	virtual void				Draw								();
	virtual void				Update								();
	virtual void				Show								(bool status);
			void				CheckSelectors						();

	virtual bool				OnKeyboardAction					(int dik, EUIMessages keyboard_action);
	virtual bool				OnMouseAction						(float x, float y, EUIMessages mouse_action);
	virtual bool				OnGamepadKeyAction					(int id, EUIMessages gamepad_action);
	virtual bool				OnGamepadKeyHold					(int id);

	void						CallMessageBoxYesNo					(LPCSTR text);
	void						CallMessageBoxOK					(LPCSTR text);
	void						OnMesBoxYes							(CUIWindow*, void*);
	void						OnMesBoxNo							(CUIWindow*, void*);

	bool						SetInfoCurUpgrade					(Upgrade_type* upgrade_type, CInventoryItem* inv_item );
	void						SeparateUpgradeItem					();
	PIItem						get_upgrade_item					();
	bool						DropAllItemsFromRuck				(bool quest_force = false); //debug func

	virtual void				UpdateActor							();
	void						UpdatePartnerBag					();
	virtual void				UpdateDeadBodyBag					();
	void						RefreshCurrentItemCell				();
	void						UpdateGamepadLegend					();

	void						OnBtnPerformTrade					(CUIWindow* w, void* d);
	void						OnBtnPerformTradeBuy				(CUIWindow* w, void* d);
	void						OnBtnPerformTradeSell				(CUIWindow* w, void* d);
	void						OnBtnExitClicked					(CUIWindow* w, void* d);

	virtual void				HideDialog							();

	void OnSuccessRepairMP(PIItem item);
	const UIInvUpgradeInfo* GetUpgradeInfo() const { return m_upgrade_info; }

	IC	UIHint*					get_hint_wnd				() { return m_hint_wnd; }

	
	void						UpdateInfoWindowVisibility	();
	bool						NeedToShowInfos				() const { return m_bShowInfoWnds; }
	bool						AnyInfoWindowOpen			() const;

	void HighlightSectionInSlot(LPCSTR section, u8 type, u16 slot_id = 0);
	CScriptGameObject* GetCurrentItemAsGameObject();
	void HighlightForEachInSlot(const luabind::functor<bool>& functor, u8 type, u16 slot_id);
	DECLARE_SCRIPT_REGISTER_FUNCTION
}; // class CUIActorMenu
