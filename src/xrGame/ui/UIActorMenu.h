#pragma once

#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "../../xrUI/Widgets/UIWndCallback.h"
#include "../../xrServerEntities/inventory_space.h"
#include "../../xrUI/Widgets/UIHint.h"
#include "../../xrUI/Widgets/UITabControl.h"
#include "../../xrUI/ui_defs.h"
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
class CTrade;
class CUIProgressBar;
class CUIItemDropAmountWnd;
class CUIGamepadLegend;
class CUIXml;
class CUIXmlInit;
class CGameFont;

class CUIActorMenu final : 
						public CUIWndCallback,
						public CUIActorMenuBase
{
	typedef CUIActorMenuBase inherited;

protected:
	UIHint*						m_hint_wnd;
	CUICellItem*				m_InfoCellItem;
	u32							m_InfoCellItem_timer;

	ui_actor_state_wnd*			m_ActorStateInfo;
	CUICharacterInfo*			m_ActorCharacterInfo;
	CUICharacterInfo*			m_PartnerCharacterInfo;

	CUIDragDropListEx*			m_pInventoryBeltList;
	CUIDragDropListEx*			m_pInventoryBagList;
	CUIDragDropListEx*			m_pInventoryStackList = nullptr;

	CUIDragDropListEx*			m_pTradeActorBagList;
	CUIDragDropListEx*			m_pTradeActorList;
	CUIDragDropListEx*			m_pTradePartnerBagList;
	CUIDragDropListEx*			m_pTradePartnerList;
	CUIDragDropListEx*			m_pDeadBodyBagList;

	CUIStatic*					m_HelmetOver;
	
	CUIStatic*					m_LeftBackground = nullptr;

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
	CUIWindow*					m_ActorWeightRow = nullptr;
	CUIProgressBar*				m_ActorWeightBar = nullptr;
	CUIProgressBar*				m_ActorVolumeBar = nullptr;
	CUIStatic*					m_ActorVolumeCaption = nullptr;
	CUIStatic*					m_ActorVolume = nullptr;
	CUIStatic*					m_ActorVolumeMax = nullptr;
	bool						m_ActorWeightRowAutoPack = false;
	bool						m_ActorWeightRowVCenter = false;
	shared_str					m_ActorWeightHintFormat;
	shared_str					m_ActorWeightHintFormatVolume;
	CGameFont*					m_ActorWeightHintFont = nullptr;
	u32							m_ActorWeightHintColor = 0xffffffff;
	bool						m_ActorWeightHintHasStyle = false;
	
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
	
private:
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
	virtual CInventoryBox*		GetInvBox							() override {return m_pInvBox;}

	virtual CUIWindow*			ui_cast_window						() override { return this; }

	virtual ui_actor_state_wnd* GetActorState() override { return m_ActorStateInfo; }

	virtual CUIDragDropListEx* GetActorList() override { return m_pInventoryBagList; }
	virtual CUIDragDropListEx* GetPartnerList() override { return m_pDeadBodyBagList; }
	virtual CUIDragDropListEx* GetBeltList() override { return m_pInventoryBeltList; }
	virtual CUIDragDropListEx* GetStackList() override { return m_pInventoryStackList; }
	virtual CUIDragDropListEx* GetTradeActorList() override { return m_pTradeActorList; }
	virtual CUIDragDropListEx* GetTradeActorBagList() override { return m_pTradeActorBagList; }
	virtual CUIDragDropListEx* GetTradePartnerList() override { return m_pTradePartnerList; }
	virtual CUIDragDropListEx* GetTradePartnerBagList() override { return m_pTradePartnerBagList; }

	virtual CTrade* GetActorTrade() override { return m_actor_trade; }
	virtual CTrade* GetPartnerTrade() override { return m_partner_trade; }

protected:			
	void						Construct							();
	void						InitActorWeightSection				(CUIXml& uiXml, CUIXmlInit& xmlInit);
	void						InitActorVolumeSection				(CUIXml& uiXml, CUIXmlInit& xmlInit);
	void						AlignActorWeightRowVertically		();
	void						UpdateActorWeightBarTooltip			();
	void						InitCallbacks						();

	ESortTabsLayoutSlot			GetSortTabsSlotByWindow				(CUIWindow* window) const;
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
	virtual void				PerformDisassemble					() override;

	virtual void				InfoCurItem							(CUICellItem* cell_item) override; //on update item
	
	void						CheckDistance						();

	void						UpdateButtonsLayout					();

	// inventory
	bool						ToSlotScript						(CScriptGameObject* GO, bool force_place, u16 slot_id);
	bool						ToBeltScript						(CScriptGameObject* GO, bool b_use_cursor_pos);

	void						SetActorInfoMP();
	void						UpdateActorMoneyMP();
	virtual void				UpdateOutfit						() override;
	void						TryDisassembleItem					(CUIWindow* w, void* d);


	virtual void				TradeShowMessage					(s64 money_actor, s64 money_patner) override;

public:
								CUIActorMenu						();
	virtual ~CUIActorMenu						() override;

	virtual CInventoryOwner*	GetInventoryOwner					() override { return m_pActorInvOwner; }
	virtual CInventoryOwner*	GetPartner							() override { return m_pPartnerInvOwner; }
	virtual bool				ShouldPutArtefactsToBag				() override { return true; }
	virtual void				SetCurrentItem						(CUICellItem* itm) override;
	virtual void				SendMessage							(CUIWindow* pWnd, s16 msg, void* pData = NULL) override;
	virtual void				Draw								() override;
	virtual void				Update								() override;
	virtual void				Show								(bool status) override;

	// trade
	virtual void				UpdatePrices						() override;

	bool						DropAllItemsFromRuck				(bool quest_force = false); //debug func

	virtual void				UpdateActor							() override;
	void						UpdatePartnerBag					();
	virtual void				UpdateDeadBodyBag					() override;

	void						OnBtnPerformTradeBuy				(CUIWindow* w, void* d);
	void						OnBtnPerformTradeSell				(CUIWindow* w, void* d);
	void						OnBtnExitClicked					(CUIWindow* w, void* d);

	virtual void				HideDialog							() override;

	void OnSuccessRepairMP(PIItem item);

	IC	UIHint*					get_hint_wnd				() { return m_hint_wnd; }
	
	void						UpdateInfoWindowVisibility	();

	void HighlightSectionInSlot(const char* section, u8 type, u16 slot_id = 0);
	CScriptGameObject* GetCurrentItemAsGameObject();
	void HighlightForEachInSlot(const luabind::functor<bool>& functor, u8 type, u16 slot_id);
	DECLARE_SCRIPT_REGISTER_FUNCTION
}; // class CUIActorMenu
