#pragma once
#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/Widgets/UIMultiTextStatic.h"
#include "inventory_space.h"
#include "UIDragDropListEx.h"
#include "UICharacterInfo.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "UIItemInfo.h"
#include "UIActorMenuBase.h"

class CInventoryOwner;
class CEatableItem;
class CTrade;
class CUI3tButton;
class SDrawStaticStruct;

class CUICellItem;

class CUITradeWnd: public CUIActorMenuBase
{
private:
	typedef CUIActorMenuBase inherited;
public:
						CUITradeWnd					();
	virtual				~CUITradeWnd				();

	virtual void		Init						();

	virtual void		SendMessage					(CUIWindow *pWnd, s16 msg, void *pData);

	void				InitTrade					(CInventoryOwner* pOur, CInventoryOwner* pOthers);
	
	virtual void 		Draw						();
	virtual void 		Update						();
	virtual void 		Show						(bool status);

	void 				DisableAll					();
	void 				EnableAll					();

	void 				SwitchToTalk				();
	void 				StartTrade					();
	void 				StopTrade					();

protected:
	CUIStatic			UIStaticTop;
	CUIStatic			UIStaticBottom;

	CUIStatic			UIOurBagWnd;
	CUIStatic			UIOurMoneyStatic;
	CUIStatic			UIOthersBagWnd;
	CUIStatic			UIOtherMoneyStatic;
	CUIDragDropListEx	UIOurBagList;
	CUIDragDropListEx	UIOthersBagList;

	CUIStatic			UIOurTradeWnd;
	CUIStatic			UIOthersTradeWnd;
	CUIMultiTextStatic	UIOurPriceCaption;
	CUIMultiTextStatic	UIOthersPriceCaption;
	CUIDragDropListEx	UIOurTradeList;
	CUIDragDropListEx	UIOthersTradeList;

	//кнопки
	CUI3tButton			UIPerformTradeButton;
	CUI3tButton			UIToTalkButton;
	bool				m_highlight_clear;

	//информация о персонажах 
	CUIStatic			UIOurIcon;
	CUIStatic			UIOthersIcon;
	CUICharacterInfo	UICharacterInfoLeft;
	CUICharacterInfo	UICharacterInfoRight;

	//информация о перетаскиваемом предмете
	CUIStatic			UIDescWnd;

	SDrawStaticStruct*	UIDealMsg;

	bool				bStarted;
	
	enum EListType{eNone,e1st,e2nd,eBoth};
	void				UpdateLists					(EListType);

	void				FillList					(TIItemContainer& cont, CUIDragDropListEx& list, bool do_colorize);

	bool				m_bDealControlsVisible;

	//указатели игрока и того с кем торгуем
	CInventory*			m_pInv;
	CInventory*			m_pOthersInv;
	CInventoryOwner*	m_pInvOwner;
	CInventoryOwner*	m_pOthersInvOwner;
	CTrade*				m_pTrade;
	CTrade*				m_pOthersTrade;

	u32					m_iOurTradePrice;
	u32					m_iOthersTradePrice;

	TIItemContainer		ruck_list;

	virtual void		SetCurrentItem				(CUICellItem* itm);
	virtual bool		ForceHighlightForSlots		() { return true; }
	virtual void		TradeShowMessage			(int money_actor, int money_patner);

public:
	virtual CInventory*			GetInventory				() { return m_pInv; }
	virtual CInventoryOwner*	GetInventoryOwner			() { return m_pInvOwner; }
	virtual CInventoryOwner*	GetPartner					() { return m_pOthersInvOwner; }
	virtual CUIDragDropListEx*	GetTradeActorList			() { return &UIOurTradeList; }
	virtual CUIDragDropListEx*	GetTradeActorBagList		() { return &UIOurBagList; }
	virtual CUIDragDropListEx*	GetTradePartnerList			() { return &UIOthersTradeList; }
	virtual CUIDragDropListEx*	GetTradePartnerBagList		() { return &UIOthersBagList; }

	virtual CTrade*				GetActorTrade				() { return m_pTrade; }
	virtual CTrade*				GetPartnerTrade				() { return m_pOthersTrade; }
	virtual void				UpdatePrices				();
};