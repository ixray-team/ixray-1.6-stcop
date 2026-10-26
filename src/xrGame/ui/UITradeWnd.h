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
	virtual ~CUITradeWnd				() override;

	virtual void		Init						();

	virtual void		SendMessage					(CUIWindow *pWnd, s16 msg, void *pData) override;

	void				InitTrade					(CInventoryOwner* pOur, CInventoryOwner* pOthers);

	virtual void 		Draw						() override;
	virtual void 		Update						() override;
	virtual void 		Show						(bool status) override;

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

	bool				m_bDealControlsVisible;

	//указатели игрока и того с кем торгуем
	CInventoryOwner*	m_pInvOwner;
	CInventoryOwner*	m_pOthersInvOwner;
	CTrade*				m_pTrade;
	CTrade*				m_pOthersTrade;

	u32					m_iOurTradePrice;
	u32					m_iOthersTradePrice;

	TIItemContainer		ruck_list;

	virtual void		SetCurrentItem				(CUICellItem* itm) override;
	virtual void		TradeShowMessage			(s64 money_actor, s64 money_patner) override;

public:
	virtual CInventoryOwner*	GetInventoryOwner			() override { return m_pInvOwner; }
	virtual CInventoryOwner*	GetPartner					() override { return m_pOthersInvOwner; }
	virtual CUIDragDropListEx*	GetTradeActorList			() override { return &UIOurTradeList; }
	virtual CUIDragDropListEx*	GetTradeActorBagList		() override { return &UIOurBagList; }
	virtual CUIDragDropListEx*	GetTradePartnerList			() override { return &UIOthersTradeList; }
	virtual CUIDragDropListEx*	GetTradePartnerBagList		() override { return &UIOthersBagList; }

	virtual CTrade*				GetActorTrade				() override { return m_pTrade; }
	virtual CTrade*				GetPartnerTrade				() override { return m_pOthersTrade; }
	virtual void				UpdatePrices				() override;
};