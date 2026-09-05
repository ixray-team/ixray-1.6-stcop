#pragma once

class CInventory;

#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "../../xrUI/Widgets/UIStatic.h"

#include "../../xrUI/Widgets/UIProgressBar.h"

#include "../../xrUI/Widgets/UIPropertiesBox.h"
#include "UIOutfitSlot.h"

#include "UIOutfitInfo.h"
#include "UIActorMenuBase.h"
#include "UIItemInfo.h"
#include "inventory_space.h"

class CArtefact;
class CUI3tButton;
class CUIDragDropListEx;
class CUICellItem;
class CUIItemDropAmountWnd;

class CUIInventoryWnd: public CUIActorMenuBase
{
private:
	typedef CUIActorMenuBase inherited;
public:
							CUIInventoryWnd					();
	virtual ~CUIInventoryWnd() = default;

	virtual void			Init							();

	void					InitInventory					();

	virtual void			SendMessage						(CUIWindow *pWnd, s16 msg, void *pData);

	virtual CInventoryOwner* GetInventoryOwner			() { return m_pInvOwner; }

	virtual void			Update							();

	virtual void			Show							(bool status);

	virtual CUIDragDropListEx*	GetActorList				() { return m_pUIBagList; }
	virtual CUIDragDropListEx*	GetBeltList					() { return m_pUIBeltList; }
	virtual void				UpdateActor					();
protected:
	CUIStatic					UIBeltSlots;
	CUIStatic					UIBack;
	CUIStatic*					UIRankFrame;
	CUIStatic*					UIRank;

	CUIStatic					UIBagWnd;
	CUIStatic					UIMoneyWnd;
	CUIStatic					UIDescrWnd;
	CUIFrameWindow				UIPersonalWnd;

	CUI3tButton*				UIExitButton;

	CUIStatic					UIStaticBottom;
	CUIStatic					UIStaticTime;
	CUIStatic					UIStaticTimeString;

	CUIStatic					UIStaticPersonal;

	CUIDragDropListEx*			m_pUIBagList;
	CUIDragDropListEx*			m_pUIBeltList;
	
	CUIStatic					UIProgressBack;
	CUIStatic					UIProgressBack_rank;
	CUIProgressBar				UIProgressBarHealth;
	CUIProgressBar				UIProgressBarPsyHealth;
	CUIProgressBar				UIProgressBarRadiation;
	CUIProgressBar				UIProgressBarRank;

	//информация о персонаже
	CUIOutfitInfo				UIOutfitInfo;

	CInventoryOwner*			m_pInvOwner;
	
	virtual void				SetCurrentItem				(CUICellItem* itm);

	TIItemContainer				ruck_list;
	u32							m_iCurrentActiveSlot;
};