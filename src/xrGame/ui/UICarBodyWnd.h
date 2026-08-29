#pragma once

#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "../../xrUI/Widgets/UIEditBox.h"
#include "inventory_space.h"
#include "UIActorMenuBase.h"

class CUIDragDropListEx;
class CUIItemInfo;
class CUICharacterInfo;
class CUIPropertiesBox;
class CUI3tButton;
class CUICellItem;
class CInventoryBox;
class CInventoryOwner;
class CUIItemDropAmountWnd;
class CWeaponMagazined;

class CUICarBodyWnd: public CUIActorMenuBase
{
private:
	typedef CUIActorMenuBase	inherited;
public:
								CUICarBodyWnd				();
	virtual						~CUICarBodyWnd				();

	virtual void				Init						();

	virtual void				SendMessage					(CUIWindow *pWnd, s16 msg, void *pData);

	void						InitCarBody					(CInventoryOwner* pOurInv, CInventoryOwner* pOthersInv);
	void						InitCarBody					(CInventoryOwner* pOur, CInventoryBox* pInvBox);
	virtual void				Draw						();
	virtual void				Update						();
		
	virtual void				Show						(bool status);

	void						DisableAll					();
	void						EnableAll					();

	virtual CInventoryOwner*	GetInventoryOwner			() { return m_pOurObject; }
	virtual CInventoryOwner*	GetPartner					() { return m_pOthersObject; }

	virtual CUIDragDropListEx*	GetActorList				() { return m_pUIOurBagList; }
	virtual CUIDragDropListEx*	GetPartnerList				() { return m_pUIOthersBagList; }
	virtual CInventoryBox*		GetInvBox					() { return m_pInventoryBox; }
protected:
	CInventoryOwner*			m_pOurObject;

	CInventoryOwner*			m_pOthersObject;
	CInventoryBox*				m_pInventoryBox;

	CUIDragDropListEx*			m_pUIOurBagList;
	CUIDragDropListEx*			m_pUIOthersBagList;

	CUIStatic*					m_pUIStaticTop;
	CUIStatic*					m_pUIStaticBottom;

	CUIFrameWindow*				m_pUIDescWnd;
	CUIStatic*					m_pUIStaticDesc;

	CUIStatic*					m_pUIOurBagWnd;
	CUIStatic*					m_pUIOthersBagWnd;

	//информация о персонажах 
	CUIStatic*					m_pUIOurIcon;
	CUIStatic*					m_pUIOthersIcon;
	CUICharacterInfo*			m_pUICharacterInfoLeft;
	CUICharacterInfo*			m_pUICharacterInfoRight;
	CUI3tButton*				m_pUITakeAll;
	CUI3tButton*				m_pUIPutAll;

	bool						m_highlight_clear;

	void						UpdateLists					();

	virtual void				SetCurrentItem				(CUICellItem* itm);
};