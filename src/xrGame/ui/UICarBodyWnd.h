#pragma once

#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "../../xrUI/Widgets/UIEditBox.h"
#include "inventory_space.h"

class CUIDragDropListEx;
class CUIItemInfo;
class CUICharacterInfo;
class CUIPropertiesBox;
class CUI3tButton;
class CUICellItem;
class CInventoryBox;
class CInventoryOwner;

class CUICarBodyWnd: public CUIDialogWnd
{
private:
	typedef CUIDialogWnd	inherited;
	bool					m_b_need_update;
public:
							CUICarBodyWnd				();
	virtual					~CUICarBodyWnd				();

	virtual void			Init						();
	virtual bool			StopAnyMove					(){return true;}

	virtual void			SendMessage					(CUIWindow *pWnd, s16 msg, void *pData);

	void					InitCarBody					(CInventoryOwner* pOurInv, CInventoryOwner* pOthersInv);
	void					InitCarBody					(CInventoryOwner* pOur, CInventoryBox* pInvBox);
	virtual void			Draw						();
	virtual void			Update						();
		
	virtual void			Show						(bool status);

	void					DisableAll					();
	void					EnableAll					();
	virtual bool			OnKeyboardAction					(int dik, EUIMessages keyboard_action);

	void					UpdateLists_delayed			();

	void					clear_highlight_lists		();
protected:
	CInventoryOwner*		m_pOurObject;

	CInventoryOwner*		m_pOthersObject;
	CInventoryBox*			m_pInventoryBox;

	CUIDragDropListEx*		m_pUIOurBagList;
	CUIDragDropListEx*		m_pUIOthersBagList;

	CUIStatic*				m_pUIStaticTop;
	CUIStatic*				m_pUIStaticBottom;

	CUIFrameWindow*			m_pUIDescWnd;
	CUIStatic*				m_pUIStaticDesc;
	CUIItemInfo*			m_pUIItemInfo;

	CUIStatic*				m_pUIOurBagWnd;
	CUIStatic*				m_pUIOthersBagWnd;

	//информация о персонажах 
	CUIStatic*				m_pUIOurIcon;
	CUIStatic*				m_pUIOthersIcon;
	CUICharacterInfo*		m_pUICharacterInfoLeft;
	CUICharacterInfo*		m_pUICharacterInfoRight;
	CUIPropertiesBox*		m_pUIPropertiesBox;
	CUI3tButton*			m_pUITakeAll;

	CUICellItem*			m_pCurrentCellItem;
	bool					m_highlight_clear;

	void					UpdateLists					();

	void					ActivatePropertiesBox		();

	void					set_highlight_item			(CUICellItem* cell_item);
	void					highlight_armament			(PIItem item, CUIDragDropListEx* ddlist);
	void					highlight_ammo_for_weapon	(PIItem weapon_item, CUIDragDropListEx* ddlist);
	void					highlight_weapons_for_ammo	(PIItem ammo_item, CUIDragDropListEx* ddlist);
	bool					highlight_addons_for_weapon	(PIItem weapon_item, CUICellItem* ci);
	void					highlight_weapons_for_addon	(PIItem addon_item, CUIDragDropListEx* ddlist);

	void					EatItem						();

	bool					ToOurBag					();
	bool					ToOthersBag					();
	
	void					SetCurrentItem				(CUICellItem* itm);
	CUICellItem*			CurrentItem					();
	PIItem					CurrentIItem				();

	// Взять все
	void					TakeAll						();


	bool					OnItemDrop					(CUICellItem* itm);
	bool					OnItemStartDrag				(CUICellItem* itm);
	bool					OnItemDbClick				(CUICellItem* itm);
	bool					OnItemSelected				(CUICellItem* itm);
	bool					OnItemRButtonClick			(CUICellItem* itm);
	bool					OnItemFocusReceive			(CUICellItem* itm);
	bool					OnItemFocusLost				(CUICellItem* itm);
	bool					OnItemFocusedUpdate			(CUICellItem* itm);

	bool					TransferItem				(PIItem itm, CInventoryOwner* owner_from, CInventoryOwner* owner_to, bool b_check);
	void					BindDragDropListEnents		(CUIDragDropListEx* lst);

};