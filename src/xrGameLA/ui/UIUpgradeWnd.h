#pragma once
#include "UIWindow.h"
#include "../inventory_space.h"
#include "../ui_base.h"

class CInventoryOwner;
struct CUIUpgradeInternal;
class CUIDragDropListEx;
class CUICellItem;
class CUIInventoryUpgradeWnd;
class UIInvUpgradeInfo;
class CUIMessageBoxEx;

namespace inventory {
	namespace upgrade {
		class Upgrade;
	}
} 

class CUIUpgradeWnd : public CUIWindow
{
private:
	enum EListType {
		eNone,
		e1st,
		e2nd,
		eBoth
	};

	typedef CUIWindow inherited;
	typedef inventory::upgrade::Upgrade 	Upgrade_type;
public:
	CUIUpgradeWnd();
	virtual				~CUIUpgradeWnd();

	virtual void		Init();

	virtual void		SendMessage(CUIWindow *pWnd, s16 msg, void *pData);

	void				InitUpgrade(CInventoryOwner* pOur, CInventoryOwner* pOthers);

	//virtual void 		Draw();
	virtual void 		Update();
	virtual void 		Show();
	virtual void 		Hide();

	void 				DisableAll();
	void 				EnableAll();

	void 				SwitchToTalk();
	void 				StartTrade();
	void 				StopTrade();

	void				CallMessageBoxYesNo(LPCSTR text);
	void				CallMessageBoxOK(LPCSTR text);
	void				UpdateActor();
	void				SeparateUpgradeItem();
	bool				SetInfoCurUpgrade	(Upgrade_type* upgrade_type, CInventoryItem* inv_item);
	void				UpdateItemsPlace(bool setupItem = true);
	bool				CanAddItem(PIItem item);
	bool				CanUpgradeItem(PIItem item);
	void				SetupUpgradeItem();

	void		xr_stdcall		OnMesBoxYes(CUIWindow*, void*);
	void		xr_stdcall		OnMesBoxNo(CUIWindow*, void*);
protected:

	CUIUpgradeInternal*	m_uidata;

	void				SetCurrentItem(CUICellItem* itm);
	CUICellItem*		CurrentItem();
	PIItem				CurrentIItem();

	bool				bStarted, bRepairMode;

	void 				SendEvent_ItemDrop(PIItem pItem);
	
	void				PerformRepair();
	void				UpdatePrices();
	void				ColorizeItem(CUICellItem* itm);

	void				UpdateLists(EListType);

	void				FillList(TIItemContainer& cont, CUIDragDropListEx& list, bool do_colorize);

	

	CInventory*			m_pInv;
	
	CInventoryOwner*	m_pInvOwner;
	CInventoryOwner*	m_pOthersInvOwner;
	TIItemContainer		ruck_list;
	CUICellItem*		m_pCurrentCellItem;

	CUIInventoryUpgradeWnd* m_pUpgradeWnd;

	UIInvUpgradeInfo*			m_upgrade_info;
	CUIMessageBoxEx*			m_message_box_yes_no;
	CUIMessageBoxEx*			m_message_box_ok;
	CUICellItem*				m_upgrade_selected;

	bool		xr_stdcall		OnItemDrop(CUICellItem* itm);
	bool		xr_stdcall		OnItemStartDrag(CUICellItem* itm);
	bool		xr_stdcall		OnItemDbClick(CUICellItem* itm);
	bool		xr_stdcall		OnItemSelected(CUICellItem* itm);
	bool		xr_stdcall		OnItemRButtonClick(CUICellItem* itm);

	void						TryRepairItem();

	void				BindDragDropListEvents(CUIDragDropListEx* lst);

};