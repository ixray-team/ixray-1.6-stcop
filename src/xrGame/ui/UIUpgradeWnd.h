#pragma once
#include "UIActorMenuBase.h"
#include "UIDragDropListEx.h"
#include "../../xrUI/Widgets/UIWndCallback.h"

class CUIStatic;
class CUIFrameWindow;
class CInventoryOwner;
class CUI3tButton;
class CUICharacterInfo;

class CUIUpgradeWnd final :
						public CUIWndCallback,
						public CUIActorMenuBase
{
private:
	typedef CUIActorMenuBase inherited;

protected:
	CUIStatic* m_static_top = nullptr;
	CUIStatic* m_static_bottom = nullptr;

	CUIStatic* m_static_character_actor = nullptr;
	CUIStatic* m_static_character_partner = nullptr;

	CUIStatic* m_dragdrop_our_background = nullptr;
	CUIDragDropListEx* m_dragdrop_our = nullptr;
	CUIStatic* m_our_money_static = nullptr;

	CInventoryOwner* m_actor_inv_owner = nullptr;
	CInventoryOwner* m_partner_inv_owner = nullptr;

	CUI3tButton* m_exit_button = nullptr;

	CUICharacterInfo* m_character_info_actor = nullptr;
	CUICharacterInfo* m_character_info_partner = nullptr;

	CUIFrameWindow* m_description_frame = nullptr;
	CUIStatic* m_description_static = nullptr;

	void OnBtnExitClicked(CUIWindow* w, void* d);

public:
	CUIUpgradeWnd();

	void Init();
	void StartUpgrade(CInventoryOwner* actor, CInventoryOwner* partner);
	virtual void Update();
	virtual void Show(bool status);

	virtual void				SendMessage					(CUIWindow *pWnd, s16 msg, void *pData);
	virtual void				SetCurrentItem				(CUICellItem* itm);
	virtual CInventoryOwner*	GetInventoryOwner			() { return m_actor_inv_owner; }
	virtual CInventoryOwner*	GetPartner					() { return m_partner_inv_owner; }
	virtual CUIDragDropListEx*	GetActorList				() { return m_dragdrop_our; }
	virtual CUIWindow*			ui_cast_window						() { return this; }
};