#pragma once
#include "uiwindow.h"


class CInventoryItem;
class CUIStatic;
class CUITextWnd;
class CUIScrollView;
class CUIProgressBar;
class CUIWpnParams;
class CUIArtefactParams;
class CUIOutfitParams;

extern const char * const 		fieldsCaptionColor;

class CUIItemInfo: public CUIWindow
{
private:
	typedef CUIWindow inherited;
	struct _desc_info
	{
		CGameFont*			pDescFont;
		u32					uDescClr;
		bool				bShowDescrText;
	};
	_desc_info				m_desc_info;
	CInventoryItem* m_pInvItem;
public:
						CUIItemInfo			();
	virtual				~CUIItemInfo		();

	void				InitItemInfo		(Fvector2 pos, Fvector2 size, LPCSTR xml_name);
	void				Init				(LPCSTR xml_name);
	void				InitItem			(CInventoryItem* pInvItem);
	void				TryAddWpnInfo		(CInventoryItem& wpn);
	void				TryAddArtefactInfo	(CInventoryItem& itm);
	void				TryAddOutfitInfo	(CInventoryItem& itm);
	void				UpdateCondition		();

	virtual void		Draw				();
	bool				m_b_force_drawing;
	CUITextWnd*			UIName;
	CUITextWnd*			UIWeight;
	CUITextWnd*			UICost;
	CUIStatic*			UICondition;
	CUIScrollView*		UIDesc;
	CUIProgressBar*		UICondProgresBar;
	CUIWpnParams*		UIWpnParams;
	CUIArtefactParams*	UIArtefactParams;
	CUIOutfitParams*	UIOutfitParams;

	Frect				UIItemImageRect; 
	CUIStatic*			UIItemImage;
};