#include "stdafx.h"
#include "UIOutfitSlot.h"
#include "UIStatic.h"
#include "UICellItem.h"
#include "../CustomOutfit.h"
#include "../actor.h"
#include "UIInventoryUtilities.h"

CUIOutfitDragDropList::CUIOutfitDragDropList()
{
	m_background				= new CUIStatic();
	m_background->SetAutoDelete	(true);
	AttachChild					(m_background);
	m_default_outfit			= "npc_icon_without_outfit";
}

CUIOutfitDragDropList::~CUIOutfitDragDropList()
{
}

#include "../level.h"
#include "../game_base_space.h"

void CUIOutfitDragDropList::SetOutfit(CUICellItem* itm)
{
	m_background->SetWndPos				(Fvector2().set(0,0));
	m_background->SetWndSize			(Fvector2().set(GetWidth(), GetHeight()));
	m_background->SetStretchTexture		(true);

	if ((GameID() != GAME_SINGLE) && !itm)
	{
		CObject *pActor = smart_cast<CActor*>(Level().CurrentEntity());

		xr_string a;
		if (pActor)
			a = *pActor->cNameVisual();
		else
			a = *m_default_outfit;

		xr_string::iterator it = std::find(a.rbegin(), a.rend(), '\\').base(); 

		// Cut leading full path
		if (it != a.begin())
			a.erase(a.begin(), it);
		// Cut trailing ".ogf"
		R_ASSERT(xr_strlen(a.c_str()) > 4);
		if ('.' == a[a.size() - 4])
			a.erase(a.size() - 4);

		m_background->InitTexture(a.c_str());
	} else {
		if(itm)
		{
			PIItem _iitem	= (PIItem)itm->m_pData;
			CCustomOutfit* pOutfit = smart_cast<CCustomOutfit*>(_iitem); VERIFY(pOutfit);

			m_background->InitTexture			(pOutfit->GetFullIconName().c_str());
		} else
			m_background->InitTexture		("npc_icon_without_outfit");
	}

	m_background->TextureOn				();
}

void CUIOutfitDragDropList::SetDefaultOutfit(LPCSTR default_outfit){
	m_default_outfit = default_outfit;
}

void CUIOutfitDragDropList::SetItem(CUICellItem* itm)
{
	if(itm)	inherited::SetItem			(itm);
	SetOutfit							(itm);
}

void CUIOutfitDragDropList::SetItem(CUICellItem* itm, Fvector2 abs_pos)
{
	if(itm)	inherited::SetItem			(itm, abs_pos);
	SetOutfit							(itm);
}

void CUIOutfitDragDropList::SetItem(CUICellItem* itm, Ivector2 cell_pos)
{
	if(itm)	inherited::SetItem			(itm, cell_pos);
	SetOutfit							(itm);
}

CUICellItem* CUIOutfitDragDropList::RemoveItem(CUICellItem* itm, bool force_root)
{
	VERIFY								(!force_root);
	CUICellItem* ci						= inherited::RemoveItem(itm, force_root);
	SetOutfit							(nullptr);
	return								ci;
}

void CUIOutfitDragDropList::ClearAll(bool bDestroy)
{
	inherited::ClearAll(bDestroy);
	SetOutfit(nullptr);
}


void CUIOutfitDragDropList::Draw()
{
	m_background->Draw					();
//.	inherited::Draw						();
}