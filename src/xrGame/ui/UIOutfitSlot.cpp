#include "StdAfx.h"
#include "UIOutfitSlot.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "UICellItem.h"
#include "../CustomOutfit.h"
#include "../Actor.h"
#include "UIInventoryUtilities.h"

CUIOutfitDragDropList::CUIOutfitDragDropList()
{
	m_background				= new CUI3dStatic();
	m_background->SetAutoDelete	(true);
	AttachChild					(m_background);
	m_default_outfit			= "npc_icon_without_outfit";
}

CUIOutfitDragDropList::~CUIOutfitDragDropList()
{
}

#include "../Level.h"

void CUIOutfitDragDropList::SetOutfit(CUICellItem* itm)
{
	m_background->SetWndPos(Fvector2().set(0, 0));
	m_background->SetWndSize(Fvector2().set(GetWidth(), GetHeight()));

	m_background->SetStretchTexture(true);
	CObject* current_entity = IsGameTypeSingle() ? Level().CurrentEntity() : Level().CurrentControlEntity();

	if (current_entity)
	{
		if (psDeviceFlags.test(rsR4))
		{
			m_background->SetVisual(current_entity->Visual());
			m_background->SetXYZ(0, M_PI, 0);
		}
		else
		{
			if (itm)
			{
				PIItem _iitem = (PIItem)itm->m_pData;
				CCustomOutfit* pOutfit = _iitem != nullptr ? _iitem->cast_outfit() : nullptr;
				VERIFY(pOutfit);
				m_background->InitTexture(pOutfit->GetFullIconName().c_str());
			}
			else
			{
				m_background->InitTexture("npc_icon_without_outfit");
			}
		}
	}
	else
	{
		m_background->SetVisual(nullptr);
	}
}

void CUIOutfitDragDropList::SetDefaultOutfit(const char* default_outfit){
	m_default_outfit = default_outfit;
}

void CUIOutfitDragDropList::SetItem(CUICellItem* itm)
{
	if(itm)	inherited::SetItem			(itm);
	SetOutfit							(itm);
}

bool CUIOutfitDragDropList::SetItem(CUICellItem* itm, Fvector2 abs_pos)
{
	if(itm)	
		inherited::SetItem			(itm, abs_pos);
	SetOutfit							(itm);
	return true;
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


void CUIOutfitDragDropList::Draw()
{
	m_background->Draw					();
//.	inherited::Draw						();
}