#include "stdafx.h"
#include "UICellCustomItems.h"
#include "UIInventoryUtilities.h"
#include "UIXmlInit.h"
#include "../eatable_item.h"
#include "../UIGameCustom.h"
#include "../Weapon.h"
#include "UIDragDropListEx.h"

#define INV_GRID_WIDTHF			50.0f
#define INV_GRID_HEIGHTF		50.0f

CUIInventoryCellItem::CUIInventoryCellItem(CInventoryItem* itm)
{
	m_pData											= (void*)itm;

	inherited::SetShader							(InventoryUtilities::GetEquipmentIconsShader());

	m_grid_size.set									(itm->GetGridWidth(),itm->GetGridHeight());
	Frect rect; 
	rect.lt.set										(	INV_GRID_WIDTHF*itm->GetXPos(), 
														INV_GRID_HEIGHTF*itm->GetYPos() );

	rect.rb.set										(	rect.lt.x+INV_GRID_WIDTHF*m_grid_size.x, 
														rect.lt.y+INV_GRID_HEIGHTF*m_grid_size.y);

	inherited::SetTextureRect						(rect);
	inherited::SetStretchTexture					(true);

	bool smallerIcons = (itm->GetGridHeight() == 1);
	m_upgrade		= CreateSpecialIcon("cell_item_upgrade",	smallerIcons);
	m_unique_mark	= CreateSpecialIcon("cell_item_uniq_mark",	smallerIcons);
	m_quest_mark	= CreateSpecialIcon("cell_item_quest_mark", smallerIcons);
}

void CUIInventoryCellItem::Update()
{
	inherited::Update();
	auto item = (CInventoryItem*)m_pData;
	bool has_upgrade = item && item->has_any_upgrades();
	bool is_unique =   item && item->IsUniqueMarked();
	bool is_quest =    item && item->IsQuestItem();

	Fvector2 pos = GetWndSize();
	pos.x += 1;
	pos.x = UpdateSpecialIcon(m_unique_mark, pos, is_unique);
	pos.x = UpdateSpecialIcon(m_quest_mark, pos, is_quest);
			UpdateSpecialIcon(m_upgrade, pos, has_upgrade);
}

bool CUIInventoryCellItem::EqualTo(CUICellItem* itm)
{
	CUIInventoryCellItem* ci = smart_cast<CUIInventoryCellItem*>(itm);
	if ( !itm )
	{
		return false;
	}
	if ( object()->object().cNameSect() != ci->object()->object().cNameSect() )
	{
		return false;
	}
	if ( !fsimilar( object()->GetCondition(), ci->object()->GetCondition(), 0.01f ) )
	{
		return false;
	}
	if ( !object()->equal_upgrades( ci->object()->upgrades() ) )
	{
		return false;
	}
	auto eatable = smart_cast<CEatableItem*>(object());
	if (eatable && eatable->GetPortionsNum() != smart_cast<CEatableItem*>(ci->object())->GetPortionsNum())
	{
		return false;
	}
	return true;
}

CUIStatic* CUIInventoryCellItem::CreateSpecialIcon(LPCSTR name, bool smaller)
{
	auto icon = new CUIStatic();
	icon->SetAutoDelete(true);
	AttachChild(icon);
	CUIXmlInit::InitStatic(*CurrentGameUI()->m_actor_menu_item, name, 0, icon);
	
	if (smaller)
	{
		// Item is small (1 cell height), so let's make icons smaller too.
		icon->SetStretchTexture(true);
		Fvector2 size = icon->GetWndSize();
		icon->SetWndSize(size.mul(0.75));
	}
	icon->Show(false);
	return icon;
}

float CUIInventoryCellItem::UpdateSpecialIcon(CUIStatic* icon, Fvector2 pos, bool enable)
{
	if (enable)
	{
		pos.x -= icon->GetWidth() + 1;
		pos.y -= icon->GetHeight() + 1;

		icon->SetWndPos(pos);
	}
	icon->Show( enable );
	return pos.x;
}


CUIAmmoCellItem::CUIAmmoCellItem(CWeaponAmmo* itm)
:inherited(itm)
{}

bool CUIAmmoCellItem::EqualTo(CUICellItem* itm)
{
	if(!inherited::EqualTo(itm))	return false;

	CUIAmmoCellItem* ci				= smart_cast<CUIAmmoCellItem*>(itm);
	if(!ci)							return false;

	return					( (object()->cNameSect() == ci->object()->cNameSect()) );
}

void CUIAmmoCellItem::Update()
{
	inherited::Update	();
	UpdateItemText		();
}

void CUIAmmoCellItem::UpdateItemText()
{
	if(NULL==m_custom_draw)
	{
		xr_vector<CUICellItem*>::iterator it = m_childs.begin();
		xr_vector<CUICellItem*>::iterator it_e = m_childs.end();
		
		u16 total				= object()->m_boxCurr;
		for(;it!=it_e;++it)
			total				+= ((CUIAmmoCellItem*)(*it))->object()->m_boxCurr;

		string32					str;
		xr_sprintf					(str,"%d",total);

		TextItemControl()->SetText					(str);
	}
	else
	{
		TextItemControl()->SetText					("");
	}
}

CUIWeaponCellItem::CUIWeaponCellItem(CWeapon* itm)
	: inherited(itm)
{
	if (itm->SilencerAttachable())
		m_addons[eSilencer].offset.set(object()->GetSilencerX(), object()->GetSilencerY());

	if (itm->ScopeAttachable())
		m_addons[eScope].offset.set(object()->GetScopeX(), object()->GetScopeY());

	if (itm->GrenadeLauncherAttachable())
		m_addons[eLauncher].offset.set(object()->GetGrenadeLauncherX(), object()->GetGrenadeLauncherY());
}

#include "../object_broker.h"
CUIWeaponCellItem::~CUIWeaponCellItem()
{
}

bool CUIWeaponCellItem::is_scope()
{
	return object()->ScopeAttachable()&&object()->IsScopeAttached();
}

bool CUIWeaponCellItem::is_silencer()
{
	return object()->SilencerAttachable()&&object()->IsSilencerAttached();
}

bool CUIWeaponCellItem::is_launcher()
{
	return object()->GrenadeLauncherAttachable()&&object()->IsGrenadeLauncherAttached();
}

void CUIWeaponCellItem::CreateIcon(eAddonType t)
{
	if (m_addons[t].icon) return;
	auto st = m_addons[t].icon	= new CUIStatic();
	st->SetAutoDelete	(true);
	AttachChild			(st);
	st->Show			(false); // we will manually draw addon icons
	st->SetShader		(InventoryUtilities::GetEquipmentIconsShader());

	st->SetTextureColor( GetTextureColor());
}

void CUIWeaponCellItem::DestroyIcon(eAddonType t)
{
	DetachChild		(m_addons[t].icon);
	m_addons[t].icon	= NULL;
}

CUIStatic* CUIWeaponCellItem::GetIcon(eAddonType t)
{
	return m_addons[t].icon;
}

void CUIWeaponCellItem::RefreshOffset()
{
	if (object()->SilencerAttachable() || object()->IsSilencerIconForced())
		m_addons[eSilencer].offset.set(object()->GetSilencerX(), object()->GetSilencerY());

	if (object()->ScopeAttachable() || object()->IsScopeIconForced())
		m_addons[eScope].offset.set(object()->GetScopeX(), object()->GetScopeY());

	if (object()->GrenadeLauncherAttachable() || object()->IsGrenadeLauncherIconForced())
		m_addons[eLauncher].offset.set(object()->GetGrenadeLauncherX(), object()->GetGrenadeLauncherY());
}

void CUIWeaponCellItem::Update()
{
	bool b						= Heading();
	inherited::Update			();

	bool bForceReInitAddons		= (b!=Heading());

	if ((object()->SilencerAttachable() && object()->IsSilencerAttached()) || object()->IsSilencerIconForced())
	{
		if (!GetIcon(eSilencer) || bForceReInitAddons)
		{
			CreateIcon	(eSilencer);
			RefreshOffset();
			InitAddon	(m_addons[eSilencer], *object()->GetSilencerName(), Heading());
		}
	}
	else
	{
		if (m_addons[eSilencer].icon)
			DestroyIcon(eSilencer);
	}

	if ((object()->ScopeAttachable() && object()->IsScopeAttached()) || object()->IsScopeIconForced())
	{
		if (!GetIcon(eScope) || bForceReInitAddons)
		{
			CreateIcon	(eScope);
			RefreshOffset();
			InitAddon	(m_addons[eScope], *object()->GetScopeName(), Heading());
		}
	}
	else
	{
		if (m_addons[eScope].icon)
			DestroyIcon(eScope);
	}

	if ((object()->GrenadeLauncherAttachable() && object()->IsGrenadeLauncherAttached()) || object()->IsGrenadeLauncherIconForced())
	{
		if (!GetIcon(eLauncher) || bForceReInitAddons)
		{
			CreateIcon	(eLauncher);
			RefreshOffset();
			InitAddon	(m_addons[eLauncher], *object()->GetGrenadeLauncherName(), Heading());
		}
	}
	else
	{
		if (m_addons[eLauncher].icon)
			DestroyIcon(eLauncher);
	}
}

void CUIWeaponCellItem::SetTextureColor( u32 color )
{
	inherited::SetTextureColor( color );
	for (size_t i = 0; i < eMaxAddon; ++i)
	{
		if (m_addons[i].icon)
		{
			m_addons[i].icon->SetTextureColor( color );
		}
	}
}

void CUIWeaponCellItem::DrawTexture()
{
	DrawAddons(true);
	inherited::DrawTexture();
	DrawAddons(false);
}

void CUIWeaponCellItem::DrawAddons(bool behind)
{
	for (size_t i = 0; i < eMaxAddon; ++i)
	{
		if (m_addons[i].drawBehind == behind && m_addons[i].icon)
			m_addons[i].icon->Draw();
	}
}

void CUIWeaponCellItem::OnAfterChild(CUIDragDropListEx* parent_list)
{
	if (GetIcon(eSilencer))
		InitAddon	(m_addons[eSilencer],	*object()->GetSilencerName(),		parent_list->GetVerticalPlacement());

	if (GetIcon(eScope))
		InitAddon	(m_addons[eScope],		*object()->GetScopeName(),			parent_list->GetVerticalPlacement());

	if (GetIcon(eLauncher))
		InitAddon	(m_addons[eLauncher],	*object()->GetGrenadeLauncherName(), parent_list->GetVerticalPlacement());
}

void CUIWeaponCellItem::InitAddon(SAddon& addon, LPCSTR section, bool b_rotate)
{
	InitAddon				(addon.icon, section, addon.offset, b_rotate);
	addon.drawBehind		= !!READ_IF_EXISTS(pSettings, r_bool, section, "inv_draw_behind", FALSE);
	addon.section			= section;
}

void CUIWeaponCellItem::InitAddon(CUIStatic* s, LPCSTR section, Fvector2 addon_offset, bool b_rotate)
{
	Frect					tex_rect;
	Fvector2				base_scale;

	if (Heading())
	{
		base_scale.x		= GetHeight()/(INV_GRID_WIDTHF*m_grid_size.x);
		base_scale.y		= GetWidth()/(INV_GRID_HEIGHTF*m_grid_size.y);
	}else
	{
		base_scale.x		= GetWidth()/(INV_GRID_WIDTHF*m_grid_size.x);
		base_scale.y		= GetHeight()/(INV_GRID_HEIGHTF*m_grid_size.y);
	}
	Fvector2				cell_size;
	cell_size.x				= pSettings->r_u32(section, "inv_grid_width")*INV_GRID_WIDTHF;
	cell_size.y				= pSettings->r_u32(section, "inv_grid_height")*INV_GRID_HEIGHTF;

	tex_rect.x1				= pSettings->r_u32(section, "inv_grid_x")*INV_GRID_WIDTHF;
	tex_rect.y1				= pSettings->r_u32(section, "inv_grid_y")*INV_GRID_HEIGHTF;

	tex_rect.rb.add			(tex_rect.lt,cell_size);

	cell_size.mul			(base_scale);

	if (b_rotate)
	{
		s->SetWndSize		(Fvector2().set(cell_size.y, cell_size.x) );
		Fvector2 new_offset;
		new_offset.x		= addon_offset.y*base_scale.y;
		new_offset.y		= GetHeight() - addon_offset.x*base_scale.x - cell_size.x;
		addon_offset		= new_offset;
		//addon_offset.x		*= UI().get_current_kx();
	}else
	{
		s->SetWndSize		(cell_size);
		addon_offset.mul	(base_scale);
	}

	s->SetWndPos			(addon_offset);
	s->SetTextureRect		(tex_rect);
	s->SetStretchTexture	(true);

	s->EnableHeading		(b_rotate);

	if (b_rotate)
	{
		s->SetHeading		(GetHeading());
		Fvector2 offs;
		offs.set			(0.0f, s->GetWndSize().y);
		s->SetHeadingPivot	(Fvector2().set(0.0f,0.0f), /*Fvector2().set(0.0f,0.0f)*/offs, true);
	}
}

void CUIWeaponCellItem::InitAddonForDrag(CUIDragItem* i, const SAddon& addon, int pos)
{
	auto s = new CUIStatic();
	s->SetAutoDelete(true);
	s->SetShader	(InventoryUtilities::GetEquipmentIconsShader());
	InitAddon		(s, *addon.section, addon.offset, Heading());
	s->SetTextureColor(i->wnd()->GetTextureColor());
	i->AttachChild	(s, pos);
}

void CUIWeaponCellItem::InitAddonsForDrag(CUIDragItem* drag, bool behind)
{
	int numAdded = 0;
	for (size_t i = 0; i < eMaxAddon; ++i)
	{
		if (m_addons[i].drawBehind == behind && m_addons[i].icon)
		{
			InitAddonForDrag(drag, m_addons[i], behind ? numAdded : -1);
			++numAdded;
		}
	}
}

CUIDragItem* CUIWeaponCellItem::CreateDragItem()
{
	CUIDragItem* i		= inherited::CreateDragItem();
	CUIStatic* s		= NULL;

	InitAddonsForDrag(i, true);
	InitAddonsForDrag(i, false);
	return				i;
}

bool CUIWeaponCellItem::EqualTo(CUICellItem* itm)
{
	if(!inherited::EqualTo(itm))	return false;

	CUIWeaponCellItem* ci			= smart_cast<CUIWeaponCellItem*>(itm);
	if(!ci)							return false;

	if ( object()->GetAddonsState() != ci->object()->GetAddonsState() )
	{
		return false;
	}
	if (this->is_scope() && ci->is_scope())
	{
		if ( object()->GetScopeName() != ci->object()->GetScopeName() )
		{
			return false;
		}
	}
	if (object()->m_ammoType != ci->object()->m_ammoType
		|| object()->GetAmmoElapsed() != ci->object()->GetAmmoElapsed())
	{
		return false;
	}

	return true;
}

CBuyItemCustomDrawCell::CBuyItemCustomDrawCell	(LPCSTR str, CGameFont* pFont)
{
	m_pFont		= pFont;
	VERIFY		(xr_strlen(str)<16);
	xr_strcpy	(m_string,str);
}

void CBuyItemCustomDrawCell::OnDraw(CUICellItem* cell)
{
	Fvector2							pos;
	cell->GetAbsolutePos				(pos);
	UI().ClientToScreenScaled			(pos, pos.x, pos.y);
	m_pFont->Out						(pos.x, pos.y, m_string);
	m_pFont->OnRender					();
}
