#include "stdafx.h"
#include "pch_script.h"
#include "InventoryBox.h"
#include "level.h"
#include "actor.h"
#include "game_object_space.h"

#include "../xrScripts/script_callback_ex.h"
#include "script_game_object.h"
#include "inventory_item.h"
#include "xrServer_Objects_ALife.h"

CInventoryBox::CInventoryBox()
{
	m_in_use = false;
	IsSafe = false;
}

CInventoryBox::~CInventoryBox()
{
}

void CInventoryBox::OnEvent(NET_Packet& P, u16 type)
{
	inherited::OnEvent	(P, type);

	switch (type)
	{
	//case GE_TRADE_BUY:
	case GE_OWNERSHIP_TAKE:
		{
			u16 id;
            P.r_u16(id);
			CObject* itm = Level().Objects.net_Find(id);  VERIFY(itm);
			m_items.push_back	(id);
			itm->H_SetParent	(this);
			itm->setVisible		(FALSE);
			itm->setEnabled		(FALSE);
		}break;

	//case GE_TRADE_SELL:
	case GE_OWNERSHIP_REJECT:
		{
			u16 id;
            P.r_u16(id);
			CObject* itm = Level().Objects.net_Find(id);  VERIFY(itm);
			xr_vector<u16>::iterator it;
			it = std::find(m_items.begin(),m_items.end(),id); VERIFY(it!=m_items.end());
			m_items.erase		(it);
			itm->H_SetParent	(NULL,!P.r_eof() && P.r_u8());

			if( m_in_use )
			{
				CGameObject* GO		= smart_cast<CGameObject*>(itm);
				Actor()->callback(GameObject::eInvBoxItemTake)( this->lua_game_object(), GO->lua_game_object() );
			}
		}break;
	};
}

void CInventoryBox::UpdateCL()
{
	inherited::UpdateCL	();
}

void CInventoryBox::net_Destroy()
{
	inherited::net_Destroy	();
}
BOOL CInventoryBox::net_Spawn(CSE_Abstract* DC)
{
	inherited::net_Spawn	(DC);
	setVisible				(TRUE);
	setEnabled				(TRUE);
	set_tip_text			("inventory_box_use");

	return					TRUE;
}

void CInventoryBox::net_Relcase(CObject* O)
{
	inherited::net_Relcase(O);
}
#include "inventory_item.h"
void CInventoryBox::AddAvailableItems(TIItemContainer& items_container) const
{
	xr_vector<u16>::const_iterator it = m_items.begin();
	xr_vector<u16>::const_iterator it_e = m_items.end();

	for(;it!=it_e;++it)
	{
		PIItem itm = smart_cast<PIItem>(Level().Objects.net_Find(*it));VERIFY(itm);
		items_container.push_back	(itm);
	}
}
