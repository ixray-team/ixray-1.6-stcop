#include "StdAfx.h"
#include "UIActorMenu.h"
#include "UIDragDropListEx.h"
#include "UICharacterInfo.h"
#include "UIInventoryUtilities.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "UICellItem.h"
#include "UICellItemFactory.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"

#include "xrMessages.h"
#include "../alife_registry_wrappers.h"
#include "../GameObject.h"
#include "../InventoryOwner.h"
#include "../Inventory.h"
#include "../inventory_item.h"
#include "../InventoryBox.h"
#include "../../xrEngine/string_table.h"
#include "../ai/monsters/basemonster/base_monster.h"
#include "../Car.h"

// -------------------------------------------------------------------------------------------------

void CUIActorMenu::InitDeadBodySearchMode()
{
	m_pDeadBodyBagList->Show		(true);
	m_LeftBackground->Show			(true);
	m_PartnerBottomInfo->Show		(true);
	m_PartnerWeight->Show			(true);
	m_takeall_button->Show			(true);

	if (m_putall_button != nullptr)
	{
		m_putall_button->Show(true);
	}

	if ( m_pPartnerInvOwner )
	{
		m_PartnerCharacterInfo->Show(true);
	}
	else
	{
		m_PartnerCharacterInfo->Show(false);
	}

	InitInventoryContents			(m_pInventoryBagList);
	UpdateDeadBodyBagList();

	CBaseMonster* monster = m_pPartnerInvOwner != nullptr ? m_pPartnerInvOwner->cast_base_monster() : nullptr;
	CCar* pCar = m_pPartnerInvOwner != nullptr ? m_pPartnerInvOwner->cast_car() : nullptr;
	
	//only for partner, box = no, monster = no
	if (m_pPartnerInvOwner != nullptr && monster == nullptr && pCar == nullptr)
	{
		CInfoPortionWrapper						known_info_registry;
		known_info_registry.registry().init		(m_pPartnerInvOwner->object_id());
		KNOWN_INFO_VECTOR& known_infos			= known_info_registry.registry().objects();

		auto it_					= known_infos.begin();
		for(int i=0;it_!=known_infos.end();++it_,++i)
		{
			NET_Packet					P;
			CGameObject::u_EventGen		(P,GE_INFO_TRANSFER, m_pActorInvOwner->object_id());
			P.w_u16						(0);
			P.w_stringZ					(it_->info_id.c_str());
			P.w_u8						(1);
			CGameObject::u_EventSend	(P);
		}
		known_infos.clear	();
	}
	UpdateDeadBodyBag();
	SetAreaSelectionTo(m_pDeadBodyBagList);
}

void CUIActorMenu::DeInitDeadBodySearchMode()
{
	m_pDeadBodyBagList->Show		(false);
	m_PartnerCharacterInfo->Show	(false);
	m_LeftBackground->Show			(false);
	m_PartnerBottomInfo->Show		(false);
	m_PartnerWeight->Show			(false);
	m_takeall_button->Show			(false);

	if (m_putall_button != nullptr)
	{
		m_putall_button->Show(false);
	}

	if ( m_pInvBox )
	{
		m_pInvBox->set_in_use( false );
	}
}

void CUIActorMenu::UpdateDeadBodyBag()
{
	string64 buf;

	const char* kg_str = g_pStringTable->translate( "st_kg" ).c_str();
	float total	= CalcItemsWeight( m_pDeadBodyBagList );
	xr_sprintf( buf, "%.1f %s", total, kg_str );
	m_PartnerWeight->SetText( buf );
	m_PartnerWeight->AdjustWidthToText();

	Fvector2 pos = m_PartnerWeight->GetWndPos();
	pos.x = m_PartnerWeight_end_x - m_PartnerWeight->GetWndSize().x - 5.0f;
	m_PartnerWeight->SetWndPos( pos );
	pos.x = pos.x - m_PartnerBottomInfo->GetWndSize().x - 5.0f;
	m_PartnerBottomInfo->SetWndPos( pos );
}
