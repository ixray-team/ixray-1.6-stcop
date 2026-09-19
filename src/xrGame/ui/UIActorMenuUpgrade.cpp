//#include "StdAfx.h"
#include "StdAfx.h"
#include "pch_script.h"
#include "UIActorMenu.h"
#include "UIInventoryUpgradeWnd.h"
#include "UIInvUpgradeInfo.h"
#include "../../xrEngine/xr_input.h"
#include "UIDragDropListEx.h"
#include "UIDragDropReferenceList.h"
#include "UICharacterInfo.h"

#include "../inventory_item.h"
#include "UICellItem.h"
#include "../InventoryOwner.h"
#include "../Inventory.h"
#include "../Actor.h"
#include "UIGameSP.h"
#include "../../xrUI/Widgets/UI3tButton.h"

#include "inventory_upgrade.h"
#include "UITalkWnd.h"

void CUIActorMenu::InitUpgradeMode()
{
	m_PartnerCharacterInfo->Show( true );
	m_PartnerMoney->Show( false );
	m_pUpgradeWnd->Show( true );
	if (m_pQuickSlot)
		m_pQuickSlot->Show(true);
	
	InitInventoryContents( m_pInventoryBagList );
	if (m_pPartnerInvOwner)
		m_pPartnerInvOwner->StartTrading();

	SetAreaSelectionTo(m_pInventoryBagList);
}

void CUIActorMenu::DeInitUpgradeMode()
{
	m_PartnerCharacterInfo->Show(false);
	m_pUpgradeWnd->Show(false);
	m_pUpgradeWnd->DeInitInventory();

	if ( m_upgrade_selected )
	{
		m_upgrade_selected->Mark( false );
		m_upgrade_selected = nullptr;
	}
	if ( m_pPartnerInvOwner )
	{
		m_pPartnerInvOwner->StopTrading();
	}

	if(!CurrentGameUI())
		return;
  
	if(CurrentGameUI()->TalkMenu && CurrentGameUI()->TalkMenu->IsActiveTalkUi())
	{
		CurrentGameUI()->TalkMenu->NeedUpdateQuestions();
	}
}
