#include "StdAfx.h"
#include "pch_script.h"
#include "InventoryBox.h"
#include "Level.h"
#include "../xrScripts/script_callback_ex.h"
#include "ui/UIActorMenu.h"
#include "UIGameCustom.h"
#include "ui/UICarBodyWnd.h"

void CInventoryBox::OnEvent(NET_Packet& P, u16 type)
{
	inherited::OnEvent(P, type);

	switch (type)
	{
	case GE_TRADE_BUY:
	case GE_OWNERSHIP_TAKE:
	{
		u16 id;
		P.r_u16(id);
		CObject* itm = Level().Objects.net_Find(id);
		VERIFY(itm);
		m_items.push_back(id);
		itm->H_SetParent(this);
		itm->setVisible(false);
		itm->setEnabled(false);

		PIItem pIItem = itm->cast_inventory_item();
		VERIFY(pIItem);
		if (CurrentGameUI())
		{
			// DO NOT remove menu mode check, since CoP inventory is not always used for deadbody, unlike SoC one
			if (CurrentGameUI()->GetCarbodyMenu() && CurrentGameUI()->GetCarbodyMenu()->GetMenuMode() == mmDeadBodySearch)
			{
				if (this == CurrentGameUI()->GetCarbodyMenu()->GetInvBox())
				{
					CurrentGameUI()->OnInventoryAction(pIItem, GE_OWNERSHIP_TAKE);
				}
			}
		};
	}break;

	case GE_TRADE_SELL:
	case GE_OWNERSHIP_REJECT:
	{
		u16 id;
		P.r_u16(id);
		CObject* itm = Level().Objects.net_Find(id);
		VERIFY(itm);
		xr_vector<u16>::iterator it;
		it = std::find(m_items.begin(), m_items.end(), id);
		VERIFY(it != m_items.end());
		m_items.erase(it);

		bool just_before_destroy = !P.r_eof() && P.r_u8();
		bool dont_create_shell = (type == GE_TRADE_SELL) || just_before_destroy;

		itm->H_SetParent(nullptr, dont_create_shell);

		if (!IsGameTypeSingle() && CurrentGameUI())
		{
			// DO NOT remove menu mode check, since CoP inventory is not always used for deadbody, unlike SoC one
			if (CurrentGameUI()->GetCarbodyMenu() && CurrentGameUI()->GetCarbodyMenu()->GetMenuMode() == mmDeadBodySearch)
			{
				if (this == CurrentGameUI()->GetCarbodyMenu()->GetInvBox())
				{
					CurrentGameUI()->OnInventoryAction(itm->cast_inventory_item(), GE_OWNERSHIP_REJECT);
				}
			}
		}

		if (m_in_use)
		{
			CGameObject* GO = itm->cast_game_object();
			Actor()->callback(GameObject::eInvBoxItemTake)(this->lua_game_object(), GO->lua_game_object());
		}
	}break;
	};
}

void CInventoryBox::UpdateCL()
{
	inherited::UpdateCL();
}

void CInventoryBox::net_Destroy()
{
	inherited::net_Destroy();
}

bool CInventoryBox::net_Spawn(CSE_Abstract* DC)
{
	inherited::net_Spawn(DC);
	setVisible(true);
	setEnabled(true);
	set_tip_text("inventory_box_use");

	if (CSE_ALifeInventoryBox* pSE_box = smart_cast<CSE_ALifeInventoryBox*>(DC))
	{
		m_can_take = pSE_box->m_can_take;
		m_closed = pSE_box->m_closed;
		set_tip_text(pSE_box->m_tip_text.c_str());
	}

	SpatialComponent->type |= ESPATIAL_TYPE::INV_BOX;

	return true;
}

void CInventoryBox::net_Relcase(CObject* O)
{
	inherited::net_Relcase(O);
}

void CInventoryBox::AddAvailableItems(TIItemContainer& items_container) const
{
	for (const u16& item : m_items)
	{
		CObject* finded_object = Level().Objects.net_Find(item);
		if(!IVERIFY_M(finded_object, "Unable to find item with id [%d] from box [%d]", item, ID()))
		{
			continue;
		}
		PIItem itm = finded_object->cast_inventory_item();
		if(IVERIFY(itm)){
			items_container.push_back(itm);
		}
	}
}

void CInventoryBox::set_can_take(bool status)
{
	m_can_take = status;
	SE_update_status();
}

void CInventoryBox::set_closed(bool status, const char* reason)
{
	m_closed = status;

	if (reason && xr_strlen(reason))
	{
		set_tip_text(reason);
	}
	else
	{
		set_tip_text("inventory_box_use");
	}

	SE_update_status();
}

void CInventoryBox::SE_update_status()
{
	NET_Packet P;
	CGameObject::u_EventGen(P, GE_INV_BOX_STATUS, ID());
	P.w_u8((m_can_take) ? 1 : 0);
	P.w_u8((m_closed) ? 1 : 0);
	P.w_stringZ(tip_text());
	CGameObject::u_EventSend(P);
}

void CTradeStorageBox::Load(LPCSTR section)
{
	CInventoryBox::Load(section);
	auto FilterSection = pSettings->r_string_nullable(section, "filter_section");
	if (!FilterSection)
	{
		return;
	}
	auto Sect = pSettings->r_section_nullable(FilterSection);
	if (!I_ASSERT_M(Sect, "Unable to find section [%s]", FilterSection))
	{
		return;
	}

	auto ProcessMultipleTradeItemsSettingsFunc = [&](this auto self, shared_str loc_section)
	{
		auto Section = pSettings->r_section_nullable(loc_section);
		if(!I_ASSERT_M(Section, "Unable to find section [%s]", loc_section.c_str()))
		{
			return;
		}

		for(auto& Item : Section->Data)
		{
			if (!pSettings->section_exist(Item.first))
			{
				if(Item.first.c_str()[0] == '$')
				{
					LPCSTR section_name = Item.first.c_str()+1;
					self(section_name);
					continue;
				}
				Msg("! Section [%s] (parsing trade list [%s]) doesn't exist!", Item.first.c_str(), loc_section.c_str());
				continue;
			}
			m_ItemFilter.emplace(Item.first);
		}
	};
	
	for (auto& elem : Sect->Data)
	{
		if (elem.first.c_str()[0] == '$')
		{
			ProcessMultipleTradeItemsSettingsFunc(elem.first.c_str()+1);
		} else
		{
			if (!pSettings->section_exist(elem.first))
			{
				Msg("! Section [%s] doesn't exist!", elem.first.c_str());
				continue;
			}
			m_ItemFilter.emplace(elem.first);
		}
	}
}
