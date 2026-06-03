////////////////////////////////////////////////////////////////////////////
//	Module 		: UIInventoryUpgradeWnd.cpp
//	Created 	: 06.10.2007
//  Modified 	: 13.03.2009
//	Author		: Evgeniy Sokolov, Prishchepa Sergey
//	Description : inventory upgrade UI window class implementation
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
#include "object_broker.h"
#include "UIInventoryUpgradeWnd.h"
#include "UICellItem.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/UIXmlInit.h"

#include "../Actor.h"
#include "../../xrScripts/script_process.h"
#include "../Inventory.h"

#include "ai_space.h"
#include "alife_simulator.h"
#include "inventory_upgrade_manager.h"
#include "inventory_upgrade.h"
#include "inventory_upgrade_property.h"

#include "UIInventoryUtilities.h"
#include "UIActorMenu.h"
#include "UIItemInfo.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/ui_defs.h"
#include "../../xrUI/UICursor.h"
#include "../Weapon.h"
#include "../WeaponRPG7.h"
#include "../CustomOutfit.h"
#include "../ActorHelmet.h"
#include "../../xrScripts/script_engine.h"
#include "script_game_object.h"
#include "../../xrEngine/xr_input.h"

using namespace luabind;
using namespace inventory::upgrade;
using namespace InventoryUtilities;


const char* g_inventory_upgrade_xml = "inventory_upgrade.xml";

CUIInventoryUpgradeWnd::Scheme::~Scheme()
{
	delete_data(cells);
}

CUIInventoryUpgradeWnd::~CUIInventoryUpgradeWnd()
{
	delete_data(m_schemes);
	xr_delete(m_selectorFrame);
}

void CUIInventoryUpgradeWnd::Init()
{
	CUIXml uiXml;
	uiXml.Load(CONFIG_PATH, UI_PATH, g_inventory_upgrade_xml);

	CUIXmlInit xml_init;
	xml_init.InitWindow(uiXml, "main", 0, this);
	m_border_texture = uiXml.ReadAttrib("border", 0, "texture");
	m_ink_texture = uiXml.ReadAttrib("inking", 0, "texture");

	if (uiXml.NavigateToNode("item_static", 0))
	{
		m_item = new CUI3dStatic();
		m_item->SetAutoDelete(true);
		AttachChild(m_item);
		xml_init.InitStatic(uiXml, "item_static", 0, m_item);
	}
	if (uiXml.NavigateToNode("background", 0))
		m_background = UIHelper::CreateStatic(uiXml, "background", this);

	if (uiXml.NavigateToNode("back", 0))
	{
		m_back = new CUIWindow();
		m_back->SetAutoDelete(true);
		xml_init.InitWindow(uiXml, "back", 0, m_back);
		AttachChild(m_back);
	}

	m_scheme_wnd = new CUIWindow();
	m_scheme_wnd->SetAutoDelete(true);
	AttachChild(m_scheme_wnd);
	xml_init.InitWindow(uiXml, "scheme", 0, m_scheme_wnd);

	m_item_info = new CUIItemInfo();
	if (m_item_info->InitItemInfo("inventory_upgrade_info.xml"))
	{
		m_item_info->SetAutoDelete(true);
		AttachChild(m_item_info);
	}
	else
	{
		xr_delete(m_item_info);
	}

	m_btn_repair = UIHelper::Create3tButton(uiXml, "repair_button", this);
	CUIActorMenu* parent_wnd = smart_cast<CUIActorMenu*>(m_pParentWnd);
	if (parent_wnd)
	{
		// XXX: restore set_hind_wnd?
		//m_btn_repair->set_hint_wnd(parent_wnd->get_hint_wnd());
	}

	if (uiXml.NavigateToNode("disassemble_button", 0))
	{
		m_btn_disassemble = UIHelper::Create3tButton(uiXml, "disassemble_button", this);
	}

	LoadCellsBacks(uiXml);
	LoadSchemes(uiXml);

	m_selectorFrame = new CUIFrameWindow();
	if (m_selectorFrame->InitTexture("ui_inv_item_selector_tri", false))
	{
		m_selectorFrame->SetVisible(false);
		AttachChild(m_selectorFrame);
	}
	else
	{
		xr_delete(m_selectorFrame);
	}
}

void CUIInventoryUpgradeWnd::InitInventory(CUICellItem* cellItem, bool can_upgrade)
{
	if (m_item_info != nullptr)
	{
		m_item_info->InitItem(cellItem);
	}

	if (cellItem == nullptr)
	{
		m_scheme_wnd->DetachAll();
		m_scheme_wnd->Show(false);
		return;
	}

	m_inv_item = static_cast<PIItem>(cellItem ? cellItem->m_pData : nullptr);

	const char* upgrIconsTexture = {};
	if (m_inv_item != nullptr)
	{
		upgrIconsTexture = pSettings->read_if_exists<str_c>(m_inv_item->m_section_id,"upgr_icons_texture",nullptr);
	}

	// Загружаем картинку
	if (m_item != nullptr && m_inv_item != nullptr)
	{
		bool is_shader = false;
		if (m_inv_item->cast_weapon())
		{
			is_shader = true;
			m_item->SetShader(GetWeaponUpgradeIconsShader(upgrIconsTexture));
			if (m_inv_item->cast_weapon_rpg7())
			{
				m_item->SetShader(GetOutfitUpgradeIconsShader(upgrIconsTexture));
			}
		}
		else if (m_inv_item->cast_outfit() || m_inv_item->cast_helmet())
		{
			is_shader = true;
			m_item->SetShader(GetOutfitUpgradeIconsShader(upgrIconsTexture));
		}

		InventoryIconParams icons_struct = GetInventoryIconParams(m_inv_item->m_section_id.c_str());
        if (psActorFlags.test(AF_3D_ICONS_INV))
        {
			IRenderVisual* prevVisual = m_item->GetVisual();
            m_item->SetVisual(icons_struct._3d_static_visual);
			if (m_item->GetVisual() != prevVisual)
			{
				m_item->SetXYZ(icons_struct._3d_static_rotate);
			}
            m_item->SetScaleFactor(icons_struct._3d_static_scale);
            m_item->SetBonesVisible(m_inv_item->object().Visual()->dcast_PKinematics());
        }
        else
            m_item->SetVisual(nullptr);

		if (is_shader)
		{
			Irect item_upgrade_grid_rect = m_inv_item->GetUpgrIconRect();
			Frect texture_rect;
			texture_rect.lt.set(item_upgrade_grid_rect.x1, item_upgrade_grid_rect.y1);
			texture_rect.rb.set(item_upgrade_grid_rect.x2, item_upgrade_grid_rect.y2);
			texture_rect.rb.add(texture_rect.lt);
			m_item->GetUIStaticItem().SetTextureRect(texture_rect);
			m_item->TextureOn();
			m_item->SetStretchTexture(true);
			Fvector2 v_r = Fvector2().set(item_upgrade_grid_rect.x2, item_upgrade_grid_rect.y2);
			if (UI().is_widescreen())
				v_r.x *= 0.8f;

			m_item->GetUIStaticItem().SetSize(v_r);
			m_item->SetWidth(v_r.x);
			m_item->SetHeight(v_r.y);
			m_item->Show(true);
		}
		else
		{
			m_item->Show(false);
		}
	}

	m_scheme_wnd->DetachAll();
	m_scheme_wnd->Show(false);

	if (m_back != nullptr)
	{
		m_back->DetachAll();
		m_back->Show(false);
	}

	m_btn_repair->Enable(false);
	if (m_btn_disassemble != nullptr)
	{
		m_btn_disassemble->Enable(false);
	}

	if (ai().get_alife() && m_inv_item != nullptr)
	{
		if (install_item(*m_inv_item, can_upgrade))
		{
			UpdateAllUpgrades();
		}
	}
}

// ------------------------------------------------------------------------------------------

void CUIInventoryUpgradeWnd::Show(bool status)
{
	inherited::Show(status);
	UpdateAllUpgrades();
}

void CUIInventoryUpgradeWnd::Update()
{
	inherited::Update();
	if (m_selectorFrame)
	{
		m_selectorFrame->SetVisible(pInput->GetControllerMode() && m_selector_shown);
	}
}

void CUIInventoryUpgradeWnd::Reset()
{
	for (const auto& scheme : m_schemes)
	{
		for (const auto& scheme_cell_item : scheme->cells)
		{
			scheme_cell_item->Reset();
			if (scheme_cell_item->m_point != nullptr)
			{
				scheme_cell_item->m_point->Reset();
			}
		}
	}
	inherited::Reset();
	inherited::ResetAll();
}

void CUIInventoryUpgradeWnd::UpdateAllUpgrades()
{
	if (m_current_scheme == nullptr || m_inv_item == nullptr)
	{
		return;
	}

	for (const auto& ui_item : m_current_scheme->cells)
	{
		ui_item->update_item(m_inv_item);
	}
}

void CUIInventoryUpgradeWnd::SetCurScheme(const shared_str& id)
{
	for (const auto& scheme : m_schemes)
	{
		if (scheme->name._get() == id._get())
		{
			m_current_scheme = scheme;
			return;
		}
	}

	R_ASSERT2(0, make_string<const char*>("Scheme <%s> does not loaded !", id.c_str()));
}

bool CUIInventoryUpgradeWnd::CheckEnableDisassembleButton(CInventoryItem& inv_item)
{
	const char* item_name = inv_item.m_section_id.c_str();
	float condition = inv_item.GetCondition();

	luabind::functor<bool> funct;

	R_ASSERT2(ai().script_engine().functor("inventory_upgrades.gunsl_need_disassemble_button", funct), make_string<const char*>("Failed to get functor <inventory_upgrades.gunsl_need_disassemble_button>, item = %s", item_name));

	return funct(item_name, condition);
}

bool CUIInventoryUpgradeWnd::install_item(CInventoryItem& inv_item, bool can_upgrade)
{
	m_scheme_wnd->DetachAll();
	if (m_back != nullptr)
	{
		m_back->DetachAll();
	}

	bool CanBeRepared = inv_item.cast_weapon() != nullptr || inv_item.cast_outfit() != nullptr || inv_item.cast_helmet() != nullptr;
	m_btn_repair->Enable(CanBeRepared && inv_item.GetCondition() < 0.99f);

	if (m_btn_disassemble != nullptr)
	{
		m_btn_disassemble->Enable(CanBeRepared && CheckEnableDisassembleButton(inv_item));
	}

	if (!can_upgrade)
	{
#ifdef DEBUG
		Msg("Inventory item <%s> cannot upgrade - Mechanic say.", inv_item.m_section_id.c_str());
#endif // DEBUG
		m_current_scheme = nullptr;
		return false;
	}

	const char* scheme_name = Level().m_upgrade_manager->get_item_scheme(inv_item);
	if (!scheme_name)
	{
#ifdef DEBUG
		Msg("Inventory item <%s> does not contain upgrade scheme.", inv_item.m_section_id.c_str());
#endif // DEBUG
		m_current_scheme = nullptr;
		return false;
	}

	SetCurScheme(scheme_name);

	for (const auto& ui_item : m_current_scheme->cells)
	{
		m_scheme_wnd->AttachChild(ui_item);

		if (m_back != nullptr && ui_item->m_point)
		{
			m_back->AttachChild(ui_item->m_point);
		}

		const char* upgrade_name = Level().m_upgrade_manager->get_upgrade_by_index(inv_item, ui_item->get_scheme_index());
		ui_item->init_upgrade(upgrade_name, inv_item);

		Upgrade_type* upgrade_p = Level().m_upgrade_manager->get_upgrade(upgrade_name);
		VERIFY(upgrade_p);
		for (u8 i = 0; i < max_properties_count; i++)
		{
			shared_str prop_name = upgrade_p->get_property_name(i);
			if (prop_name.size())
			{
				Property_type* prop_p = Level().m_upgrade_manager->get_property(prop_name);
				VERIFY(prop_p);
			}
		}

		ui_item->set_texture(UIUpgrade::LAYER_ITEM, upgrade_p->icon_name());
		ui_item->set_texture(UIUpgrade::LAYER_POINT, m_point_textures[UIUpgrade::STATE_ENABLED].c_str()); //default
		ui_item->set_texture(UIUpgrade::LAYER_COLOR, m_cell_textures[UIUpgrade::STATE_ENABLED].c_str()); //default
		ui_item->set_texture(UIUpgrade::LAYER_BORDER, m_border_texture.c_str());
		ui_item->set_texture(UIUpgrade::LAYER_INK, m_ink_texture.c_str());
	}

	m_scheme_wnd->Show(true);

	if (m_item != nullptr)
	{
		m_item->Show(true);
	}

	if (m_back != nullptr)
	{
		m_back->Show(true);
	}

	UpdateAllUpgrades();
	return true;
}

UIUpgrade* CUIInventoryUpgradeWnd::FindUIUpgrade(Upgrade_type const* upgr)
{
	if (m_current_scheme == nullptr)
	{
		return nullptr;
	}

	for (const auto& ui_item : m_current_scheme->cells)
	{
		Upgrade_type* i_upgr = ui_item->get_upgrade();
		if (upgr == i_upgr)
		{
			return ui_item;
		}
	}

	return nullptr;
}

bool CUIInventoryUpgradeWnd::DBClickOnUIUpgrade(Upgrade_type const* upgr)
{
	UpdateAllUpgrades();
	UIUpgrade* uiupgr = FindUIUpgrade(upgr);
	if (uiupgr)
	{
		uiupgr->OnClick();
		return true;
	}

	return false;
}

void CUIInventoryUpgradeWnd::AskUsing(const char* text, const char* upgrade_name)
{
	VERIFY(m_inv_item);
	VERIFY(upgrade_name);
	VERIFY(m_pParentWnd);

	UpdateAllUpgrades();

	m_cur_upgrade_id = upgrade_name;

	CUIActorMenu* parent_wnd = smart_cast<CUIActorMenu*>(m_pParentWnd);
	if (parent_wnd)
	{
		parent_wnd->CallMessageBoxYesNo(text);
	}
}

void CUIInventoryUpgradeWnd::OnMesBoxYes()
{
	if (Level().m_upgrade_manager->upgrade_install(*m_inv_item, m_cur_upgrade_id, false))
	{
		VERIFY(m_pParentWnd);
		CUIActorMenu* parent_wnd = smart_cast<CUIActorMenu*>(m_pParentWnd);
		if (parent_wnd)
		{
			//Alundaio: tell script that item has been upgraded
			luabind::functor<void>	funct;
			ai().script_engine().functor("inventory_upgrades.effect_upgrade_item", funct);
			if (funct)
			{
				CGameObject* GO = m_inv_item->cast_game_object();
				funct(GO->lua_game_object(), m_cur_upgrade_id);
			}
			//-Alundaio
			parent_wnd->UpdateActor();
			parent_wnd->SeparateUpgradeItem();
		}
	}
	UpdateAllUpgrades();
}

void CUIInventoryUpgradeWnd::HighlightHierarchy(shared_str const& upgrade_id)
{
	UpdateAllUpgrades();
	Level().m_upgrade_manager->highlight_hierarchy(*m_inv_item, upgrade_id);
}

void CUIInventoryUpgradeWnd::ResetHighlight()
{
	UpdateAllUpgrades();
	Level().m_upgrade_manager->reset_highlight(*m_inv_item);
}

void CUIInventoryUpgradeWnd::set_info_cur_upgrade(Upgrade_type* upgrade)
{
	UIUpgrade* uiu = FindUIUpgrade(upgrade);
	if (uiu)
	{
		if (!pInput->GetControllerMode() && Device.dwTimeContinual < uiu->FocusReceiveTime() + (m_item_info ? m_item_info->delay : 0))
		{
			upgrade = nullptr; // visible = false
		}
	}
	else
	{
		upgrade = nullptr;
	}

	CUIActorMenu* parent_wnd = smart_cast<CUIActorMenu*>(m_pParentWnd);
	if (parent_wnd)
	{
		if (parent_wnd->SetInfoCurUpgrade(upgrade, m_inv_item))
		{
			UpdateAllUpgrades();
		}
	}
}

void CUIInventoryUpgradeWnd::DeInitInventory()
{
	set_info_cur_upgrade(nullptr);
	if (m_back != nullptr)
	{
		m_back->DetachAll();
		m_back->Show(false);
	}

	if (m_item != nullptr)
	{
		m_item->DetachAll();
		m_item->Show(false);
	}

	m_btn_repair->Enable(false);
	m_scheme_wnd->DetachAll();
	m_scheme_wnd->Show(false);

	if (m_btn_disassemble != nullptr)
	{
		m_btn_disassemble->Enable(false);
	}

	m_inv_item = nullptr;
}

bool CUIInventoryUpgradeWnd::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	if (m_item && m_item->GetVisual())
	{
		if (mouse_action == WINDOW_LBUTTON_UP)
		{
			if (m_item->IsCaptMoving())
				m_item->SetCaptMoving(false);
		}
		if (m_item->CursorOverWindow() && mouse_action == WINDOW_LBUTTON_DOWN)
		{
			m_item->SetCaptMoving(true);
		}

		// bool bShift = !!pInput->iGetAsyncKeyState(SDL_SCANCODE_LSHIFT);
		bool bCtrl = !!pInput->iGetAsyncKeyState(SDL_SCANCODE_LCTRL);
		if (m_item->IsCaptMoving() && m_item->CursorOverWindow())
		{
			// need to fix input invertion on 180 degs rotate
			Fvector xyz = m_item->GetXYZ();
			Fvector2 delta_pos = GetUICursor().GetCursorPositionDelta();
			if (bCtrl)
				xyz.z += delta_pos.y / 30.f;
			// else if (bShift)
			// xyz.x += delta_pos.y / 50.f;
			else
				xyz.y += delta_pos.x / 40.f;
			m_item->SetXYZ(xyz.x, xyz.y, xyz.z);
		}
	}
	return inherited::OnMouseAction(x, y, mouse_action);
}

// Controller UI

//  Uses wnd angles and midside points to find nearest point and switch to closest nearby window in the needed direction
bool CUIInventoryUpgradeWnd::SelectorMove(eUIDirection4 dir)
{
	if (!m_selectedUpgrade)
		return false;

	Fvector2 aSrcPoint;
	Fvector2 srcPoint;
	m_selectedUpgrade->GetAbsolutePos(aSrcPoint);
	srcPoint = aSrcPoint;

	switch (dir)
	{
	case eUIDirection4_Left:
		{
			srcPoint.y += m_selectedUpgrade->GetHeight() / 2.0f;
		}
		break;
	case eUIDirection4_Right:
		{
			srcPoint.y += m_selectedUpgrade->GetHeight() / 2.0f;
			srcPoint.x += m_selectedUpgrade->GetWidth();
		}
		break;
	case eUIDirection4_Up:
		{
			srcPoint.x += m_selectedUpgrade->GetWidth() / 2.0f;
		}
		break;
	case eUIDirection4_Down:
		{
			srcPoint.x += m_selectedUpgrade->GetWidth() / 2.0f;
			srcPoint.y += m_selectedUpgrade->GetHeight();
		}
		break;
	}
	
	// Find the closest object
	double minDistSquared = pow(5000.0, 2);
	xr_vector<CUIWindow*>& wndList = m_scheme_wnd->GetChildWndList();
	UIUpgrade* pDstWnd = nullptr;
	for (xr_vector<CUIWindow*>::iterator it = wndList.begin(); it != wndList.end(); ++it)
	{
		CUIWindow* pWnd = *it;
		if (pWnd == m_selectedUpgrade || !dynamic_cast<UIUpgrade*>(pWnd))
			continue;

		Fvector2 dstPos;
		pWnd->GetAbsolutePos(dstPos);
		float dstWidth = pWnd->GetWidth();
		float dstHeight = pWnd->GetHeight();
		float dstHalfWidth = dstWidth / 2.0f;
		float dstHalfHeight = dstHeight / 2.0f;

		Fvector2 points[] = {
			// 4 Angles
			{dstPos.x, dstPos.y},
			{dstPos.x + dstWidth, dstPos.y},
			{dstPos.x + dstWidth, dstPos.y + dstHeight},
			{dstPos.x, dstPos.y + dstHeight},

			// and 4 side - middles
			{dstPos.x + dstHalfWidth, dstPos.y},
			{dstPos.x + dstWidth, dstPos.y + dstHalfHeight},
			{dstPos.x + dstHalfWidth, dstPos.y + dstHeight},
			{dstPos.x, dstPos.y + dstHalfHeight},
		};

		// Get the distance to closest point from the list
		double dstDistanceSqr = pow(5000.0, 2);
		for (int i = 0; i < 8; ++i)
		{
			float diffX = points[i].x - srcPoint.x;
			float diffY = points[i].y - srcPoint.y;

			if (dir == eUIDirection4_Up || dir == eUIDirection4_Down)
			{
				if ((diffY < 0 && dir == eUIDirection4_Down) || (diffY > 0 && dir == eUIDirection4_Up))
					continue;
				if (fabs(diffX) > 0.15f * fabs(diffY))
					continue;
				if (fabs(diffX) > fabs(diffY))
					continue;
			}
			else if (dir == eUIDirection4_Left || dir == eUIDirection4_Right)
			{
				if ((diffX < 0 && dir == eUIDirection4_Right) || (diffX > 0 && dir == eUIDirection4_Left))
					continue;
				if (fabs(diffY) > fabs(diffX))
					continue;
			}

			float distanceSquared = pow(diffX, 2) + pow(diffY, 2);
			if (distanceSquared < dstDistanceSqr)
				dstDistanceSqr = distanceSquared;
		}
		if (dstDistanceSqr < minDistSquared)
		{
			minDistSquared = dstDistanceSqr;
			pDstWnd = static_cast<UIUpgrade*>(pWnd);
		}
	}

	if (pDstWnd)
	{
		SetUpgradeSelected(pDstWnd);

		CUIActorMenu* pMenu = static_cast<CUIActorMenu*>(GetParent());
		SetInfoVisible(pMenu->NeedToShowInfos());
		return true;
	}

	return false;
}

void CUIInventoryUpgradeWnd::SetUpgradeSelected(UIUpgrade* pUpgrade)
{
	if (m_selectedUpgrade == pUpgrade)
		return;

	if (m_selectedUpgrade)
	{
		m_selectedUpgrade->SetSelected(false);
	}

	m_selectedUpgrade = pUpgrade;

	if (!m_selectorFrame)
	{
		return;
	}

	if (pUpgrade)
	{
		m_selectedUpgrade->SetSelected(true);

		// Update frame
		Fvector2 frmSize = pUpgrade->GetWndSize();
		Fvector2 frmPos = pUpgrade->GetWndPos();
		frmPos.add(m_scheme_wnd->GetWndPos());
		m_selectorFrame->SetWndSize(frmSize);
		m_selectorFrame->SetWndPos(frmPos);
		m_selector_shown = true;
	}
	else
	{
		m_selector_shown = false;
	}
}


void CUIInventoryUpgradeWnd::SetActiveForController(bool status)
{
	if (status)
	{
		if (m_current_scheme->cells.size() > 0)
		{
			SetUpgradeSelected(m_current_scheme->cells.front());
			CUIActorMenu* pMenu = static_cast<CUIActorMenu*>(GetParent());
			SetInfoVisible(pMenu->NeedToShowInfos());
		}
	}
	else
	{
		SetUpgradeSelected(nullptr);
		SetInfoVisible(false);
	}
}


bool CUIInventoryUpgradeWnd::CanApplySelectedUpgrade()
{
	return (m_selectedUpgrade && m_selectedUpgrade->CanBeApplied());
}

void CUIInventoryUpgradeWnd::ApplySelectedUpgrade()
{
	m_selectedUpgrade->OnClick();
}

void CUIInventoryUpgradeWnd::SetInfoVisible(bool status)
{
	if (!status)
		set_info_cur_upgrade(nullptr);
	else if (m_selectedUpgrade)
	{
		set_info_cur_upgrade(m_selectedUpgrade->get_upgrade());
	}
}