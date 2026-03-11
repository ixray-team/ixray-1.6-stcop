#include "StdAfx.h"
#include "pch_script.h"
#include "UIWpnParams.h"
#include "../../xrUI/UIXmlInit.h"
#include "../Level.h"
#include "game_base_space.h"
#include "../ai_space.h"
#include "../../xrScripts/script_engine.h"
#include "inventory_item_object.h"
#include "UIInventoryUtilities.h"
#include "Weapon.h"
#include "WeaponBinoculars.h"
#include "WeaponKnife.h"
#include "Silencer.h"
#include "../../xrUI/UIHelper.h"

using namespace InventoryUtilities;

// =====================================================================

CUIWpnParams::CUIWpnParams()
{
	AttachChild(&m_textAccuracy);
	AttachChild(&m_textDamage);
	AttachChild(&m_textHandling);
	AttachChild(&m_textRPM);

	AttachChild(&m_progressAccuracy);
	AttachChild(&m_progressDamage);
	AttachChild(&m_progressHandling);
	AttachChild(&m_progressRPM);
}

void CUIWpnParams::InitFromXml(CUIXml& xml_doc)
{
	if (!xml_doc.NavigateToNode("wpn_params", 0))	return;
	CUIXmlInit::InitWindow			(xml_doc, "wpn_params", 0, this);
	if (xml_doc.NavigateToNode("wpn_params:prop_line"))
		m_Prop_line = UIHelper::CreateStatic(xml_doc, "wpn_params:prop_line", this);

	if (xml_doc.NavigateToNode("wpn_params:static_accuracy"))
		m_icon_acc = UIHelper::CreateStatic(xml_doc, "wpn_params:static_accuracy", this);
	if (xml_doc.NavigateToNode("wpn_params:static_damage"))
		m_icon_dam = UIHelper::CreateStatic(xml_doc, "wpn_params:static_damage", this);
	if (xml_doc.NavigateToNode("wpn_params:static_handling"))
		m_icon_han = UIHelper::CreateStatic(xml_doc, "wpn_params:static_handling", this);
	if (xml_doc.NavigateToNode("wpn_params:static_rpm"))
		m_icon_rpm = UIHelper::CreateStatic(xml_doc, "wpn_params:static_rpm", this);

	CUIXmlInit::InitStatic			(xml_doc, "wpn_params:cap_accuracy",		0, &m_textAccuracy);
	CUIXmlInit::InitStatic			(xml_doc, "wpn_params:cap_damage",			0, &m_textDamage);
	CUIXmlInit::InitStatic			(xml_doc, "wpn_params:cap_handling",		0, &m_textHandling);
	CUIXmlInit::InitStatic			(xml_doc, "wpn_params:cap_rpm",				0, &m_textRPM);

	m_progressAccuracy.InitFromXml	( xml_doc, "wpn_params:progress_accuracy" );
	m_progressDamage.InitFromXml	( xml_doc, "wpn_params:progress_damage" );
	m_progressHandling.InitFromXml	( xml_doc, "wpn_params:progress_handling" );
	m_progressRPM.InitFromXml		( xml_doc, "wpn_params:progress_rpm" );

	if(IsGameTypeSingle())
	{
		if (xml_doc.NavigateToNode("wpn_params:static_ammo"))
			m_stAmmo = UIHelper::CreateStatic(xml_doc, "wpn_params:static_ammo", this);
		if (xml_doc.NavigateToNode("wpn_params:cap_ammo_count"))
			m_textAmmoCount = UIHelper::CreateStatic(xml_doc, "wpn_params:cap_ammo_count", this);
		if (xml_doc.NavigateToNode("wpn_params:cap_ammo_count2"))
			m_textAmmoCount2 = UIHelper::CreateStatic(xml_doc, "wpn_params:cap_ammo_count2", this);
		if (xml_doc.NavigateToNode("wpn_params:cap_ammo_types"))
			m_textAmmoTypes = UIHelper::CreateStatic(xml_doc, "wpn_params:cap_ammo_types", this);
		if (xml_doc.NavigateToNode("wpn_params:cap_ammo_used_type"))
			m_textAmmoUsedType = UIHelper::CreateStatic(xml_doc, "wpn_params:cap_ammo_used_type", this);
		if (xml_doc.NavigateToNode("wpn_params:static_ammo_type1"))
			m_stAmmoType1 = UIHelper::Create3dStatic(xml_doc, "wpn_params:static_ammo_type1", this);
		if (xml_doc.NavigateToNode("wpn_params:static_ammo_type2"))
			m_stAmmoType2 = UIHelper::Create3dStatic(xml_doc, "wpn_params:static_ammo_type2", this);
		if (xml_doc.NavigateToNode("wpn_params:static_ammo_type3"))
			m_stAmmoType3 = UIHelper::Create3dStatic(xml_doc, "wpn_params:static_ammo_type3", this);
	}

}

void CUIWpnParams::SetInfo(CInventoryItem* slot_wpn, CInventoryItem& cur_wpn)
{
	CWeapon& weapon = *cur_wpn.cast_weapon();
	CWeapon* slot_weapon = slot_wpn != nullptr ? slot_wpn->cast_weapon() : nullptr;

	float cur_rpm = iFloor(weapon.GetRPM() * 53.0f) / 53.0f;
	float cur_accur = iFloor(weapon.GetAccuracy() * 53.0f) / 53.0f;
	float cur_hand = iFloor(weapon.GetHandling() * 53.0f) / 53.0f;
	float cur_damage = (IsGameTypeSingle()) ?
		iFloor(weapon.GetDamage() * 53.0f) / 53.0f
		: iFloor(weapon.GetDamageMP() * 53.0f) / 53.0f;

	float slot_rpm = cur_rpm;
	float slot_accur = cur_accur;
	float slot_hand = cur_hand;
	float slot_damage = cur_damage;

	if (slot_wpn && (slot_wpn != &cur_wpn))
	{
		slot_rpm = iFloor(slot_weapon->GetRPM() * 53.0f) / 53.0f;
		slot_accur = iFloor(slot_weapon->GetAccuracy() * 53.0f) / 53.0f;
		slot_hand = iFloor(slot_weapon->GetHandling() * 53.0f) / 53.0f;
		slot_damage = (IsGameTypeSingle()) ?
			iFloor(slot_weapon->GetDamage() * 53.0f) / 53.0f
			: iFloor(slot_weapon->GetDamageMP() * 53.0f) / 53.0f;
	}

	m_progressAccuracy.SetTwoPos(cur_accur, slot_accur);
	m_progressDamage.SetTwoPos(cur_damage, slot_damage);
	m_progressHandling.SetTwoPos(cur_hand, slot_hand);
	m_progressRPM.SetTwoPos(cur_rpm, slot_rpm);

	if (IsGameTypeSingle())
	{
		int ammo_count = weapon.GetAmmoMagSize();
		int ammo_count2 = ammo_count;

		if (slot_wpn && slot_weapon)
		{
			ammo_count2 = slot_weapon->GetAmmoMagSize();
		}

		if (m_textAmmoCount2)
		{
			if (ammo_count == ammo_count2)
			{
				m_textAmmoCount2->SetTextColor(color_rgba(170, 170, 170, 255));
			}
			else if (ammo_count < ammo_count2)
			{
				m_textAmmoCount2->SetTextColor(color_rgba(255, 0, 0, 255));
			}
			else
			{
				m_textAmmoCount2->SetTextColor(color_rgba(0, 255, 0, 255));
			}

			string128 str;
			xr_sprintf(str, sizeof(str), "%d", ammo_count);
			m_textAmmoCount2->SetText(str);
		}

		xr_vector<shared_str>& ammo_types = weapon.m_ammoTypes;
		if (!ammo_types.empty())
		{
			if (m_textAmmoUsedType)
			{
				string128 str;
				xr_sprintf(str, sizeof(str), "%s", pSettings->r_string(ammo_types[0].c_str(), "inv_name_short"));
				m_textAmmoUsedType->SetTextST(str);
			}

			if (m_stAmmoType1)
			{
				InventoryIconParams icons_struct = GetInventoryIconParams(ammo_types[0].c_str());
				if (psActorFlags.test(AF_3D_ICONS_INV))
				{
					m_stAmmoType1->SetVisual(icons_struct._3d_static_visual);
					m_stAmmoType1->SetXYZ(icons_struct._3d_static_rotate);
					m_stAmmoType1->SetScaleFactor(icons_struct._3d_static_scale);
				}
				else
				{
					m_stAmmoType1->SetVisual(nullptr);
				}

				m_stAmmoType1->SetShader(GetEquipmentIconsShader(icons_struct.icons_texture));
				float scaleIcon = icons_struct.scaleIcon;
				Frect tex_rect = {};
				tex_rect.x1 = icons_struct.inv_grid_x * INV_GRID_WIDTH(scaleIcon);
				tex_rect.y1 = icons_struct.inv_grid_y * INV_GRID_HEIGHT(scaleIcon);
				tex_rect.x2 = icons_struct.inv_grid_width * INV_GRID_WIDTH(scaleIcon);
				tex_rect.y2 = icons_struct.inv_grid_height * INV_GRID_HEIGHT(scaleIcon);
				tex_rect.rb.add(tex_rect.lt);
				m_stAmmoType1->SetTextureRect(tex_rect);
				m_stAmmoType1->TextureOn();
				m_stAmmoType1->SetStretchTexture(true);
				m_stAmmoType1->SetWndSize(Fvector2().set((tex_rect.x2 - tex_rect.x1) * UI().get_current_kx() / scaleIcon, (tex_rect.y2 - tex_rect.y1) / scaleIcon));

			}

			bool enable_ammo_type_2 = ammo_types.size() > 1;
			if (m_stAmmoType2)
			{
				m_stAmmoType2->Show(enable_ammo_type_2);
			}

			if (enable_ammo_type_2 && m_stAmmoType2)
			{
				InventoryIconParams icons_struct = GetInventoryIconParams(ammo_types[1].c_str());
				if (psActorFlags.test(AF_3D_ICONS_INV))
				{
					m_stAmmoType2->SetVisual(icons_struct._3d_static_visual);
					m_stAmmoType2->SetXYZ(icons_struct._3d_static_rotate);
					m_stAmmoType2->SetScaleFactor(icons_struct._3d_static_scale);
				}
				else
				{
					m_stAmmoType2->SetVisual(nullptr);
				}

				m_stAmmoType2->SetShader(GetEquipmentIconsShader(icons_struct.icons_texture));
				float scaleIcon = icons_struct.scaleIcon;
				Frect tex_rect = {};
				tex_rect.x1 = icons_struct.inv_grid_x * INV_GRID_WIDTH(scaleIcon);
				tex_rect.y1 = icons_struct.inv_grid_y * INV_GRID_HEIGHT(scaleIcon);
				tex_rect.x2 = icons_struct.inv_grid_width * INV_GRID_WIDTH(scaleIcon);
				tex_rect.y2 = icons_struct.inv_grid_height * INV_GRID_HEIGHT(scaleIcon);
				tex_rect.rb.add(tex_rect.lt);
				m_stAmmoType2->SetTextureRect(tex_rect);
				m_stAmmoType2->TextureOn();
				m_stAmmoType2->SetStretchTexture(true);
				m_stAmmoType2->SetWndSize(Fvector2().set((tex_rect.x2 - tex_rect.x1) * UI().get_current_kx() / scaleIcon, (tex_rect.y2 - tex_rect.y1) / scaleIcon));

			}
			
			bool enable_ammo_type_3 = ammo_types.size() > 2;
			if (m_stAmmoType3)
			{
				m_stAmmoType3->Show(enable_ammo_type_3);
			}

			if (enable_ammo_type_3 && m_stAmmoType3)
			{
				InventoryIconParams icons_struct = GetInventoryIconParams(ammo_types[2].c_str());
				if (psActorFlags.test(AF_3D_ICONS_INV))
				{
					m_stAmmoType3->SetVisual(icons_struct._3d_static_visual);
					m_stAmmoType3->SetXYZ(icons_struct._3d_static_rotate);
					m_stAmmoType3->SetScaleFactor(icons_struct._3d_static_scale);
				}
				else
				{
					m_stAmmoType3->SetVisual(nullptr);
				}

				m_stAmmoType3->SetShader(GetEquipmentIconsShader(icons_struct.icons_texture));
				float scaleIcon = icons_struct.scaleIcon;
				Frect tex_rect = {};
				tex_rect.x1 = icons_struct.inv_grid_x * INV_GRID_WIDTH(scaleIcon);
				tex_rect.y1 = icons_struct.inv_grid_y * INV_GRID_HEIGHT(scaleIcon);
				tex_rect.x2 = icons_struct.inv_grid_width * INV_GRID_WIDTH(scaleIcon);
				tex_rect.y2 = icons_struct.inv_grid_height * INV_GRID_HEIGHT(scaleIcon);
				tex_rect.rb.add(tex_rect.lt);
				m_stAmmoType3->SetTextureRect(tex_rect);
				m_stAmmoType3->TextureOn();
				m_stAmmoType3->SetStretchTexture(true);

				m_stAmmoType3->SetWndSize(Fvector2().set((tex_rect.x2 - tex_rect.x1) * UI().get_current_kx() / scaleIcon, (tex_rect.y2 - tex_rect.y1) / scaleIcon));
			}
		}
	}
}


bool CUIWpnParams::Check(CInventoryItem& wpn_section)
{
	if (wpn_section.cast_weapon() == nullptr)
	{
		return false;
	}

	LPCSTR wpn_sect = wpn_section.object().cNameSect().c_str();
	if (pSettings->line_exist(wpn_sect, "fire_dispersion_base"))
	{
		//Alundaio: Most likely a fake weapon or melee weapon
		if (pSettings->line_exist(wpn_sect, "ammo_mag_size") && pSettings->r_u32(wpn_sect, "ammo_mag_size") == 0)
			return false;

		if (wpn_section.cast_weapon_binoculars())
			return false;
		if (wpn_section.cast_weapon_knife())
			return false;

		return true;		
	}
	return false;
}

// -------------------------------------------------------------------------------------------------

CUIConditionParams::CUIConditionParams()
{
	AttachChild( &m_progress );
	AttachChild( &m_text );
}

CUIConditionParams::~CUIConditionParams()
{
}

void CUIConditionParams::InitFromXml(CUIXml& xml_doc)
{
	if (!xml_doc.NavigateToNode("condition_params", 0))	return;
	CUIXmlInit::InitWindow	(xml_doc, "condition_params", 0, this);
	CUIXmlInit::InitStatic	( xml_doc, "condition_params:caption", 0, &m_text );
	m_progress.InitFromXml	( xml_doc, "condition_params:progress_state" );
}

void CUIConditionParams::SetInfo( CInventoryItem const* slot_item, CInventoryItem const& cur_item )
{
	float cur_value  = cur_item.GetConditionToShow() * 100.0f + 1.0f - EPS;
	float slot_value = cur_value;

	if ( slot_item && (slot_item != &cur_item) /*&& (cur_item.object().cNameSect()._get() == slot_item->object().cNameSect()._get())*/ )
	{
		slot_value = slot_item->GetConditionToShow() * 100.0f + 1.0f - EPS;
	}
	m_progress.SetTwoPos( cur_value, slot_value );
}