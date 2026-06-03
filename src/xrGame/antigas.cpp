#include "stdafx.h"
#include "inventory_item.h"
#include "ai_space.h"
#include "Inventory.h"
#include "InventoryOwner.h"
#include "Actor.h"
#include "Entity.h"
#include "ai_object_location.h"
#include "alife_simulator_base.h"
#include "alife_simulator.h"
#include "antigas_filter.h"
#include "antigas.h"
#include "CustomOutfit.h"
#include "ActorHelmet.h"
#include "InventoryBox.h"

IAntigas::IAntigas()
{
	selfObject = nullptr;

	bIsHelmet = false;
	bIsOutfit = false;
	bIsFilterInstalled = false;
	bIsAllowed = false;

	filter_breath_sounds.clear();
	breath_sounds.clear();

	fFilterIconWidth = 30;
	fFilterIconHeight = 30;

	fFilterIconOffsetX = (50 * 2) - (fFilterIconWidth * 1.9);
	fFilterIconOffsetY = (50 * 2) - (fFilterIconHeight * 1.7);

	fFilterCondition = 0.0f;
	m_filter_section = "";

	m_FilterProtection.resize(ALife::eHitTypeMax);
	m_FilterDamage.resize(ALife::eHitTypeMax);
	m_InitialItemProtections.resize(ALife::eHitTypeMax);

	for (int i = 0; i < ALife::eHitTypeMax; i++)
	{
		m_FilterProtection[i] = 0.0f;
		m_FilterDamage[i] = 0.0f;
		m_InitialItemProtections[i] = 0.0f;
	}
}

IAntigas::~IAntigas()
{
}

void IAntigas::OnUpdate(CObject* O, const Fvector& pos)
{
	bool is_playing = false;
	size_t cnt_filter = filter_breath_sounds.size();
	size_t cnt_no_filter = breath_sounds.size();

	if (IsFilterInstalled()) {
		if (cnt_filter > 0)
		{
			for (size_t i = 0; i < cnt_filter; i++)
			{
				if (filter_breath_sounds[i].is_playing())
				{
					is_playing = true;
					break;
				}
			}

			if (!is_playing)
			{
				filter_breath_sounds[Random.randI(cnt_filter)].play_at_pos(O, pos);
			}
		}
	}
	else
	{
		if (cnt_no_filter > 0)
		{
			for (size_t i = 0; i < cnt_no_filter; i++)
			{
				if (breath_sounds[i].is_playing())
				{
					is_playing = true;
					break;
				}
			}

			if (!is_playing)
			{
				breath_sounds[Random.randI(cnt_no_filter)].play_at_pos(O, pos);
			}
		}
	}
}


float IAntigas::GetFilterIconOffsetX()
{
	return fFilterIconOffsetX;
}

float IAntigas::GetFilterIconOffsetY()
{
	return fFilterIconOffsetY;
}

float IAntigas::GetFilterIconWidth()
{
	return fFilterIconWidth;
}

float IAntigas::GetFilterIconHeight()
{
	return fFilterIconHeight;
}

void IAntigas::AddSound(const char* snd_path, bool isFilter)
{
	ref_sound r_sound;
	string_path s_path = {};
	FS.update_path(s_path, _game_sounds_, snd_path);
	if (FS.exist(s_path))
	{
		r_sound.create(snd_path, st_Effect, sg_Undefined);
		if (isFilter)
		{
			filter_breath_sounds.push_back(r_sound);
		} 
		else
		{
			breath_sounds.push_back(r_sound);
		}
	}
}

void IAntigas::Load(const char* section)
{
	SetAllowed(pSettings->read_if_exists<bool>(section, "is_antigas", false));
	if (IsAllowed())
	{
		if (pSettings->line_exist(section, "antigas_allow_filter_sections"))
		{
			m_AllowedFilterSections.clear();

			const char* separated = pSettings->r_string(section, "antigas_allow_filter_sections");
			int count = _GetItemCount(separated);

			for (int i = 0; i < count; ++i)
			{
				string128 section;
				_GetItem(separated, i, section);
				m_AllowedFilterSections.push_back(section);
				selfObject->m_HiglightRelatedItemSections.push_back(section);
			}
		}

		if (pSettings->line_exist(section, "antigas_breath_sounds_with_filter"))
		{
			const char* b_separated = pSettings->r_string(section, "antigas_breath_sounds_with_filter");
			int b_count = _GetItemCount(b_separated);
			filter_breath_sounds.clear();
			for (int i = 0; i < b_count; ++i)
			{
				string128 s_path;
				_GetItem(b_separated, i, s_path);
				AddSound(s_path, true);
			}
		}

		if (pSettings->line_exist(section, "antigas_breath_sounds_no_filter"))
		{
			const char* b_separated = pSettings->r_string(section, "antigas_breath_sounds_no_filter");
			int b_count = _GetItemCount(b_separated);
			breath_sounds.clear();
			for (int i = 0; i < b_count; ++i)
			{
				string128 s_path;
				_GetItem(b_separated, i, s_path);
				AddSound(s_path, false);
			}
		}

		UpdateState();
	}
}

void OnInstall_upgrade_impl(const char* section, bool test)
{
	
}

void IAntigas::Hit(float hit_power, ALife::EHitType hit_type, float targetImmunity)
{
	float dmg_coeff = m_FilterDamage[hit_type];
	if (dmg_coeff > 0 && (hit_power * dmg_coeff) > 0)
	{
		SetFilterCondition(GetFilterCondition() - (hit_power * dmg_coeff));
	}
}

void IAntigas::RestoreDefaultValues()
{
	if (!IsAllowed())
		return;

	if (selfObject == nullptr)
		return;

	if (auto armor = selfObject->cast_armorbase())
	{
		armor->OverrideHitTypeProtection(ALife::eHitTypeBurn, m_InitialItemProtections[ALife::eHitTypeBurn]);
		armor->OverrideHitTypeProtection(ALife::eHitTypeRadiation, m_InitialItemProtections[ALife::eHitTypeRadiation]);
		armor->OverrideHitTypeProtection(ALife::eHitTypeChemicalBurn, m_InitialItemProtections[ALife::eHitTypeChemicalBurn]);
		armor->OverrideHitTypeProtection(ALife::eHitTypeLightBurn, m_InitialItemProtections[ALife::eHitTypeLightBurn]);
	}
}

void IAntigas::SetOwner(CArmorBase* CItem, HitImmunity::HitTypeSVec m_HitTypeProtection)
{
	selfObject = CItem->cast_inventory_item();
	CloneInitialProtectionParams(m_HitTypeProtection);
}

void IAntigas::OnNetSave(NET_Packet& packet)
{
	packet.w_u8(IsFilterInstalled() ? 1 : 0);
	packet.w_float(GetFilterCondition());
	packet.w_stringZ(m_filter_section);

	packet.w_u32(last_filter_id);
	packet.w_float(last_filter_condition);
	packet.w_u8(is_condition_applyed ? 1 : 0);
}

void IAntigas::OnNetLoad(IReader& packet)
{
	bIsFilterInstalled = packet.r_u8() == 1;
	SetFilterCondition(packet.r_float());
	packet.r_stringZ(m_filter_section);

	last_filter_id = packet.r_u32();
	last_filter_condition = packet.r_float();
	is_condition_applyed = packet.r_u8() == 1;

	if (IsFilterInstalled())
	{
		const char* section = m_filter_section.c_str();
		// immunities
		m_FilterProtection[ALife::eHitTypeBurn] = pSettings->read_if_exists<float>(section, "antigas_filter_protection_burn", 0.0f);
		clamp(m_FilterProtection[ALife::eHitTypeBurn], 0.0f, 1.0f);

		m_FilterProtection[ALife::eHitTypeRadiation] = pSettings->read_if_exists<float>(section, "antigas_filter_protection_radiation", 0.0f);
		clamp(m_FilterProtection[ALife::eHitTypeRadiation], 0.0f, 1.0f);

		m_FilterProtection[ALife::eHitTypeChemicalBurn] = pSettings->read_if_exists<float>(section, "antigas_filter_protection_chemical_burn", 0.0f);
		clamp(m_FilterProtection[ALife::eHitTypeChemicalBurn], 0.0f, 1.0f);

		m_FilterProtection[ALife::eHitTypeLightBurn] = m_FilterProtection[ALife::eHitTypeBurn];
		clamp(m_FilterProtection[ALife::eHitTypeLightBurn], 0.0f, 1.0f);

		// damage
		m_FilterDamage[ALife::eHitTypeBurn] = pSettings->read_if_exists<float>(section, "antigas_filter_coeff_damage_burn", 0.0f);
		clamp(m_FilterDamage[ALife::eHitTypeBurn], 0.0f, 1.0f);

		m_FilterDamage[ALife::eHitTypeRadiation] = pSettings->read_if_exists<float>(section, "antigas_filter_coeff_damage_radiation", 0.0f);
		clamp(m_FilterDamage[ALife::eHitTypeRadiation], 0.0f, 1.0f);

		m_FilterDamage[ALife::eHitTypeChemicalBurn] = pSettings->read_if_exists<float>(section, "antigas_filter_coeff_damage_chemical_burn", 0.0f);
		clamp(m_FilterDamage[ALife::eHitTypeChemicalBurn], 0.0f, 1.0f);

		m_FilterDamage[ALife::eHitTypeLightBurn] = m_FilterProtection[ALife::eHitTypeBurn];
		clamp(m_FilterDamage[ALife::eHitTypeLightBurn], 0.0f, 1.0f);
	}
}

float IAntigas::GetScaledByConditionFilterProtection(ALife::EHitType hit_type)
{
	if (!IsFilterInstalled())
	{
		return 0.0f;
	}

	float cond = GetFilterCondition();
	float p_value = m_FilterProtection[hit_type];

	if (p_value <= 0 || cond <= 0)
		return 0.f;

	return ((p_value * 100) * cond) / 100;
}

void IAntigas::CloneInitialProtectionParams(HitImmunity::HitTypeSVec m_HitTypeProtection)
{
	m_InitialItemProtections[ALife::eHitTypeBurn] = m_HitTypeProtection[ALife::eHitTypeBurn];
	clamp(m_InitialItemProtections[ALife::eHitTypeBurn], 0.0f, 1.0f);

	m_InitialItemProtections[ALife::eHitTypeRadiation] = m_HitTypeProtection[ALife::eHitTypeRadiation];
	clamp(m_InitialItemProtections[ALife::eHitTypeRadiation], 0.0f, 1.0f);

	m_InitialItemProtections[ALife::eHitTypeChemicalBurn] = m_HitTypeProtection[ALife::eHitTypeChemicalBurn];
	clamp(m_InitialItemProtections[ALife::eHitTypeChemicalBurn], 0.0f, 1.0f);

	m_InitialItemProtections[ALife::eHitTypeLightBurn] = m_HitTypeProtection[ALife::eHitTypeLightBurn];
	clamp(m_InitialItemProtections[ALife::eHitTypeLightBurn], 0.0f, 1.0f);
}

const char* IAntigas::GetFilterSection()
{
	return m_filter_section.c_str();
}

float IAntigas::GetFilterCondition()
{
	return fFilterCondition;
}

void IAntigas::SetFilterSection(shared_str new_section)
{
	m_filter_section = new_section;
}

void IAntigas::SetFilterCondition(float new_condition)
{
	fFilterCondition = new_condition;
	clamp(fFilterCondition, 0.0f, 1.0f);
}

bool IAntigas::IsAllowed()
{
	return bIsAllowed;
}

void IAntigas::SetAllowed(bool flag)
{
	bIsAllowed = flag;
}

void IAntigas::SetFilterInstalledState(bool flag)
{
	bIsFilterInstalled = flag;
}

bool IAntigas::IsFilterInstalled()
{
	return bIsFilterInstalled;
}

bool IAntigas::IsFilterInWhiteList(shared_str filter_section)
{
	int count = m_AllowedFilterSections.size();
	for (size_t i = 0; i < m_AllowedFilterSections.size(); ++i)
	{
		if (m_AllowedFilterSections[i] != nullptr && xr_strcmp(m_AllowedFilterSections[i], filter_section) == 0)
		{
			return true;
		}
	}

	return false;
}

extern CSE_Abstract* CALifeSimulator__spawn_item2(
	CALifeSimulator* self_,
	const char* section,
	const Fvector& position,
	u32 level_vertex_id,
	GameGraph::_GRAPH_ID game_vertex_id,
	ALife::_OBJECT_ID id_parent
);

bool IAntigas::InstallFilter(CInventoryItem* inventory_item)
{
	if (!IsAllowed())
		return false;

	if (IsFilterInstalled())
		return false;

	if (selfObject == nullptr)
		return false;

	if (inventory_item == nullptr)
		return false;

	AntigasFilter* oAntigasFilter = smart_cast<AntigasFilter*>(inventory_item);
	if (oAntigasFilter == nullptr)
		return false;

	if (!IsFilterInWhiteList(inventory_item->m_section_id))
		return false;

	m_FilterProtection = oAntigasFilter->m_FilterProtection;
	m_FilterDamage = oAntigasFilter->m_FilterDamage;
	SetFilterSection(inventory_item->m_section_id);
	SetFilterCondition(inventory_item->GetCondition());
	SetFilterInstalledState(true);
	UpdateState();

	//inventory_item->SetDropManual(true);
	//inventory_item->m_pInventory->DropItem(inventory_item->cast_game_object(), true, true);
	
	inventory_item->object().DestroyObject();

	/*
	NET_Packet P;
	inventory_item->object().u_EventGen(P, GE_OWNERSHIP_REJECT, inventory_item->parent_id());
	P.w_u16(inventory_item->object().ID());
	P.w_u8(1);
	inventory_item->object().u_EventSend(P);
	*/

	return true;
}

bool IAntigas::UninstallFilter()
{
	if (!IsAllowed())
		return false;

	if (!IsFilterInstalled())
		return false;

	if (selfObject == nullptr)
		return false;

	if (CObject* obj_parent = selfObject->object().H_Parent())
	{
		CInventoryOwner* nvOwner = obj_parent->cast_inventory_owner();
		CInventoryBox* nvBox = obj_parent->cast_inventory_box();
		if (nvOwner == nullptr && nvBox == nullptr)
		{
			return false;
		}

		CALifeSimulator* sim = const_cast<CALifeSimulator*>(&ai().alife());

		CSE_Abstract* s_obj = CALifeSimulator__spawn_item2(
			sim,
			GetFilterSection(),
			obj_parent->Position(),
			obj_parent->cast_game_object()->ai_location().level_vertex_id(),
			obj_parent->cast_game_object()->ai_location().game_vertex_id(),
			obj_parent->ID()
		);

		last_filter_id = s_obj->ID;
		is_condition_applyed = false;
		last_filter_condition = GetFilterCondition();

		SetFilterCondition(0.0f);
		SetFilterInstalledState(false);
		UpdateState();
	}

	return true;
}

void IAntigas::UpdateCL()
{
	if (!is_condition_applyed && last_filter_id != u32(-1)) {
		if (CObject* co = Level().Objects.net_Find(last_filter_id))
		{
			if (CInventoryItem* io = co->cast_inventory_item())
			{
				io->SetCondition(last_filter_condition);
				is_condition_applyed = true;
				last_filter_id = u32(-1);
				last_filter_condition = 0.0f;
			}
		}
	}
}

void IAntigas::UpdateState()
{
	if (!IsAllowed())
		return;

	if (selfObject == nullptr)
		return;

	if (CArmorBase* armor = selfObject->cast_armorbase())
	{
		armor->OverrideHitTypeProtection(
			ALife::eHitTypeBurn,
			m_InitialItemProtections[ALife::eHitTypeBurn] + GetScaledByConditionFilterProtection(ALife::eHitTypeBurn)
		);

		armor->OverrideHitTypeProtection(
			ALife::eHitTypeRadiation,
			m_InitialItemProtections[ALife::eHitTypeRadiation] + GetScaledByConditionFilterProtection(ALife::eHitTypeRadiation)
		);

		armor->OverrideHitTypeProtection(
			ALife::eHitTypeChemicalBurn,
			m_InitialItemProtections[ALife::eHitTypeChemicalBurn] + GetScaledByConditionFilterProtection(ALife::eHitTypeChemicalBurn)
		);

		armor->OverrideHitTypeProtection(
			ALife::eHitTypeLightBurn,
			m_InitialItemProtections[ALife::eHitTypeLightBurn] + GetScaledByConditionFilterProtection(ALife::eHitTypeLightBurn)
		);
	}
}

bool IAntigas::OnPropertiesBoxForUsing(CUIPropertiesBox* m_UIPropertiesBox)
{
	if (IsAllowed() && IsFilterInstalled())
	{
		m_UIPropertiesBox->AddItem(
			"antigas_detach_filter",
			nullptr, 
			DETACH_ANTIGAS_FILTER
		);
		return true;
	}

	return false;
}

bool IAntigas::OnProcessPropertiesBoxClicked(CUIPropertiesBox* m_UIPropertiesBox)
{
	if (IsAllowed())
	{
		switch (m_UIPropertiesBox->GetClickedItem()->GetTAG())
		{
			case DETACH_ANTIGAS_FILTER:
				UninstallFilter();
				return true;
		}
	}

	return false;
}