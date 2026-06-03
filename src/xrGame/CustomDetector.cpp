#include "stdafx.h"
#include "CustomDetector.h"
#include "Inventory.h"
#include "Actor.h"

CCustomDetector::~CCustomDetector()
{
	m_artefacts.destroy();
	TurnDetectorInternal(false);
	xr_delete(m_ui);
}

void CCustomDetector::Load(const char* section)
{
    m_animation_slot = 7;
	inherited::Load(section);

	m_fAfDetectRadius = pSettings->read_if_exists<float>(section,"af_radius",30.0f);
	m_fAfVisRadius = pSettings->read_if_exists<float>(section,"af_vis_radius",2.0f);
	m_artefacts.load(section, "af");

	SpatialComponent->type |= ESPATIAL_TYPE::ANOMALY_DETECTOR;

	IPowerManager::SetSelfObject(cast_inventory_item(), H_Parent());
	IPowerManager::Load(section, cast_inventory_item());
}

void CCustomDetector::shedule_Update(u32 dt)
{
	PROF_EVENT(__FUNCTION__)

	inherited::shedule_Update(dt);

	IPowerManager::SetEnabled(m_bWorking);

	bool isInHands = false;
	if (attachable_hud_item* itm = g_player_hud->attached_item(1))
	{
		if (itm->m_parent_hud_item == this)
		{
			isInHands = true;
		}
	}

	if (isInHands && IPowerManager::IsAllow())
	{
		if (IPowerManager::GetLeftPowerValue() <= 0)
		{
			m_need_refresh = true;
			m_bWorking = false;
		}
		else
		{
			if (m_need_refresh)
			{
				m_bWorking = true;
				m_need_refresh = false;
			}
		}
	}

	if (!IsWorking())
	{
		return;
	}

	Fvector	P;
	P.set(H_Parent()->Position());
	m_artefacts.feel_touch_update(P, m_fAfDetectRadius);
}


void CCustomDetector::UpdateWork()
{
	if (!IsWorking() || !m_ui)
	{
		return;
	}

	UpdateAf();

	m_ui->update();
}

void CCustomDetector::OnH_B_Independent(bool just_before_destroy)
{
	inherited::OnH_B_Independent(just_before_destroy);

	m_artefacts.clear();
}

void CCustomDetector::TurnDetectorInternal(bool b)
{
	m_bWorking = b;

	if (IPowerManager::IsAllow() && IPowerManager::GetLeftPowerValue() <= 0)
	{
		m_bWorking = false;
	}

	if (b && m_ui == nullptr)
	{
		CreateUI();
	}
}

void CCustomDetector::save(NET_Packet& output_packet)
{
	inherited::save(output_packet);
	IPowerManager::net_save(output_packet);
}

void CCustomDetector::load(IReader& input_packet)
{
	inherited::load(input_packet);
	IPowerManager::net_load(input_packet);
}