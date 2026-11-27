#include "StdAfx.h"
#include "CustomDetector.h"
#include "Inventory.h"
#include "Actor.h"

CCustomDetector::~CCustomDetector()
{
	TurnDetectorInternal(false);
	m_artefacts.destroy();
	xr_delete(m_ui);
}

void CCustomDetector::Load(const char* section)
{
    m_animation_slot = 7;
	inherited::Load(section);

	m_fAfDetectRadius = READ_IF_EXISTS(pSettings, r_float, section, "af_radius", 30.0f);
	m_fAfVisRadius = READ_IF_EXISTS(pSettings, r_float, section, "af_vis_radius", 2.0f);
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
			TurnDetectorInternal(false);
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
	if (!IsWorking() || !m_ui || !HudItemData() || GetState() == eHiding)
	{
		return;
	}

	UpdateAf();

	m_ui->update();
}

bool CCustomDetector::UpdateArtifactSignal(ITEM_INFO& Info, float RelativeDistance)
{
	ITEM_TYPE& Type = *Info.curr_ref;
	Info.cur_period = Type.freq.x + (Type.freq.y - Type.freq.x) * (RelativeDistance * RelativeDistance);
	if (Info.snd_time <= Info.cur_period)
	{
		Info.snd_time += Device.fTimeDelta;
		return false;
	}

	Info.snd_time = 0.0f;
	if (ActiveSignal != &Type.detect_snds)
	{
		StopArtifactSignal();
		ActiveSignal = &Type.detect_snds;
	}

	HUD_SOUND_ITEM::PlaySound(*ActiveSignal, zero_vel, this, true);
	if (ActiveSignal->m_activeSnd)
	{
		const float Pitch = 0.9f + (1.4f - 0.9f) * (1.0f - RelativeDistance);
		ActiveSignal->m_activeSnd->snd.set_frequency(Pitch);
	}

	return true;
}

void CCustomDetector::StopArtifactSignal()
{
	if (ActiveSignal)
	{
		HUD_SOUND_ITEM::StopSound(*ActiveSignal);
		ActiveSignal = nullptr;
	}
}

void CCustomDetector::OnStateSwitch(u8 State)
{
	inherited::OnStateSwitch(State);
	if (State == eHiding || State == eHidden)
	{
		StopArtifactSignal();
	}
}

void CCustomDetector::on_b_hud_detach()
{
	StopArtifactSignal();
	inherited::on_b_hud_detach();
}

void CCustomDetector::OnH_B_Independent(bool just_before_destroy)
{
	inherited::OnH_B_Independent(just_before_destroy);
	StopArtifactSignal();

	m_artefacts.clear();
}

void CCustomDetector::TurnDetectorInternal(bool b)
{
	m_bWorking = b && (!IPowerManager::IsAllow() || IPowerManager::GetLeftPowerValue() > 0.0f);
	if (!m_bWorking)
	{
		StopArtifactSignal();
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