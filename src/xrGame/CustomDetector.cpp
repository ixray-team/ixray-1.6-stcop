#include "StdAfx.h"
#include "CustomDetector.h"
#include "Inventory.h"
#include "Actor.h"

CCustomDetector::~CCustomDetector()
{
	m_artefacts.destroy();
	TurnDetectorInternal(false);
	xr_delete(m_ui);
}

void CCustomDetector::Load(LPCSTR section)
{
    m_animation_slot = 7;
	inherited::Load(section);

	m_fAfDetectRadius = READ_IF_EXISTS(pSettings, r_float, section, "af_radius", 30.0f);
	m_fAfVisRadius = READ_IF_EXISTS(pSettings, r_float, section, "af_vis_radius", 2.0f);
	m_artefacts.load(section, "af");

	SpatialComponent->spatial.type |= ESPATIAL_TYPE::ANOMALY_DETECTOR;
}

void CCustomDetector::shedule_Update(u32 dt)
{
	PROF_EVENT(__FUNCTION__)

	inherited::shedule_Update(dt);

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

	if (b && m_ui == nullptr)
	{
		CreateUI();
	}
}