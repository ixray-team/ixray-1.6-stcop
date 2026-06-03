#include "stdafx.h"
#include "ElectronicsProblemsManager.h"

CElectronicsProblemsManager::CElectronicsProblemsManager()
{
	m_last_update_time = Device.dwTimeGlobal;
}

void CElectronicsProblemsManager::ResetElectronicsProblems()
{
	m_target_electronics_problems_counter = 0.0f;
}

void CElectronicsProblemsManager::ResetElectronicsProblems_Full()
{
	ResetElectronicsProblems();

	m_current_electronics_problems_counter = 0.0f;
	m_previous_electronics_problems_counter = 0.0f;
	m_last_problems_update_was_decrease = false;
}

float CElectronicsProblemsManager::PreviousElectronicsProblemsCnt() const
{
	return m_previous_electronics_problems_counter;
}

bool CElectronicsProblemsManager::ElectronicsProblemsImmediateApply()
{
	m_current_electronics_problems_counter = m_target_electronics_problems_counter;
	return true;
}

bool CElectronicsProblemsManager::ElectronicsProblemsInc()
{
	m_target_electronics_problems_counter += 1.0f;
	return true;
}

float CElectronicsProblemsManager::TargetElectronicsProblemsCnt() const
{
	return m_target_electronics_problems_counter;
}

float CElectronicsProblemsManager::CurrentElectronicsProblemsCnt() const
{
	return m_current_electronics_problems_counter;
}

bool CElectronicsProblemsManager::ElectronicsProblemsDec()
{
	if (m_target_electronics_problems_counter > 0.0f)
	{
		m_target_electronics_problems_counter -= 1.0f;
		return true;
	}
	else
	{
		return false;
	}
}

bool CElectronicsProblemsManager::IsElectronicsProblemsDecreasing() const
{
	return m_last_problems_update_was_decrease;
}

void CElectronicsProblemsManager::UpdateElectronicsProblemsCnt(u32 dt)
{
	float max_delta = static_cast<float>(dt) / 2000.0f;
	float delta = m_target_electronics_problems_counter - m_current_electronics_problems_counter;

	m_previous_electronics_problems_counter = m_current_electronics_problems_counter;

	if (m_target_electronics_problems_counter == m_current_electronics_problems_counter)
	{
		return;
	}

	if (abs(delta) <= abs(max_delta))
	{
		m_current_electronics_problems_counter = m_target_electronics_problems_counter;
	}
	else
	{
		m_current_electronics_problems_counter += copysign(max_delta, delta);
		m_last_problems_update_was_decrease = (delta < 0.0f);
	}
}

void CElectronicsProblemsManager::UpdateCL()
{
	u32 ct = Device.dwTimeGlobal;
	u32 dt = 0;
	if (m_last_update_time != 0)
	{
		dt = Device.GetTimeDeltaSafe(m_last_update_time, ct);
	}

	m_last_update_time = ct;
	clamp(dt, 0u, 1000u);

	UpdateElectronicsProblemsCnt(dt);

	Device.hudViewportData.IsElectronicsProblemsDecreasing = IsElectronicsProblemsDecreasing();
	Device.hudViewportData.CurrentElectronicsProblemsCnt = CurrentElectronicsProblemsCnt();
	Device.hudViewportData.TargetElectronicsProblemsCnt = TargetElectronicsProblemsCnt();
}