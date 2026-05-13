#include "stdafx.h"

#include "UIActionRepeaters.h"
#include "../../xrEngine/device.h"
#include "../../xrEngine/xr_input.h"
#include "../../xrEngine/xr_level_controller.h"

CUIActionRepeatersManager* g_ActionRepeaters = nullptr;
UI_API CUIActionRepeatersManager* ActionRepeaters()
{
	if (!g_ActionRepeaters)
		g_ActionRepeaters = new CUIActionRepeatersManager();
	return g_ActionRepeaters;
}



CUIActionRepeatersManager::UIActionRepeatInfo::UIActionRepeatInfo(int initialDelay, int repeatPeriod)
	:wait_for_click(true), press_start_time(0ul), repeated_press_last_time(0ul), initial_delay(initialDelay), period(repeatPeriod)
{}

CUIActionRepeatersManager::UIActionRepeatInfo::UIActionRepeatInfo()
	: UIActionRepeatInfo(200, 200)
{}




CUIActionRepeatersManager::CUIActionRepeatersManager()
	: m_defaultInitialDelay(240), m_defaultPeriod(140)
{
	Device.seqFrame.Add(this, REG_PRIORITY_LOW);
}

void CUIActionRepeatersManager::Register(ActionRepeatersOwnerKey owner, int actionId, int initial_delay, int period)
{
	UIActionRepeatInfo& ari = m_repeatInfosList[owner][actionId];
	ari.initial_delay = (initial_delay == 0) ? m_defaultInitialDelay : initial_delay;
	ari.period = (period == 0) ? m_defaultPeriod : period;
	ari.wait_for_click = true;
}

//void CUIActionRepeatersManager::RegisterMultiple(ActionRepeatersOwnerKey owner, xr_vector<int> actionIds, int initial_delay, int period)
//{
//	if (initial_delay == 0)
//		initial_delay = m_defaultInitialDelay;
//	if (period == 0)
//		period = m_defaultPeriod;
//
//	for (xr_vector<int>::iterator it = actionIds.begin(); it != actionIds.end(); ++it)
//	{
//		RegisterActionRepeater(owner, *it, initial_delay, period);
//	}
//}

void CUIActionRepeatersManager::Unregister(ActionRepeatersOwnerKey owner, int actionId)
{
	if (!IsRegistered(owner, actionId))
		return;
	ActionRepeatersMap& repeaters = m_repeatInfosList[owner];
	repeaters.erase(actionId);
}

void CUIActionRepeatersManager::UnregisterOwner(ActionRepeatersOwnerKey owner)
{
	ActionRepeatersOwnerMap::const_iterator oIt = m_repeatInfosList.find(owner);
	if (oIt != m_repeatInfosList.end())
		m_repeatInfosList.erase(oIt);
}

bool CUIActionRepeatersManager::IsRegistered(ActionRepeatersOwnerKey owner, int actionId) const
{
	ActionRepeatersOwnerMap::const_iterator oIt = m_repeatInfosList.find(owner);
	if (oIt == m_repeatInfosList.end())
		return false;
	if (oIt->second.find(actionId) == oIt->second.end())
		return false;

	return true;
}

void CUIActionRepeatersManager::SetActionStarted(ActionRepeatersOwnerKey owner, int actionId)
{
	if (!IsRegistered(owner, actionId))
		return;
	UIActionRepeatInfo& ari = m_repeatInfosList[owner][actionId];
	ari.wait_for_click = false;
	ari.press_start_time = Device.dwTimeContinual;
	ari.repeated_press_last_time = ari.press_start_time;
}

bool CUIActionRepeatersManager::IsActionStarted(ActionRepeatersOwnerKey owner, int actionId)
{
	if (!IsRegistered(owner, actionId))
		return false;
	UIActionRepeatInfo& ari = m_repeatInfosList[owner][actionId];
	return !ari.wait_for_click;
}

bool CUIActionRepeatersManager::CanRepeatActionNow(ActionRepeatersOwnerKey owner, int actionId)
{
	if (m_repeatInfosList.find(owner) == m_repeatInfosList.end())
		return false;

	ActionRepeatersMap& repeaters = m_repeatInfosList[owner];
	if (repeaters.find(actionId) == repeaters.end())
		return false;

	UIActionRepeatInfo& ari = repeaters[actionId];

	if (ari.wait_for_click)
		return false;

	const u32 tm = Device.dwTimeContinual;
	const u32 startOfPeriods = ari.press_start_time + ari.initial_delay;
	if (tm < startOfPeriods)
		return false;

	if (ari.repeated_press_last_time < ari.press_start_time)
		ari.repeated_press_last_time = ari.press_start_time;

	u32 index = (tm - startOfPeriods) / ari.period;
	u32 oldIndex = (ari.repeated_press_last_time - startOfPeriods) / ari.period;
	if (index != oldIndex)
	{
		ari.repeated_press_last_time = tm;
		return true;
	}

	return false;
}

void CUIActionRepeatersManager::ResetAll(const int* pSkipActions, int skipActionsCnt)
{
	// Make all of them wait for click
	for (ActionRepeatersOwnerMap::iterator oIt = m_repeatInfosList.begin(); oIt != m_repeatInfosList.end(); ++oIt)
	{
		for (ActionRepeatersMap::iterator it = oIt->second.begin(); it != oIt->second.end(); ++it)
		{
			if (pSkipActions && skipActionsCnt > 0)
			{
				bool bSkipThisAction = false;
				for (int i = 0; i < skipActionsCnt; ++i)
				{
					if (pSkipActions[i] == it->first)
					{
						bSkipThisAction = true;
						break;
					}
				}
				if (bSkipThisAction)
					continue;
			}

			it->second.wait_for_click = true;
		}
	}
}

void CUIActionRepeatersManager::ResetAll(CUIActionRepeatersOwner* skipOneOwner)
{
	for (ActionRepeatersOwnerMap::iterator oIt = m_repeatInfosList.begin(); oIt != m_repeatInfosList.end(); ++oIt)
	{
		if (skipOneOwner && oIt->first == skipOneOwner)
			continue;

		for (ActionRepeatersMap::iterator it = oIt->second.begin(); it != oIt->second.end(); ++it)
		{
			it->second.wait_for_click = true;
		}
	}
}

void CUIActionRepeatersManager::ResetAll()
{
	for (ActionRepeatersOwnerMap::iterator oIt = m_repeatInfosList.begin(); oIt != m_repeatInfosList.end(); ++oIt)
	{
		for (ActionRepeatersMap::iterator it = oIt->second.begin(); it != oIt->second.end(); ++it)
		{
			it->second.wait_for_click = true;
		}
	}
}

void CUIActionRepeatersManager::Reset(int actionId)
{
	for (ActionRepeatersOwnerMap::iterator oIt = m_repeatInfosList.begin(); oIt != m_repeatInfosList.end(); ++oIt)
	{
		for (ActionRepeatersMap::iterator it = oIt->second.begin(); it != oIt->second.end(); ++it)
		{
			if (it->first == actionId)
				it->second.wait_for_click = true;
		}
	}
}

void CUIActionRepeatersManager::Reset(CUIActionRepeatersOwner* owner, int actionId)
{
	for (ActionRepeatersOwnerMap::iterator ownerIt = m_repeatInfosList.begin(); ownerIt != m_repeatInfosList.end(); ++ownerIt)
	{
		if (ownerIt->first == owner)
		{
			for (ActionRepeatersMap::iterator it = ownerIt->second.begin(); it != ownerIt->second.end(); ++it)
			{
				if (it->first == actionId)
				{
					it->second.wait_for_click = true;
					return;
				}
			}
		}
	}
}

void CUIActionRepeatersManager::OnFrame()
{
	if (pInput->GetControllerMode())
	{
		//detect key releases for actions wanting repeats and reset them
		for (ActionRepeatersOwnerMap::iterator ownerIt = m_repeatInfosList.begin(); ownerIt != m_repeatInfosList.end(); ++ownerIt)
		{
			for (ActionRepeatersMap::iterator actionIt = ownerIt->second.begin(); actionIt != ownerIt->second.end(); ++actionIt)
			{
				if (actionIt->second.wait_for_click)
					continue;
				
				if (!any_binded_key_for_action_pressed_c(actionIt->first))
				{
					actionIt->second.wait_for_click = true;
				}
			}
		}
	}
}