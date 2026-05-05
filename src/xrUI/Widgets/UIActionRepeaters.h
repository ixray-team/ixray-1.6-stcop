#pragma once

#include "../../xrScripts/script_export_space.h"
#include "../../xrEngine/pure.h"


class UI_API CUIActionRepeatersOwner
{
protected:

	DECLARE_SCRIPT_REGISTER_FUNCTION
};

// Action repeater
// We are not repeating a key press, but rather an action for a specific window when hold events arrive to that window
// Use case: 
// - register actionrepeater for some action in a window, unregister in destructor
// - in Show on Init or other method reset all actions of the manager, so that when we return to another window its wait_for_click for all actions equals true
// - in OnKeyboardPress let it know that we started a press
// - in OnKeyboardHold check if we can repeat the action
// Also there can be a situation when a key is pressed, 1 window is processing it, and then we switch to another window
// we want a new keypress and hold in that window

class UI_API CUIActionRepeatersManager : public pureFrame
{
public:
	typedef CUIActionRepeatersOwner* ActionRepeatersOwnerKey;

	CUIActionRepeatersManager();

	void Register					(ActionRepeatersOwnerKey owner, int actionId, int initial_delay = 0, int period = 0);
	//void RegisterMultiple			(ActionRepeatersOwnerKey owner, xr_vector<int> actionIds, int initial_delay = 0, int period = 0);
	void Unregister					(ActionRepeatersOwnerKey owner, int actionId);
	bool IsRegistered				(ActionRepeatersOwnerKey owner, int actionId) const;
	void UnregisterOwner			(ActionRepeatersOwnerKey owner);

	// Reset states, so that when we come back to a window we dont get a hold without a preceding press
	void ResetAll					(const int* pSkipActions, int skipActionsCnt);
	void ResetAll					(CUIActionRepeatersOwner* skipOneOwner);
	void ResetAll					();
	void Reset						(int actionId);
	void Reset						(ActionRepeatersOwnerKey owner, int actionId);
	bool CanRepeatActionNow			(ActionRepeatersOwnerKey owner, int actionId);
	void SetActionStarted			(ActionRepeatersOwnerKey owner, int actionId);
	bool IsActionStarted			(ActionRepeatersOwnerKey owner, int actionId);

	void OnFrame() override;

protected:
	struct UIActionRepeatInfo
	{
		bool wait_for_click;
		u32 press_start_time;
		u32 repeated_press_last_time;
		int initial_delay;
		int period;

		UIActionRepeatInfo(int initialDelay, int repeatPeriod);
		UIActionRepeatInfo();
	};

	typedef xr_map<int, UIActionRepeatInfo> ActionRepeatersMap;
	typedef xr_map<ActionRepeatersOwnerKey, ActionRepeatersMap> ActionRepeatersOwnerMap;

protected:
	// Per window (pointer) store a map of actioncode-repeatinfo
	ActionRepeatersOwnerMap m_repeatInfosList;

	int m_defaultInitialDelay;
	int m_defaultPeriod;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};


extern CUIActionRepeatersManager* g_ActionRepeaters;
extern UI_API CUIActionRepeatersManager* ActionRepeaters();