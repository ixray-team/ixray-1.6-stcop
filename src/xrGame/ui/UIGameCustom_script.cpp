#include "StdAfx.h"
#include "pch_script.h"
#include "UIGameCustom.h"
#include "Level.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UIDialogHolder.h"
#include "../../xrUI/Widgets/UIDialogWnd.h"

using namespace luabind;

CUIGameCustom* get_hud(){
	return CurrentGameUI();
}

static void script_show_sleep_dialog()
{
	if (CUIGameCustom* hud = CurrentGameUI())
		hud->ShowSleepDialog();
}

static void script_show_sleep_dialog_at_hour(int hours)
{
	if (CUIGameCustom* hud = CurrentGameUI())
		hud->ShowSleepDialogAtHour(hours);
}

static void script_set_sleep_hour_presets(CUIGameCustom* hud, const luabind::object& tbl)
{
	if (!hud)
		return;

	xr_vector<int> hours;
	if (luabind::get_type(tbl) == LUA_TTABLE)
	{
		for (luabind::object::iterator it = tbl.begin(), end = tbl.end(); it != end; ++it)
		{
			const luabind::object value = *it;
			if (luabind::get_type(value) == LUA_TNUMBER)
				hours.push_back(luabind::object_cast<int>(value));
		}
	}
	hud->SetSleepHourPresets(std::move(hours));
}

static void script_hide_sleep_dialog()
{
	if (CUIGameCustom* hud = CurrentGameUI())
		hud->HideSleepDialog();
}

static void script_cancel_sleep_dialog()
{
	if (CUIGameCustom* hud = CurrentGameUI())
		hud->CancelSleepDialog();
}

static bool script_confirm_sleep()
{
	if (CUIGameCustom* hud = CurrentGameUI())
		return hud->ConfirmSleep();
	return false;
}

static void script_force_sleep(int hours)
{
	if (CUIGameCustom* hud = CurrentGameUI())
		hud->ForceSleep(hours);
}

static bool script_abort_sleep()
{
	if (CUIGameCustom* hud = CurrentGameUI())
		return hud->AbortSleep();
	return false;
}

static bool script_is_sleep_dialog_ready()
{
	if (CUIGameCustom* hud = CurrentGameUI())
		return hud->IsSleepDialogReady();
	return false;
}

static bool script_is_sleep_dialog_shown()
{
	if (CUIGameCustom* hud = CurrentGameUI())
		return hud->IsSleepDialogShown();
	return false;
}

static int script_get_sleep_selected_hours()
{
	if (CUIGameCustom* hud = CurrentGameUI())
		return hud->GetSleepSelectedHours();
	return 1;
}

static void script_set_sleep_selected_hours(int hours)
{
	if (CUIGameCustom* hud = CurrentGameUI())
		hud->SetSleepSelectedHours(hours);
}

static bool script_is_actor_sleeping()
{
	if (CUIGameCustom* hud = CurrentGameUI())
		return hud->IsActorSleeping();
	return false;
}

static u8 script_get_sleep_phase()
{
	if (CUIGameCustom* hud = CurrentGameUI())
		return hud->GetSleepPhase();
	return 0;
}

static void script_set_sleep_blocked(CUIGameCustom* hud, bool blocked)
{
	if (hud)
		hud->SetSleepBlocked(blocked, nullptr);
}

static void script_set_sleep_blocked_warn(CUIGameCustom* hud, bool blocked, LPCSTR warningText)
{
	if (hud)
		hud->SetSleepBlocked(blocked, warningText);
}

static void script_set_sleep_blocked_free(bool blocked)
{
	if (CUIGameCustom* hud = CurrentGameUI())
		hud->SetSleepBlocked(blocked, nullptr);
}

static void script_set_sleep_blocked_warn_free(bool blocked, LPCSTR warningText)
{
	if (CUIGameCustom* hud = CurrentGameUI())
		hud->SetSleepBlocked(blocked, warningText);
}

#pragma optimize("s",on)
void CUIGameCustom::script_register(lua_State *L)
{
	module(L)
		[
			class_< SDrawStaticStruct >("SDrawStaticStruct")
			.def_readwrite("m_endTime",		&SDrawStaticStruct::m_endTime)
			.def("wnd",					&SDrawStaticStruct::wnd),

			class_<CUIGameCustom, CDialogHolder>("CUIGameCustom")
			.def("TopInputReceiver", 		&CUIGameCustom::TopInputReceiver)
			.def("SetMainInputReceiver",	&CUIGameCustom::SetMainInputReceiver)
			.def("AddDialogToRender",		&CUIGameCustom::AddDialogToRender)
			.def("RemoveDialogToRender",	&CUIGameCustom::RemoveDialogToRender)
			.def("AddCustomStatic",			+[](CUIGameCustom* gameUi, const char* id, bool singleInstance)
            {
                return gameUi->AddCustomStatic(id, singleInstance);
            })
			.def("AddCustomMessage",		(void(CUIGameCustom::*)(const char*, float, float, float, CGameFont*, u16, u32/*, const char**/))&CUIGameCustom::AddCustomMessage)
			.def("AddCustomMessage",		(void(CUIGameCustom::*)(const char*, float, float, float, CGameFont*, u16, u32/*, const char**/, float))&CUIGameCustom::AddCustomMessage)
			.def("CustomMessageOut",		&CUIGameCustom::CustomMessageOut)
			.def("RemoveCustomMessage",		&CUIGameCustom::RemoveCustomMessage)
			.def("AddCustomStatic",			&CUIGameCustom::AddCustomStatic)
			.def("AddHudMessage",			&CUIGameCustom::AddHudMessage)
			.def("RemoveCustomStatic",		&CUIGameCustom::RemoveCustomStatic)
			.def("HideActorMenu",			&CUIGameCustom::HideActorMenu)
			//Alundaio
			.def("ShowActorMenu",			&CUIGameCustom::ShowActorMenu)
			.def("UpdateActorMenu",			&CUIGameCustom::UpdateActorMenu)
			.def("CurrentItemAtCell",		&CUIGameCustom::CurrentItemAtCell)
			//-Alundaio
			.def("HidePdaMenu",				&CUIGameCustom::HidePdaMenu)
			.def("show_messages",			&CUIGameCustom::ShowMessagesWindow)
			.def("hide_messages",			&CUIGameCustom::HideMessagesWindow)
			.def("GetCustomStatic",			&CUIGameCustom::GetCustomStatic)
			.def("update_fake_indicators",	&CUIGameCustom::update_fake_indicators)
			.def("enable_fake_indicators",	&CUIGameCustom::enable_fake_indicators)
			.def("ShowSleepDialog",			&CUIGameCustom::ShowSleepDialog)
			.def("ShowSleepDialogAtHour",	&CUIGameCustom::ShowSleepDialogAtHour)
			.def("SetSleepHourPresets",		&script_set_sleep_hour_presets)
			.def("ClearSleepHourPresets",	&CUIGameCustom::ClearSleepHourPresets)
			.def("IsSleepDialogReady",		&CUIGameCustom::IsSleepDialogReady)
			.def("IsSleepDialogShown",		&CUIGameCustom::IsSleepDialogShown)
			.def("HideSleepDialog",			&CUIGameCustom::HideSleepDialog)
			.def("CancelSleepDialog",		&CUIGameCustom::CancelSleepDialog)
			.def("ConfirmSleep",			&CUIGameCustom::ConfirmSleep)
			.def("ForceSleep",				&CUIGameCustom::ForceSleep)
			.def("AbortSleep",				&CUIGameCustom::AbortSleep)
			.def("GetSleepSelectedHours",	&CUIGameCustom::GetSleepSelectedHours)
			.def("SetSleepSelectedHours",	&CUIGameCustom::SetSleepSelectedHours)
			.def("IsActorSleeping",			&CUIGameCustom::IsActorSleeping)
			.def("GetSleepPhase",			&CUIGameCustom::GetSleepPhase)
			.def("SetSleepHoursRange",		&CUIGameCustom::SetSleepHoursRange)
			.def("ClearSleepHoursRange",	&CUIGameCustom::ClearSleepHoursRange)
			.def("SetSleepAllowBleeding",	&CUIGameCustom::SetSleepAllowBleeding)
			.def("ClearSleepAllowBleeding",	&CUIGameCustom::ClearSleepAllowBleeding)
			.def("SetSleepRestorePower",	&CUIGameCustom::SetSleepRestorePower)
			.def("ClearSleepRestorePower",	&CUIGameCustom::ClearSleepRestorePower)
			.def("SetSleepMute",			&CUIGameCustom::SetSleepMute)
			.def("ClearSleepMute",			&CUIGameCustom::ClearSleepMute)
			.def("ClearSleepSessionOverrides", &CUIGameCustom::ClearSleepSessionOverrides)
			.def("SetSleepBlocked",			&script_set_sleep_blocked)
			.def("SetSleepBlocked",			&script_set_sleep_blocked_warn),

			def("get_hud",					&get_hud),
			def("show_sleep_dialog",		&script_show_sleep_dialog),
			def("show_sleep_dialog_at_hour",&script_show_sleep_dialog_at_hour),
			def("hide_sleep_dialog",		&script_hide_sleep_dialog),
			def("cancel_sleep_dialog",		&script_cancel_sleep_dialog),
			def("confirm_sleep",			&script_confirm_sleep),
			def("force_sleep",				&script_force_sleep),
			def("abort_sleep",				&script_abort_sleep),
			def("is_sleep_dialog_ready",	&script_is_sleep_dialog_ready),
			def("is_sleep_dialog_shown",	&script_is_sleep_dialog_shown),
			def("get_sleep_selected_hours",	&script_get_sleep_selected_hours),
			def("set_sleep_selected_hours",	&script_set_sleep_selected_hours),
			def("is_actor_sleeping",		&script_is_actor_sleeping),
			def("get_sleep_phase",			&script_get_sleep_phase),
			def("set_sleep_blocked",		&script_set_sleep_blocked_free),
			def("set_sleep_blocked",		&script_set_sleep_blocked_warn_free)
		];
}
