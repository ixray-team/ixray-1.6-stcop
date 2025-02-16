#include "pch_script.h"
#include "UIGameCustom.h"
#include "level.h"
#include "hudmanager.h"
#include "ui/uistatic.h"

using namespace luabind;

CUIGameCustom* get_hud(){
	return CurrentGameUI();
}

#pragma optimize("s",on)
void CUIGameCustom::script_register(lua_State *L)
{
	module(L)
		[
			class_< SDrawStaticStruct >("SDrawStaticStruct")
			.def_readwrite("m_endTime",		&SDrawStaticStruct::m_endTime)
			.def("wnd",					&SDrawStaticStruct::wnd),

			class_< CUIGameCustom >("CUIGameCustom")
			.def("AddDialogToRender",		&CUIGameCustom::AddDialogToRender)
			.def("RemoveDialogToRender",	&CUIGameCustom::RemoveDialogToRender)
			.def("AddCustomMessage",		(void(CUIGameCustom::*)(LPCSTR, float, float, float, CGameFont*, u16, u32))&CUIGameCustom::AddCustomMessage)
			.def("AddCustomMessage",		(void(CUIGameCustom::*)(LPCSTR, float, float, float, CGameFont*, u16, u32, float))&CUIGameCustom::AddCustomMessage)
			.def("CustomMessageOut",		&CUIGameCustom::CustomMessageOut)
			.def("RemoveCustomMessage",		&CUIGameCustom::RemoveCustomMessage)
			.def("CommonMessageOut",		&CUIGameCustom::CommonMessageOut)
			.def("AddCustomStatic",			&CUIGameCustom::AddCustomStatic)
			.def("RemoveCustomStatic",		&CUIGameCustom::RemoveCustomStatic)
			.def("ShowGameIndicators",		&CUIGameCustom::ShowGameIndicators)
			.def("GameIndicatorsShown",		&CUIGameCustom::GameIndicatorsShown)
			.def("GetCustomStatic",			&CUIGameCustom::GetCustomStatic)
			.def("OnKeyboardAction",			&CUIGameCustom::OnKeyboardAction)
			.def("OnMouseAction",			&CUIGameCustom::OnMouseAction),
			def("get_hud",					&get_hud)
		];
}
