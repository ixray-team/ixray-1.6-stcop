// File:        UIComboBox_script.cpp
// Description: exports CUIComobBox to LUA environment
// Created:     11.12.2004
// Author:      Serhiy O. Vynnychenko
// Mail:        narrator@gsc-game.kiev.ua
//
// Copyright 2004 GSC Game World
//

#include "pch_script.h"
#include "UIComboBox.h"
#include "UIListBoxItem.h"

using namespace luabind;

#pragma optimize("s",on)
void CUIComboBox::script_register(lua_State *L)
{
	module(L)
	[
		class_<CUIComboBox, CUIWindow>("CUIComboBox")
		.def(						constructor<>())
		.def("Init",				(void (CUIComboBox::*)(float, float, float))   &CUIComboBox::InitComboBox)
		.def("Init",				(void (CUIComboBox::*)(Fvector2, float))   &CUIComboBox::InitComboBox)
		.def("Init",				(void (CUIComboBox::*)(float, float, float, float))   &CUIComboBox::InitComboBox)	
		.def("SetVertScroll",		&CUIComboBox::SetVertScroll)
		.def("SetListLength",		&CUIComboBox::SetListLength)
		.def("CurrentID",			&CUIComboBox::CurrentID)
		.def("SetCurrentID",		&CUIComboBox::SetItemIDX)
		.def("AddItemToBox",		&CUIComboBox::AddItem_script)
		.def("disable_id",			&CUIComboBox::disable_id)
		.def("enable_id",			&CUIComboBox::enable_id)
		.def("AddItem",				&CUIComboBox::AddItem_)
		.def("GetText",				&CUIComboBox::GetText)
		.def("GetTextOf",			&CUIComboBox::GetTextOf)
		.def("SetText",				&CUIComboBox::SetText)
		.def("ClearList",			&CUIComboBox::ClearList)
		.def("SetCurrentOptValue",	&CUIComboBox::SetCurrentOptValue)
		.def("SetCurrentValueScript",&CUIComboBox::SetCurrentValueScript)
	];
}