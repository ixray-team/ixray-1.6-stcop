#include "pch_script.h"
#include "UIOptionsItem.h"
#include "UIOptionsManagerScript.h"

using namespace luabind;

void CUIOptionsManagerScript::SetCurrentValues(const char* group){
	CUIOptionsItem::GetOptionsManager()->SetCurrentValues(group);
}

void CUIOptionsManagerScript::SaveBackupValues(const char* group){
	CUIOptionsItem::GetOptionsManager()->SaveBackupValues(group);
}

void CUIOptionsManagerScript::SaveValues(const char* group){
	CUIOptionsItem::GetOptionsManager()->SaveValues(group);
}

bool CUIOptionsManagerScript::IsGroupChanged(const char* group){
	return CUIOptionsItem::GetOptionsManager()->IsGroupChanged(group);
}

void CUIOptionsManagerScript::UndoGroup(const char* group){
	CUIOptionsItem::GetOptionsManager()->UndoGroup(group);
}

void CUIOptionsManagerScript::OptionsPostAccept(){
	CUIOptionsItem::GetOptionsManager()->OptionsPostAccept();
}

void CUIOptionsManagerScript::SendMessage2Group(const char* group, const char* message){
	CUIOptionsItem::GetOptionsManager()->SendMessage2Group(group, message);
}

void CUIOptionsManagerScript::DoVidRestart(){
	CUIOptionsItem::GetOptionsManager()->DoVidRestart();
}

void CUIOptionsManagerScript::DoSndRestart(){
	CUIOptionsItem::GetOptionsManager()->DoSndRestart();
}

void CUIOptionsManagerScript::DoSystemRestart(){
	CUIOptionsItem::GetOptionsManager()->DoSystemRestart();
}

void CUIOptionsManagerScript::DoLevelRestart(){
	CUIOptionsItem::GetOptionsManager()->DoLevelRestart();
}

bool CUIOptionsManagerScript::NeedSystemRestart()
{
	return CUIOptionsItem::GetOptionsManager()->NeedSystemRestart();
}

bool CUIOptionsManagerScript::NeedVidRestart()
{
	return CUIOptionsItem::GetOptionsManager()->NeedVidRestart();
}

bool CUIOptionsManagerScript::NeedLevelRestart()
{
	return CUIOptionsItem::GetOptionsManager()->NeedLevelRestart();
}

#pragma optimize("s",on)
void CUIOptionsManagerScript::script_register(lua_State *L)
{
	module(L)
		[
			class_<CUIOptionsManagerScript>("COptionsManager")
			.def(						constructor<>())
			.def("SaveBackupValues",	&CUIOptionsManagerScript::SaveBackupValues )
			.def("SetCurrentValues",	&CUIOptionsManagerScript::SetCurrentValues )
			.def("SaveValues",			&CUIOptionsManagerScript::SaveValues )
			.def("IsGroupChanged",		&CUIOptionsManagerScript::IsGroupChanged )
			.def("UndoGroup",			&CUIOptionsManagerScript::UndoGroup )
			.def("OptionsPostAccept",	&CUIOptionsManagerScript::OptionsPostAccept )
			.def("SendMessage2Group",	&CUIOptionsManagerScript::SendMessage2Group )
			.def("NeedSystemRestart",	&CUIOptionsManagerScript::NeedSystemRestart )
			.def("NeedVidRestart",		&CUIOptionsManagerScript::NeedVidRestart )
			.def("NeedLevelRestart",	&CUIOptionsManagerScript::NeedLevelRestart)
			.def("DoVidRestart",		&CUIOptionsManagerScript::DoVidRestart )
			.def("DoSndRestart",		&CUIOptionsManagerScript::DoSndRestart )
			.def("DoSystemRestart",		&CUIOptionsManagerScript::DoSystemRestart )
			.def("DoLevelRestart",		&CUIOptionsManagerScript::DoLevelRestart )
		];
}