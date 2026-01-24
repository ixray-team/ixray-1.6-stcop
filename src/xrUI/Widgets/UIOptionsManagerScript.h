
#pragma once
#ifndef IXRAY_NO_LUA
#include "../../xrScripts/script_export_space.h"
#endif
class UI_API CUIOptionsManagerScript 
{
public:
	void 		SaveBackupValues		(LPCSTR group);
	void 		SetCurrentValues		(LPCSTR group);
	void 		SaveValues				(LPCSTR group);
	void 		UndoGroup				(LPCSTR group);
	void 		OptionsPostAccept		();
	void 		SendMessage2Group		(LPCSTR group, LPCSTR message);
	bool 		NeedSystemRestart		();
	bool 		NeedVidRestart			();
#ifndef IXRAY_NO_LUA
	DECLARE_SCRIPT_REGISTER_FUNCTION
#endif
};
