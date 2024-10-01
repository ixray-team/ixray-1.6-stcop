
#pragma once
#include "../../xrScripts/script_export_space.h"

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
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
