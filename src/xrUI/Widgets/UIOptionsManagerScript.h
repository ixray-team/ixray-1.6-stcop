
#pragma once
#include "../../xrScripts/script_export_space.h"

class UI_API CUIOptionsManagerScript 
{
public:
	void 		SaveBackupValues		(const char* group);
	void 		SetCurrentValues		(const char* group);
	void 		SaveValues				(const char* group);
	void 		UndoGroup				(const char* group);
	void 		OptionsPostAccept		();
	void 		SendMessage2Group		(const char* group, const char* message);
	bool 		NeedSystemRestart		();
	bool 		NeedVidRestart			();
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
