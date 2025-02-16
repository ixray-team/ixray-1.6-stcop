
#pragma once
#include "../../xrScripts/script_export_space.h"

class CUIOptionsManagerScript
{
public:
	void 		SaveBackupValues		(LPCSTR group);
	void 		SetCurrentValues		(LPCSTR group);
	void 		SaveValues				(LPCSTR group);
	bool 		IsGroupChanged				(LPCSTR group);
	void 		UndoGroup				(LPCSTR group);
	void 		OptionsPostAccept		();
	void 		SendMessage2Group		(LPCSTR group, LPCSTR message);
	bool 		NeedSystemRestart		();
	bool 		NeedVidRestart			();
	bool 		NeedLevelRestart		();
	void 		DoVidRestart		();
	void 		DoSndRestart		();
	void 		DoSystemRestart		();
	void 		DoLevelRestart		();
	DECLARE_SCRIPT_REGISTER_FUNCTION
};

