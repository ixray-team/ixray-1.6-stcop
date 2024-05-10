#include "stdafx.h"
#pragma hdrstop

#include "../Public/PropertiesListTypes.h"
//------------------------------------------------------------------------------
            
xr_string	ShortcutValue::GetDrawText		(TOnDrawTextEvent )
{
	xr_string 	txt;// = ToStr(MxShortCutToText(value->hotkey)).c_str();
	if (value->key == 0)
	{
		txt.append("none");
		return txt;
	}
	if (value->ext.test(xr_shortcut::flCtrl))
	{
		txt.append("Ctrl+");
	}
	if(value->ext.test(xr_shortcut::flShift))
	{
		txt.append("Shift+");
	}
	if (value->ext.test(xr_shortcut::flAlt))
	{
		txt.append("Alt+");
	}

	txt += SDL_GetScancodeName((SDL_Scancode)value->key);

	return 			txt;
}

xr_string GameTypeValue::GetDrawText(TOnDrawTextEvent)
{
	string512 str;
    xr_sprintf(str,sizeof(str),"%s%s%s%s%s",
	GetValue().MatchType(eGameIDSingle)?"Single ":"",
	GetValue().MatchType(eGameIDDeathmatch)?"DM ":"",
	GetValue().MatchType(eGameIDTeamDeathmatch)?"TDM ":"",
	GetValue().MatchType(eGameIDArtefactHunt)?"AH ":"",
	GetValue().MatchType(eGameIDCaptureTheArtefact)?"CTA":""    );
	return xr_string(str);
}
