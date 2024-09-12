#include "stdafx.h"
#pragma hdrstop

#include "../Public/PropertiesListTypes.h"
//------------------------------------------------------------------------------
            
xr_string	ShortcutValue::GetDrawText		(TOnDrawTextEvent )
{
	xr_string 	txt;// = ToStr(MxShortCutToText(value->hotkey)).c_str();
	if (value->key == 0)
	{
		txt.append(g_pStringTable->translate("ed_st_none_caption").c_str());
		return txt;
	}
	if (value->ext.test(xr_shortcut::flCtrl))
	{
		txt.append("Ctrl +");
	}
	if(value->ext.test(xr_shortcut::flShift))
	{
		txt.append("Shift +");
	}
	if (value->ext.test(xr_shortcut::flAlt))
	{
		txt.append("Alt +");
	}

	txt += SDL_GetScancodeName((SDL_Scancode)value->key);

	return 			txt;
}

xr_string GameTypeValue::GetDrawText(TOnDrawTextEvent)
{
	string512 str;
    xr_sprintf(str,sizeof(str),"%s%s%s%s%s",
	GetValue().MatchType(eGameIDSingle) ? g_pStringTable->translate("ed_st_sp").c_str() : "",
	GetValue().MatchType(eGameIDDeathmatch) ? g_pStringTable->translate("ed_st_dm").c_str() : "",
	GetValue().MatchType(eGameIDTeamDeathmatch) ? g_pStringTable->translate("ed_st_tdm").c_str() :"",
	GetValue().MatchType(eGameIDArtefactHunt) ? g_pStringTable->translate("ed_st_ah").c_str() :"",
	GetValue().MatchType(eGameIDCaptureTheArtefact) ? g_pStringTable->translate("ed_st_cta").c_str() :""    );
	return xr_string(str);
}
