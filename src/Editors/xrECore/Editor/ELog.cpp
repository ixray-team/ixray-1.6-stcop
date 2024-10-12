//----------------------------------------------------
// file: NetDeviceELog.cpp
//----------------------------------------------------

#include "stdafx.h"
#pragma hdrstop

#include "ELog.h"
#include "UILogForm.h"
#include "ui_main.h"
void  ELogCallback(LPCSTR txt)
{
	if (0 == txt[0]) return;
	TMsgDlgType mt = TMsgDlgType::mtCustom;
	if (strncmp(txt, "! ", 2) == 0)
	{
		mt = mtError;
	}
	if (strncmp(txt, "~ ", 2) == 0)
	{
		mt = mtConfirmation;
	}
	if (strncmp(txt, "* ", 2) == 0)
	{
		mt = mtInformation;
	}

	UILogForm::AddMessage(txt);
	//if(UI)
	//	UI->WriteConsole(mt, txt);

}

//----------------------------------------------------
CLog ELog;
//----------------------------------------------------
inline TMsgDlgButtons MessageDlg(const char*text, TMsgDlgType mt, int btn)
{
	UINT Flags = 0;
	const char* Title = "";
	switch (mt)
	{
	case mtCustom:
		break;
	case mtError:
		Title = "Error";
		Flags = MB_ICONERROR;
		break;
	case mtInformation:
		Title = "Info";
		Flags = MB_ICONINFORMATION;
		break;
	case mtConfirmation:
		Title = "Warning";
		Flags = MB_ICONWARNING;
		break;
	default:
		R_ASSERT(0);
		break;
	}
	if (btn == mbYes)
	{
		Flags |=  MB_OK;
	}
	if (btn == mbOK)
	{
		Flags |= MB_OK;
	}
	else if(btn==(mbYes|mbNo))
	{
		Flags |= MB_YESNO;
	}
	else if (btn == (mbYes | mbNo|mbCancel))
	{
		Flags |= MB_YESNOCANCEL;
	}
	else
	{
		R_ASSERT(0);
	}
	int msgboxID = MessageBoxA(
		NULL,
		text,
		Title,
		Flags
	);
	switch (msgboxID)
	{
	case IDCANCEL:
		return mrCancel;	// TODO: add code
		break;
	case IDYES:
		return mrYes;
		break;
	case IDNO:
		return mrNo;
		break;
	case IDOK:
		return mrOK;
		break;
	}
	if (btn | mbCancel)return mrCancel;
	if (btn | mbNo)return mrNo;
	if (btn == mbOK)return mrOK;
	return mrYes;
}
int CLog::DlgMsg (TMsgDlgType mt, int btn, LPCSTR _Format, ...)
{
    in_use = true;
	char buf[4096];
	va_list l;
	va_start( l, _Format );
	vsprintf( buf, _Format, l );

	int res=0;
#if 1 
    ExecCommand(COMMAND_RENDER_FOCUS);

    res=MessageDlg(buf, mt, btn);
    if (mtConfirmation==mt){
        switch (res){
        case mrYes: 	strcat(buf," - Yes."); 	break;
        case mrNo: 		strcat(buf," - No."); 	break;
        case mrCancel:  strcat(buf," - Cancel.");	break;
        default: strcat(buf," - Something.");
        }
    }
#endif
#ifdef _LW_EXPORT
	switch(mt){
	case mtError:		g_msg->error(buf,0);	break;
	case mtInformation: g_msg->info(buf,0);		break;
	default:			g_msg->info(buf,0);		break;
	}
#endif
#ifdef _MAX_PLUGIN
	switch(mt){
	case mtError:		MessageBox(0,buf,"Error",		MB_OK|MB_ICONERROR);		break;
	case mtInformation: MessageBox(0,buf,"Information",	MB_OK|MB_ICONINFORMATION);	break;
	default:			MessageBox(0,buf,"Information",	MB_OK|MB_ICONINFORMATION);	break;
	}
#endif

    Msg(mt, buf);

    in_use = false;

    return res;
}

void CLog::Close()
{
	//SetLogCB(0);
	UILogForm::Destroy();
}


int CLog::DlgMsg (TMsgDlgType mt, LPCSTR _Format, ...)
{
    in_use = true;
	char buf[4096];
	va_list l;
	va_start( l, _Format );
	vsprintf( buf, _Format, l );

    int res=0;
#if 1
    ExecCommand(COMMAND_RENDER_FOCUS);

    if (mtConfirmation==mt)	res=MessageDlg(buf, mt,  mbYes | mbNo | mbCancel);
    else                   	res=MessageDlg(buf, mt,  mbOK);

    if (mtConfirmation==mt){
        switch (res){
        case mrYes: 	strcat(buf," - Yes."); 	break;
        case mrNo: 		strcat(buf," - No."); 	break;
        case mrCancel:  strcat(buf," - Cancel.");	break;
        default: strcat(buf," - Something.");
        }
    }
#endif
#ifdef _LW_EXPORT
	switch(mt){
	case mtError:		g_msg->error(buf,0);	break;
	case mtInformation: g_msg->info(buf,0);		break;
	default:			g_msg->info(buf,0);		break;
	}
#endif
#ifdef _MAX_PLUGIN
	switch(mt){
	case mtError:		MessageBox(0,buf,"Error",		MB_OK|MB_ICONERROR);		break;
	case mtInformation: MessageBox(0,buf,"Information",	MB_OK|MB_ICONINFORMATION);	break;
	default:			MessageBox(0,buf,"Information",	MB_OK|MB_ICONINFORMATION);	break;
	}
#endif

    Msg(mt,buf);

    in_use = false;
    
    return res;
}

void CLog::Msg(TMsgDlgType mt, LPCSTR _Format, ...)
{
	char buf[4096];
	va_list l;
	va_start(l, _Format);
	vsprintf(buf, _Format, l);

	xr_string OutString = buf;

	switch (mt)
	{
		case mtError: OutString = "! " + OutString; break;
	}

	//UILogForm::AddMessage(OutString);

	::Msg(OutString.c_str());
}
//----------------------------------------------------
