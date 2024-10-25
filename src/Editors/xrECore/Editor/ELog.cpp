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
	if (0 == txt[0])
		return;

	TMsgDlgType mt = TMsgDlgType::mtCustom;
	if (strncmp(txt, "! ", 2) == 0)
	{
		mt = mtError;
		UILogForm::SetActive();
	
	}
	else if (strncmp(txt, "~ ", 2) == 0)
	{
		mt = mtConfirmation;
	}
	else if (strncmp(txt, "* ", 2) == 0)
	{
		mt = mtInformation;
	}

	UILogForm::AddMessage(txt);
}

//----------------------------------------------------
CLog ELog;
//----------------------------------------------------
inline TMsgDlgButtons MessageDlg(const char*text, TMsgDlgType mt, int btn)
{
	SDL_MessageBoxFlags Flags = SDL_MESSAGEBOX_BUTTONS_LEFT_TO_RIGHT;
	const char* Title = "";
	switch (mt)
	{
	case mtCustom:
		break;
	case mtError:
		Title = "Error";
		Flags = SDL_MESSAGEBOX_ERROR;
		break;
	case mtInformation:
		Title = "Info";
		Flags = SDL_MESSAGEBOX_INFORMATION;
		break;
	case mtConfirmation:
		Title = "Warning";
		Flags = SDL_MESSAGEBOX_WARNING;
		break;
	default:
		R_ASSERT(0);
		break;
	}

	const SDL_MessageBoxButtonData btnOk[] =
	{
		{ 0, 0, "OK" },
	};

	const SDL_MessageBoxButtonData btnYes[] =
	{
		{ 0, 0, "OK" },
	};

	const SDL_MessageBoxButtonData btnYesNo[] =
	{
		{ 0, 0, "Yes" },
		{ 0, 1, "No" },
	};

	const SDL_MessageBoxButtonData btnYesNoCancel[] =
	{
		{ 0, 0, "Yes" },
		{ 0, 1, "No" },
		{ 0, 2, "Cancel" },
	};

	const SDL_MessageBoxData messageboxYes =
	{
		(u32)Flags, /* .flags */
		nullptr,						/* .window */
		Title,						/* .title */
		text,						/* .message */
		SDL_arraysize(btnYes),		/* .numbuttons */
		btnYes,					/* .buttons */
		nullptr						/* .colorScheme */
	};

	const SDL_MessageBoxData messageboxOk =
	{
		(u32)Flags, /* .flags */
		nullptr,						/* .window */
		Title,						/* .title */
		text,						/* .message */
		SDL_arraysize(btnOk),		/* .numbuttons */
		btnOk,					/* .buttons */
		nullptr						/* .colorScheme */
	};

	const SDL_MessageBoxData messageboxYesNo =
	{
		(u32)Flags, /* .flags */
		nullptr,						/* .window */
		Title,						/* .title */
		text,						/* .message */
		SDL_arraysize(btnYesNo),		/* .numbuttons */
		btnYesNo,					/* .buttons */
		nullptr						/* .colorScheme */
	};

	const SDL_MessageBoxData messageboxYesNoCancel =
	{
		(u32)Flags, /* .flags */
		nullptr,						/* .window */
		Title,						/* .title */
		text,						/* .message */
		SDL_arraysize(btnYesNoCancel),		/* .numbuttons */
		btnYesNoCancel,					/* .buttons */
		nullptr						/* .colorScheme */
	};

	int buttonid = -1;

	if (btn == mbYes)
	{
		SDL_ShowMessageBox(&messageboxYes, &buttonid);
	}
	if (btn == mbOK)
	{
		SDL_ShowMessageBox(&messageboxOk, &buttonid);
	}
	else if (btn == (mbYes | mbNo))
	{
		SDL_ShowMessageBox(&messageboxYesNo, &buttonid);
	}
	else if (btn == (mbYes | mbNo | mbCancel))
	{
		SDL_ShowMessageBox(&messageboxYesNoCancel, &buttonid);
	}
	else
	{
		R_ASSERT(0);
	}

	if (btn != mbOK)
	{
		switch (buttonid)
		{
		case 0:
			return mrYes;
			break;
		case 1:
			return mrNo;
			break;
		case 2:
			return mrCancel;	// TODO: add code
			break;
		}
	}
	else 
	{
		switch (buttonid)
		{
		case 0:
			return mrOK;
			break;
		}
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
