//----------------------------------------------------
// file: NetDeviceELog.cpp
//----------------------------------------------------

#include "stdafx.h"

#include "ELog.h"
#include "UILogForm.h"
#include "ui_main.h"

//----------------------------------------------------
CLog ELog;

namespace details
{
	static constexpr SDL_MessageBoxButtonData btnOk		= { 0, mrOK, "OK" };
	static constexpr SDL_MessageBoxButtonData btnYes	= { 0, mrYes, "Yes" };
	static constexpr SDL_MessageBoxButtonData btnNo		= { 0, mrNo, "No" };
	static constexpr SDL_MessageBoxButtonData btnCancel	= { SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT , mrCancel, "Cancel" };
	static constexpr SDL_MessageBoxButtonData btnSkip	= { 0, mrSkip, "Skip All" };
};

void  ELogCallback(LPCSTR txt)
{
	if (0 == txt[0])
		return;

	TMsgDlgType mt = TMsgDlgType::mtCustom;
	if (strncmp(txt, "! ", 2) == 0 || strncmp(txt, "ERROR: ", 8) == 0)
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
inline TMsgDlgButtons MessageDlg(const char*text, TMsgDlgType mt, int btn)
{
	int Flags = SDL_MESSAGEBOX_BUTTONS_LEFT_TO_RIGHT;
	const char* Title = "";
	switch (mt)
	{
	case mtCustom:
		break;
	case mtSkip:
	case mtError:
		Title = "Error";
		Flags = Flags | SDL_MESSAGEBOX_ERROR;
		break;
	case mtInformation:
		Title = "Info";
		Flags = Flags | SDL_MESSAGEBOX_INFORMATION;
		break;
	case mtConfirmation:
		Title = "Confirmation";
		Flags = Flags | SDL_MESSAGEBOX_INFORMATION;
		break;
	case mtWarning:
		Title = "Warning";
		Flags = Flags | SDL_MESSAGEBOX_WARNING;
		break;
	default:
		R_ASSERT(0);
		break;
	}

	xr_vector<SDL_MessageBoxButtonData> ButtonsList;

	int buttonid = -1;

	if (btn == mbYes || btn == mbOK)
	{
		ButtonsList.push_back(details::btnOk);
	}
	else if (btn == (mbOK | mbCancel))
	{
		ButtonsList.push_back(details::btnOk);
		ButtonsList.push_back(details::btnCancel);
	}
	else if (btn == (mbOK | mbSkip))
	{
		ButtonsList.push_back(details::btnOk);
		ButtonsList.push_back(details::btnSkip);
	}
	else if (btn == (mbYes | mbNo))
	{
		ButtonsList.push_back(details::btnYes);
		ButtonsList.push_back(details::btnNo);

		if (mt == mtSkip)
		{
			ButtonsList.push_back(details::btnSkip);
		}
	}
	else if (btn == (mbYes | mbNo | mbCancel))
	{
		ButtonsList.push_back(details::btnYes);
		ButtonsList.push_back(details::btnNo);
		ButtonsList.push_back(details::btnCancel);
	}
	else
	{
		R_ASSERT(0);
	}

	const SDL_MessageBoxData messageboxContainer =
	{
		(u32)Flags,												/* .flags */
		nullptr,												/* .window */
		Title,													/* .title */
		text,													/* .message */
		ButtonsList.size(),	/* .numbuttons */
		ButtonsList.data(),										/* .buttons */
		nullptr													/* .colorScheme */
	};

	SDL_ShowMessageBox(&messageboxContainer, &buttonid);

	return (TMsgDlgButtons)buttonid;
}

int CLog::DlgMsg (TMsgDlgType mt, int btn, LPCSTR _Format, ...)
{
    in_use = true;
	char buf[4096];
	va_list l;
	va_start( l, _Format );
	vsprintf( buf, _Format, l );

	int res=0;

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

    Msg(mt, buf);

    in_use = false;

    return res;
}

void CLog::Close()
{
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
    ExecCommand(COMMAND_RENDER_FOCUS);

    if (mtConfirmation==mt)	res = MessageDlg(buf, mt,  mbYes | mbNo | mbCancel);
	else if (mtSkip == mt)	res = MessageDlg(buf, mtConfirmation, mbOK | mbSkip);
    else                   	res = MessageDlg(buf, mt,  mbOK);

    if (mtConfirmation==mt){
        switch (res){
        case mrYes: 	strcat(buf," - Yes."); 	break;
        case mrNo: 		strcat(buf," - No."); 	break;
        case mrCancel:  strcat(buf," - Cancel.");	break;
        default: strcat(buf," - Something.");
        }
    }

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

	::Msg(OutString.c_str());
}
//----------------------------------------------------
