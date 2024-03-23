#pragma once

#include <wx/wx.h>

#include <chrono>

#ifdef _WINDOWS_
#include <wx/msw/msgdlg.h>
#else
#include <wx/gtk2/msgdlg.h>
#endif

class Main : public wxApp
{
public:
	virtual bool OnInit();
	
};

IMPLEMENT_APP(Main)
