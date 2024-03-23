#pragma	once

#include <wx/wx.h>
#include <wx/frame.h>

#ifdef _WINDOWS_
#include <wx/msw/listbox.h>
#else
#include <wx/gtk2/listbox.h>
#endif


class LogWindow : public wxFrame
{
public:
	LogWindow(const wxString& title);
	//void OnClick(wxCommandEvent& event);
  

	wxListBox* idcLog;
	wxListBox* idcPhaseTime;
	wxStaticBox* idcStageBox;
	wxStaticText* idcInfo;
	wxStaticText* idcTiming;
	wxStaticBox* idcPhaseBox;
	wxStaticText* idcStage;
	wxStaticText* idcPText;
	wxGauge* idcProgress;
};

extern class i_lc_log
{
public:
	virtual void clMsg(LPCSTR msg) = 0;
	virtual void clLog(LPCSTR msg) = 0;
	virtual void Status(LPCSTR msg) = 0;
	virtual	void Progress(const float F) = 0;
	virtual	void Phase(LPCSTR phase_name) = 0;
} *lc_log;

void clMsg(const char* format, ...);
void Status(const char* format, ...);
void Progress(const float F);
void Phase(const char* phase_name);
void logThread(void *dummy);
void logCallback(LPCSTR c);

void addString(wxWindowID id, int position, wxString message);
void deleteString(wxWindowID id);
