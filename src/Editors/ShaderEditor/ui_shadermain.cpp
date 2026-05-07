//---------------------------------------------------------------------------
#include "stdafx.h"


#include "ui_shadermain.h"
#include "UI_shadertools.h"
#include "../../xrEngine/xr_input.h"

//---------------------------------------------------------------------------

CShaderMain::CShaderMain()
{
    EPrefs			= new CCustomPreferences();
}
//---------------------------------------------------------------------------

CShaderMain::~CShaderMain()
{
    xr_delete		(EPrefs);
}
//---------------------------------------------------------------------------

CCommandVar CShaderTool::CommandSave(CCommandVar p1, CCommandVar p2)
{
    Save			(0,0);
    return true;
}
CCommandVar CShaderTool::CommandSaveBackup(CCommandVar p1, CCommandVar p2)
{
    ExecCommand		(COMMAND_SAVE);
    return true;
}
CCommandVar CShaderTool::CommandReload(CCommandVar p1, CCommandVar p2)
{
    Reload			();
    return true;
}
CCommandVar CShaderTool::CommandClear(CCommandVar p1, CCommandVar p2)
{
    UI->CurrentView().m_Camera.Reset();
    return true;
}
CCommandVar CShaderTool::CommandUpdateList(CCommandVar p1, CCommandVar p2)
{
	UpdateList		();
    return true;
}

void CShaderMain::RegisterCommands()
{
	inherited::RegisterCommands();
    // tools
	REGISTER_CMD_CE	(COMMAND_SAVE,           	"File\\Save", 	STools,CShaderTool::CommandSave,true);
	REGISTER_CMD_C	(COMMAND_SAVE_BACKUP,	 	STools,CShaderTool::CommandSaveBackup);
	REGISTER_CMD_CE	(COMMAND_LOAD,			 	"File\\Reload",	STools,CShaderTool::CommandReload,true);
	REGISTER_CMD_CE	(COMMAND_CLEAR,			 	"File\\Clear", 	STools,CShaderTool::CommandClear,true);
    REGISTER_CMD_CE	(COMMAND_UPDATE_LIST,	 	"Update List",	STools,CShaderTool::CommandUpdateList,true);
}

void CShaderMain::OnDrawUI()
{
    TUI::OnDrawUI();
    for (auto& tool : STools->m_Tools)
    {
        tool.second->OnDrawUI();
    }
}

char* CShaderMain::GetCaption()
{
	return (LPSTR)STools->CurrentToolsName();// "shaders&materials";
}           

bool  CShaderMain::ApplyShortCut(DWORD Key, TShiftState Shift)
{
    return inherited::ApplyShortCut(Key,Shift);
}
//---------------------------------------------------------------------------

bool  CShaderMain::ApplyGlobalShortCut(DWORD Key, TShiftState Shift)
{
    return inherited::ApplyGlobalShortCut(Key,Shift);
}
//---------------------------------------------------------------------------

void CShaderMain::RealUpdateScene()
{
	inherited::RealUpdateScene	();

}
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// Common
//---------------------------------------------------------------------------
void CShaderMain::ResetStatus()
{
	/*VERIFY(m_bReady);
    if (fraBottomBar->paStatus->Caption!=""){
	    fraBottomBar->paStatus->Caption=""; fraBottomBar->paStatus->Repaint();
    }*/
}
void CShaderMain::SetStatus(const char* s, bool bOutLog)
{
/*	VERIFY(m_bReady);
    if (fraBottomBar->paStatus->Caption!=s){
	    fraBottomBar->paStatus->Caption=s; fraBottomBar->paStatus->Repaint();
    	if (bOutLog&&s&&s[0]) ELog.Msg(mtInformation,s);
    }*/
}
void CShaderMain::ProgressDraw()
{
    inherited::ProgressDraw();
	//fraBottomBar->RedrawBar();
}
//---------------------------------------------------------------------------
void CShaderMain::RealQuit()
{
    UI->Quit();
}
//---------------------------------------------------------------------------

