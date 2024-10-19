//---------------------------------------------------------------------------
#include "stdafx.h"
#pragma hdrstop       

#include "UI_ParticleMain.h"
#include "UI_ParticleTools.h"     
#include "../xrEngine/xr_input.h"

//---------------------------------------------------------------------------
CParticleMain*	PUI=(CParticleMain*)UI;
//---------------------------------------------------------------------------

CParticleMain::CParticleMain()  
{
    EPrefs			= new CCustomPreferences();
}
//---------------------------------------------------------------------------

CParticleMain::~CParticleMain()
{
    xr_delete		(EPrefs);
}
//---------------------------------------------------------------------------

CCommandVar CParticleTool::CommandSelectPreviewObj(CCommandVar p1, CCommandVar p2)
{
    SelectPreviewObject(p1);
    return TRUE;
}
CCommandVar CParticleTool::CommandEditPreviewProps(CCommandVar p1, CCommandVar p2)
{
    return TRUE;
}

CCommandVar CParticleTool::CommandSaveXR(CCommandVar p1, CCommandVar p2)
{
    Save							(true);
    ExecCommand						(COMMAND_UPDATE_CAPTION);
    return TRUE;
}

CCommandVar CParticleTool::CommandLoadXR(CCommandVar p1, CCommandVar p2)
{
    xr_string temp_fn;
    if (EFS.GetOpenName("$game_data$", temp_fn, false, NULL, 0))
    {
        string_path gamedata = {};
        FS.update_path(gamedata, "$game_data$", "");
        xr_path temp = gamedata;
        size_t Pos = temp_fn.find(temp.xfilename(), 0);

        if (Pos == xr_string::npos)
        {
            Msg("Incorrect Path!!! [%s]", temp_fn.c_str());
            return false;
        }

        xr_string NormalPath = temp_fn.substr(Pos);
        RImplementation.PSLibrary.OnDestroy();
        RImplementation.PSLibrary.Load(NormalPath.c_str());
        PTools->ResetCurrent();
        ExecCommand(COMMAND_UPDATE_PROPERTIES);
        ExecCommand(COMMAND_UPDATE_CAPTION);
    }
    return TRUE;
}

CCommandVar CParticleTool::CommandSave(CCommandVar p1, CCommandVar p2)
{
    Save		(false);
    ExecCommand	(COMMAND_UPDATE_CAPTION);
    return 		TRUE;
}
CCommandVar CParticleTool::CommandSaveBackup(CCommandVar p1, CCommandVar p2)
{
    ExecCommand(COMMAND_SAVE);
    return TRUE;
}
CCommandVar CParticleTool::CommandReload(CCommandVar p1, CCommandVar p2)
{
    if (!IfModified()) 	return FALSE;
    Reload				();
    ExecCommand(COMMAND_UPDATE_CAPTION);
    return TRUE;
}
CCommandVar CParticleTool::CommandValidate(CCommandVar p1, CCommandVar p2)
{
	Validate(true);
    return TRUE;
}
CCommandVar CParticleTool::CommandClear(CCommandVar p1, CCommandVar p2)
{
    UI->CurrentView().m_Camera.Reset();
    ResetPreviewObject();
    ExecCommand(COMMAND_UPDATE_CAPTION);
    return TRUE;
}
CCommandVar CParticleTool::CommandPlayCurrent(CCommandVar p1, CCommandVar p2)
{
    PlayCurrent();
    return TRUE;
}
CCommandVar CParticleTool::CommandStopCurrent(CCommandVar p1, CCommandVar p2)
{
    StopCurrent(p1);
    return TRUE;
}

CCommandVar CommandRefreshUIBar(CCommandVar p1, CCommandVar p2)
{
    /*fraTopBar->RefreshBar	();
    fraLeftBar->RefreshBar	();
    fraBottomBar->RefreshBar();*/
    return TRUE;
}
CCommandVar CommandRestoreUIBar(CCommandVar p1, CCommandVar p2)
{
   /* fraTopBar->fsStorage->RestoreFormPlacement();
    fraLeftBar->fsStorage->RestoreFormPlacement();
    fraBottomBar->fsStorage->RestoreFormPlacement();*/
    return TRUE;
}
CCommandVar CommandSaveUIBar(CCommandVar p1, CCommandVar p2)
{
  /*  fraTopBar->fsStorage->SaveFormPlacement();
    fraLeftBar->fsStorage->SaveFormPlacement();
    fraBottomBar->fsStorage->SaveFormPlacement();*/
    return TRUE;
}
CCommandVar CommandUpdateToolBar(CCommandVar p1, CCommandVar p2)
{
    /*fraLeftBar->UpdateBar();*/
    return TRUE;
}
CCommandVar CommandUpdateCaption(CCommandVar p1, CCommandVar p2)
{
    /*frmMain->UpdateCaption();*/
    return TRUE;
}

CCommandVar CommandJumpToItem(CCommandVar p1, CCommandVar p2)
{
    PTools->CommandJumpToItem();
    return TRUE;
}

void CParticleMain::RegisterCommands()
{
	inherited::RegisterCommands();
    // tools       
	//REGISTER_CMD_CE	(COMMAND_SELECT_PREVIEW_OBJ,"Select Preview Object",PTools,CParticleTool::CommandSelectPreviewObj, true);
	//REGISTER_CMD_CE	(COMMAND_EDIT_PREVIEW_PROPS,"Select Preview Props",	PTools,CParticleTool::CommandEditPreviewProps, true);
	REGISTER_CMD_CE	(COMMAND_SAVE,            	"File\\Save",			PTools,CParticleTool::CommandSave, true);
	REGISTER_CMD_C	(COMMAND_SAVE_BACKUP,       PTools,CParticleTool::CommandSaveBackup);
	REGISTER_CMD_CE	(COMMAND_LOAD,            	"File\\Reload",			PTools,CParticleTool::CommandReload, true);
	REGISTER_CMD_C	(COMMAND_VALIDATE,          PTools,CParticleTool::CommandValidate);
	REGISTER_CMD_CE	(COMMAND_CLEAR,             "File\\Clear",			PTools,CParticleTool::CommandClear, true);
	REGISTER_CMD_CE	(COMMAND_PLAY_CURRENT,      "Particles\\Play",		PTools,CParticleTool::CommandPlayCurrent, true);
    REGISTER_SUB_CMD_CE (COMMAND_STOP_CURRENT,	"Particles",			PTools,CParticleTool::CommandStopCurrent, true);
    	APPEND_SUB_CMD	("Stop Immediate",		0,0);
    	APPEND_SUB_CMD	("Stop Deffered", 		1,0);
    REGISTER_SUB_CMD_END;
	REGISTER_CMD_S	(COMMAND_REFRESH_UI_BAR,    CommandRefreshUIBar);
	REGISTER_CMD_S	(COMMAND_RESTORE_UI_BAR,    CommandRestoreUIBar);
	REGISTER_CMD_S	(COMMAND_SAVE_UI_BAR,     	CommandSaveUIBar);
	REGISTER_CMD_S	(COMMAND_UPDATE_TOOLBAR,    CommandUpdateToolBar);
	REGISTER_CMD_S	(COMMAND_UPDATE_CAPTION,    CommandUpdateCaption);
	REGISTER_CMD_S	(COMMAND_JUMP_TO_ITEM,     CommandJumpToItem);
	REGISTER_CMD_C	(COMMAND_SAVE_XR,     		PTools, CParticleTool::CommandSaveXR);
	REGISTER_CMD_C	(COMMAND_LOAD_XR,     		PTools, CParticleTool::CommandLoadXR);
	REGISTER_CMD_C	(COMMAND_COMPACT_PARTICLES,	PTools, CParticleTool::Compact);
	REGISTER_CMD_CE	(COMMAND_CREATE_GROUP_FROM_SELECTED,"Particles\\CreateGroupFromEffect",	PTools, CParticleTool::CreateGroupFromSelected, true);
}

void CParticleMain::OnDrawUI()
{
    inherited::OnDrawUI();
    PTools->OnDrawUI();
}

char* CParticleMain::GetCaption()
{
	return (char*)"particles";
}

bool  CParticleMain::ApplyShortCut(DWORD Key, TShiftState Shift)
{
    return inherited::ApplyShortCut(Key,Shift);
}
//---------------------------------------------------------------------------

bool  CParticleMain::ApplyGlobalShortCut(DWORD Key, TShiftState Shift)
{
    return inherited::ApplyGlobalShortCut(Key,Shift);
}
//---------------------------------------------------------------------------

void CParticleMain::RealUpdateScene()
{
	inherited::RealUpdateScene	();
}
//---------------------------------------------------------------------------


//---------------------------------------------------------------------------
// Common
//---------------------------------------------------------------------------
void CParticleMain::ResetStatus()
{
	/*VERIFY(m_bReady);
    if (fraBottomBar->paStatus->Caption!=""){
	    fraBottomBar->paStatus->Caption=""; fraBottomBar->paStatus->Repaint();
    }*/
}
void CParticleMain::SetStatus(LPCSTR s, bool bOutLog)
{
	/*VERIFY(m_bReady);
    if (fraBottomBar->paStatus->Caption!=s){
	    fraBottomBar->paStatus->Caption=s; fraBottomBar->paStatus->Repaint();
    	if (bOutLog&&s&&s[0]) ELog.Msg(mtInformation,s);
    }*/
}
void CParticleMain::ProgressDraw()
{
    inherited::ProgressDraw();
/*	fraBottomBar->RedrawBar();*/
}

//---------------------------------------------------------------------------
void CParticleMain::RealQuit()
{
    inherited::Quit();
}
//---------------------------------------------------------------------------

