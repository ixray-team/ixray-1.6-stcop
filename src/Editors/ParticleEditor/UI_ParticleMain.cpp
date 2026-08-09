//---------------------------------------------------------------------------
#include "stdafx.h"
       

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
    return true;
}
CCommandVar CParticleTool::CommandEditPreviewProps(CCommandVar p1, CCommandVar p2)
{
    return true;
}

CCommandVar CParticleTool::CommandSaveXR(CCommandVar p1, CCommandVar p2)
{
    Save(true);
    return true;
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
    }
    return true;
}

CCommandVar CParticleTool::CommandSave(CCommandVar p1, CCommandVar p2)
{
    Save		(false);
    return 		true;
}
CCommandVar CParticleTool::CommandSaveBackup(CCommandVar p1, CCommandVar p2)
{
    ExecCommand(COMMAND_SAVE);
    return true;
}
CCommandVar CParticleTool::CommandReload(CCommandVar p1, CCommandVar p2)
{
    if (!IfModified()) 	return false;
    Reload				();
    return true;
}
CCommandVar CParticleTool::CommandValidate(CCommandVar p1, CCommandVar p2)
{
	Validate(true);
    return true;
}
CCommandVar CParticleTool::CommandClear(CCommandVar p1, CCommandVar p2)
{
    UI->CurrentView().m_Camera.Reset();
    ResetPreviewObject();
    return true;
}
CCommandVar CParticleTool::CommandPlayCurrent(CCommandVar p1, CCommandVar p2)
{
    PlayCurrent();
    return true;
}
CCommandVar CParticleTool::CommandStopCurrent(CCommandVar p1, CCommandVar p2)
{
    StopCurrent(p1);
    return true;
}

CCommandVar CommandJumpToItem(CCommandVar p1, CCommandVar p2)
{
    PTools->CommandJumpToItem();
    return true;
}

void CParticleMain::RegisterCommands()
{
	inherited::RegisterCommands();
    // tools
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