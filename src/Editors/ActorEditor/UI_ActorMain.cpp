#include "stdafx.h"
#include "UI_ActorMain.h"

CActorMain*	AUI=(CActorMain*)UI;

CCommandVar CActorTools::CommandSaveBackup(CCommandVar p1, CCommandVar p2)
{
	string_path 		fn;
	xr_strconcat		(fn, Core.UserName, "_backup.object");
	FS.update_path		(fn,"$objects$",fn);
	ExecCommand			(COMMAND_SAVE,xr_string(fn));
	return true;
}
CCommandVar CActorTools::CommandSave(CCommandVar p1, CCommandVar p2)
{
	if (p2==1){
		xr_string temp_fn	= ATools->m_LastFileName.c_str();
		if (EFS.GetSaveName(_objects_, temp_fn, nullptr, -1, "*.object;*.ogf")){
			if (!strext(temp_fn.c_str()))
				temp_fn += ".object";
			return 			ExecCommand(COMMAND_SAVE,temp_fn,0);
		}
	}else{
		if (p1.IsInteger())
			return 				ExecCommand(COMMAND_SAVE,xr_string(ATools->m_LastFileName.c_str()),0);
		xr_string temp_fn		= xr_string(p1);
		if (temp_fn.empty()){
			return 				ExecCommand(COMMAND_SAVE,temp_fn,1);
		}else{
			xr_strlwr			(temp_fn);
			CTimer T;
			T.Start();
			CCommandVar			res;
			if (Tools->Save(temp_fn.c_str())){
				ELog.Msg		(mtInformation,"Object '%s' successfully saved. Saving time - %3.2f(s).",m_LastFileName.c_str(),T.GetElapsed_sec());
				m_LastFileName	= temp_fn.c_str();
				EPrefs->AppendRecentFile	(m_LastFileName.c_str());
				res				= true;
			}else{
				res				= false;
			}
			return 				res;
		}
	}
	return 					false;
}

CCommandVar CActorTools::CommandImport(CCommandVar p1, CCommandVar p2)
{
	xr_string temp_fn = p1.IsString() ? xr_string(p1) : xr_string("");
	if (p1.IsString() || EFS.GetOpenName(_import_, temp_fn))
	{
		FS_Path* pp = FS.get_path(_import_);

		if (temp_fn.npos != temp_fn.find(pp->m_Path))
		{
			xr_strlwr(temp_fn);
			temp_fn = FS.fix_path(temp_fn);
			FS.TryLoad(temp_fn);

			if (!Tools->IfModified())
				return	false;

			ExecCommand(COMMAND_CLEAR);
			CTimer T;
			T.Start();
			if (!ATools->Import(NULL, temp_fn.c_str()))
				return	false;

			m_LastFileName = temp_fn.c_str();
			ELog.Msg(mtInformation, "Object '%s' successfully imported. Loading time - %3.2f(s).", m_LastFileName.c_str(), T.GetElapsed_sec());
			if (ExecCommand(COMMAND_SAVE, temp_fn, 1))
			{
				xr_string mfn;
				mfn = temp_fn;
				EFS.MarkFile(mfn.c_str(), true);
			}
			else
			{
				ExecCommand(COMMAND_CLEAR);
			}
			return true;
		}
		else {
			ELog.Msg(mtError, "Invalid file path. ");
		}
	}
	return false;
}

CCommandVar CActorTools::CommandImportOMF(CCommandVar p1, CCommandVar p2)
{
	xr_string temp_fn = p1.IsString() ? xr_string(p1) : xr_string("");
	if (p1.IsString() || EFS.GetOpenName(_import_, temp_fn, false, nullptr, -1, "*.omf"))
	{
		if (!FS.TryLoad(temp_fn))
		{
			return false;
		}

		CTimer T;
		T.Start();
		if (!ATools->ImportOMF(temp_fn.c_str()))
		{
			return false;
		}

		ELog.Msg(mtInformation, "Motions from '%s' successfully imported. Loading time - %3.2f(s).", temp_fn.c_str(), T.GetElapsed_sec());
		return true;
	}
	return false;
}

CCommandVar CActorTools::CommandExportDM(CCommandVar p1, CCommandVar p2)
{
	CCommandVar res 				= false;
	xr_string fn=p1.IsString()?xr_string(p1):xr_string("");
	if (p1.IsString()||EFS.GetSaveName("$game_dm$",fn)){
		if (0!=(res=ExportDM(fn.c_str())))	ELog.Msg(mtInformation,"Export complete.");
		else        		    			ELog.Msg(mtError,"Export failed.");
	}
	return res;
}
CCommandVar CActorTools::CommandExportOBJ(CCommandVar p1, CCommandVar p2)
{
	CCommandVar res 				= false;
	xr_string fn=p1.IsString()?xr_string(p1):xr_string("");
	
	if (p1.IsString()||EFS.GetSaveName("$import$",fn,0,5))
	{
		if (0!=(res=ExportOBJ(fn.c_str())))	
			ELog.Msg(mtInformation,"Export complete.");
		else        		    			
			ELog.Msg(mtError,"Export failed.");
	}
	return res;
}
CCommandVar CActorTools::CommandExportOGF(CCommandVar p1, CCommandVar p2)
{
	CCommandVar res 				= false;
	xr_string fn=p1.IsString()?xr_string(p1):xr_string("");
	if (p1.IsString()||EFS.GetSaveName("$game_meshes$",fn,0,0)){
		if (0!=(res=ATools->ExportOGF(fn.c_str())))	
			ELog.Msg(mtInformation,"Export complete.");
		else		        		    			
			ELog.Msg(mtError,"Export failed.");
	}
	return res;
}
CCommandVar CActorTools::CommandExportOMF(CCommandVar p1, CCommandVar p2)
{
	CCommandVar res 				= false;
	xr_string fn=p1.IsString()?xr_string(p1):xr_string("");
	
	if (p1.IsString()||EFS.GetSaveName("$game_meshes$",fn,0,1))
	{
		if (0!=(res=ExportOMF(fn.c_str())))	
			ELog.Msg(mtInformation,"Export complete.");
		else        		    			
			ELog.Msg(mtError,"Export failed.");
	}
	return res;
}
CCommandVar CActorTools::CommandExportCPP(CCommandVar p1, CCommandVar p2)
{
	CCommandVar res 				= false;
	xr_string fn=p1.IsString()?xr_string(p1):xr_string("");
	if (p1.IsString()||EFS.GetSaveName(_import_,fn,0,7))
	{
		if (0!=(res=ExportCPP(fn.c_str())))	
			ELog.Msg(mtInformation,"Export complete.");
		else        		    			
			ELog.Msg(mtError,"Export failed.");
	}
	return res;
}

CCommandVar CActorTools::CommandUndo(CCommandVar p1, CCommandVar p2)
{
	if(!Undo())	
		ELog.Msg( mtInformation, "Undo buffer empty" );
	else		
		return ExecCommand(COMMAND_CHANGE_ACTION, etaSelect);
		
	return false;
}
CCommandVar CActorTools::CommandRedo(CCommandVar p1, CCommandVar p2)
{
	if(!Redo())	
		ELog.Msg( mtInformation, "Redo buffer empty" );
	else		
		return ExecCommand(COMMAND_CHANGE_ACTION, etaSelect);
		
	return false;
}
CCommandVar CActorTools::CommandOptimizeMotions(CCommandVar p1, CCommandVar p2)
{
	OptimizeMotions();
	return true;
}
CCommandVar CActorTools::CommandMakeThumbnail(CCommandVar p1, CCommandVar p2)
{
	MakeThumbnail();
	return true;
}

CCommandVar CActorTools::CommandBatchConvert(CCommandVar p1, CCommandVar p2)
{
	CCommandVar res = false;
	xr_string fn;
	if (EFS.GetOpenName("$import$", fn, false, 0, 6))
	{
		if (0 != (res = BatchConvert(fn.c_str())))
			ELog.Msg(mtInformation, "Convert complete.");
		else
			ELog.Msg(mtError, "Convert failed.");
	}
	return res;
}

char* CActorMain::GetCaption()
{
	return (char*)(ATools->GetEditFileName().empty() ? "noname" : ATools->GetEditFileName().c_str());
}

void CActorMain::ResetStatus()
{
	VERIFY(m_bReady);
}

void CActorMain::SetStatus(const char* s, bool bOutLog)
{
	VERIFY(m_bReady);
}

//---------------------------------------------------------------------------
extern ECORE_API bool g_force16BitTransformQuant;
extern ECORE_API bool g_force32BitTransformQuant;
//---------------------------------------------------------------------------

constexpr size_t ConfigVer = 2;

void CAEPreferences::Load()
{
	inherited::Load();

	const auto& AePrefs = JSONData["ae_prefs"];
	PrefConfigVer = AePrefs["version"];

	bAlwaysShowKeyBar12 = AePrefs["always_show_keybar12"];
	bAlwaysShowKeyBar34 = AePrefs["always_show_keybar34"];

	g_force16BitTransformQuant = AePrefs["anims_bit"]["16"];
	g_force32BitTransformQuant = AePrefs["anims_bit"]["32"];

	if (AePrefs.contains("SmoothGroup"))
	{
		SmoothGroup = (ESmoothGroup)AePrefs["SmoothGroup"].get<int>();
	}
}

void CAEPreferences::Save()
{
	inherited::Save();

	JSONData["ae_prefs"]["always_show_keybar12"] = bAlwaysShowKeyBar12;
	JSONData["ae_prefs"]["always_show_keybar34"] = bAlwaysShowKeyBar34;

	JSONData["ae_prefs"]["anims_bit"]["16"] = g_force16BitTransformQuant;
	JSONData["ae_prefs"]["anims_bit"]["32"] = g_force32BitTransformQuant;
	JSONData["ae_prefs"]["SmoothGroup"] = (int)SmoothGroup;

	JSONData["ae_prefs"]["version"] = ConfigVer;
}

void CAEPreferences::FillProp(PropItemVec& props)
{
	inherited::FillProp(props);

	PHelper().CreateBool	(props,"Keybar\\show footsteps 12",	&bAlwaysShowKeyBar12);
	PHelper().CreateBool	(props,"Keybar\\show footsteps 34",	&bAlwaysShowKeyBar34);

	/*
	PHelper().CreateBOOL	(props,"Tools\\MotionExport\\Force 16bit MotionT",	&g_force16BitTransformQuant);
	PHelper().CreateBOOL	(props,"Tools\\MotionExport\\Force 32bit MotionT",	&g_force32BitTransformQuant);
	*/
}


CActorMain::CActorMain()
{
	EPrefs = new CAEPreferences();
}

CActorMain::~CActorMain()
{
	xr_delete(EPrefs);
}







//---------------------------------------------------------------------------
// Common command
//---------------------------------------------------------------------------
CCommandVar CommandShowClipMaker(CCommandVar p1, CCommandVar p2)
{
	ATools->ShowClipMaker();
	return true;
}
CCommandVar CommandMakePreview(CCommandVar p1, CCommandVar p2)
{
	ATools->MakePreview();
	return true;
}
CCommandVar CommandPreviewObjPref(CCommandVar p1, CCommandVar p2)
{
	ATools->SetPreviewObjectPrefs();
	return true;
}
CCommandVar CommandSelectPreviewObj(CCommandVar p1, CCommandVar p2)
{
	ATools->SelectPreviewObject(p1);
	return true;
}
CCommandVar CommandLoadFirstRecent(CCommandVar p1, CCommandVar p2)
{
	if (EPrefs->FirstRecentFile())
		return ExecCommand(COMMAND_LOAD, xr_string(EPrefs->FirstRecentFile()));

	return false;
}

CCommandVar CommandChangeTarget(CCommandVar p1, CCommandVar p2)
{
	if (p1.IsString()) {
		ATools->SelectListItem(xr_string(p1).c_str(), 0, true, false, true);
	}
	else {
		switch (p1) {
		case 0: ATools->SelectListItem(BONES_PREFIX, 0, true, false, true); 	break;
		case 1: ATools->SelectListItem(MOTIONS_PREFIX, 0, true, false, true); 	break;
		case 2: ATools->SelectListItem(OBJECT_PREFIX, 0, true, false, true); 	break;
		case 3: ATools->SelectListItem(SURFACES_PREFIX, 0, true, false, true); 	break;
		}
	}
	return true;
}
CCommandVar CActorTools::CommandClear(CCommandVar p1, CCommandVar p2)
{
	if (!IfModified())
		return false;

	m_LastFileName = "";
	UI->CurrentView().m_Camera.Reset();
	Clear();
	ExecCommand(COMMAND_UPDATE_PROPERTIES);
	UndoClear();
	return true;
}

CCommandVar CActorTools::CommandLoad(CCommandVar p1, CCommandVar p2)
{
	xr_string temp_fn = p1.IsString() ? xr_string(p1) : xr_string("");
	if (!p1.IsString()) 
	{
		temp_fn = ChangeFileExt(m_LastFileName, "").c_str();
		if (!EFS.GetOpenName(_objects_, temp_fn, false, nullptr, -1, "*.object;*.ogf"))
			return false;
	}

	if (!temp_fn.empty()) 
	{
		xr_strlwr(temp_fn);
		temp_fn = FS.fix_path(temp_fn);

		if (!IfModified())
			return false;

		if (!FS.TryLoad(temp_fn))
		{
			Msg("#!Can't load file: %s", temp_fn.c_str());
			return false;
		}

		ExecCommand(COMMAND_CLEAR);

		bool bReadOnly = !FS.can_modify_file(temp_fn.c_str());
		m_Flags.set(flReadOnlyMode, bReadOnly);

		CTimer T;
		T.Start();

		if (!Load(temp_fn.c_str()))
		{
			return false;
		}

		m_LastFileName = temp_fn.c_str();
		ELog.Msg(mtInformation, "Object '%s' successfully loaded. Loading time - %3.2f(s).", m_LastFileName.c_str(), T.GetElapsed_sec());
		EPrefs->AppendRecentFile(m_LastFileName.c_str());
		ExecCommand(COMMAND_UPDATE_PROPERTIES);

		UndoClear();
		UndoSave();
	}
	return true;
}

void CActorMain::RegisterCommands()
{
	inherited::RegisterCommands();
	// tools
	REGISTER_CMD_CE(COMMAND_CLEAR, "File\\Clear Scene", ATools, CActorTools::CommandClear, true);
	REGISTER_CMD_CE(COMMAND_LOAD, "File\\Load", ATools, CActorTools::CommandLoad, true);
	REGISTER_CMD_C(COMMAND_SAVE_BACKUP, ATools, CActorTools::CommandSaveBackup);
	REGISTER_SUB_CMD_CE(COMMAND_SAVE, "File", ATools, CActorTools::CommandSave, true);
	APPEND_SUB_CMD("Save", 0, 0);
	APPEND_SUB_CMD("Save As", 0, 1);
	REGISTER_SUB_CMD_END;
	REGISTER_CMD_CE(COMMAND_IMPORT, "File\\Import", ATools, CActorTools::CommandImport, true);
	REGISTER_CMD_CE(COMMAND_IMPORT_OMF, "File\\Import OMF", ATools, CActorTools::CommandImportOMF, true);
	REGISTER_CMD_CE(COMMAND_EXPORT_DM, "File\\Export DM", ATools, CActorTools::CommandExportDM, true);
	REGISTER_CMD_CE(COMMAND_EXPORT_OBJ, "File\\Export OBJ", ATools, CActorTools::CommandExportOBJ, true);
	REGISTER_CMD_CE(COMMAND_EXPORT_OGF, "File\\Export OGF", ATools, CActorTools::CommandExportOGF, true);
	REGISTER_CMD_CE(COMMAND_EXPORT_OMF, "File\\Export OMF", ATools, CActorTools::CommandExportOMF, true);
	REGISTER_CMD_CE(COMMAND_EXPORT_CPP, "File\\Export CPP", ATools, CActorTools::CommandExportCPP, true);
	REGISTER_CMD_CE(COMMAND_UNDO, "Edit\\Undo", ATools, CActorTools::CommandUndo, false);
	REGISTER_CMD_CE(COMMAND_REDO, "Edit\\Redo", ATools, CActorTools::CommandRedo, false);
	REGISTER_CMD_C(COMMAND_OPTIMIZE_MOTIONS, ATools, CActorTools::CommandOptimizeMotions);
	REGISTER_CMD_CE(COMMAND_MAKE_THUMBNAIL, "Make Thumbnail", ATools, CActorTools::CommandMakeThumbnail, false);
	REGISTER_CMD_CE(COMMAND_BATCH_CONVERT, "File\\Batch Convert", ATools, CActorTools::CommandBatchConvert, false);
	// ui
	REGISTER_CMD_S(COMMAND_SHOW_CLIPMAKER, CommandShowClipMaker);
	REGISTER_CMD_S(COMMAND_MAKE_PREVIEW, CommandMakePreview);
	REGISTER_CMD_S(COMMAND_PREVIEW_OBJ_PREF, CommandPreviewObjPref);
	REGISTER_CMD_S(COMMAND_SELECT_PREVIEW_OBJ, CommandSelectPreviewObj);
	REGISTER_CMD_SE(COMMAND_LOAD_FIRSTRECENT, "File\\Load First Recent", CommandLoadFirstRecent, true);
	REGISTER_SUB_CMD_SE(COMMAND_CHANGE_TARGET, "Change Target", CommandChangeTarget, true);
	APPEND_SUB_CMD(BONES_PREFIX, xr_string(BONES_PREFIX), 0);
	APPEND_SUB_CMD(MOTIONS_PREFIX, xr_string(MOTIONS_PREFIX), 0);
	APPEND_SUB_CMD(OBJECT_PREFIX, xr_string(OBJECT_PREFIX), 0);
	APPEND_SUB_CMD(SURFACES_PREFIX, xr_string(SURFACES_PREFIX), 0);
	REGISTER_SUB_CMD_END;
}

void CActorMain::OnDrawUI()
{
	TUI::OnDrawUI();
	UIBoneForm::Update();
}

Ivector2 CActorMain::GetRenderMousePosition() const
{
	return MainForm->GetRenderForm()->GetMousePos();
}

