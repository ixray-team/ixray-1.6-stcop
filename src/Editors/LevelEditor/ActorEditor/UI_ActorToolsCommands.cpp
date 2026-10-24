#include "stdafx.h"
#include "ActorEditor.h"

CCommandVar CActorTools::CommandSaveBackup(CCommandVar p1, CCommandVar p2)
{
	string_path 		fn;
	xr_strconcat		(fn, Core.UserName, "_backup.object");
	FS.update_path		(fn,"$objects$",fn);
	CommandSave(xr_string(fn));
	return true;
}
CCommandVar CActorTools::CommandSave(CCommandVar p1, CCommandVar p2)
{
	if (p2==1){
		xr_string temp_fn	= ATools->m_LastFileName.c_str();
		if (EFS.GetSaveName(_objects_, temp_fn, nullptr, -1, "*.object;*.ogf")){
			if (!strext(temp_fn.c_str()))
				temp_fn += ".object";
			return 			CommandSave(temp_fn,0);
		}
	}else{
		if (p1.IsInteger())
			return 				CommandSave(xr_string(ATools->m_LastFileName.c_str()),0);
		xr_string temp_fn		= xr_string(p1);
		if (temp_fn.empty()){
			return 				CommandSave(temp_fn,1);
		}else{
			xr_strlwr			(temp_fn);
			CTimer T;
			T.Start();
			CCommandVar			res;
			if (ATools->Save(temp_fn.c_str())){
				ELog.Msg		(mtInformation,"Object '%s' successfully saved. Saving time - %3.2f(s).",m_LastFileName.c_str(),T.GetElapsed_sec());
				m_LastFileName	= temp_fn.c_str();
				static_cast<CLevelPreferences*>(EPrefs)->AppendActorRecentFile	(m_LastFileName.c_str());
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

			if (!ATools->IfModified())
				return	false;

			CommandClear();
			CTimer T;
			T.Start();
			if (!ATools->Import(NULL, temp_fn.c_str()))
				return	false;

			m_LastFileName = temp_fn.c_str();
			ELog.Msg(mtInformation, "Object '%s' successfully imported. Loading time - %3.2f(s).", m_LastFileName.c_str(), T.GetElapsed_sec());
			if (CommandSave( temp_fn, 1))
			{
				xr_string mfn;
				mfn = temp_fn;
				EFS.MarkFile(mfn.c_str(), true);
			}
			else
			{
				CommandClear();
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
	{
		SetAction(etaSelect);
		return true;
	}

	return false;
}
CCommandVar CActorTools::CommandRedo(CCommandVar p1, CCommandVar p2)
{
	if(!Redo())	
		ELog.Msg( mtInformation, "Redo buffer empty" );
	else
	{
		SetAction(etaSelect);
		return true;
	}

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

CCommandVar CActorTools::CommandClear(CCommandVar p1, CCommandVar p2)
{
	if (!IfModified())
		return false;

	m_LastFileName = "";
	EContext.UI->CurrentView().m_Camera.Reset();
	Clear();
	UpdateProperties();
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

		CommandClear();

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
		static_cast<CLevelPreferences*>(EPrefs)->AppendActorRecentFile(m_LastFileName.c_str());
		UpdateProperties();

		UndoClear();
		UndoSave();

		UIActorLibrary::SyncWithActor(m_LastFileName.c_str());
	}
	return true;
}
