#include "stdafx.h"
#include "Utils/Cursor3D.h"
#include "../xrengine\GameFont.h"
#include "UI\UIEditLibrary.h"
#include "Editor/Utils/ContentView.h"
#include "../xrECore/Editor/UIEditLightAnim.h"

ECORE_API extern bool bIsLevelEditor;
CLevelMain* LUI = (CLevelMain*)UI;

CLevelMain::CLevelMain()
{
	m_Cursor = new C3DCursor();
	EPrefs = new CLevelPreferences();
}

CLevelMain::~CLevelMain()
{
	xr_delete(EPrefs);
	xr_delete(m_Cursor);
}

// Tools commands
CCommandVar CLevelTool::CommandChangeTarget(CCommandVar p1, CCommandVar p2)
{
	if (Scene->GetTool(p1)->IsEnabled())
	{
		SetTarget(p1, p2);
		ExecCommand(COMMAND_UPDATE_PROPERTIES);
		return TRUE;
	}
	return FALSE;
}

CCommandVar CLevelTool::CommandShowObjectList(CCommandVar p1, CCommandVar p2)
{
	if (LUI->GetEState()==esEditScene) ShowObjectList();
	return TRUE;
}

// Main commands
CCommandVar CommandLibraryEditor(CCommandVar p1, CCommandVar p2)
{
	UIEditLibrary::Show();

	return TRUE;
}

CCommandVar CommandLAnimEditor(CCommandVar p1, CCommandVar p2)
{
	UIEditLightAnim::Show();
	return TRUE;
}

CCommandVar CommandLoadCustomIcons(CCommandVar p1, CCommandVar p2)
{
	GContentView->LoadCustomIcons();
	return TRUE;
}

CCommandVar CommandRemoveCustomIcon(CCommandVar p1, CCommandVar p2)
{
	GContentView->RemoveCustomIcon(p1);
	return TRUE;
}

CCommandVar CLevelTool::CommandEnableTarget(CCommandVar p1, CCommandVar p2)
{
	ESceneToolBase* M = Scene->GetTool(p1);
	VERIFY(M);
	BOOL res = FALSE;
	if (p2)
	{
		res = ExecCommand(COMMAND_LOAD_LEVEL_PART, M->FClassID, TRUE);
		if (res)
			M->m_EditFlags.set(ESceneToolBase::flEnable, TRUE);
	}
	else
	{
		if (!Scene->IfModified())
		{
			M->m_EditFlags.set(ESceneToolBase::flEnable, TRUE);
			res = FALSE;
		}
		else
		{
			res = ExecCommand(COMMAND_UNLOAD_LEVEL_PART, M->FClassID, TRUE);
			if (res)
				M->m_EditFlags.set(ESceneToolBase::flEnable, FALSE);
		}
		if (res)
			ExecCommand(COMMAND_CHANGE_TARGET, OBJCLASS_SCENEOBJECT);
	}

	return res;
}

CCommandVar CLevelTool::CommandShowTarget(CCommandVar p1, CCommandVar p2)
{
	ESceneToolBase* M 	= Scene->GetTool(p1);
	if(p2)
		M->m_EditFlags.set(ESceneToolBase::flVisible,TRUE);
	else
		M->m_EditFlags.set(ESceneToolBase::flVisible,FALSE);
		
	return TRUE;
}

CCommandVar CLevelTool::CommandReadonlyTarget(CCommandVar p1, CCommandVar p2)
{
	ESceneToolBase* M = Scene->GetTool(p1); VERIFY(M);
	BOOL res = TRUE;
	if (p2)
	{
		if (!Scene->IfModified())
		{
			M->m_EditFlags.set(ESceneToolBase::flForceReadonly, FALSE);
			res = FALSE;
		}
	}
	if (res)
	{
		Reset();
	}
	return res;
}

CCommandVar CLevelTool::CommandMultiRenameObjects(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() )
	{
		if (mrYes==ELog.DlgMsg(mtConfirmation, mbYes |mbNo, "Are you sure to rename selected objects?"))
		{
			int cnt			= Scene->MultiRenameObjects();
			if (cnt)
			{
				ExecCommand	(COMMAND_UPDATE_PROPERTIES);
				Scene->UndoSave();
			}
			ELog.DlgMsg		( mtInformation, "%d - objects are renamed.", cnt );
		}
	}else
	{
		ELog.DlgMsg			( mtError, "Scene sharing violation" );
	}
	return 					FALSE;
}
CCommandVar CommandLoadLevelPart(CCommandVar p1, CCommandVar p2)
{
	xr_string temp_fn	= LTools->m_LastFileName.c_str();
	if (!temp_fn.empty())
		return			Scene->LoadLevelPart(temp_fn.c_str(),p1);
	return				TRUE;
}
CCommandVar CommandUnloadLevelPart(CCommandVar p1, CCommandVar p2)
{
	xr_string temp_fn	= LTools->m_LastFileName.c_str();
	if (!temp_fn.empty())
		return			Scene->UnloadLevelPart(temp_fn.c_str(),p1);
	return				TRUE;
}

static xr_task_group LoaderEvent;

CCommandVar CommandLoad(CCommandVar p1, CCommandVar p2)
{
	LoaderEvent.wait();

	if (!Scene->locked())
	{
		if (!p1.IsString())
		{
			xr_string temp_fn = LTools->m_LastFileName.c_str();
			if (EFS.GetOpenName(_maps_, temp_fn, false, 0, -1, "*.level;*.tmp"))
				return 			ExecCommand(COMMAND_LOAD, temp_fn);
		}
		else
		{
			xr_string temp_fn = p1;
			xr_strlwr(temp_fn);

			if (!Scene->IfModified())
				return FALSE;

			UI->SetStatus("Level loading...");
			ExecCommand(COMMAND_CLEAR);
			FS.TryLoad(temp_fn.c_str());
			IReader* R = FS.r_open(temp_fn.c_str());
			if (!R)return false;
			char ch;
			R->r(&ch, sizeof(ch));
			bool is_ltx = (ch == '[');
			FS.r_close(R);
			LTools->m_LastFileName = temp_fn.c_str();

			LoaderEvent.run
			(
				[temp_fn, is_ltx]
				{
					bool Result = (is_ltx) ? Scene->LoadLTX(temp_fn.c_str(), false) : Scene->Load(temp_fn.c_str(), false);

					if (Result)
					{
						UI->ResetStatus();
						Scene->UndoClear();

						BOOL bk1 = Scene->m_RTFlags.test(EScene::flRT_Unsaved);
						BOOL bk2 = Scene->m_RTFlags.test(EScene::flRT_Modified);

						Scene->UndoSave();

						Scene->m_RTFlags.set(EScene::flRT_Unsaved, bk1);
						Scene->m_RTFlags.set(EScene::flRT_Modified, bk2);

						ExecCommand(COMMAND_CLEAN_LIBRARY);
						ExecCommand(COMMAND_UPDATE_CAPTION);
						ExecCommand(COMMAND_CHANGE_ACTION, etaSelect);
						EPrefs->AppendRecentFile(temp_fn.c_str());
					}
					else
					{
						ELog.DlgMsg(mtError, "Can't load map '%s'", temp_fn.c_str());
						LTools->m_LastFileName = "";
					}
					// update props
					ExecCommand(COMMAND_UPDATE_PROPERTIES);
					UI->RedrawScene();
				}
			);
		}

		return TRUE;
	}
	else
	{
		ELog.DlgMsg(mtError, "Scene sharing violation");
		return FALSE;
	}
	return TRUE;
}

CCommandVar CommandSaveBackup(CCommandVar p1, CCommandVar p2)
{
	LoaderEvent.wait();

	string_path 	fn;
	xr_strconcat(fn,Core.CompName,"_",Core.UserName,"_backup.level");
	FS.update_path	(fn,_maps_,fn);
	return 			ExecCommand(COMMAND_SAVE,xr_string(fn));
}
CCommandVar CommandSave(CCommandVar p1, CCommandVar p2)
{
	LoaderEvent.wait();

	if( !Scene->locked() )
	{
		if (p2==1)
		{
			xr_string temp_fn	= LTools->m_LastFileName.c_str();
			if (EFS.GetSaveName	( _maps_, temp_fn ))
				return 			ExecCommand(COMMAND_SAVE,temp_fn, 66);
			else
				return          FALSE;
		}else{
			if (p1.IsInteger())
				return 			ExecCommand(COMMAND_SAVE,xr_string(LTools->m_LastFileName.c_str()),0);
				
			xr_string temp_fn	= xr_string(p1);
			if (temp_fn.empty())
			{
				return 			ExecCommand(COMMAND_SAVE,temp_fn,1);
			}
			else
			{
				xr_strlwr(temp_fn);

				UI->SetStatus("Level saving...");
					Scene->SaveLTX(temp_fn.c_str(), false, (p2 == 66));

				UI->ResetStatus	();
				// set new name
				if (0!=xr_strcmp(Tools->m_LastFileName.c_str(),temp_fn.c_str()))
				{
					Tools->m_LastFileName 	= temp_fn.c_str();
				}
				ExecCommand		(COMMAND_UPDATE_CAPTION);
				EPrefs->AppendRecentFile(temp_fn.c_str());
				return 			TRUE;
			}
		}
	} else {
		ELog.DlgMsg			( mtError, "Scene sharing violation" );
		return				FALSE;
	}
}

CCommandVar CommandClear(CCommandVar p1, CCommandVar p2)
{
	LoaderEvent.wait();

	if( !Scene->locked() )
	{
		Scene->Stop();
		
		if (!Scene->IfModified()) 
			return TRUE;
		UI->CurrentView().m_Camera.Reset	();
		Scene->Reset			();
		Scene->m_LevelOp.Reset	();
		Tools->m_LastFileName 		= "";
		LTools->m_LastSelectionName = "";
		Scene->UndoClear		();
		ExecCommand				(COMMAND_UPDATE_CAPTION);
		ExecCommand				(COMMAND_CHANGE_TARGET,OBJCLASS_SCENEOBJECT);
		ExecCommand				(COMMAND_CHANGE_ACTION,etaSelect,estDefault);
		ExecCommand				(COMMAND_UPDATE_PROPERTIES,1);
		Scene->UndoSave			();
		return 					TRUE;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
		return					FALSE;
	}
}
CCommandVar CommandLoadFirstRecent(CCommandVar p1, CCommandVar p2)
{
	if (EPrefs->FirstRecentFile())
		return 					ExecCommand(COMMAND_LOAD,xr_string(EPrefs->FirstRecentFile()));
	return 						FALSE;
}

CCommandVar CommandClearDebugDraw(CCommandVar p1, CCommandVar p2)
{
	Tools->ClearDebugDraw		();
	UI->RedrawScene				();
	return 						TRUE;
}

#include "Utils/ClipMaker.h"
CCommandVar CommandShowClipEditor(CCommandVar p1, CCommandVar p2)
{
	if(g_clip_maker==NULL)
	   g_clip_maker = new TClipMaker();

	//if(!g_clip_maker->)	
	{
		ESceneCustomOTool* st = Scene->GetOTool(OBJCLASS_SPAWNPOINT);

		ObjectList&	ol = st->GetObjects();
		ObjectList::iterator it = ol.begin();    
		ObjectList::iterator it_e = ol.end();    

		CCustomObject* CO = NULL;
		for(;it!=it_e;++it)
		{
			if((*it)->Selected()==true)
			{
				CO=*it;
				break;
			}
		}
		if(!CO)
			return TRUE;
			
		CSpawnPoint* sp = smart_cast<CSpawnPoint*>(CO);

		
		if (CKinematicsAnimated* KA = PKinematicsAnimated(sp->m_SpawnData.m_Visual->visual))
		{
			g_clip_maker->ShowEditor(KA);
			UI->Push(g_clip_maker);
		}
	}
	return 							TRUE;
}

CCommandVar CommandImportXrAICompilerError(CCommandVar p1, CCommandVar p2)
{
	xr_string fn;
	if (EFS.GetOpenName("$app_root$", fn, false, NULL, 0)) 
	{
		Scene->LoadXrAICompilerError(fn.c_str());
	}
	UI->RedrawScene();
	return TRUE;
}

CCommandVar CommandImportCompilerError(CCommandVar p1, CCommandVar p2)
{
	xr_string fn;
	if(EFS.GetOpenName("$logs$", fn, false, NULL, 0)){
		Scene->LoadCompilerError(fn.c_str());
	}
	UI->RedrawScene		();
	return TRUE;
}
CCommandVar CommandExportCompilerError(CCommandVar p1, CCommandVar p2)
{
	xr_string fn;
	if(EFS.GetSaveName("$logs$", fn, NULL, 0)){
		Scene->SaveCompilerError(fn.c_str());
	}
	return TRUE;
}
CCommandVar CommandValidateScene(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->Validate	(true,true,true,true,true,true);
		return 			TRUE;
	} else {
		ELog.DlgMsg		( mtError, "Scene sharing violation" );
		return 			FALSE;
	}
}
CCommandVar CommandCleanLibrary(CCommandVar p1, CCommandVar p2)
{
	if ( !Scene->locked() ){
		Lib.CleanLibrary();
		return 			TRUE;
	}else{
		ELog.DlgMsg		(mtError, "Scene must be empty before refreshing library!");
		return 			FALSE;
	}
}

CCommandVar CommandReloadObjects(CCommandVar p1, CCommandVar p2)
{
	Lib.ReloadObjects	();

	ObjectIt _F = Scene->FirstObj(OBJCLASS_SECTOR);
	ObjectIt _E = Scene->LastObj(OBJCLASS_SECTOR);
	for (; _F != _E; _F++)
	{
		CSector* _S = (CSector*)(*_F);
		_S->ReloadObjectsReferences();
	}

	_F = Scene->FirstObj(OBJCLASS_SCENEOBJECT);
	_E = Scene->LastObj(OBJCLASS_SCENEOBJECT);
	for (; _F != _E; _F++)
	{
		CSceneObject* _S = (CSceneObject*)(*_F);
		_S->ReloadReferences();
	}

	return 				TRUE;
}

CCommandVar CommandCut(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->CutSelection(LTools->CurrentClassID());
	   /* fraLeftBar->miPaste->Enabled = true;
		fraLeftBar->miPaste2->Enabled = true;*/
		Scene->UndoSave	();
		return 			TRUE;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
		return 			FALSE;
	}
	return FALSE;
}
CCommandVar CommandCopy(CCommandVar p1, CCommandVar p2)
{
	  if( !Scene->locked() ){
		Scene->CopySelection(LTools->CurrentClassID());
		return 			TRUE;
	} else {
		ELog.DlgMsg		( mtError, "Scene sharing violation" );
		return 			FALSE;
	}
	return FALSE;
}

CCommandVar CommandPaste(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->PasteSelection();
		Scene->UndoSave	();
		return 			TRUE;
	} else {
		ELog.DlgMsg		( mtError, "Scene sharing violation" );
		return  		FALSE;
	}
	return FALSE;
}

CCommandVar CommandDuplicate(CCommandVar p1, CCommandVar p2)
{
    if (!Scene->locked()) {
		Scene->DuplicateSelection(LTools->CurrentClassID());
        Scene->UndoSave();
        return 			TRUE;
    }
    else {
        ELog.DlgMsg(mtError, "Scene sharing violation");
        return 			FALSE;
    }
    return FALSE;
}

CCommandVar CommandLoadSelection(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() )
	{
		xr_string fn			= LTools->m_LastSelectionName;
		if( EFS.GetOpenName(_maps_, fn ) )
		{
			LPCSTR maps_path	= FS.get_path(_maps_)->m_Path;
			if (fn.c_str()==strstr(fn.c_str(),maps_path))
				LTools->m_LastSelectionName = fn.c_str()+xr_strlen(maps_path);
			UI->SetStatus		("Fragment loading...");

			Scene->LoadSelection(fn.c_str());

			UI->ResetStatus		();
			Scene->UndoSave		();
			ExecCommand			(COMMAND_CHANGE_ACTION,etaSelect);
			ExecCommand			(COMMAND_UPDATE_PROPERTIES);
			UI->RedrawScene		();
			return 				TRUE;
		}               	
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
	}
	return FALSE;
}        
CCommandVar CommandSaveSelection(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		xr_string fn			= LTools->m_LastSelectionName;
		if( EFS.GetSaveName		( _maps_, fn ) ){
			LPCSTR maps_path	= FS.get_path(_maps_)->m_Path;
			if (fn.c_str()==strstr(fn.c_str(),maps_path))
				LTools->m_LastSelectionName = fn.c_str()+xr_strlen(maps_path);
			UI->SetStatus		("Fragment saving...");
			Scene->SaveSelection(LTools->CurrentClassID(),fn.c_str());
			UI->ResetStatus		();
			return 				TRUE;
		}
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
	}
	return 						FALSE;
}

CCommandVar CommandUndo(CCommandVar p1, CCommandVar p2)
{
	LTools->GetProperties()->ClearProperties();

	if (!Scene->locked())
	{
		if (!Scene->Undo())
			ELog.DlgMsg(mtInformation, "Undo buffer empty");
		else
		{
			LTools->Reset();
			ExecCommand(COMMAND_CHANGE_ACTION, etaSelect);
			return TRUE;
		}
	}
	else
	{
		ELog.DlgMsg(mtError, "Scene sharing violation");
	}
	return FALSE;
}

CCommandVar CommandRedo(CCommandVar p1, CCommandVar p2)
{
	LTools->GetProperties()->ClearProperties();

	if (!Scene->locked()) 
	{
		if (!Scene->Redo()) 
			ELog.DlgMsg(mtInformation, "Redo buffer empty");
		else 
		{
			LTools->Reset();
			ExecCommand(COMMAND_CHANGE_ACTION, etaSelect);
			return TRUE;
		}
	}
	else
	{
		ELog.DlgMsg(mtError, "Scene sharing violation");
	}
	return FALSE;
}

CCommandVar CommandClearSceneSummary(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->ClearSummaryInfo	();
		return 					TRUE;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
		return 					FALSE;
	}
}
CCommandVar CommandCollectSceneSummary(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->CollectSummaryInfo();
		return 					TRUE;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
		return 					FALSE;
	}
}
CCommandVar CommandShowSceneSummary(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->ShowSummaryInfo();
		return 					TRUE;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
		return 					FALSE;
	}
}
CCommandVar CommandExportSceneSummary(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->ExportSummaryInfo(xr_string(p1).c_str());
		return 					TRUE;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
		return 					FALSE;
	}
}

CCommandVar CommandSceneHighlightTexture(CCommandVar p1, CCommandVar p2)
{
	/*if( !Scene->locked() ){
		LPCSTR new_val 		 	= 0;
		if (TfrmChoseItem::SelectItem(smTexture,new_val,1)){
			Scene->HighlightTexture(new_val,false,0,0,false);
			return 				TRUE;
		}
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
	}
	return 						FALSE;*/
	return FALSE;
}

CCommandVar CommandOptions(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		ExecCommand				(COMMAND_SHOW_PROPERTIES, p1, p2);
		return 					TRUE;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
		return 					FALSE;
	}
}

CCommandVar CommandBuild(CCommandVar p1, CCommandVar p2)
{
	if (!Scene->locked())
	{
		if (mrYes == ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to build level?"))
		{
			LoaderEvent.wait();

			LoaderEvent.run
			(
				[]()
				{
					Builder.Compile(false);

				}
			);

			return true;
		}
	}
	else
	{
		ELog.DlgMsg(mtError, "Scene sharing violation");
	}
	return FALSE;
}

CCommandVar CommandUpdateGizmo(CCommandVar p1, CCommandVar p2)
{
	// LTools->GetGimzo()->bApplyUpdatePos = true;
	return FALSE;
}
CCommandVar CommandMakeGizmo(CCommandVar p1, CCommandVar p2)
{
	// auto GizmoPtr = LTools->GetGimzo();
	// GizmoPtr->bApplyChangePos = !GizmoPtr->bApplyChangePos;
	return FALSE;
}
CCommandVar CommandMakeAIMap(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		if (mrYes==ELog.DlgMsg(mtConfirmation, mbYes |mbNo, "Are you sure to export ai-map?"))
			return 				Builder.MakeAIMap(false);
	}else{
		ELog.DlgMsg( mtError, "Scene sharing violation" );
	}
	return 						FALSE;
}
CCommandVar CommandMakeAIMapLegacy(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		if (mrYes==ELog.DlgMsg(mtConfirmation, mbYes |mbNo, "Are you sure to export ai-map?"))
			return 				Builder.MakeAIMap(true);
	}else{
		ELog.DlgMsg( mtError, "Scene sharing violation" );
	}
	return 						FALSE;
}

CCommandVar CommandMakeGame(CCommandVar p1, CCommandVar p2)
{
	if (!Scene->locked())
	{
		if (mrYes == ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to export game?"))
		{
			LoaderEvent.wait();

			LoaderEvent.run
			(
				[]()
				{
					Builder.MakeGame();
				}
			);

			return true;
		}
	}
	else {
		ELog.DlgMsg(mtError, "Scene sharing violation");
	}
	return 						FALSE;
}

CCommandVar CommandMakePuddles(CCommandVar p1, CCommandVar p2)
{
	if (!Scene->locked()) 
	{
		if (mrYes == ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to export puddles?"))
		{
			LoaderEvent.wait();

			LoaderEvent.run
			(
				[]()
				{
					Builder.MakePuddles();
				}
			);

			return true;
		}
	}
	else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
	}
	return FALSE;
}

CCommandVar CommandMakeDetails(CCommandVar p1, CCommandVar p2)
{
	if (!Scene->locked())
	{
		if (mrYes == ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to export details?"))
		{
			LoaderEvent.wait();

			LoaderEvent.run
			(
				[]()
				{
					Builder.MakeDetails();
				}
			);

			return true;
		}
	}
	else
	{
		ELog.DlgMsg(mtError, "Scene sharing violation");
	}
	return 						FALSE;
}

CCommandVar CommandMakeHOM(CCommandVar p1, CCommandVar p2)
{
	if (!Scene->locked()) 
	{
		if (mrYes == ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to export HOM?"))
		{
			LoaderEvent.wait();

			LoaderEvent.run
			(
				[]()
				{
					Builder.MakeHOM();
				}
			);

			return true;
		}
	}
	else 
	{
		ELog.DlgMsg(mtError, "Scene sharing violation");
	}
	return 						FALSE;
}

CCommandVar CommandMakeSOM(CCommandVar p1, CCommandVar p2)
{
	if (!Scene->locked()) 
	{
		if (mrYes == ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to export Sound Occlusion Model?"))
		{
			LoaderEvent.wait();

			LoaderEvent.run
			(
				[]()
				{
					Builder.MakeSOM();
				}
			);

			return true;
		}
	}
	else {
		ELog.DlgMsg(mtError, "Scene sharing violation");
	}
	return 						FALSE;
}

CCommandVar CommandInvertSelectionAll(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->InvertSelection	(LTools->CurrentClassID());
		return 					TRUE;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
	}
	return 						FALSE;
}

CCommandVar CommandSelectAll(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->SelectObjects	(true,LTools->CurrentClassID());
		return 					TRUE;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
	}
	return 						FALSE;
}

CCommandVar CommandDeselectAll(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->SelectObjects	(false,LTools->CurrentClassID());
		return 					TRUE;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
		return 					FALSE;
	}
}

CCommandVar CommandDeleteSelection(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->RemoveSelection	( LTools->CurrentClassID() );
		Scene->UndoSave			();
		return					TRUE;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
		return 					FALSE;
	}
}

CCommandVar CommandHideUnsel(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->ShowObjects		( false, LTools->CurrentClassID(), true, false );
		Scene->UndoSave			();
		ExecCommand				(COMMAND_UPDATE_PROPERTIES);
		return 					TRUE;
	} else {
		ELog.DlgMsg				( mtError, "Scene sharing violation" );
		return 					FALSE;
	}
}
CCommandVar CommandHideSel(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->ShowObjects		( bool(p1), LTools->CurrentClassID(), true, true );
		Scene->UndoSave			();
		ExecCommand				(COMMAND_UPDATE_PROPERTIES);
		return 					TRUE;
	} else {
		ELog.DlgMsg				( mtError, "Scene sharing violation" );
		return 					FALSE;
	}
}

CCommandVar CommandCreateShapeSphere(CCommandVar p1, CCommandVar p2)
{
	Fvector p, n;
	if (LUI->PickGround(p, UI->m_ContextRStart, UI->m_ContextRDir, 1, &n))
	{
		// before callback
		string256 namebuffer;
		Scene->GenObjectName(OBJCLASS_SHAPE, namebuffer, Scene->LevelPrefix().c_str());
		auto obj = Scene->GetOTool(OBJCLASS_SHAPE)->CreateObject(nullptr, namebuffer);
		if (!obj->Valid())
		{
			xr_delete(obj);
			return 0;
		}

		CEditShape* shape = static_cast<CEditShape*>(obj);
		Fsphere M;
		M.identity();
		shape->add_sphere(M);
		obj->MoveTo(p, n);
		Scene->SelectObjects(false, OBJCLASS_SHAPE);
		Scene->AppendObject(obj);
		ExecCommand(COMMAND_CHANGE_TARGET, OBJCLASS_SHAPE);
	}

	return TRUE;
}

CCommandVar CommandCreateShapeBox(CCommandVar p1, CCommandVar p2)
{
	Fvector p, n;
	if (LUI->PickGround(p, UI->m_ContextRStart, UI->m_ContextRDir, 1, &n))
	{
		// before callback
		string256 namebuffer;
		Scene->GenObjectName(OBJCLASS_SHAPE, namebuffer, Scene->LevelPrefix().c_str());
		auto obj = Scene->GetOTool(OBJCLASS_SHAPE)->CreateObject(nullptr, namebuffer);
		if (!obj->Valid())
		{
			xr_delete(obj);
			return 0;
		}

		CEditShape* shape = static_cast<CEditShape*>(obj);
		Fmatrix M;
		M.identity();
		shape->add_box(M);
		obj->MoveTo(p, n);
		Scene->SelectObjects(false, OBJCLASS_SHAPE);
		Scene->AppendObject(obj);
		ExecCommand(COMMAND_CHANGE_TARGET, OBJCLASS_SHAPE);
	}
	return TRUE;
}

CCommandVar CommandHideAll(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->ShowObjects		( bool(p1), LTools->CurrentClassID(), false );
		Scene->UndoSave			();
		ExecCommand				(COMMAND_UPDATE_PROPERTIES);
		return 					TRUE;
	}else{
		ELog.DlgMsg				( mtError, "Scene sharing violation" );
		return 					FALSE;
	}
}
CCommandVar CommandLockAll(CCommandVar p1, CCommandVar p2)
{
    if( !Scene->locked() ){
        Scene->LockObjects		(bool(p1),LTools->CurrentClassID(),false);
        Scene->UndoSave			();
	    return 					TRUE;
    }else{
        ELog.DlgMsg				( mtError, "Scene sharing violation" );
	    return 					FALSE;
    }
}
CCommandVar CommandLockSel(CCommandVar p1, CCommandVar p2)
{
    if( !Scene->locked() ){
        Scene->LockObjects		(bool(p1),LTools->CurrentClassID(),true,true);
        Scene->UndoSave			();
	    return 					TRUE;
    }else{
        ELog.DlgMsg				( mtError, "Scene sharing violation" );
	    return 					FALSE;
    }
}
CCommandVar CommandLockUnsel(CCommandVar p1, CCommandVar p2)
{
    if( !Scene->locked() ){
        Scene->LockObjects		(bool(p1),LTools->CurrentClassID(),true,false);
        Scene->UndoSave			();
        return					TRUE;
   }else{
        ELog.DlgMsg				( mtError, "Scene sharing violation" );                   
	    return 					FALSE;
    }
}
CCommandVar CommandSetSnapObjects(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->SetSnapList		();
		return 					TRUE;
	}else{
		ELog.DlgMsg				( mtError, "Scene sharing violation" );
		return 					FALSE;
	}
}
CCommandVar CommandAddSelSnapObjects(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->AddSelToSnapList	();
		return 					TRUE;
	}else{
		ELog.DlgMsg				( mtError, "Scene sharing violation" );
		return 					FALSE;
	}
}
CCommandVar CommandDelSelSnapObjects(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->DelSelFromSnapList();
		return 					TRUE;
	}else{
		ELog.DlgMsg				( mtError, "Scene sharing violation" );
		return 					FALSE;
	}
}
CCommandVar CommandClearSnapObjects(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->ClearSnapList	(true);
		return 					TRUE;
	}else{
		ELog.DlgMsg				( mtError, "Scene sharing violation" );
		return 					FALSE;
	}
}
CCommandVar CommandSelectSnapObjects(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->SelectSnapList	();
		return 					TRUE;
	}else{
		ELog.DlgMsg				( mtError, "Scene sharing violation" );
		return 					FALSE;
	}
}
CCommandVar CommandRefreshSnapObjects(CCommandVar p1, CCommandVar p2)
{
 //   fraLeftBar->UpdateSnapList();
	return 						TRUE;
}
/*
CCommandVar CommandRefreshSoundEnvs(CCommandVar p1, CCommandVar p2)
{
	::Sound->refresh_env_library();
	return 						TRUE;
//		::Sound->_restart();
}
*/

CCommandVar CommandRefreshSoundEnvGeometry(CCommandVar p1, CCommandVar p2)
{
	LSndLib->RefreshEnvGeometry();
	return 						TRUE;
}
CCommandVar CommandShowContextMenu(CCommandVar p1, CCommandVar p2)
{
	LUI->ShowContextMenu		(p1);
	return 						TRUE;
}

CCommandVar CommandUpdateToolBar(CCommandVar p1, CCommandVar p2)
{
 /*   fraLeftBar->UpdateBar		();*/
	return 						TRUE;
}
CCommandVar CommandUpdateCaption(CCommandVar p1, CCommandVar p2)
{
  /*  frmMain->UpdateCaption		();*/
	return 						TRUE;
}
//------
CCommandVar CommandCreateSoundLib(CCommandVar p1, CCommandVar p2)
{
	SndLib						= new CLevelSoundManager();
	LSndLib = (CLevelSoundManager*)SndLib;
	return 						TRUE;
}

extern BOOL ai_map_shown;
CCommandVar CommandToggleAiMapVisibility(CCommandVar p1, CCommandVar p2)
{
	ai_map_shown 				= !ai_map_shown;
	return 						TRUE;
}

CCommandVar CommandImport(CCommandVar p1, CCommandVar p2);

void CLevelMain::RegisterCommands()
{
	inherited::RegisterCommands	();
	// tools
	REGISTER_SUB_CMD_CE	(COMMAND_CHANGE_TARGET,             "Change Target", 		LTools,CLevelTool::CommandChangeTarget, true);
		APPEND_SUB_CMD	("Object", 							OBJCLASS_SCENEOBJECT,	0);
		APPEND_SUB_CMD	("Light", 							OBJCLASS_LIGHT, 		0);
		APPEND_SUB_CMD	("Sound Source",					OBJCLASS_SOUND_SRC, 	0);
		APPEND_SUB_CMD	("Sound Env", 		                OBJCLASS_SOUND_ENV, 	0);
		APPEND_SUB_CMD	("Glow", 			                OBJCLASS_GLOW, 			0);
		APPEND_SUB_CMD	("Shape", 			                OBJCLASS_SHAPE, 		0);
		APPEND_SUB_CMD	("Spawn Point", 	                OBJCLASS_SPAWNPOINT, 	0);
		APPEND_SUB_CMD	("Way", 			                OBJCLASS_WAY, 			0);
		APPEND_SUB_CMD	("Way Point", 		                OBJCLASS_WAY, 			1);
		APPEND_SUB_CMD	("Toggle Way Mode",	                OBJCLASS_WAY, 			2);
		APPEND_SUB_CMD	("Sector", 			                OBJCLASS_SECTOR, 		0);
		APPEND_SUB_CMD	("Portal", 			                OBJCLASS_PORTAL, 		0);
		APPEND_SUB_CMD	("Group", 			                OBJCLASS_GROUP, 		0);
		APPEND_SUB_CMD	("Particle System",                 OBJCLASS_PS, 			0);
		APPEND_SUB_CMD	("Detail Objects", 	                OBJCLASS_DO, 			0);
		APPEND_SUB_CMD	("AI Map", 			                OBJCLASS_AIMAP, 		0);
		APPEND_SUB_CMD	("Static Wallmark",                 OBJCLASS_WM, 			0);
	REGISTER_SUB_CMD_END;    
	REGISTER_CMD_C	    (COMMAND_ENABLE_TARGET,           	LTools,CLevelTool::CommandEnableTarget);
	REGISTER_CMD_C	    (COMMAND_SHOW_TARGET,           	LTools,CLevelTool::CommandShowTarget);
	REGISTER_CMD_C	    (COMMAND_READONLY_TARGET,          	LTools,CLevelTool::CommandReadonlyTarget);
	REGISTER_CMD_C	    (COMMAND_MULTI_RENAME_OBJECTS,     	LTools,CLevelTool::CommandMultiRenameObjects);

	REGISTER_CMD_CE	    (COMMAND_SHOW_OBJECTLIST,           "Scene\\Show Object List",		LTools,CLevelTool::CommandShowObjectList, false);
	// common
	REGISTER_CMD_S	    (COMMAND_LIBRARY_EDITOR,           	CommandLibraryEditor);
	REGISTER_CMD_S	    (COMMAND_LANIM_EDITOR,            	CommandLAnimEditor);
	REGISTER_CMD_S		(COMMAND_LOAD_LEVEL_PART,			CommandLoadLevelPart);
	REGISTER_CMD_S		(COMMAND_UNLOAD_LEVEL_PART,			CommandUnloadLevelPart);
	REGISTER_CMD_SE	    (COMMAND_LOAD,              		"File\\Load Level", 			CommandLoad, 			true);
	REGISTER_SUB_CMD_SE (COMMAND_SAVE, 						"File",							CommandSave,			true);
		APPEND_SUB_CMD	("Save",							0,								0);
		APPEND_SUB_CMD	("Save As",							0,								1);
	REGISTER_SUB_CMD_END;
	REGISTER_CMD_S	    (COMMAND_SAVE_BACKUP,              	CommandSaveBackup);
	REGISTER_CMD_SE	    (COMMAND_CLEAR,              		"File\\Clear Scene", 			CommandClear,			true);
	REGISTER_CMD_SE	    (COMMAND_IMPORT,              		"File\\Import", 			CommandImport,			true);
	REGISTER_CMD_SE	    (COMMAND_LOAD_FIRSTRECENT,          "File\\Load First Recent",		CommandLoadFirstRecent, true);
	REGISTER_CMD_S	    (COMMAND_CLEAR_DEBUG_DRAW, 		    CommandClearDebugDraw);
	REGISTER_CMD_S	    (COMMAND_IMPORT_COMPILER_ERROR,     CommandImportCompilerError);
	REGISTER_CMD_S      (COMMAND_IMPORT_AICOMPILER_ERROR,   CommandImportXrAICompilerError);
	REGISTER_CMD_S	    (COMMAND_EXPORT_COMPILER_ERROR,     CommandExportCompilerError);
	REGISTER_CMD_S	    (COMMAND_VALIDATE_SCENE,            CommandValidateScene);
	REGISTER_CMD_S	    (COMMAND_CLEAN_LIBRARY,           	CommandCleanLibrary);
	REGISTER_CMD_S	    (COMMAND_RELOAD_OBJECTS,            CommandReloadObjects);
	REGISTER_CMD_SE	    (COMMAND_CUT,              			"Edit\\Cut",					CommandCut,false);
	REGISTER_CMD_SE	    (COMMAND_COPY,              		"Edit\\Copy",					CommandCopy,false);
	REGISTER_CMD_SE	    (COMMAND_PASTE,              		"Edit\\Paste",					CommandPaste,false);
	REGISTER_CMD_SE     (COMMAND_DUPLICATE,					"Edit\\Duplicate",				CommandDuplicate, false);
	REGISTER_CMD_S	    (COMMAND_LOAD_SELECTION,            CommandLoadSelection);
	REGISTER_CMD_S	    (COMMAND_SAVE_SELECTION,            CommandSaveSelection);
	REGISTER_CMD_SE	    (COMMAND_UNDO,              		"Edit\\Undo",					CommandUndo,false);
	REGISTER_CMD_SE	    (COMMAND_REDO,              		"Edit\\Redo",					CommandRedo,false);
	REGISTER_CMD_S	    (COMMAND_CLEAR_SCENE_SUMMARY,	    CommandClearSceneSummary);
	REGISTER_CMD_S	    (COMMAND_COLLECT_SCENE_SUMMARY,     CommandCollectSceneSummary);
	REGISTER_CMD_S	    (COMMAND_SHOW_SCENE_SUMMARY,        CommandShowSceneSummary);
	REGISTER_CMD_S	    (COMMAND_EXPORT_SCENE_SUMMARY,      CommandExportSceneSummary);
	REGISTER_CMD_S	    (COMMAND_SCENE_HIGHLIGHT_TEXTURE,	CommandSceneHighlightTexture);
	REGISTER_CMD_SE	    (COMMAND_OPTIONS,              		"Scene\\Options",		        CommandOptions,false);
	REGISTER_CMD_SE	    (COMMAND_BUILD,              		"Compile\\Build",		        CommandBuild,false);
	REGISTER_CMD_SE	    (COMMAND_MAKE_GAME,              	"Compile\\Make Game",	        CommandMakeGame,false);
	REGISTER_CMD_SE	    (COMMAND_MAKE_PUDDLES,             	"Compile\\Make Puddles",	    CommandMakePuddles,false);
	REGISTER_CMD_SE	    (COMMAND_MAKE_AIMAP,              	"Compile\\Make AI Map",	        CommandMakeAIMap,false);
	REGISTER_CMD_SE	    (COMMAND_MAKE_AIMAP_LEGACY,        	"Compile\\Make AI Map Legacy",  CommandMakeAIMapLegacy,false);
	REGISTER_CMD_SE	    (COMMAND_MOVE_GIZMO,              	"Gizmo\\Set at camera",	        CommandMakeGizmo,false);
	REGISTER_CMD_SE	    (COMMAND_UPDATE_GIZMO,             	"Gizmo\\Update at camera",	    CommandUpdateGizmo,false);
	REGISTER_CMD_SE	    (COMMAND_MAKE_DETAILS,              "Compile\\Make Details",        CommandMakeDetails,false);
	REGISTER_CMD_SE	    (COMMAND_MAKE_HOM,              	"Compile\\Make HOM",	        CommandMakeHOM,false);
	REGISTER_CMD_SE	    (COMMAND_MAKE_SOM,              	"Compile\\Make SOM",	        CommandMakeSOM,false);
	REGISTER_CMD_SE	    (COMMAND_INVERT_SELECTION_ALL,      "Selection\\Invert", 			CommandInvertSelectionAll,false);
	REGISTER_CMD_SE	    (COMMAND_SELECT_ALL,              	"Selection\\Select All", 		CommandSelectAll,false);
	REGISTER_CMD_SE	    (COMMAND_DESELECT_ALL,              "Selection\\Unselect All", 		CommandDeselectAll,false);
	REGISTER_CMD_SE	    (COMMAND_DELETE_SELECTION,          "Edit\\Delete", 				CommandDeleteSelection,false);
	REGISTER_CMD_SE	    (COMMAND_HIDE_UNSEL,              	"Visibility\\Hide Unselected",	CommandHideUnsel,false);
	REGISTER_CMD_SE	    (COMMAND_HIDE_SEL,              	"Visibility\\Hide Selected", 	CommandHideSel,false);
	REGISTER_CMD_SE	    (COMMAND_HIDE_ALL,              	"Visibility\\Hide All", 		CommandHideAll,false);
	REGISTER_CMD_SE	    (COMMAND_CREATE_SHAPE_BOX,         	"Create\\Box", 					CommandCreateShapeBox,false);
	REGISTER_CMD_SE	    (COMMAND_CREATE_SHAPE_SPHERE,      	"Create\\Sphere", 				CommandCreateShapeSphere,false);
	REGISTER_CMD_S	    (COMMAND_LOCK_ALL,              	CommandLockAll);
	REGISTER_CMD_S	    (COMMAND_LOCK_SEL,					CommandLockSel);
	REGISTER_CMD_S	    (COMMAND_LOCK_UNSEL,              	CommandLockUnsel);
	REGISTER_CMD_S		(COMMAND_SET_SNAP_OBJECTS,          CommandSetSnapObjects);
	REGISTER_CMD_S	    (COMMAND_ADD_SEL_SNAP_OBJECTS,      CommandAddSelSnapObjects);
	REGISTER_CMD_S	    (COMMAND_DEL_SEL_SNAP_OBJECTS,      CommandDelSelSnapObjects);
	REGISTER_CMD_S	    (COMMAND_CLEAR_SNAP_OBJECTS,        CommandClearSnapObjects);
	REGISTER_CMD_S	    (COMMAND_SELECT_SNAP_OBJECTS,       CommandSelectSnapObjects);
	REGISTER_CMD_S	    (COMMAND_REFRESH_SNAP_OBJECTS,      CommandRefreshSnapObjects);
//	REGISTER_CMD_S	    (COMMAND_REFRESH_SOUND_ENVS,        CommandRefreshSoundEnvs);
	REGISTER_CMD_S	    (COMMAND_ICON_LOAD, CommandLoadCustomIcons);
	REGISTER_CMD_S	    (COMMAND_ICON_REMOVE, CommandRemoveCustomIcon);
	REGISTER_CMD_S	    (COMMAND_REFRESH_SOUND_ENV_GEOMETRY,CommandRefreshSoundEnvGeometry);
	REGISTER_CMD_S	    (COMMAND_SHOWCONTEXTMENU,           CommandShowContextMenu);
	REGISTER_CMD_S	    (COMMAND_UPDATE_TOOLBAR,            CommandUpdateToolBar);
	REGISTER_CMD_S	    (COMMAND_UPDATE_CAPTION,            CommandUpdateCaption);
	REGISTER_CMD_S	    (COMMAND_CREATE_SOUND_LIB,          CommandCreateSoundLib);
	REGISTER_CMD_SE	    (COMMAND_TOGGLE_AIMAP_VISIBILITY,   "Visibility\\Toggle AI-Map",			CommandToggleAiMapVisibility,true);
	REGISTER_CMD_S	    (COMMAND_SHOW_CLIP_EDITOR,			CommandShowClipEditor);
	
}

char* CLevelMain::GetCaption()
{
	return (char*)(Tools->m_LastFileName.empty()?"noname":Tools->m_LastFileName.c_str());
}

bool  CLevelMain::ApplyShortCut(DWORD Key, TShiftState Shift)
{
	if (Scene->IsPlayInEditor())return true;
	return inherited::ApplyShortCut(Key,Shift);
}


bool  CLevelMain::ApplyGlobalShortCut(DWORD Key, TShiftState Shift)
{
	return inherited::ApplyGlobalShortCut(Key,Shift);
}

void RetrieveSceneObjPointAndNormal( Fvector& hitpoint, Fvector* hitnormal, const SRayPickInfo &pinf, int bSnap )
{
	if(pinf.e_mesh == 0)
	{
	  hitpoint = pinf.pt;
	   if (hitnormal && pinf.visual_inf.K )
			*hitnormal = pinf.visual_inf.normal;
	   return;
	}
	if (Tools->GetSettings(etfVSnap) && bSnap)
	{
		Fvector pn;
		float u = pinf.inf.u;
		float v = pinf.inf.v;
		float w = 1 - (u + v);
		Fvector verts[3];
		pinf.e_obj->GetFaceWorld(pinf.s_obj->_Transform(), pinf.e_mesh, pinf.inf.id, verts);

		if ((w > u) && (w > v))
			pn.set(verts[0]);
		else if ((u > w) && (u > v))
			pn.set(verts[1]);
		else
			pn.set(verts[2]);

		if (pn.distance_to(pinf.pt) < LTools->m_MoveSnap)
			hitpoint.set(pn);
		else
			hitpoint.set(pinf.pt);
	}
	else
	{
		hitpoint.set(pinf.pt);
	}

	if (hitnormal)
	{
		Fvector verts[3];
		pinf.e_obj->GetFaceWorld(pinf.s_obj->_Transform(),pinf.e_mesh,pinf.inf.id,verts);
		hitnormal->mknormal(verts[0],verts[1],verts[2]);
	}
}


bool EditLibPickObjectGeometry(  Fvector& hitpoint,  const Fvector& start, const Fvector& direction, int bSnap, Fvector* hitnormal )
{
	SRayPickInfo pinf;
	/*if( TfrmEditLibrary::RayPick( start, direction, &pinf ) )
	{
		RetrieveSceneObjPointAndNormal( hitpoint,  hitnormal, pinf, bSnap );
		return true;
	}*/
	return false;
}

bool ScenePickObjectGeometry(Fvector& hitpoint, const Fvector& start, const Fvector& direction, int bSnap, Fvector* hitnormal)
{
	constexpr std::array ObjClasses = 
	{
	   OBJCLASS_SPAWNPOINT,
	   OBJCLASS_SCENEOBJECT,
	   OBJCLASS_TERRAIN
	};

	xr_optional<SRayPickInfo> Hits;

	for (ESceneItemsGuids objClass : ObjClasses)
	{
		SRayPickInfo currentInfo;
		if (Scene->RayPickObject(currentInfo.inf.range, start, direction, objClass, &currentInfo, Scene->GetSnapList(false)))
		{
			if (!Hits || currentInfo.inf.range < Hits->inf.range)
			{
				Hits = currentInfo;
			}
		}
	}

	if (Hits)
	{
		RetrieveSceneObjPointAndNormal(hitpoint, hitnormal, *Hits, bSnap);
		return true;
	}

	return false;
}

bool PickObjectGeometry( EEditorState est, Fvector& hitpoint,  const Fvector& start, const Fvector& direction, int bSnap, Fvector* hitnormal )
{

	switch(est)
	{
	   case esEditLibrary:
			return EditLibPickObjectGeometry( hitpoint, start, direction, bSnap, hitnormal );
	   case esEditScene:
			return ScenePickObjectGeometry( hitpoint, start, direction, bSnap, hitnormal );
		default:
			NODEFAULT;
	}
	return false;
}

bool PickGrid(  Fvector& hitpoint,  const Fvector& start, const Fvector& direction, int bSnap, Fvector* hitnormal )
{
	 
	// pick grid
	Fvector normal;
	normal.set( 0, 1, 0 );
	float clcheck = direction.dotproduct( normal );

	if( fis_zero( clcheck ) )
		 return false;

	float alpha = - start.dotproduct(normal) / clcheck;
	
	if( alpha <= 0 )
		return false;

	hitpoint.x = start.x + direction.x * alpha;
	hitpoint.y = start.y + direction.y * alpha;
	hitpoint.z = start.z + direction.z * alpha;

	if (Tools->GetSettings(etfGSnap) && bSnap)
	{
		hitpoint.x = snapto(hitpoint.x, LTools->m_MoveSnap);
		hitpoint.z = snapto(hitpoint.z, LTools->m_MoveSnap);
		hitpoint.y = 0.f;
	}
	
	if (hitnormal)
		hitnormal->set(0,1,0);
	return true;
}

bool CLevelMain::PickGround(Fvector& hitpoint, const Fvector& start, const Fvector& direction, int bSnap, Fvector* hitnormal){
	VERIFY(m_bReady);
	
	EEditorState est = GetEState();
	if( est!= esEditLibrary && est != esEditScene )
		return false;
   
	// pick object geometry
	if( (bSnap==-1) || ( Tools->GetSettings(etfOSnap) && (bSnap==1) ) )
	{
	  bool b =  PickObjectGeometry( est, hitpoint, start, direction, bSnap,  hitnormal );
	  if(b)
	  return true;
	}

	return   PickGrid( hitpoint, start, direction, bSnap,  hitnormal );

}


bool CLevelMain::SelectionFrustum(CFrustum& frustum)
{
	VERIFY(m_bReady);
	Fvector st,d,p[4];
	Ivector2 pt[4];

	float depth = 0;

	float x1=m_StartCp.x, x2=m_CurrentCp.x;
	float y1=m_StartCp.y, y2=m_CurrentCp.y;

	if(!(x1!=x2&&y1!=y2)) return false;

	pt[0].set(_min(x1,x2),_min(y1,y2));
	pt[1].set(_max(x1,x2),_min(y1,y2));
	pt[2].set(_max(x1,x2),_max(y1,y2));
	pt[3].set(_min(x1,x2),_max(y1,y2));

	SRayPickInfo pinf;
	for (int i=0; i<4; i++){
		UI->CurrentView().m_Camera.MouseRayFromPoint(st, d, pt[i]);
		if (EPrefs->bp_lim_depth){
			pinf.inf.range = UI->CurrentView().m_Camera._Zfar(); // max pick range
			if (Scene->RayPickObject(pinf.inf.range, st, d, OBJCLASS_SCENEOBJECT, &pinf, 0))
				if (pinf.inf.range > depth) depth = pinf.inf.range;
		}
	}
	if (depth<UI->CurrentView().m_Camera._Znear()) depth = UI->CurrentView().m_Camera._Zfar();
	else depth += EPrefs->bp_depth_tolerance;

	for (int i=0; i<4; i++){
		UI->CurrentView().m_Camera.MouseRayFromPoint(st, d, pt[i]);
		p[i].mad(st,d,depth);
	}

	Fvector pos = UI->CurrentView().m_Camera.GetPosition();
	frustum.CreateFromPoints(p,4,pos);

	Fplane P; P.build(p[0],p[1],p[2]);
	if (P.classify(st)>0) P.build(p[2],p[1],p[0]);
	frustum._add(P);

	return true;
}

void CLevelMain::RealUpdateScene()
{
	inherited::RealUpdateScene	();
	if (GetEState()==esEditScene)
	{
		Scene->OnObjectsUpdate	();
		LTools->OnObjectsUpdate	(); // �������� ��� ��� ���-�� ������� � ���������
		RedrawScene				();
	}
}



void CLevelMain::ShowContextMenu(int cls)
{
	VERIFY(m_bReady);
   /* POINT pt;
	GetCursorPos(&pt);
	fraLeftBar->miProperties->Enabled = false;
	if (Scene->SelectionCount( true, cls )) fraLeftBar->miProperties->Enabled = true;
	RedrawScene(true);
	fraLeftBar->pmObjectContext->TrackButton = tbRightButton;
	fraLeftBar->pmObjectContext->Popup(pt.x,pt.y);*/
}






// Common

void CLevelMain::ResetStatus()
{
	VERIFY(m_bReady);
  /*  if (fraBottomBar->paStatus->Caption!=""){
		fraBottomBar->paStatus->Caption=""; fraBottomBar->paStatus->Repaint();
	}*/
}
void CLevelMain::SetStatus(LPCSTR s, bool bOutLog)
{
	VERIFY(m_bReady);

	if (bOutLog && s && s[0])
		ELog.Msg(mtInformation, s);

	UI->ProgressStatusName = s;
}
void CLevelMain::ProgressDraw()
{
	inherited::ProgressDraw();
	//fraBottomBar->RedrawBar();
}

void CLevelMain::RealQuit()
{
	//frmMain->Close();
}


void CLevelMain::SaveSettings(nlohmann::json& js)
{
	inherited::SaveSettings(js);
	SSceneSummary::Save();
}
void CLevelMain::LoadSettings(nlohmann::json& js)
{
	inherited::LoadSettings(js);
	SSceneSummary::Load();
}

Ivector2 CLevelMain::GetRenderMousePosition() const
{
	TUI::Viewport& Viewport = UI->CurrentView();
	return Viewport.ViewportForm->GetMousePos();
}

void CLevelMain::OnDrawUI()
{
	inherited::OnDrawUI();
	UIObjectList::Update();
	if (LTools->GetToolForm())
	{
		LTools->GetToolForm()->OnDrawUI();
	}
}

bool CLevelMain::KeyDown(WORD Key, TShiftState Shift)
{
	if (TUI::KeyDown(Key, Shift))
		return true;

	return false;
}

void CLevelMain::OnStats(CGameFont* font)
{
	float Height = font->GetHeight();
	font->SetColor(color_rgba(255, 0, 0, 255));
	font->SetHeight(14);
	if (!Scene->m_RTFlags.is(EScene::flIsBuildedCForm))
	{
		font->OutNext("NEED REBUILD CFORM");
	}
	if (!Scene->m_RTFlags.is(EScene::flIsBuildedAIMap))
	{
		font->OutNext("NEED REBUILD AIMAP");
	}
	if (!Scene->m_RTFlags.is(EScene::flIsBuildedGameGraph))
	{
		font->OutNext("NEED REBUILD GAME GRAPH");
	}


	font->SetHeight(Height);
}




bool CLevelMain::IsPlayInEditor()
{
	return Scene->IsPlayInEditor();
}

static void jacobi_eigen_3x3(float A[3][3], float evals[3], Fvector vecs[3]) {
	float V[3][3] = { {1,0,0},{0,1,0},{0,0,1} };
	float D[3][3];
	for (int i = 0; i < 3; ++i) for (int j = 0; j < 3; ++j) D[i][j] = A[i][j];

	const int MAX_ITERS = 60;
	for (int iter = 0; iter < MAX_ITERS; ++iter) {
		int p = 0, q = 1;
		float maxv = fabsf(D[0][1]);
		if (fabsf(D[0][2]) > maxv) { p = 0; q = 2; maxv = fabsf(D[0][2]); }
		if (fabsf(D[1][2]) > maxv) { p = 1; q = 2; maxv = fabsf(D[1][2]); }
		if (maxv < 1e-9f) break;
		float apq = D[p][q];
		float app = D[p][p];
		float aqq = D[q][q];
		float phi = 0.5f * atan2f(2.0f * apq, aqq - app);
		float c = cosf(phi), s = sinf(phi);
		for (int i = 0; i < 3; ++i) {
			float dip = D[i][p], diq = D[i][q];
			D[i][p] = c * dip - s * diq;
			D[i][q] = s * dip + c * diq;
		}
		for (int i = 0; i < 3; ++i) {
			float dpi = D[p][i], dqi = D[q][i];
			D[p][i] = c * dpi - s * dqi;
			D[q][i] = s * dpi + c * dqi;
		}
		D[p][q] = D[q][p] = 0.0f;
		float new_app = c * c * app - 2.0f * s * c * apq + s * s * aqq;
		float new_aqq = s * s * app + 2.0f * s * c * apq + c * c * aqq;
		D[p][p] = new_app; D[q][q] = new_aqq;
		for (int i = 0; i < 3; ++i) {
			float vip = V[i][p], viq = V[i][q];
			V[i][p] = c * vip - s * viq;
			V[i][q] = s * vip + c * viq;
		}
	}
	evals[0] = D[0][0]; evals[1] = D[1][1]; evals[2] = D[2][2];
	for (int c = 0; c < 3; ++c) {
		vecs[c].set(V[0][c], V[1][c], V[2][c]);
		float len = vecs[c].magnitude();
		if (len > 1e-9f) vecs[c].mul(1.0f / len);
		else {
			if (c == 0) vecs[c].set(1, 0, 0);
			else if (c == 1) vecs[c].set(0, 1, 0);
			else vecs[c].set(0, 0, 1);
		}
	}
}

bool ConvertLevelSndEnvToLtx(const char* src_path, const char* dst_path)
{
	if (!src_path || !dst_path)
	{
		ELog.DlgMsg(mtError, "ConvertLevelSndEnvToLtx: invalid paths");
		return false;
	}

	IReader* F = FS.r_open(src_path);
	if (!F)
	{
		ELog.DlgMsg(mtError, "ConvertLevelSndEnvToLtx: cannot open '%s'", src_path);
		return false;
	}

	// chunk 0 - names (unique names list)
	xr_vector<shared_str> env_names;
	if (IReader* names = F->open_chunk(0))
	{
		while (!names->eof())
		{
			string256 s;
			names->r_stringZ(s, sizeof(s));
			env_names.emplace_back(s);
		}
		names->close();
	}
	else
	{
		ELog.DlgMsg(mtError, "ConvertLevelSndEnvToLtx: missing chunk 0 in '%s'", src_path);
		FS.r_close(F);
		return false;
	}

	// chunk 1 - geometry
	IReader* geom_ch = F->open_chunk(1);
	if (!geom_ch) {
		ELog.DlgMsg(mtError, "ConvertLevelSndEnvToLtx: missing chunk 1 in '%s'", src_path);
		FS.r_close(F);
		return false;
	}

	// copy geom chunk into memory
	u32 geom_size = geom_ch->length();
	u8* data = (u8*)xr_malloc(geom_size);
	Memory.mem_copy(data, geom_ch->pointer(), geom_size);
	IReader* geom = new IReader(data, geom_size, 0);

	// read header
	hdrCFORM H;
	geom->r(&H, sizeof(hdrCFORM));
	if (H.vertcount == 0 || H.facecount == 0) {
		Msg("ConvertLevelSndEnvToLtx: empty geometry in %s", src_path);
		xr_free(data);
		geom_ch->close();
		geom->close();
		FS.r_close(F);
		return false;
	}

	// pointers to arrays in chunk
	Fvector* verts = (Fvector*)geom->pointer();
	CDB::TRI* tris = (CDB::TRI*)(verts + H.vertcount);

	const int TRIS_PER_BOX = 12;

	struct SoundZone {
		u32 material{};           // raw material (inner<<16)|outer
		shared_str inner;       // name
		shared_str outer;       // name
		xr_vector<Fvector> verts; // все вершины этого бокса
	};

	xr_vector<SoundZone> zones;

	// Проходим по всем треугольникам и группируем их по боксам
	for (u32 i = 0; i < H.facecount; i += TRIS_PER_BOX) {
		if (i + TRIS_PER_BOX > H.facecount) break;

		// Берем материал из первого треугольника бокса
		u32 mat = tris[i].dummy;
		u16 id_inner = (u16)((mat & 0xffff0000u) >> 16);
		u16 id_outer = (u16)((mat & 0x0000ffffu) >> 0);

		// validate against names table
		if (id_inner >= env_names.size() || id_outer >= env_names.size()) {
			continue;
		}

		SoundZone zone;
		zone.material = mat;
		zone.inner = env_names[id_inner];
		zone.outer = env_names[id_outer];

		// Собираем все уникальные вершины этого бокса
		xr_hash_set<u32> unique_vert_indices;
		for (u32 j = 0; j < TRIS_PER_BOX; j++) {
			const CDB::TRI& T = tris[i + j];
			unique_vert_indices.insert(T.verts[0]);
			unique_vert_indices.insert(T.verts[1]);
			unique_vert_indices.insert(T.verts[2]);
		}

		// Добавляем вершины в зону
		for (u32 vert_idx : unique_vert_indices) {
			if (vert_idx < H.vertcount) {
				zone.verts.push_back(verts[vert_idx]);
			}
		}

		zones.push_back(std::move(zone));
	}

	// ini writer
	CInifile ini(dst_path, FALSE, FALSE, FALSE);

	auto normalize_angle = [](float a) -> float {
		const float TWO_PI = 2.0f * PI;
		while (a > PI) a -= TWO_PI;
		while (a <= -PI) a += TWO_PI;
		return a;
		};

	// helper: compute covariance & mean
	auto compute_covariance_and_mean = [](const xr_vector<Fvector>& pts, Fvector& mean_out, float cov[3][3]) {
		mean_out.set(0, 0, 0);
		size_t n = pts.size();
		for (const auto& p : pts) mean_out.add(p);
		mean_out.mul(1.0f / float(n));
		for (int i = 0; i < 3; ++i) for (int j = 0; j < 3; ++j) cov[i][j] = 0.0f;
		for (const auto& p : pts) {
			Fvector d; d.sub(p, mean_out);
			cov[0][0] += d.x * d.x; cov[0][1] += d.x * d.y; cov[0][2] += d.x * d.z;
			cov[1][0] += d.y * d.x; cov[1][1] += d.y * d.y; cov[1][2] += d.y * d.z;
			cov[2][0] += d.z * d.x; cov[2][1] += d.z * d.y; cov[2][2] += d.z * d.z;
		}
		float invn = 1.0f / float(n);
		for (int i = 0; i < 3; ++i) for (int j = 0; j < 3; ++j) cov[i][j] *= invn;
		};

	u32 object_count = (u32)zones.size();
	for (u32 idx = 0; idx < object_count; idx++)
	{
		SoundZone& Z = zones[idx];
		if (Z.verts.empty()) continue;

		// PCA для вычисления ориентации бокса
		Fvector mean; float cov[3][3];
		compute_covariance_and_mean(Z.verts, mean, cov);

		float evals[3]; Fvector axes[3];
		jacobi_eigen_3x3(cov, evals, axes);

		// sort eigenpairs by descending eigenvalue
		for (int i = 0; i < 3; ++i) {
			for (int j = i + 1; j < 3; ++j) {
				if (evals[j] > evals[i]) {
					std::swap(evals[i], evals[j]);
					std::swap(axes[i], axes[j]);
				}
			}
		}

		// Выбираем лучшую ориентацию (как в вашем оригинальном коде)
		Fvector best_ax, best_ay, best_az;
		Fvector best_rotation; best_rotation.set(0, 0, 0);
		Fvector best_scale; best_scale.set(0, 0, 0);
		Fvector best_world_center; best_world_center.set(0, 0, 0);
		float best_score = FLT_MAX;

		int permutations[6][3] = {
			{0,1,2},{0,2,1},{1,0,2},
			{1,2,0},{2,0,1},{2,1,0}
		};

		for (int perm = 0; perm < 6; ++perm) {
			int i = permutations[perm][0];
			int j = permutations[perm][1];
			int k = permutations[perm][2];

			Fvector ax = axes[i]; ax.normalize();
			Fvector ay = axes[j]; ay.normalize();
			Fvector az = axes[k]; az.normalize();

			// ensure right-handed
			Fvector cross; cross.crossproduct(ax, ay);
			if (cross.dotproduct(az) < 0.0f) {
				ay.mul(-1.0f);
			}

			// project points to compute extents
			float min_x = FLT_MAX, max_x = -FLT_MAX;
			float min_y = FLT_MAX, max_y = -FLT_MAX;
			float min_z = FLT_MAX, max_z = -FLT_MAX;
			for (const auto& v : Z.verts) {
				Fvector rel; rel.sub(v, mean);
				float px = rel.dotproduct(ax);
				float py = rel.dotproduct(ay);
				float pz = rel.dotproduct(az);
				if (px < min_x) min_x = px; if (px > max_x) max_x = px;
				if (py < min_y) min_y = py; if (py > max_y) max_y = py;
				if (pz < min_z) min_z = pz; if (pz > max_z) max_z = pz;
			}

			Fvector scale;
			scale.x = (max_x - min_x);
			scale.y = (max_y - min_y);
			scale.z = (max_z - min_z);

			Fvector local_center;
			local_center.x = (min_x + max_x) * 0.5f;
			local_center.y = (min_y + max_y) * 0.5f;
			local_center.z = (min_z + max_z) * 0.5f;

			Fvector world_center = mean;
			world_center.mad(world_center, ax, local_center.x);
			world_center.mad(world_center, ay, local_center.y);
			world_center.mad(world_center, az, local_center.z);

			Fmatrix R; R.identity();
			R.i.set(ax); R.j.set(ay); R.k.set(az);

			Fvector rot; R.getXYZ(rot);
			rot.x = normalize_angle(rot.x);
			rot.y = normalize_angle(rot.y);
			rot.z = normalize_angle(rot.z);

			float score = fabsf(rot.x) + fabsf(rot.y) + fabsf(rot.z);
			if (scale.x <= 0 || scale.y <= 0 || scale.z <= 0) score += 1e6f;

			if (score < best_score) {
				best_score = score;
				best_ax = ax; best_ay = ay; best_az = az;
				best_rotation = rot;
				best_scale = scale;
				best_world_center = world_center;
			}
		}

		const float MIN_SCALE = 0.0001f;
		if (best_scale.x < MIN_SCALE) best_scale.x = MIN_SCALE;
		if (best_scale.y < MIN_SCALE) best_scale.y = MIN_SCALE;
		if (best_scale.z < MIN_SCALE) best_scale.z = MIN_SCALE;

		// write to ini
		string64 sect; xr_sprintf(sect, "object_%u", idx);
		ini.w_u32(sect, "clsid", 10);
		ini.w_u32(sect, "co_flags", 0);
		ini.w_string(sect, "env_inner", Z.inner.c_str());
		ini.w_string(sect, "env_outer", Z.outer.c_str());

		string64 nm; xr_sprintf(nm, "sound_env_%u", idx);
		ini.w_string(sect, "name", nm);

		ini.w_fvector3(sect, "position", best_world_center);
		ini.w_fvector3(sect, "rotation", best_rotation);
		ini.w_u32(sect, "rt_flags", 57);
		ini.w_fvector3(sect, "scale", best_scale);
		ini.w_u32(sect, "version", 18);
	}
	ini.w_u32("main", "objects_count", object_count);
	ini.save_as(dst_path);

	// cleanup
	xr_free(data);
	geom_ch->close();
	geom->close();
	FS.r_close(F);

	Msg("* ConvertLevelSndEnvToLtx finished: %s -> %s (objects: %u)", src_path, dst_path, (u32)object_count);
	return true;
}

#include "../../utils/xrLC_Light/R_light.h"

bool ConvertAllLightsToLtx(const char* src_path, const char* dst_path)
{
	if (!src_path || !dst_path) {
		ELog.DlgMsg(mtError, "ConvertAllLightsToLtx: invalid paths");
		return false;
	}

	IReader* F = FS.r_open(src_path);
	if (!F) {
		ELog.DlgMsg(mtError, "ConvertAllLightsToLtx: cannot open '%s'", src_path);
		return false;
	}

	// Создаем выходной INI
	CInifile ini(dst_path, FALSE, FALSE, FALSE);

	// Создаем секцию [guid]
	ini.w_u64("guid", "guid_g0", 6148914691236517205ULL);
	ini.w_u64("guid", "guid_g1", 6148914691236517205ULL);

	// Создаем секцию [lcontrols]
	ini.w_string("lcontrols", "$hemi", "1");
	ini.w_string("lcontrols", "$static", "0");
	ini.w_string("lcontrols", "$sun", "2");

	// Создаем секцию [main] с временными значениями
	ini.w_u32("main", "objects_count", 0);
	ini.w_u32("main", "version", 0);
	ini.w_u32("main", "flags", 0);
	ini.w_u32("main", "lcontrol_last_idx", 3);
	ini.w_fvector2("main", "sun_shadow_dir", Fvector2().set(-0.436332f, 5.096362f));

	// Создаем секцию [modif]
	ini.w_string("modif", "name", "");
	ini.w_u32("modif", "time", 0);

	// Читаем chunk 1 - Все статические света
	IReader* lights_chunk = F->open_chunk(1);
	if (!lights_chunk) {
		ELog.DlgMsg(mtError, "ConvertAllLightsToLtx: missing lights chunk in '%s'", src_path);
		FS.r_close(F);
		return false;
	}

	u32 size = lights_chunk->length();
	u32 element_size = sizeof(R_Light);
	u32 count = size / element_size;

	if (count * element_size != size) {
		ELog.DlgMsg(mtError, "ConvertAllLightsToLtx: invalid data size in '%s'", src_path);
		lights_chunk->close();
		FS.r_close(F);
		return false;
	}

	Msg("* Lights: found %d lights in chunk", count);

	u32 object_count = 0;

	for (u32 i = 0; i < count; i++) {
		R_Light Ldata;
		lights_chunk->r(&Ldata, sizeof(R_Light));

		// Импортируем ВСЕ света независимо от типа
		string64 sect_name;
		xr_sprintf(sect_name, "object_%u", object_count);

		// Записываем параметры света в формате CLight
		ini.w_u32(sect_name, "clsid", 3); // CLSID_LIGHT
		ini.w_u32(sect_name, "co_flags", 2);

		// Позиция
		ini.w_fvector3(sect_name, "position", Ldata.position);

		// Вращение - для направленных источников используем direction
		Fvector rotation;
		if (Ldata.type == LT_DIRECT) {
			// Для направленного света используем реальное направление
			rotation.setHP(Ldata.direction.y, Ldata.direction.x);
		}
		else {
			// Для точечных - направление вниз
			rotation.set(0.0f, 0.0f, 1.0f);
		}
		ini.w_fvector3(sect_name, "rotation", rotation);

		// Цвет - конвертируем из Fvector в Fcolor
		Fcolor color;
		color.set(Ldata.diffuse.x, Ldata.diffuse.y, Ldata.diffuse.z, 1.0f);
		ini.w_fcolor(sect_name, "color", color);

		// Яркость - вычисляем из интенсивности
		float brightness = (Ldata.diffuse.x + Ldata.diffuse.y + Ldata.diffuse.z) / 3.0f;
		ini.w_float(sect_name, "brightness", brightness);

		// Диапазон
		ini.w_float(sect_name, "range", Ldata.range);

		// Затухание
		ini.w_float(sect_name, "attenuation0", Ldata.attenuation0);
		ini.w_float(sect_name, "attenuation1", Ldata.attenuation1);
		ini.w_float(sect_name, "attenuation2", Ldata.attenuation2);

		// Falloff
		float falloff = Ldata.falloff > 0 ? Ldata.falloff : 1.0f;
		ini.w_float(sect_name, "falloff", falloff);

		// Угол конуса - для точечных источников всегда 0, для направленных тоже 0
		// (в R_Light нет поля phi/cone, это параметр для spot lights которого нет в статических)
		ini.w_float(sect_name, "cone", 0.0f);

		// Виртуальный размер
		ini.w_float(sect_name, "virtual_size", 0.0f);

		// Тип света
		ini.w_u32(sect_name, "type", Ldata.type);

		// Флаги - статический свет
		ini.w_u32(sect_name, "light_flags", 1);

		// Контроллер света - определяем по типу
		u32 lcontrol = 0; // по умолчанию static
		if (Ldata.type == LT_DIRECT) {
			lcontrol = 2; // sun
		}
		else if (Ldata.level > 0) {
			lcontrol = 1; // hemi
		}
		ini.w_u32(sect_name, "light_control", lcontrol);

		// Имя - с указанием типа
		string64 light_name;
		const char* type_name = "unknown";
		switch (Ldata.type) {
		case LT_POINT: type_name = "point"; break;
		case LT_DIRECT: type_name = "direct"; break;
		case LT_SECONDARY: type_name = "secondary"; break;
		}
		xr_sprintf(light_name, "light_%s_%04u", type_name, object_count);
		ini.w_string(sect_name, "name", light_name);

		// Другие параметры
		ini.w_u32(sect_name, "rt_flags", 17);
		ini.w_fvector3(sect_name, "scale", Fvector().set(1.0f, 1.0f, 1.0f));
		ini.w_string(sect_name, "use_in_d3d", "off");
		ini.w_u32(sect_name, "version", 17);
		ini.w_string(sect_name, "anim_ref_name", "");
		ini.w_string(sect_name, "fallof_texture", "");

		object_count++;

		Msg("* Light %d: type=%d(%s), level=%d, pos=[%.3f,%.3f,%.3f], range=%.3f, energy=%.3f",
			i, Ldata.type, type_name, Ldata.level,
			Ldata.position.x, Ldata.position.y, Ldata.position.z,
			Ldata.range, Ldata.energy);
	}

	// Обновляем количество объектов в секции [main]
	//ini.w_u32("main", "objects_count", object_count);

	lights_chunk->close();
	FS.r_close(F);

	ini.save_as(dst_path);
	Msg("* ConvertAllLightsToLtx: converted %d lights to '%s'", object_count, dst_path);

	if (object_count == 0) {
		ELog.DlgMsg(mtError, "No lights found in the file!");
		return false;
	}

	return true;
}

CCommandVar CommandImport(CCommandVar p1, CCommandVar p2)
{
	LoaderEvent.wait();

	if (!Scene->locked())
	{
		if (!Scene->IfModified())
			return TRUE;

		string_path query{};
		FS.update_path(query, "$import$", "level.snd_env");

		string_path output{};
		FS.update_path(output, "$import$", "export\\sound_env.part");

		ConvertLevelSndEnvToLtx(query, output);


		string_path query1{};
		FS.update_path(query1, "$import$", "build.lights"); // бинарный файл с хеми-лайтами

		string_path output1{};
		FS.update_path(output1, "$import$", "export\\hemi_lights.ltx");

		ConvertAllLightsToLtx(query1, output1);


		return TRUE;
	}
	else
	{
		ELog.DlgMsg(mtError, "Scene sharing violation");
		return FALSE;
	}
}