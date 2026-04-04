#include "stdafx.h"
#include "Utils/Cursor3D.h"
#include "../xrengine\GameFont.h"
#include "UI\UIEditLibrary.h"
#include "Editor/Utils/ContentView.h"
#include "../xrECore/Editor/UIEditLightAnim.h"
#include "Editor/Utils/GitIntegration.h"
#include "Editor/Utils/GitLFSConfig.h"
#include "UI/UIGitWindow.h"
#include "UI/UISharedMaterialsLibrary.h"

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
		return true;
	}
	return false;
}

// Main commands
CCommandVar CommandLibraryEditor(CCommandVar p1, CCommandVar p2)
{
	UIEditLibrary::Show();

	return true;
}

CCommandVar CommandSharedMaterialsEditor(CCommandVar p1, CCommandVar p2)
{
	UISharedMaterialsLibrary::Show();

	return TRUE;
}

CCommandVar CommandLAnimEditor(CCommandVar p1, CCommandVar p2)
{
	UIEditLightAnim::Show();
	return true;
}

CCommandVar CommandLoadCustomIcons(CCommandVar p1, CCommandVar p2)
{
	GContentView->LoadCustomIcons();
	return true;
}

CCommandVar CommandRemoveCustomIcon(CCommandVar p1, CCommandVar p2)
{
	GContentView->RemoveCustomIcon(p1);
	return true;
}

CCommandVar CLevelTool::CommandEnableTarget(CCommandVar p1, CCommandVar p2)
{
	ESceneToolBase* M = Scene->GetTool(p1);
	VERIFY(M);
	bool res = false;
	if (p2)
	{
		res = ExecCommand(COMMAND_LOAD_LEVEL_PART, M->FClassID, true);
		if (res)
			M->m_EditFlags.set(ESceneToolBase::flEnable, true);
	}
	else
	{
		if (!Scene->IfModified())
		{
			M->m_EditFlags.set(ESceneToolBase::flEnable, true);
			res = false;
		}
		else
		{
			res = ExecCommand(COMMAND_UNLOAD_LEVEL_PART, M->FClassID, true);
			if (res)
				M->m_EditFlags.set(ESceneToolBase::flEnable, false);
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
		M->m_EditFlags.set(ESceneToolBase::flVisible,true);
	else
		M->m_EditFlags.set(ESceneToolBase::flVisible,false);
		
	return true;
}

CCommandVar CLevelTool::CommandReadonlyTarget(CCommandVar p1, CCommandVar p2)
{
	ESceneToolBase* M = Scene->GetTool(p1); VERIFY(M);
	bool res = true;
	if (p2)
	{
		if (!Scene->IfModified())
		{
			M->m_EditFlags.set(ESceneToolBase::flForceReadonly, false);
			res = false;
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
	return 					false;
}
CCommandVar CommandLoadLevelPart(CCommandVar p1, CCommandVar p2)
{
	xr_string temp_fn	= LTools->m_LastFileName.c_str();
	if (!temp_fn.empty())
		return			Scene->LoadLevelPart(temp_fn.c_str(),p1);
	return				true;
}
CCommandVar CommandUnloadLevelPart(CCommandVar p1, CCommandVar p2)
{
	xr_string temp_fn	= LTools->m_LastFileName.c_str();
	if (!temp_fn.empty())
		return			Scene->UnloadLevelPart(temp_fn.c_str(),p1);
	return				true;
}

CCommandVar CommandLoad(CCommandVar p1, CCommandVar p2)
{
	LUI->LoaderEvent.wait();

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

			if (IsUTF8(temp_fn.c_str()))
			{
				temp_fn = Platform::UTF8_to_CP1251(temp_fn);
			}

			xr_strlwr(temp_fn);

			if (!Scene->IfModified())
				return false;

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

			LUI->LoaderEvent.run
			(
				[temp_fn, is_ltx]
				{
					bool Result = (is_ltx) ? Scene->LoadLTX(temp_fn.c_str(), false) : Scene->Load(temp_fn.c_str(), false);

					if (Result)
					{
						UI->ResetStatus();
						Scene->UndoClear();

						bool bk1 = Scene->m_RTFlags.test(EScene::flRT_Unsaved);
						bool bk2 = Scene->m_RTFlags.test(EScene::flRT_Modified);

						Scene->UndoSave();

						Scene->m_RTFlags.set(EScene::flRT_Unsaved, bk1);
						Scene->m_RTFlags.set(EScene::flRT_Modified, bk2);

						ExecCommand(COMMAND_CLEAN_LIBRARY);
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

		return true;
	}
	else
	{
		ELog.DlgMsg(mtError, "Scene sharing violation");
		return false;
	}
	return true;
}

CCommandVar CommandSaveBackup(CCommandVar p1, CCommandVar p2)
{
	LUI->LoaderEvent.wait();

	string_path 	fn;
	xr_strconcat(fn,Core.CompName,"_",Core.UserName,"_backup.level");
	FS.update_path	(fn,_maps_,fn);
	return 			ExecCommand(COMMAND_SAVE,xr_string(fn));
}

CCommandVar CommandSave(CCommandVar p1, CCommandVar p2)
{
	LUI->LoaderEvent.wait();

	if (!Scene->locked())
	{
		if (p2 == 1)
		{
			xr_string temp_fn = LTools->m_LastFileName.c_str();
			if (EFS.GetSaveName(_maps_, temp_fn))
			{
				return ExecCommand(COMMAND_SAVE, temp_fn, 66);
			}

			return false;
		}
		else
		{
			if (p1.IsInteger())
			{
				return ExecCommand(COMMAND_SAVE, xr_string(LTools->m_LastFileName.c_str()), 0);
			}

			xr_string temp_fn = xr_string(p1);
			if (temp_fn.empty())
			{
				return ExecCommand(COMMAND_SAVE, temp_fn, 1);
			}
			else
			{
				xr_strlwr(temp_fn);

				UI->SetStatus("Level saving...");
				Scene->SaveLTX(temp_fn.c_str(), false, (p2 == 66));

				// Track saved file with Git LFS if applicable
				if (Git && Git->IsRepository && Git->LfsAvailable)
				{
					Git->ProcessFileForLFS(temp_fn.c_str());
					
					// Also track associated part files
					string_path partName;
					xr_strconcat(partName, temp_fn.c_str(), ".level");
					Git->ProcessFileForLFS(partName);
				}

				UI->ResetStatus();
				// set new name
				if (0 != xr_strcmp(Tools->m_LastFileName.c_str(), temp_fn.c_str()))
				{
					Tools->m_LastFileName = temp_fn.c_str();
				}
				EPrefs->AppendRecentFile(temp_fn.c_str());
				return 			true;
			}
		}
	}
	else
	{
		ELog.DlgMsg(mtError, "Scene sharing violation");
		return				false;
	}
}

CCommandVar CommandClear(CCommandVar p1, CCommandVar p2)
{
	LUI->LoaderEvent.wait();

	if( !Scene->locked() )
	{
		Scene->Stop();
		
		if (!Scene->IfModified()) 
			return true;
		UI->CurrentView().m_Camera.Reset	();
		Scene->Reset			();
		Scene->m_LevelOp.Reset	();
		Tools->m_LastFileName 		= "";
		LTools->m_LastSelectionName = "";
		Scene->UndoClear		();
		ExecCommand				(COMMAND_CHANGE_TARGET,OBJCLASS_SCENEOBJECT);
		ExecCommand				(COMMAND_CHANGE_ACTION,etaSelect,estDefault);
		ExecCommand				(COMMAND_UPDATE_PROPERTIES,1);
		Scene->UndoSave			();
		return 					true;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
		return					false;
	}
}
CCommandVar CommandLoadFirstRecent(CCommandVar p1, CCommandVar p2)
{
	if (EPrefs->FirstRecentFile())
		return 					ExecCommand(COMMAND_LOAD,xr_string(EPrefs->FirstRecentFile()));
	return 						false;
}

CCommandVar CommandClearDebugDraw(CCommandVar p1, CCommandVar p2)
{
	Tools->ClearDebugDraw		();
	UI->RedrawScene				();
	return 						true;
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
			return true;
			
		CSpawnPoint* sp = smart_cast<CSpawnPoint*>(CO);

		
		if (CKinematicsAnimated* KA = PKinematicsAnimated(sp->m_SpawnData.m_Visual->visual))
		{
			g_clip_maker->ShowEditor(KA);
			UI->Push(g_clip_maker);
		}
	}
	return 							true;
}

CCommandVar CommandImportXrAICompilerError(CCommandVar p1, CCommandVar p2)
{
	xr_string fn;
	if (EFS.GetOpenName("$app_root$", fn, false, NULL, 0)) 
	{
		Scene->LoadXrAICompilerError(fn.c_str());
	}
	UI->RedrawScene();
	return true;
}

CCommandVar CommandImportCompilerError(CCommandVar p1, CCommandVar p2)
{
	xr_string fn;
	if(EFS.GetOpenName("$logs$", fn, false, NULL, 0)){
		Scene->LoadCompilerError(fn.c_str());
	}
	UI->RedrawScene		();
	return true;
}
CCommandVar CommandExportCompilerError(CCommandVar p1, CCommandVar p2)
{
	xr_string fn;
	if(EFS.GetSaveName("$logs$", fn, NULL, 0)){
		Scene->SaveCompilerError(fn.c_str());
	}
	return true;
}
CCommandVar CommandValidateScene(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->Validate	(true,true,true,true,true,true);
		return 			true;
	} else {
		ELog.DlgMsg		( mtError, "Scene sharing violation" );
		return 			false;
	}
}
CCommandVar CommandCleanLibrary(CCommandVar p1, CCommandVar p2)
{
	if ( !Scene->locked() ){
		Lib.CleanLibrary();
		return 			true;
	}else{
		ELog.DlgMsg		(mtError, "Scene must be empty before refreshing library!");
		return 			false;
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

	return 				true;
}

CCommandVar CommandCut(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->CutSelection(LTools->CurrentClassID());
	   /* fraLeftBar->miPaste->Enabled = true;
		fraLeftBar->miPaste2->Enabled = true;*/
		Scene->UndoSave	();
		return 			true;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
		return 			false;
	}
	return false;
}
CCommandVar CommandCopy(CCommandVar p1, CCommandVar p2)
{
	  if( !Scene->locked() ){
		Scene->CopySelection(LTools->CurrentClassID());
		return 			true;
	} else {
		ELog.DlgMsg		( mtError, "Scene sharing violation" );
		return 			false;
	}
	return false;
}

CCommandVar CommandPaste(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->PasteSelection();
		Scene->UndoSave	();
		return 			true;
	} else {
		ELog.DlgMsg		( mtError, "Scene sharing violation" );
		return  		false;
	}
	return false;
}

CCommandVar CommandDuplicate(CCommandVar p1, CCommandVar p2)
{
    if (!Scene->locked()) {
		Scene->DuplicateSelection(LTools->CurrentClassID());
        Scene->UndoSave();
        return 			true;
    }
    else {
        ELog.DlgMsg(mtError, "Scene sharing violation");
        return 			false;
    }
    return false;
}

CCommandVar CommandLoadSelection(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() )
	{
		xr_string fn			= LTools->m_LastSelectionName;
		if( EFS.GetOpenName(_maps_, fn ) )
		{
			const char* maps_path	= FS.get_path(_maps_)->m_Path;
			if (fn.c_str()==strstr(fn.c_str(),maps_path))
				LTools->m_LastSelectionName = fn.c_str()+xr_strlen(maps_path);
			UI->SetStatus		("Fragment loading...");

			Scene->LoadSelection(fn.c_str());

			UI->ResetStatus		();
			Scene->UndoSave		();
			ExecCommand			(COMMAND_CHANGE_ACTION,etaSelect);
			ExecCommand			(COMMAND_UPDATE_PROPERTIES);
			UI->RedrawScene		();
			return 				true;
		}               	
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
	}
	return false;
}        
CCommandVar CommandSaveSelection(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		xr_string fn			= LTools->m_LastSelectionName;
		if( EFS.GetSaveName		( _maps_, fn ) ){
			const char* maps_path	= FS.get_path(_maps_)->m_Path;
			if (fn.c_str()==strstr(fn.c_str(),maps_path))
				LTools->m_LastSelectionName = fn.c_str()+xr_strlen(maps_path);
			UI->SetStatus		("Fragment saving...");
			Scene->SaveSelection(LTools->CurrentClassID(),fn.c_str());
			UI->ResetStatus		();
			return 				true;
		}
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
	}
	return 						false;
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
			return true;
		}
	}
	else
	{
		ELog.DlgMsg(mtError, "Scene sharing violation");
	}
	return false;
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
			return true;
		}
	}
	else
	{
		ELog.DlgMsg(mtError, "Scene sharing violation");
	}
	return false;
}

CCommandVar CommandClearSceneSummary(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->ClearSummaryInfo	();
		return 					true;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
		return 					false;
	}
}
CCommandVar CommandCollectSceneSummary(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->CollectSummaryInfo();
		return 					true;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
		return 					false;
	}
}
CCommandVar CommandShowSceneSummary(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->ShowSummaryInfo();
		return 					true;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
		return 					false;
	}
}
CCommandVar CommandExportSceneSummary(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->ExportSummaryInfo(xr_string(p1).c_str());
		return 					true;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
		return 					false;
	}
}

CCommandVar CommandSceneHighlightTexture(CCommandVar p1, CCommandVar p2)
{
	if (!Scene->locked())
	{
		UIChooseForm::SelectItem(smTexture, 1);
		return true;
	}
	else
	{
		ELog.DlgMsg(mtError, "Scene sharing violation");
	}

	return false;
}

CCommandVar CommandOptions(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		ExecCommand				(COMMAND_SHOW_PROPERTIES, p1, p2);
		return 					true;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
		return 					false;
	}
}

CCommandVar CommandBuild(CCommandVar p1, CCommandVar p2)
{
	if (!Scene->locked())
	{
		if (mrYes == ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to build level?"))
		{
			LUI->LoaderEvent.wait();

			LUI->LoaderEvent.run
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
	return false;
}

CCommandVar CommandMakeAIMap(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		if (mrYes==ELog.DlgMsg(mtConfirmation, mbYes |mbNo, "Are you sure to export ai-map?"))
			return 				Builder.MakeAIMap(false);
	}else{
		ELog.DlgMsg( mtError, "Scene sharing violation" );
	}
	return 						false;
}
CCommandVar CommandMakeAIMapLegacy(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		if (mrYes==ELog.DlgMsg(mtConfirmation, mbYes |mbNo, "Are you sure to export ai-map?"))
			return 				Builder.MakeAIMap(true);
	}else{
		ELog.DlgMsg( mtError, "Scene sharing violation" );
	}
	return 						false;
}

CCommandVar CommandMakeGame(CCommandVar p1, CCommandVar p2)
{
	if (!Scene->locked())
	{
		if (mrYes == ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to export game?"))
		{
			LUI->LoaderEvent.wait();

			LUI->LoaderEvent.run
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
	return 						false;
}

CCommandVar CommandMakePuddles(CCommandVar p1, CCommandVar p2)
{
	if (!Scene->locked()) 
	{
		if (mrYes == ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to export puddles?"))
		{
			LUI->LoaderEvent.wait();

			LUI->LoaderEvent.run
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
	return false;
}

CCommandVar CommandMakeDetails(CCommandVar p1, CCommandVar p2)
{
	if (!Scene->locked())
	{
		if (mrYes == ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to export details?"))
		{
			LUI->LoaderEvent.wait();

			LUI->LoaderEvent.run
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
	return 						false;
}

CCommandVar CommandMakeHOM(CCommandVar p1, CCommandVar p2)
{
	if (!Scene->locked()) 
	{
		if (mrYes == ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to export HOM?"))
		{
			LUI->LoaderEvent.wait();

			LUI->LoaderEvent.run
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
	return 						false;
}

CCommandVar CommandMakeSOM(CCommandVar p1, CCommandVar p2)
{
	if (!Scene->locked()) 
	{
		if (mrYes == ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to export Sound Occlusion Model?"))
		{
			LUI->LoaderEvent.wait();

			LUI->LoaderEvent.run
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
	return 						false;
}

CCommandVar CommandInvertSelectionAll(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->InvertSelection	(LTools->CurrentClassID());
		return 					true;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
	}
	return 						false;
}

CCommandVar CommandSelectAll(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->SelectObjects	(true,LTools->CurrentClassID());
		return 					true;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
	}
	return 						false;
}

CCommandVar CommandDeselectAll(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->SelectObjects	(false,LTools->CurrentClassID());
		return 					true;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
		return 					false;
	}
}

CCommandVar CommandDeleteSelection(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->RemoveSelection	( LTools->CurrentClassID() );
		Scene->UndoSave			();
		return					true;
	} else {
		ELog.DlgMsg( mtError, "Scene sharing violation" );
		return 					false;
	}
}

CCommandVar CommandHideUnsel(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->ShowObjects		( false, LTools->CurrentClassID(), true, false );
		Scene->UndoSave			();
		ExecCommand				(COMMAND_UPDATE_PROPERTIES);
		return 					true;
	} else {
		ELog.DlgMsg				( mtError, "Scene sharing violation" );
		return 					false;
	}
}
CCommandVar CommandHideSel(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->ShowObjects		( bool(p1), LTools->CurrentClassID(), true, true );
		Scene->UndoSave			();
		ExecCommand				(COMMAND_UPDATE_PROPERTIES);
		return 					true;
	} else {
		ELog.DlgMsg				( mtError, "Scene sharing violation" );
		return 					false;
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

	return true;
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
	return true;
}

CCommandVar CommandHideAll(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->ShowObjects		( bool(p1), LTools->CurrentClassID(), false );
		Scene->UndoSave			();
		ExecCommand				(COMMAND_UPDATE_PROPERTIES);
		return 					true;
	}else{
		ELog.DlgMsg				( mtError, "Scene sharing violation" );
		return 					false;
	}
}
CCommandVar CommandLockAll(CCommandVar p1, CCommandVar p2)
{
    if( !Scene->locked() ){
        Scene->LockObjects		(bool(p1),LTools->CurrentClassID(),false);
        Scene->UndoSave			();
	    return 					true;
    }else{
        ELog.DlgMsg				( mtError, "Scene sharing violation" );
	    return 					false;
    }
}
CCommandVar CommandLockSel(CCommandVar p1, CCommandVar p2)
{
    if( !Scene->locked() ){
        Scene->LockObjects		(bool(p1),LTools->CurrentClassID(),true,true);
        Scene->UndoSave			();
	    return 					true;
    }else{
        ELog.DlgMsg				( mtError, "Scene sharing violation" );
	    return 					false;
    }
}
CCommandVar CommandLockUnsel(CCommandVar p1, CCommandVar p2)
{
    if( !Scene->locked() ){
        Scene->LockObjects		(bool(p1),LTools->CurrentClassID(),true,false);
        Scene->UndoSave			();
        return					true;
   }else{
        ELog.DlgMsg				( mtError, "Scene sharing violation" );                   
	    return 					false;
    }
}
CCommandVar CommandSetSnapObjects(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->SetSnapList		();
		return 					true;
	}else{
		ELog.DlgMsg				( mtError, "Scene sharing violation" );
		return 					false;
	}
}
CCommandVar CommandAddSelSnapObjects(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->AddSelToSnapList	();
		return 					true;
	}else{
		ELog.DlgMsg				( mtError, "Scene sharing violation" );
		return 					false;
	}
}
CCommandVar CommandDelSelSnapObjects(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->DelSelFromSnapList();
		return 					true;
	}else{
		ELog.DlgMsg				( mtError, "Scene sharing violation" );
		return 					false;
	}
}
CCommandVar CommandClearSnapObjects(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->ClearSnapList	(true);
		return 					true;
	}else{
		ELog.DlgMsg				( mtError, "Scene sharing violation" );
		return 					false;
	}
}
CCommandVar CommandSelectSnapObjects(CCommandVar p1, CCommandVar p2)
{
	if( !Scene->locked() ){
		Scene->SelectSnapList	();
		return 					true;
	}else{
		ELog.DlgMsg				( mtError, "Scene sharing violation" );
		return 					false;
	}
}

CCommandVar CommandRefreshSoundEnvGeometry(CCommandVar p1, CCommandVar p2)
{
	LSndLib->RefreshEnvGeometry();
	return 						true;
}
CCommandVar CommandShowContextMenu(CCommandVar p1, CCommandVar p2)
{
	LUI->ShowContextMenu(p1);
	return true;
}

//------
CCommandVar CommandCreateSoundLib(CCommandVar p1, CCommandVar p2)
{
	SndLib						= new CLevelSoundManager();
	LSndLib = (CLevelSoundManager*)SndLib;
	return 						true;
}

extern bool ai_map_shown;
CCommandVar CommandToggleAiMapVisibility(CCommandVar p1, CCommandVar p2)
{
	ai_map_shown 				= !ai_map_shown;
	return 						true;
}

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

	// common
	REGISTER_CMD_S	    (COMMAND_LIBRARY_EDITOR,           	CommandLibraryEditor);
	REGISTER_CMD_S	    (COMMAND_SHARED_MATERIALS_EDITOR,   CommandSharedMaterialsEditor);
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
	REGISTER_CMD_S	    (COMMAND_ICON_LOAD, CommandLoadCustomIcons);
	REGISTER_CMD_S	    (COMMAND_ICON_REMOVE, CommandRemoveCustomIcon);
	REGISTER_CMD_S	    (COMMAND_REFRESH_SOUND_ENV_GEOMETRY,CommandRefreshSoundEnvGeometry);
	REGISTER_CMD_S	    (COMMAND_SHOWCONTEXTMENU,           CommandShowContextMenu);
	REGISTER_CMD_S	    (COMMAND_CREATE_SOUND_LIB,          CommandCreateSoundLib);
	REGISTER_CMD_SE	    (COMMAND_TOGGLE_AIMAP_VISIBILITY,   "Visibility\\Toggle AI-Map",			CommandToggleAiMapVisibility,true);
	REGISTER_CMD_S	    (COMMAND_SHOW_CLIP_EDITOR,			CommandShowClipEditor);
	
}

char* CLevelMain::GetCaption()
{
	return (char*)(Tools->m_LastFileName.empty()?"noname":Tools->m_LastFileName.c_str());
}

bool CLevelMain::ApplyShortCut(u32 Key, TShiftState Shift)
{
	if (Scene->IsPlayInEditor())
	{
		return true;
	}

	return inherited::ApplyShortCut(Key, Shift);
}

void RetrieveSceneObjPointAndNormal(Fvector& hitpoint, Fvector* hitnormal, const SRayPickInfo& pinf, int bSnap)
{
	if (pinf.e_mesh == 0)
	{
		hitpoint = pinf.pt;
		if (hitnormal && pinf.visual_inf.K)
		{
			*hitnormal = pinf.visual_inf.normal;
		}

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
		{
			pn.set(verts[0]);
		}
		else if ((u > w) && (u > v))
		{
			pn.set(verts[1]);
		}
		else
		{
			pn.set(verts[2]);
		}

		if (pn.distance_to(pinf.pt) < LTools->m_MoveSnap)
		{
			hitpoint.set(pn);
		}
		else
		{
			hitpoint.set(pinf.pt);
		}
	}
	else
	{
		hitpoint.set(pinf.pt);
	}

	if (hitnormal)
	{
		Fvector verts[3];
		pinf.e_obj->GetFaceWorld(pinf.s_obj->_Transform(), pinf.e_mesh, pinf.inf.id, verts);
		hitnormal->mknormal(verts[0], verts[1], verts[2]);
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

bool PickObjectGeometry(EEditorState est, Fvector& hitpoint, const Fvector& start, const Fvector& direction, int bSnap, Fvector* hitnormal)
{
	switch (est)
	{
		case esEditLibrary: return EditLibPickObjectGeometry(hitpoint, start, direction, bSnap, hitnormal);
		case esEditScene:   return ScenePickObjectGeometry(hitpoint, start, direction, bSnap, hitnormal);
		default:            NODEFAULT;
	}

	return false;
}

bool PickGrid(Fvector& hitpoint, const Fvector& start, const Fvector& direction, int bSnap, Fvector* hitnormal)
{
	// pick grid
	Fvector normal;
	normal.set(0, 1, 0);
	float clcheck = direction.dotproduct(normal);

	if (fis_zero(clcheck))
	{
		return false;
	}

	float alpha = -start.dotproduct(normal) / clcheck;

	if (alpha <= 0)
	{
		return false;
	}

	hitpoint.x = start.x + direction.x * alpha;
	hitpoint.y = start.y + direction.y * alpha;
	hitpoint.z = start.z + direction.z * alpha;

	if (std::abs(hitpoint.x) < EPS_L)
	{
		hitpoint.x = 0.0f;
	}

	if (std::abs(hitpoint.y) < EPS_L)
	{
		hitpoint.y = 0.0f;
	}

	if (std::abs(hitpoint.z) < EPS_L)
	{
		hitpoint.z = 0.0f;
	}

	if (Tools->GetSettings(etfGSnap) && bSnap)
	{
		hitpoint.x = snapto(hitpoint.x, LTools->m_MoveSnap);
		hitpoint.z = snapto(hitpoint.z, LTools->m_MoveSnap);
		hitpoint.y = 0.f;
	}

	if (hitnormal)
	{
		hitnormal->set(0, 1, 0);
	}
	return true;
}

bool CLevelMain::PickGround(Fvector& hitpoint, const Fvector& start, const Fvector& direction, int bSnap, Fvector* hitnormal)
{
	VERIFY(m_bReady);

	EEditorState est = GetEState();
	if (est != esEditLibrary && est != esEditScene)
	{
		return false;
	}

	// pick object geometry
	if ((bSnap == -1) || (Tools->GetSettings(etfOSnap) && (bSnap == 1)))
	{
		bool b = PickObjectGeometry(est, hitpoint, start, direction, bSnap, hitnormal);
		if (b)
		{
			return true;
		}
	}

	return PickGrid(hitpoint, start, direction, bSnap, hitnormal);
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

	pt[0].set(std::min(x1,x2),std::min(y1,y2));
	pt[1].set(std::max(x1,x2),std::min(y1,y2));
	pt[2].set(std::max(x1,x2),std::max(y1,y2));
	pt[3].set(std::min(x1,x2),std::max(y1,y2));

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
}

void CLevelMain::ResetStatus()
{
	UI->ProgressStatusName.clear();
}

void CLevelMain::SetStatus(const char* s, bool bOutLog)
{
	VERIFY(m_bReady);

	if (bOutLog && s && s[0])
	{
		ELog.Msg(mtInformation, s);
	}

	UI->ProgressStatusName = s;
}

void CLevelMain::RealQuit()
{
	//frmMain->Close();
}

void CLevelMain::ProgressDraw()
{
	inherited::ProgressDraw();
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
	UIGitWindow::Update();
	if (LTools->GetToolForm())
	{
		LTools->GetToolForm()->OnDrawUI();
	}
}

bool CLevelMain::IsPlayInEditor()
{
	return Scene->IsPlayInEditor();
}