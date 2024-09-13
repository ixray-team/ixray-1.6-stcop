#include "stdafx.h"



SceneBuilder Builder;


ICF static void simple_hemi_callback(float x, float y, float z, float E, LPVOID P)
{
    SceneBuilder::BLVec* dst 	= (SceneBuilder::BLVec*)P;
    SceneBuilder::SBuildLight 	T;
    T.energy     				= E;
    T.light.direction.set		(x,y,z);
    dst->push_back  			(T);
}

SceneBuilder::SceneBuilder()
{
    m_iDefaultSectorNum = 0;
    l_scene_stat		= 0;
    l_verts				= 0;
    l_faces				= 0;
    l_smgroups			= 0;
    l_vert_cnt			= 0;
    l_face_cnt 			= 0;
	l_vert_it	 		= 0;
	l_face_it			= 0;
    object_for_render	= 0;
	m_save_as_object	= false;
}

SceneBuilder::~SceneBuilder()
{
}


#define CHECK_BREAK     	if (UI->NeedAbort()) break;
#define VERIFY_COMPILE(x,c1,c2) CHECK_BREAK \
							if (!x){ELog.Msg(mtError, g_pStringTable->translate("ed_st_le_error").c_str(), c1,c2); break;}

BOOL SceneBuilder::Compile(bool b_selected_only, bool show_message )
{
	if(m_save_as_object)
	{
		EvictResource	();
        GetBounding		();
        CompileStatic	(b_selected_only);
        EvictResource	();
		return TRUE;
	}

	xr_string error_text		= "";
	UI->ResetBreak				();
	if(UI->ContainEState(esBuildLevel)) return false;
	ELog.Msg( mtInformation, g_pStringTable->translate("ed_st_build_start").c_str() );

    UI->BeginEState(esBuildLevel);
    try{
        do{
	        // check debug
            bool bTestPortal = Scene->ObjCount(OBJCLASS_SECTOR)||Scene->ObjCount(OBJCLASS_PORTAL);
	        // validate scene
    	    VERIFY_COMPILE(Scene->Validate(false,bTestPortal,true,true,true,true),g_pStringTable->translate("ed_st_validation_failed").c_str(), g_pStringTable->translate("ed_st_invalid_scene").c_str());
			// fill simple hemi
            simple_hemi.clear	();
	        xrHemisphereBuild	(1,2.f,simple_hemi_callback,&simple_hemi);
        	// build
            VERIFY_COMPILE		(PreparePath(),				g_pStringTable->translate("ed_st_level_path_failed").c_str(), "");
            VERIFY_COMPILE		(PrepareFolders(),			g_pStringTable->translate("ed_st_prepare_folders_failed").c_str(), "");
            VERIFY_COMPILE		(EvictResource(),	  		g_pStringTable->translate("ed_st_resource_evict_failed").c_str(), "");
            VERIFY_COMPILE		(GetBounding(),				g_pStringTable->translate("ed_st_level_bound_failed").c_str(), "");
            VERIFY_COMPILE		(RenumerateSectors(), 		g_pStringTable->translate("ed_st_renum_sectors_failed").c_str(), "");
            VERIFY_COMPILE		(CompileStatic(false),	  	g_pStringTable->translate("ed_st_static_failed").c_str(), "");
            VERIFY_COMPILE		(EvictResource(),	  		g_pStringTable->translate("ed_st_resource_evict_failed").c_str(), "");
            VERIFY_COMPILE		(BuildLTX(),		  		g_pStringTable->translate("ed_st_ltx_failed").c_str(), "");
            VERIFY_COMPILE		(BuildGame(),		  		g_pStringTable->translate("ed_st_game_failed").c_str(), "");
            VERIFY_COMPILE		(BuildSceneStat(),			g_pStringTable->translate("ed_st_scene_stats_failed").c_str(), "");
            BuildHOMModel		();
            BuildSOMModel		();
    	    // build tools
            SceneToolsMapPairIt _I 	= Scene->FirstTool();
            SceneToolsMapPairIt _E	= Scene->LastTool();
            for (; _I!=_E; ++_I)
            {
            	if (_I->first!=OBJCLASS_DUMMY)
                {
                    if (_I->second->Valid())
                    {
                        VERIFY_COMPILE(_I->second->Export(m_LevelPath),_I->second->ClassDesc(),g_pStringTable->translate("ed_st_export_failed").c_str());
                        ELog.Msg(mtInformation,g_pStringTable->translate("ed_st_process_done").c_str(), _I->second->ClassDesc());
                    }else
                    {
                        ELog.Msg(mtError,g_pStringTable->translate("ed_st_process_failed").c_str(), _I->second->ClassDesc());
                    }
                }
            }
		    Clear			();
        } while(0);

        if (!error_text.empty()) 	ELog.DlgMsg(mtError,error_text.c_str());
        else if (UI->NeedAbort())	ELog.DlgMsg(mtInformation,g_pStringTable->translate("ed_st_build_terminated").c_str());
        else					if(show_message)	ELog.DlgMsg(mtInformation,g_pStringTable->translate("ed_st_build_success").c_str());
    }catch(...){
    	ELog.DlgMsg(mtError,g_pStringTable->translate("ed_st_build_error").c_str());
        UI->EndEState();
        return false;
    }
    UI->EndEState();

	return error_text.empty();
}


BOOL SceneBuilder::MakeGame( )
{
	xr_string error_text="";
	UI->ResetBreak();
	if(UI->ContainEState(esBuildLevel)) return false;
	ELog.Msg( mtInformation, g_pStringTable->translate("ed_st_making_started").c_str() );

    UI->BeginEState(esBuildLevel);
    try{
        do{
	        // clear error
            Tools->ClearDebugDraw();
	        // validate scene
    	    VERIFY_COMPILE(Scene->Validate(false,false,false,false,false,false), g_pStringTable->translate("ed_st_validation_failed").c_str(), g_pStringTable->translate("ed_st_invalid_scene").c_str());
        	// build
            VERIFY_COMPILE(PreparePath(),				g_pStringTable->translate("ed_st_level_path_failed").c_str(),"");
            VERIFY_COMPILE(GetBounding(),				g_pStringTable->translate("ed_st_level_bound_failed").c_str(),"");
            VERIFY_COMPILE(RenumerateSectors(), 		g_pStringTable->translate("ed_st_renum_sectors_failed").c_str(),"");
            VERIFY_COMPILE(BuildLTX(),					g_pStringTable->translate("ed_st_ltx_failed").c_str(),"");
            VERIFY_COMPILE(BuildGame(),					g_pStringTable->translate("ed_st_game_failed").c_str(),"");
        } while(0);

        if (!error_text.empty()) 	ELog.DlgMsg(mtError,error_text.c_str());
        else if (UI->NeedAbort())	ELog.DlgMsg(mtInformation,g_pStringTable->translate("ed_st_making_terminated").c_str());
        else						ELog.DlgMsg(mtInformation,g_pStringTable->translate("ed_st_making_finished").c_str());
    }catch(...){
    	ELog.DlgMsg(mtError, g_pStringTable->translate("ed_st_build_error").c_str());
        abort();
    }
    UI->EndEState();

	return error_text.empty();
}

BOOL SceneBuilder::MakePuddles()
{
    ELog.Msg(mtInformation, g_pStringTable->translate("ed_st_making_started").c_str());

    ESceneCustomOTool* ToolPtr = (ESceneCustomOTool*)Scene->GetTool(OBJCLASS_PUDDLES);
    auto& ObjectList = ToolPtr->GetObjects();

    if (ObjectList.empty())
    {
        ELog.DlgMsg(mtError, g_pStringTable->translate("ed_st_no_puddles").c_str());
        return false;
    }

    PreparePath();

    xr_string ltx_filename = MakeLevelPath("level.puddles");

    if (FS.exist(ltx_filename.c_str()))
        EFS.MarkFile(ltx_filename.c_str(), true);

    // -- defaults --           
    IWriter* F = FS.w_open(ltx_filename.c_str());

    for (CCustomObject* Object : ObjectList)
    {
        string128 buff;
        sprintf(buff, "[%s]", Object->FName.c_str());
        F->w_string(buff);

        RtlZeroMemory(buff, sizeof(buff));

        sprintf(buff, "position= %0.3f, %0.3f, %0.3f", Object->FPosition.x, Object->FPosition.y, Object->FPosition.z);
        F->w_string(buff);
        F->w_string("rotation = 0");
        RtlZeroMemory(buff, sizeof(buff));

        sprintf(buff, "max_height = %0.3f", Object->FScale.y);
        F->w_string(buff);
        RtlZeroMemory(buff, sizeof(buff));

        sprintf(buff, "size_xz = %0.3f, %0.3f", Object->FScale.x, Object->FScale.z);
        F->w_string(buff);
    }

    FS.w_close(F);

    return true;
}


BOOL SceneBuilder::MakeAIMap()
{
	xr_string error_text;
    do{
		VERIFY_COMPILE(PreparePath(),				g_pStringTable->translate("ed_st_level_path_failed").c_str(),"");
		VERIFY_COMPILE(BuildAIMap(),				g_pStringTable->translate("ed_st_ai_map_failed").c_str(), "");
    }while(0);
    if (!error_text.empty()) 	ELog.DlgMsg(mtError,error_text.c_str());
    else if (UI->NeedAbort())	ELog.DlgMsg(mtInformation,g_pStringTable->translate("ed_st_build_terminated").c_str());
    else						ELog.DlgMsg(mtInformation,g_pStringTable->translate("ed_st_build_success").c_str());

	return error_text.empty();
}


BOOL SceneBuilder::MakeDetails()
{
	xr_string error_text;
    do{
		VERIFY_COMPILE(PreparePath(), g_pStringTable->translate("ed_st_level_path_failed").c_str(),"");
        // save details
		VERIFY_COMPILE(Scene->GetTool(OBJCLASS_DO)->Export(m_LevelPath), g_pStringTable->translate("ed_st_export_failed").c_str(),"");
    }while(0);
    if (!error_text.empty()) 	ELog.DlgMsg(mtError,error_text.c_str());
    else if (UI->NeedAbort())	ELog.DlgMsg(mtInformation,g_pStringTable->translate("ed_st_build_terminated").c_str());
    else						ELog.DlgMsg(mtInformation,g_pStringTable->translate("ed_st_build_success").c_str());

	return error_text.empty();
}


BOOL SceneBuilder::MakeHOM( )
{
	xr_string error_text="";
	UI->ResetBreak();
	if(UI->ContainEState(esBuildLevel)) return false;
	ELog.Msg( mtInformation, g_pStringTable->translate("ed_st_making_started").c_str());

    UI->BeginEState(esBuildLevel);
    try{
        do{
        	// build
            VERIFY_COMPILE(PreparePath(),				g_pStringTable->translate("ed_st_level_path_failed").c_str(),"");
            VERIFY_COMPILE(BuildHOMModel(),				g_pStringTable->translate("ed_st_hom_failed").c_str(), "");
        } while(0);

        if (!error_text.empty()) 	ELog.DlgMsg(mtError,error_text.c_str());
        else if (UI->NeedAbort())	ELog.DlgMsg(mtInformation,g_pStringTable->translate("ed_st_build_terminated").c_str());
        else						ELog.DlgMsg(mtInformation,g_pStringTable->translate("ed_st_build_success").c_str());
    }catch(...){
    	ELog.DlgMsg(mtError, g_pStringTable->translate("ed_st_build_error").c_str());
        abort();
    }
    UI->EndEState();

	return error_text.empty();
}


BOOL SceneBuilder::MakeSOM( )
{
	xr_string error_text="";
	UI->ResetBreak();
	if(UI->ContainEState(esBuildLevel)) return false;
	ELog.Msg( mtInformation, g_pStringTable->translate("ed_st_making_started").c_str());

    UI->BeginEState(esBuildLevel);
    try{
        do{
        	// build
            VERIFY_COMPILE(PreparePath(),				g_pStringTable->translate("ed_st_level_path_failed").c_str(),"");
            VERIFY_COMPILE(BuildSOMModel(),				g_pStringTable->translate("ed_st_som_failed").c_str(), "");
        } while(0);

        if (!error_text.empty()) 	ELog.DlgMsg(mtError,error_text.c_str());
        else if (UI->NeedAbort())	ELog.DlgMsg(mtInformation,g_pStringTable->translate("ed_st_build_terminated").c_str());
        else						ELog.DlgMsg(mtInformation,g_pStringTable->translate("ed_st_build_success").c_str());
    }catch(...){
    	ELog.DlgMsg(mtError, g_pStringTable->translate("ed_st_build_error").c_str());
        abort();
    }
    UI->EndEState();

	return error_text.empty();
}

#include "../XrECore/Editor/EditObject.h"
void SceneBuilder::OnRender()
{
	if (object_for_render){
        object_for_render->OnFrame();
        object_for_render->RenderSingle(Fidentity);
    }
}

