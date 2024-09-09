#include "stdafx.h"
#include "../Tools/Puddles/EScenePuddlesTools.h"

void EScene::RegisterSceneTools(ESceneToolBase* mt)
{
    m_SceneTools[mt->FClassID]= mt;
    mt->OnCreate			();
}

void EScene::CreateSceneTools()
{
    RegisterSceneTools	   	(new ESceneDummyTool	()	); //+
    RegisterSceneTools	   	(new ESceneObjectTool	() 	); //+

    RegisterSceneTools	   	(new ESceneLightTool	()	); //+
    RegisterSceneTools	   	(new ESceneSoundSrcTool()	); //+
    RegisterSceneTools	   	(new ESceneSoundEnvTool()	); //+
	RegisterSceneTools	   	(new ESceneGroupTool	()	); //+
    RegisterSceneTools	   	(new ESceneShapeTool	()	); //+
    RegisterSceneTools	   	(new ESceneGlowTool		()	); //+
    RegisterSceneTools	   	(new ESceneSpawnTool	()	); //+
    RegisterSceneTools	   	(new ESceneWayTool		()	); //+
    RegisterSceneTools	   	(new ESceneSectorTool	()	); //+
    RegisterSceneTools	   	(new EScenePortalTool	()	); //+
    RegisterSceneTools	   	(new EScenePuddlesTool  ()	); //+
    RegisterSceneTools	   	(new EScenePSTool		()	); //+
    RegisterSceneTools	   	(new EDetailManager		()	); //+
    RegisterSceneTools	   	(new ESceneAIMapTool	()	); //+
    RegisterSceneTools		(new ESceneWallmarkTool	()	); //+
    RegisterSceneTools		(new ESceneFogVolumeTool()	); //+
}

void EScene::DestroySceneTools()
{
    SceneToolsMapPairIt _I = m_SceneTools.begin();
    SceneToolsMapPairIt _E = m_SceneTools.end();
    for (; _I!=_E; _I++)
    {
    	if(_I->second)
        	_I->second->OnDestroy();

    	xr_delete(_I->second);
    }
    m_SceneTools.clear		();
}

