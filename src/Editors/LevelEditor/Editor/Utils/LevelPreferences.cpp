#include "stdafx.h"
#include "LevelPreferences.h"
#include "ContentView.h"

#include "../xrECore/Editor/UILogForm.h"

void CLevelPreferences::Load()
{
	inherited::Load		();

	OpenObjectList = JSONData["windows"]["object_list"];
	OpenProperties = JSONData["windows"]["properties"];
	OpenWorldProperties = JSONData["windows"]["world_properties"];
	
	if (JSONData.contains("Compilers Path") && JSONData["Compilers Path"].contains("xrLC"))
	{
		Compiler_xrLC = ((std::string)JSONData["Compilers Path"]["xrLC"]).c_str();
	}

	if (JSONData.contains("Compilers Path") && JSONData["Compilers Path"].contains("xrAI"))
	{
		Compiler_xrAI = ((std::string)JSONData["Compilers Path"]["xrAI"]).c_str();
	}

	if (JSONData.contains("Compilers Path") && JSONData["Compilers Path"].contains("xrDO"))
	{
		Compiler_xrDO = ((std::string)JSONData["Compilers Path"]["xrDO"]).c_str();
	}

	if (JSONData.contains("PIE") && JSONData["PIE"].contains("ArtPos"))
	{
		PIEArtSpawnPos = JSONData["PIE"]["ArtPos"];
	}

	if (JSONData.contains("LibaryEditor") && JSONData["LibaryEditor"].contains("Preview"))
	{
		PreviewRenderLibrary = JSONData["LibaryEditor"]["Preview"];
	}

	if (JSONData["windows"].contains("snap_list"))
	{
		OpenSnapList = JSONData["windows"]["snap_list"];
	}

	if (JSONData["windows"].contains("log_clear_in_pie"))
	{
		UILogForm::bClearInPIE = JSONData["windows"]["log_clear_in_pie"];
	}

	if (JSONData["windows"].contains("light_anim"))
	{
		OpenLightAnim = JSONData["windows"]["light_anim"];
	}

	if (JSONData["ContentBrowser"].contains("CurPath"))
	{
		GContentView->CurrentDir = JSONData["ContentBrowser"]["CurPath"];
	}

	if (JSONData["ContentBrowser"].contains("ViewMode"))
	{
		GContentView->ViewMode = JSONData["ContentBrowser"]["ViewMode"];
	}

	if (JSONData["ContentBrowser"].contains("ISEPath"))
	{
		GContentView->ISEPath = JSONData["ContentBrowser"]["ISEPath"];
		if (GContentView->IsSpawnElement)
		{
			GContentView->RescanISEDirectory(GContentView->ISEPath);
		}
	}
	

	if (JSONData.contains("gizmo") && JSONData["gizmo"].contains("matrixmode"))
	{
		imManipulator.MatrixMode = JSONData["gizmo"]["matrixmode"];
	}

	if (JSONData["ContentBrowser"].contains("IsSpawnElement"))
	{
		GContentView->IsSpawnElement = JSONData["ContentBrowser"]["IsSpawnElement"];
	}

	SceneToolsMapPairIt _I 	= Scene->FirstTool();
	SceneToolsMapPairIt _E 	= Scene->LastTool();
	for (; _I != _E; _I++)
	{
		if (_I->second && (_I->first != OBJCLASS_DUMMY) && JSONData["targets"].contains(_I->second->ClassName()))
		{
			_I->second->m_EditFlags.flags = JSONData["targets"][_I->second->ClassName()];
		}
	}
}

void CLevelPreferences::Save()
{
	inherited::Save		();

	JSONData["gizmo"]["matrixmode"] = imManipulator.MatrixMode;
	JSONData["windows"]["object_list"] = OpenObjectList;
	JSONData["windows"]["properties"] = OpenProperties;
	JSONData["windows"]["world_properties"] = OpenWorldProperties;
	JSONData["windows"]["snap_list"] = OpenSnapList;
	JSONData["windows"]["log_clear_in_pie"] = UILogForm::bClearInPIE;
	JSONData["windows"]["light_anim"] = OpenLightAnim;
	
	JSONData["LibaryEditor"]["Preview"] = PreviewRenderLibrary;
	JSONData["PIE"]["ArtPos"] = PIEArtSpawnPos;

	JSONData["Compilers Path"]["xrLC"] = Compiler_xrLC.c_str();
	JSONData["Compilers Path"]["xrAI"] = Compiler_xrAI.c_str();
	JSONData["Compilers Path"]["xrDO"] = Compiler_xrDO.c_str();

	JSONData["ContentBrowser"]["CurPath"] = GContentView->CurrentDir;
	JSONData["ContentBrowser"]["ISEPath"] = GContentView->ISEPath;
	JSONData["ContentBrowser"]["IsSpawnElement"] = GContentView->IsSpawnElement;
	JSONData["ContentBrowser"]["ViewMode"] = GContentView->ViewMode;

	SceneToolsMapPairIt _I 	= Scene->FirstTool();
	SceneToolsMapPairIt _E 	= Scene->LastTool();
	for (; _I != _E; _I++)
		if (_I->second && (_I->first != OBJCLASS_DUMMY))
			JSONData["targets"][_I->second->ClassName()] = _I->second->m_EditFlags.flags;
}

void CLevelPreferences::OnEnabledChange(PropValue* prop)
{
	ESceneToolBase* M		= Scene->GetTool(prop->tag); VERIFY(M);
	ExecCommand				(COMMAND_ENABLE_TARGET,prop->tag,M->IsEnabled());
}

void CLevelPreferences::OnReadonlyChange(PropValue* prop)
{
	ESceneToolBase* M		= Scene->GetTool(prop->tag); VERIFY(M);
	ExecCommand				(COMMAND_READONLY_TARGET,prop->tag,M->IsForceReadonly());
}

void CLevelPreferences::FillProp(PropItemVec& items)
{
	inherited::FillProp	(items);
	SceneToolsMapPairIt _I 	= Scene->FirstTool();
	SceneToolsMapPairIt _E 	= Scene->LastTool();
	for (; _I!=_E; _I++)
		if (_I->second&&(_I->first!=OBJCLASS_DUMMY)){
			if (_I->second->AllowEnabling()){
				PropValue* V 	= PHelper().CreateFlag32(items,PrepareKey("Scene\\Targets\\Enable",_I->second->ClassDesc()),	&_I->second->m_EditFlags, ESceneToolBase::flEnable);
				V->tag			= _I->second->FClassID;
				V->OnChangeEvent.bind(this,&CLevelPreferences::OnEnabledChange);
			}
			PropValue* V		= PHelper().CreateFlag32(items,PrepareKey("Scene\\Targets\\Read Only",_I->second->ClassDesc()),	&_I->second->m_EditFlags, ESceneToolBase::flForceReadonly);
			V->tag				= _I->second->FClassID;
			V->OnChangeEvent.bind(this,&CLevelPreferences::OnReadonlyChange);
		}
}

