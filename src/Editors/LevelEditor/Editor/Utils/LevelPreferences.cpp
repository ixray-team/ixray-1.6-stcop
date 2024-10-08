#include "stdafx.h"
#include "LevelPreferences.h"

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

    if (JSONData["windows"].contains("snap_list"))
    {
        OpenSnapList = JSONData["windows"]["snap_list"];
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

    JSONData["windows"]["object_list"] = OpenObjectList;
    JSONData["windows"]["properties"] = OpenProperties;
    JSONData["windows"]["world_properties"] = OpenWorldProperties;
    JSONData["windows"]["snap_list"] = OpenSnapList;
    JSONData["Compilers Path"]["xrLC"] = Compiler_xrLC.c_str();
    JSONData["Compilers Path"]["xrAI"] = Compiler_xrAI.c_str();
    JSONData["Compilers Path"]["xrDO"] = Compiler_xrDO.c_str();

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

