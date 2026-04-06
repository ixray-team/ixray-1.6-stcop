#include "stdafx.h"
#include "LevelPreferences.h"
#include "ContentView.h"

#include "../xrECore/Editor/UILogForm.h"

template<typename T>
T GetSafe(const nlohmann::json& j, const char* key, T& Out)
{
	if (!j.is_object())
	{
		return Out;
	}

	auto it = j.find(key);
	if (it == j.end() || it->is_null())
	{
		return Out;
	}

	try
	{
		if constexpr (std::is_same_v<T, xr_string>)
		{
			Out = it->get<std::string>().c_str();
		}
		else if constexpr (std::is_same_v<T, shared_str>)
		{
			Out = it->get<std::string>().c_str();
		}
		else if constexpr (std::is_same_v<std::remove_cvref_t<T>, char*>)
		{
			auto OutTemp = it->get<std::string>();
			xr_strcpy(Out, OutTemp.c_str());
		}
		else
		{
			Out = it->get<T>();
		}
	}
	catch (...)
	{
	}

	return Out;
}

const nlohmann::json* GetObjectSafe(const nlohmann::json& j, const char* key)
{
	if (!j.is_object())
	{
		return nullptr;
	}

	auto it = j.find(key);
	if (it == j.end() || !it->is_object())
	{
		return nullptr;
	}

	return &(*it);
}

void CLevelPreferences::Load()
{
	if (JSONData.is_discarded() || JSONData.is_null())
	{
		Msg("Invalid JSON data");
		return;
	}

	inherited::Load();

	if (const auto* windows = GetObjectSafe(JSONData, "windows"))
	{
		GetSafe(*windows, "object_list", OpenObjectList);
		GetSafe(*windows, "properties", OpenProperties);
		GetSafe(*windows, "world_properties", OpenWorldProperties);
		GetSafe(*windows, "snap_list", OpenSnapList);
		GetSafe(*windows, "light_anim", OpenLightAnim);

		GetSafe(*windows, "log_clear_in_pie", UILogForm::bClearInPIE);
	}

	if (const auto* comp = GetObjectSafe(JSONData, "Compilers Path"))
	{
		GetSafe(*comp, "xrLC", Compiler_xrLC);
		GetSafe(*comp, "xrAI", Compiler_xrAI);
		GetSafe(*comp, "xrDO", Compiler_xrDO);
	}

	if (const auto* cb = GetObjectSafe(JSONData, "ContentBrowser"))
	{
		GetSafe(*cb, "CurPath", GContentView->CurrentDir);

		if (!std::filesystem::exists(xr_path(GContentView->CurrentDir)))
		{
			GContentView->CurrentDir = GContentView->RootDir;
		}

		GetSafe(*cb, "ViewMode", GContentView->ViewMode);
		GetSafe(*cb, "ISEPath", GContentView->VirtualPath);
		GetSafe(*cb, "IsSpawnElement", GContentView->IsSpawnElement);

		if (GContentView->IsSpawnElement)
		{
			GContentView->RescanISEDirectory(GContentView->VirtualPath);
		}
	}

	if (const auto* pie = GetObjectSafe(JSONData, "PIE"))
	{
		GetSafe(*pie, "ArtPos", PIEArtSpawnPos);
	}

	if (const auto* gizmo = GetObjectSafe(JSONData, "gizmo"))
	{
		GetSafe(*gizmo, "matrixmode", imManipulator.MatrixMode);
	}

	if (const auto* targets = GetObjectSafe(JSONData, "targets"))
	{
		for (auto it = Scene->FirstTool(); it != Scene->LastTool(); ++it)
		{
			if (!it->second || it->first == OBJCLASS_DUMMY)
			{
				continue;
			}

			u32 flags;
			if (GetSafe(*targets, it->second->ClassName(), flags))
			{
				it->second->m_EditFlags.flags = flags;
			}
		}
	}

	if (const auto* scene = GetObjectSafe(JSONData, "Scene"))
	{
		GetSafe(*scene, "ValidNames", Scene->IsValidateDublicateNames);
		GetSafe(*scene, "ValidLod", Scene->IsValidateLODs);
		GetSafe(*scene, "ValidMake", Scene->IsValidateAtMake);
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
	JSONData["ContentBrowser"]["ISEPath"] = GContentView->VirtualPath;
	JSONData["ContentBrowser"]["IsSpawnElement"] = GContentView->IsSpawnElement;
	JSONData["ContentBrowser"]["ViewMode"] = GContentView->ViewMode;
	JSONData["Scene"]["ValidMake"] = Scene->IsValidateAtMake;
	JSONData["Scene"]["ValidLod"] = Scene->IsValidateLODs;
	JSONData["Scene"]["ValidNames"] = Scene->IsValidateDublicateNames;

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
		if (_I->second&&(_I->first!=OBJCLASS_DUMMY))
		{
			if (_I->second->AllowEnabling())
			{
				PropValue* V 	= PHelper().CreateFlag32(items, PrepareKey("Tools\\Targets\\Enable", _I->second->ClassDesc()), &_I->second->m_EditFlags, ESceneToolBase::flEnable);
				V->tag			= _I->second->FClassID;
				V->OnChangeEvent.bind(this, &CLevelPreferences::OnEnabledChange);
			}
			PropValue* V		= PHelper().CreateFlag32(items, PrepareKey("Tools\\Targets\\Read Only", _I->second->ClassDesc()), &_I->second->m_EditFlags, ESceneToolBase::flForceReadonly);
			V->tag				= _I->second->FClassID;
			V->OnChangeEvent.bind(this, &CLevelPreferences::OnReadonlyChange);
		}
}

