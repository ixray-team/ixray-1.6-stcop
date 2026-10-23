#include "stdafx.h"
#include "LevelPreferences.h"
#include "ContentView.h"
#include "../../ParticleEditor/UIParticleEditorForm.h"

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

void CLevelPreferences::LoadContentView(const nlohmann::json& Data, CContentView* View)
{
	GetSafe(Data, "CurPath", View->CurrentDir);

	if (!std::filesystem::exists(xr_path(View->CurrentDir)))
	{
		View->CurrentDir = View->RootDir;
	}

	GetSafe(Data, "ViewMode", View->ViewMode);
	GetSafe(Data, "ISEPath", View->VirtualPath);
	GetSafe(Data, "IsSpawnElement", View->IsSpawnElement);
	GetSafe(Data, "IsParticles", View->IsParticles);

	if (View->IsSpawnElement)
	{
		View->RescanISEDirectory(View->VirtualPath);
	}
	else if (View->IsParticles)
	{
		// The particles folder is virtual: its listing comes from the particle library
		View->RescanParticlesDirectory(View->VirtualPath);
	}
}

void CLevelPreferences::SaveContentView(nlohmann::json& Data, const CContentView* View) const
{
	Data["CurPath"] = View->CurrentDir;
	Data["ISEPath"] = View->VirtualPath;
	Data["IsSpawnElement"] = View->IsSpawnElement;
	Data["IsParticles"] = View->IsParticles;
	Data["ViewMode"] = View->ViewMode;
}

void CLevelPreferences::LoadParticleBrowser(CContentView* View)
{
	if (ParticleBrowserSettings.empty())
	{
		return;
	}

	const nlohmann::json Data = nlohmann::json::parse(ParticleBrowserSettings.c_str(), nullptr, false);
	if (Data.is_object())
	{
		LoadContentView(Data, View);
	}
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
		GetSafe(*windows, "OpenLighting", OpenLighting);
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

	if (const auto* comp = GetObjectSafe(JSONData, "Compilers"))
	{
		GetSafe(*comp, "LODsForAllMU", LODsForAllMU);
		GetSafe(*comp, "DuplicateObjectPositionAsError", DuplicateObjectPositionAsError);
	}

	if (const auto* lod = GetObjectSafe(JSONData, "ExperimentalLOD"))
	{
		GetSafe(*lod, "UseMULODs", UseMULODs);
		GetSafe(*lod, "DisableBillboardLOD", DisableBillboardLOD);
	}

	if (const auto* cb = GetObjectSafe(JSONData, "ContentBrowser"))
	{
		LoadContentView(*cb, GContentView);
	}

	// The Particles tab browser is created after the preferences are loaded
	ParticleBrowserSettings.clear();
	if (const auto* pcb = GetObjectSafe(JSONData, "ParticleContentBrowser"))
	{
		ParticleBrowserSettings = pcb->dump().c_str();
	}
	if (CContentView* ParticleBrowser = UIParticleEditorForm::GetContentView())
	{
		LoadParticleBrowser(ParticleBrowser);
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
		GetSafe(*scene, "AIMapDefaultLegacy", Scene->IsAIMapDefaultLegacy);
	}
}

void CLevelPreferences::Save()
{
	inherited::Save		();

	JSONData["gizmo"]["matrixmode"] = imManipulator.MatrixMode;
	JSONData["windows"]["object_list"] = OpenObjectList;
	JSONData["windows"]["properties"] = OpenProperties;
	JSONData["windows"]["world_properties"] = OpenWorldProperties;
	JSONData["windows"]["OpenLighting"] = OpenLighting;
	JSONData["windows"]["snap_list"] = OpenSnapList;
	JSONData["windows"]["log_clear_in_pie"] = UILogForm::bClearInPIE;
	JSONData["windows"]["light_anim"] = OpenLightAnim;
	
	JSONData["LibaryEditor"]["Preview"] = PreviewRenderLibrary;
	JSONData["PIE"]["ArtPos"] = PIEArtSpawnPos;

	JSONData["Compilers Path"]["xrLC"] = Compiler_xrLC.c_str();
	JSONData["Compilers Path"]["xrAI"] = Compiler_xrAI.c_str();
	JSONData["Compilers Path"]["xrDO"] = Compiler_xrDO.c_str();
	JSONData["Compilers"]["LODsForAllMU"] = LODsForAllMU;
	JSONData["Compilers"]["DuplicateObjectPositionAsError"] = DuplicateObjectPositionAsError;

	JSONData["ExperimentalLOD"]["UseMULODs"] = UseMULODs;
	JSONData["ExperimentalLOD"]["DisableBillboardLOD"] = DisableBillboardLOD;

	SaveContentView(JSONData["ContentBrowser"], GContentView);
	if (CContentView* ParticleBrowser = UIParticleEditorForm::GetContentView())
	{
		SaveContentView(JSONData["ParticleContentBrowser"], ParticleBrowser);
	}
	else if (!ParticleBrowserSettings.empty())
	{
		JSONData["ParticleContentBrowser"] = nlohmann::json::parse(ParticleBrowserSettings.c_str(), nullptr, false);
	}
	JSONData["Scene"]["ValidMake"] = Scene->IsValidateAtMake;
	JSONData["Scene"]["ValidLod"] = Scene->IsValidateLODs;
	JSONData["Scene"]["ValidNames"] = Scene->IsValidateDublicateNames;
	JSONData["Scene"]["AIMapDefaultLegacy"] = Scene->IsAIMapDefaultLegacy;

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

	PHelper().CreateBool(items, "Experimental\\LOD\\Use MU LODs (4-level)", &UseMULODs);
	PHelper().CreateBool(items, "Experimental\\LOD\\Disable Billboard LOD", &DisableBillboardLOD);

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

