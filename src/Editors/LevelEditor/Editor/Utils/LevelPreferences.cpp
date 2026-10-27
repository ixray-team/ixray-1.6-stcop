#include "stdafx.h"
#include "LevelPreferences.h"
#include "../../ActorEditor/ActorEditor.h"

#include "../xrECore/Editor/UILogForm.h"

extern ECORE_API bool g_force16BitTransformQuant;
extern ECORE_API bool g_force32BitTransformQuant;

template <typename T>
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
		else if constexpr (std::is_same_v<T, std::remove_cvref_t<char*>>)
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

void CLevelPreferences::AppendActorRecentFile(const char* Name)
{
	const xr_string File = Platform::ANSI_TO_UTF8(Name);
	auto It = std::find(ActorRecentFiles.begin(), ActorRecentFiles.end(), File);
	if (It != ActorRecentFiles.end())
	{
		ActorRecentFiles.erase(It);
	}

	ActorRecentFiles.insert(ActorRecentFiles.begin(), File);
	while (ActorRecentFiles.size() > 10)
	{
		ActorRecentFiles.pop_back();
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
		CEditorWndStates::Get().Load(*windows);

		GetSafe(*windows, "log_clear_in_pie", UILogForm::bClearInPIE);
	}

	if (const auto* partition = GetObjectSafe(JSONData, "world_partition"))
	{
		bool PartitionEnabled = GWorldPartition.IsEnabled();
		GetSafe(*partition, "enabled", PartitionEnabled);
		GWorldPartition.SetEnabled(PartitionEnabled);
		GetSafe(*partition, "unload_disabled", GWorldPartition.UnloadDisabled);
		GetSafe(*partition, "stream_around_camera", GWorldPartition.StreamAroundCamera);
		GetSafe(*partition, "stream_radius", GWorldPartition.StreamRadius);
		GetSafe(*partition, "stream_budget", GWorldPartition.StreamBudget);
	}

	if (const auto* comp = GetObjectSafe(JSONData, "Compilers Path"))
	{
		// Новый формат: единый путь к xrLevelBuilder.exe.
		GetSafe(*comp, "Path", CompilerPath);
	}

	// Если в конфиге пусто или ключа нет — используем стандартное имя.
	if (CompilerPath.empty())
	{
		CompilerPath = "xrLevelBuilder.exe";
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

	if (const auto* ae = GetObjectSafe(JSONData, "ActorEditor"))
	{
		GetSafe(*ae, "AlwaysShowKeyBar12", ActorAlwaysShowKeyBar12);
		GetSafe(*ae, "AlwaysShowKeyBar34", ActorAlwaysShowKeyBar34);
		GetSafe(*ae, "Force16BitAnims", g_force16BitTransformQuant);
		GetSafe(*ae, "Force32BitAnims", g_force32BitTransformQuant);

		int Smooth = (int)SmoothGroup;
		SmoothGroup = (ESmoothGroup)GetSafe(*ae, "SmoothGroup", Smooth);

		ActorRecentFiles.clear();
		auto Recent = ae->find("RecentFiles");
		if (Recent != ae->end() && Recent->is_array())
		{
			for (const auto& File : *Recent)
			{
				if (File.is_string())
				{
					ActorRecentFiles.push_back(File.get<std::string>().c_str());
				}
			}
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
		GetSafe(*scene, "AIMapDefaultLegacy", Scene->IsAIMapDefaultLegacy);
	}
}

void CLevelPreferences::Save()
{
	inherited::Save();

	JSONData["gizmo"]["matrixmode"] = imManipulator.MatrixMode;
	JSONData["windows"] = CEditorWndStates::Get().Save();
	JSONData["windows"]["log_clear_in_pie"] = UILogForm::bClearInPIE;
	JSONData["world_partition"]["enabled"] = GWorldPartition.IsEnabled();
	JSONData["world_partition"]["unload_disabled"] = GWorldPartition.UnloadDisabled;
	JSONData["world_partition"]["stream_around_camera"] = GWorldPartition.StreamAroundCamera;
	JSONData["world_partition"]["stream_radius"] = GWorldPartition.StreamRadius;
	JSONData["world_partition"]["stream_budget"] = GWorldPartition.StreamBudget;

	JSONData["LibaryEditor"]["Preview"] = PreviewRenderLibrary;
	JSONData["PIE"]["ArtPos"] = PIEArtSpawnPos;

	JSONData["ActorEditor"]["AlwaysShowKeyBar12"] = ActorAlwaysShowKeyBar12;
	JSONData["ActorEditor"]["AlwaysShowKeyBar34"] = ActorAlwaysShowKeyBar34;
	JSONData["ActorEditor"]["Force16BitAnims"] = g_force16BitTransformQuant;
	JSONData["ActorEditor"]["Force32BitAnims"] = g_force32BitTransformQuant;
	JSONData["ActorEditor"]["SmoothGroup"] = (int)SmoothGroup;
	JSONData["ActorEditor"]["RecentFiles"] = nlohmann::json::array();
	for (const xr_string& File : ActorRecentFiles)
	{
		JSONData["ActorEditor"]["RecentFiles"].push_back(File.c_str());
	}

	// Compilers: единый путь к xrLevelBuilder.exe.
	JSONData["Compilers Path"]["Path"] = CompilerPath.c_str();
	JSONData["Compilers"]["LODsForAllMU"] = LODsForAllMU;
	JSONData["Compilers"]["DuplicateObjectPositionAsError"] = DuplicateObjectPositionAsError;

	JSONData["ExperimentalLOD"]["UseMULODs"] = UseMULODs;
	JSONData["ExperimentalLOD"]["DisableBillboardLOD"] = DisableBillboardLOD;

	JSONData["Scene"]["ValidMake"] = Scene->IsValidateAtMake;
	JSONData["Scene"]["ValidLod"] = Scene->IsValidateLODs;
	JSONData["Scene"]["ValidNames"] = Scene->IsValidateDublicateNames;
	JSONData["Scene"]["AIMapDefaultLegacy"] = Scene->IsAIMapDefaultLegacy;

	SceneToolsMapPairIt _I = Scene->FirstTool();
	SceneToolsMapPairIt _E = Scene->LastTool();
	for (; _I != _E; _I++)
	{
		if (_I->second && (_I->first != OBJCLASS_DUMMY))
		{
			JSONData["targets"][_I->second->ClassName()] = _I->second->m_EditFlags.flags;
		}
	}
}

void CLevelPreferences::OnEnabledChange(PropValue* prop)
{
	ESceneToolBase* M = Scene->GetTool(prop->tag);
	VERIFY(M);
	ExecCommand(COMMAND_ENABLE_TARGET, prop->tag, M->IsEnabled());
}

void CLevelPreferences::OnReadonlyChange(PropValue* prop)
{
	ESceneToolBase* M = Scene->GetTool(prop->tag);
	VERIFY(M);
	ExecCommand(COMMAND_READONLY_TARGET, prop->tag, M->IsForceReadonly());
}

void CLevelPreferences::FillProp(PropItemVec& items)
{
	inherited::FillProp(items);

	PHelper().CreateBool(items, "Experimental\\LOD\\Use MU LODs (4-level)", &UseMULODs);
	PHelper().CreateBool(items, "Experimental\\LOD\\Disable Billboard LOD", &DisableBillboardLOD);

	PHelper().CreateBool(items, "Actor Editor\\Keybar\\Show Footsteps 12", &ActorAlwaysShowKeyBar12);
	PHelper().CreateBool(items, "Actor Editor\\Keybar\\Show Footsteps 34", &ActorAlwaysShowKeyBar34);

	SceneToolsMapPairIt _I = Scene->FirstTool();
	SceneToolsMapPairIt _E = Scene->LastTool();
	for (; _I != _E; _I++)
	{
		if (_I->second && (_I->first != OBJCLASS_DUMMY))
		{
			if (_I->second->AllowEnabling())
			{
				PropValue* V = PHelper().CreateFlag32(items, PrepareKey("Tools\\Targets\\Enable", _I->second->ClassDesc()), &_I->second->m_EditFlags, ESceneToolBase::flEnable);
				V->tag = _I->second->FClassID;
				V->OnChangeEvent.bind(this, &CLevelPreferences::OnEnabledChange);
			}
			PropValue* V = PHelper().CreateFlag32(items, PrepareKey("Tools\\Targets\\Read Only", _I->second->ClassDesc()), &_I->second->m_EditFlags, ESceneToolBase::flForceReadonly);
			V->tag = _I->second->FClassID;
			V->OnChangeEvent.bind(this, &CLevelPreferences::OnReadonlyChange);
		}
	}
}