#include "stdafx.h"
#include "WristwatchVisual.h"

#include "r__dsgraph_structure.h"
#include "SkeletonCustom.h"
#include "Shader.h"
#include "../../xrEngine/WristwatchSettings.h"

namespace
{
struct SWatchesChildShaderBackup
{
	shared_str shader;
	shared_str texture;
	bool valid = false;
};

shared_str g_watchesModelPath;
xr_vector<SWatchesChildShaderBackup> g_watchesShaderBackup;
bool g_watchesShadersApplied = false;
EWristwatchDisplayType g_watchesAppliedMode = EWristwatchDisplayType::Hybrid;

LPCSTR GetDigitalShaderName()
{
	return "models\\wristwatch_digital";
}

LPCSTR GetGlassShaderName()
{
	return "models\\wristwatch_glass";
}

LPCSTR GetHiddenShaderName()
{
	return "models\\wristwatch_hidden";
}

void BuildDigitalTextureList(string_path& outTextures)
{
	const SWristwatchRuntimeSettings& settings = GetWristwatchRuntimeSettings();
	xr_sprintf(
		outTextures,
		"%s,%s,%s",
		settings.digitalTexture.c_str(),
		settings.glassTexture.c_str(),
		settings.fontTexture.c_str());
}

bool IsGlassTexture(const shared_str& textureName)
{
	if (textureName.size() == 0)
	{
		return false;
	}

	string_path lowerName;
	xr_strcpy(lowerName, textureName.c_str());
	_strlwr(lowerName);

	return strstr(lowerName, "glass") != nullptr;
}

bool IsWatchUiChild(CKinematics& kinematics, u32 childIdx, u16 watchUiBone)
{
	if (watchUiBone == BI_NONE)
	{
		return true;
	}

	const CBoneData& boneData = kinematics.LL_GetData(watchUiBone);
	return childIdx < boneData.child_faces.size() && !boneData.child_faces[childIdx].empty();
}

void CaptureShaderBackup(const ref_shader& shader, SWatchesChildShaderBackup& backup)
{
	backup.valid = false;
	backup.shader = "models\\model";

	if (!shader)
	{
		return;
	}

	const Shader& shaderResource = *shader;
	for (u32 elementIdx = 0; elementIdx < SHADER_ELEMENTS_MAX; ++elementIdx)
	{
		if (!shaderResource.E[elementIdx] || shaderResource.E[elementIdx]->passes.empty())
		{
			continue;
		}

		const SPass& pass = *shaderResource.E[elementIdx]->passes[0];
		if (!pass.T || pass.T->empty() || !pass.T->operator[](0).second)
		{
			continue;
		}

		backup.texture = pass.T->operator[](0).second->cName;
		backup.valid = true;
		return;
	}
}

void BackupShaders(CKinematics& kinematics)
{
	if (g_watchesShaderBackup.size() == kinematics.children.size())
	{
		return;
	}

	if (!g_watchesShaderBackup.empty() && g_watchesShaderBackup.size() != kinematics.children.size())
	{
		Msg("! [wristwatch] watches model child count changed (%u -> %u), rebuilding shader backup",
			static_cast<u32>(g_watchesShaderBackup.size()),
			static_cast<u32>(kinematics.children.size()));
	}

	g_watchesShaderBackup.clear();
	g_watchesShaderBackup.reserve(kinematics.children.size());

	for (dxRender_Visual* child : kinematics.children)
	{
		SWatchesChildShaderBackup backup;
		if (child != nullptr)
		{
			CaptureShaderBackup(child->shader, backup);
		}

		g_watchesShaderBackup.push_back(backup);
	}
}

void RestoreOriginalShaders(CKinematics& kinematics)
{
	BackupShaders(kinematics);

	for (u32 childIdx = 0; childIdx < kinematics.children.size(); ++childIdx)
	{
		dxRender_Visual* child = kinematics.children[childIdx];
		if (child == nullptr || childIdx >= g_watchesShaderBackup.size())
		{
			continue;
		}

		const SWatchesChildShaderBackup& backup = g_watchesShaderBackup[childIdx];
		if (backup.valid)
		{
			child->shader.create(backup.shader.c_str(), backup.texture.c_str());
		}
	}
}

void ApplyDigitalShaders(CKinematics& kinematics)
{
	BackupShaders(kinematics);

	const u16 watchUiBone = kinematics.LL_BoneID("watch_ui");
	const LPCSTR digitalShader = GetDigitalShaderName();
	const LPCSTR glassShader = GetGlassShaderName();
	const LPCSTR hiddenShader = GetHiddenShaderName();

	string_path digitalTextures;
	BuildDigitalTextureList(digitalTextures);

	bool hasWatchUiMesh = false;
	for (u32 childIdx = 0; childIdx < kinematics.children.size(); ++childIdx)
	{
		if (IsWatchUiChild(kinematics, childIdx, watchUiBone))
		{
			hasWatchUiMesh = true;
			break;
		}
	}

	for (u32 childIdx = 0; childIdx < kinematics.children.size(); ++childIdx)
	{
		dxRender_Visual* child = kinematics.children[childIdx];
		if (child == nullptr || childIdx >= g_watchesShaderBackup.size())
		{
			continue;
		}

		const SWatchesChildShaderBackup& backup = g_watchesShaderBackup[childIdx];
		const bool isGlassTexture = IsGlassTexture(backup.texture);
		const bool isWatchUi = IsWatchUiChild(kinematics, childIdx, watchUiBone);

		if (isWatchUi)
		{
			child->shader.create(digitalShader, digitalTextures);
			continue;
		}

		if (isGlassTexture)
		{
			if (hasWatchUiMesh)
			{
				child->shader.create(glassShader, backup.texture.c_str());
			}
			else
			{
				child->shader.create(digitalShader, digitalTextures);
			}

			continue;
		}

		if (backup.valid)
		{
			child->shader.create(backup.shader.c_str(), backup.texture.c_str());
		}
	}
}
}

void WristwatchVisual::ResetForModel(const shared_str& modelPath)
{
	if (g_watchesModelPath == modelPath)
	{
		return;
	}

	g_watchesModelPath = modelPath;
	g_watchesShaderBackup.clear();
	g_watchesShadersApplied = false;
}

void WristwatchVisual::ApplyDisplayShaders(EWristwatchDisplayType displayType, IKinematics* watchesModel)
{
	if (watchesModel == nullptr)
	{
		return;
	}

	IRenderVisual* visual = watchesModel->dcast_RenderVisual();
	CKinematics* kinematics = visual ? static_cast<CKinematics*>(visual) : nullptr;
	if (kinematics == nullptr || kinematics->children.empty())
	{
		return;
	}

	if (g_watchesShadersApplied && g_watchesAppliedMode == displayType)
	{
		return;
	}

	if (displayType == EWristwatchDisplayType::Digital || displayType == EWristwatchDisplayType::Hybrid)
	{
		ApplyDigitalShaders(*kinematics);
	}
	else
	{
		RestoreOriginalShaders(*kinematics);
	}

	g_watchesAppliedMode = displayType;
	g_watchesShadersApplied = true;
}

void WristwatchVisual::Shutdown()
{
	g_watchesShaderBackup.clear();
	g_watchesModelPath = nullptr;
	g_watchesShadersApplied = false;
	g_watchesAppliedMode = EWristwatchDisplayType::Hybrid;
}

void R_dsgraph_structure::wristwatch_reset_model(const shared_str& modelPath)
{
	WristwatchVisual::ResetForModel(modelPath);
}

void R_dsgraph_structure::wristwatch_apply_display_shaders(u8 displayType, IKinematics* watchesModel)
{
	const u8 clampedDisplayType = displayType > static_cast<u8>(EWristwatchDisplayType::Hybrid)
		? static_cast<u8>(EWristwatchDisplayType::Hybrid)
		: displayType;
	WristwatchVisual::ApplyDisplayShaders(static_cast<EWristwatchDisplayType>(clampedDisplayType), watchesModel);
}
