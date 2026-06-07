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

enum class EGlassDetectReason : u8
{
	None = 0,
	HiddenShader,
	GlassShader,
	MeshName,
	GlassTexture,
	GlassTextureMarker,
};

shared_str g_watchesModelPath;
xr_vector<SWatchesChildShaderBackup> g_watchesShaderBackup;
bool g_watchesShadersApplied = false;
EWristwatchDisplayType g_watchesAppliedMode = EWristwatchDisplayType::Hybrid;

void BuildDigitalTextureList(string_path& outTextures)
{
	const SWristwatchRuntimeSettings& settings = GetWristwatchRuntimeSettings();
	xr_sprintf(
		outTextures,
		"%s,%s",
		settings.digitalTexture.c_str(),
		settings.fontTexture.c_str());
}

void BuildGlassTextureList(string_path& outTextures)
{
	const SWristwatchRuntimeSettings& settings = GetWristwatchRuntimeSettings();
	xr_sprintf(
		outTextures,
		"%s,%s",
		settings.glassTexture.c_str(),
		settings.glassBumpTexture.c_str());
}

void NormalizeResourcePath(string_path& path)
{
	if (path[0] == '\0')
	{
		return;
	}

	_strlwr(path);

	for (char* p = path; *p != '\0'; ++p)
	{
		if (*p == '/')
		{
			*p = '\\';
		}
	}

	char* hashSuffix = strchr(path, '#');
	if (hashSuffix != nullptr)
	{
		*hashSuffix = '\0';
	}
}

bool ResourcePathMatches(const shared_str& left, const shared_str& right)
{
	if (left.size() == 0 || right.size() == 0)
	{
		return false;
	}

	string_path leftPath;
	string_path rightPath;
	xr_strcpy(leftPath, left.c_str());
	xr_strcpy(rightPath, right.c_str());
	NormalizeResourcePath(leftPath);
	NormalizeResourcePath(rightPath);

	return xr_strcmp(leftPath, rightPath) == 0;
}

const char* WristwatchVisualDebugName(dxRender_Visual* visual)
{
	return visual != nullptr ? visual->dbg_name.c_str() : "<null>";
}

const char* GlassDetectReasonName(EGlassDetectReason reason)
{
	switch (reason)
	{
	case EGlassDetectReason::HiddenShader:
		return "hidden_shader";
	case EGlassDetectReason::GlassShader:
		return "glass_shader";
	case EGlassDetectReason::MeshName:
		return "mesh_name";
	case EGlassDetectReason::GlassTexture:
		return "glass_texture";
	case EGlassDetectReason::GlassTextureMarker:
		return "glass_texture_marker";
	default:
		return "none";
	}
}

bool MeshNameContainsSubstr(dxRender_Visual* visual, const shared_str& substr)
{
	if (visual == nullptr || substr.size() == 0)
	{
		return false;
	}

	string_path meshName;
	xr_strcpy(meshName, visual->dbg_name.c_str());
	_strlwr(meshName);

	string_path marker;
	xr_strcpy(marker, substr.c_str());
	_strlwr(marker);

	return strstr(meshName, marker) != nullptr;
}

bool IsHiddenGlassChild(const SWatchesChildShaderBackup& backup, const shared_str& hiddenShader)
{
	return backup.valid && ResourcePathMatches(backup.shader, hiddenShader);
}

bool IsGlassShaderChild(const SWatchesChildShaderBackup& backup, const shared_str& glassShader)
{
	return backup.valid && ResourcePathMatches(backup.shader, glassShader);
}

bool TexturePathContainsGlassMarker(const shared_str& textureName)
{
	if (textureName.size() == 0)
	{
		return false;
	}

	string_path texturePath;
	xr_strcpy(texturePath, textureName.c_str());
	NormalizeResourcePath(texturePath);

	return strstr(texturePath, "watches_glass") != nullptr;
}

bool IsGlassChildMesh(
	const SWatchesChildShaderBackup& backup,
	const SWristwatchRuntimeSettings& settings,
	dxRender_Visual* visual,
	EGlassDetectReason& outReason)
{
	outReason = EGlassDetectReason::None;

	if (IsHiddenGlassChild(backup, settings.shaderHidden))
	{
		outReason = EGlassDetectReason::HiddenShader;
		return true;
	}

	if (IsGlassShaderChild(backup, settings.shaderGlass))
	{
		outReason = EGlassDetectReason::GlassShader;
		return true;
	}

	if (MeshNameContainsSubstr(visual, settings.glassMeshSubstr))
	{
		outReason = EGlassDetectReason::MeshName;
		return true;
	}

	if (backup.valid && TexturePathContainsGlassMarker(backup.texture))
	{
		outReason = EGlassDetectReason::GlassTextureMarker;
		return true;
	}

	if (settings.glassTexture.size() == 0 || settings.digitalTexture.size() == 0)
	{
		return false;
	}

	if (settings.glassTexture == settings.digitalTexture)
	{
		return false;
	}

	if (backup.valid && ResourcePathMatches(backup.texture, settings.glassTexture))
	{
		outReason = EGlassDetectReason::GlassTexture;
		return true;
	}

	return false;
}

bool IsWatchUiChild(CKinematics& kinematics, u32 childIdx, u16 watchUiBone)
{
	if (watchUiBone == BI_NONE)
	{
		return true;
	}

	const CBoneData& boneData = kinematics.LL_GetData(watchUiBone);
	return static_cast<size_t>(childIdx) < boneData.child_faces.size() &&
		!boneData.child_faces[childIdx].empty();
}

void SuppressGlassChildDraw(
	dxRender_Visual* child,
	const SWatchesChildShaderBackup& backup,
	const SWristwatchRuntimeSettings& settings)
{
	if (settings.forceSkipGlassDraw)
	{
		child->shader.destroy();
		return;
	}

	const char* textureName = backup.valid ? backup.texture.c_str() : "";
	child->shader.create(settings.shaderHidden.c_str(), textureName);
}

void CaptureShaderBackup(const ref_shader& shader, SWatchesChildShaderBackup& backup, LPCSTR fallbackShader)
{
	backup.valid = false;
	backup.shader = fallbackShader != nullptr && fallbackShader[0] != '\0' ? fallbackShader : "";

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
			CaptureShaderBackup(child->shader, backup, GetWristwatchRuntimeSettings().shaderFallback.c_str());
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
		if (backup.valid && backup.shader.size() > 0)
		{
			child->shader.create(backup.shader.c_str(), backup.texture.c_str());
		}
	}
}

void ApplyDigitalShaders(CKinematics& kinematics)
{
	if (!IsWristwatchContentConfigured())
	{
		return;
	}

	BackupShaders(kinematics);

	const SWristwatchRuntimeSettings& settings = GetWristwatchRuntimeSettings();
	const u16 watchUiBone = kinematics.LL_BoneID(settings.boneUi.c_str());
	const LPCSTR digitalShader = settings.shaderDigital.c_str();
	const LPCSTR glassShader = settings.shaderGlass.c_str();

	string_path digitalTextures;
	BuildDigitalTextureList(digitalTextures);

	string_path glassTextures;

	bool hasWatchUiMesh = false;
	for (u32 childIdx = 0; childIdx < kinematics.children.size(); ++childIdx)
	{
		if (IsWatchUiChild(kinematics, childIdx, watchUiBone))
		{
			hasWatchUiMesh = true;
			break;
		}
	}

	Msg("* [wristwatch] ApplyDigitalShaders model='%s' children=%u watch_ui_bone=%u has_watch_ui_mesh=%d force_skip_glass=%d debug_lcd_pass=%u",
		kinematics.dbg_name.c_str(),
		static_cast<u32>(kinematics.children.size()),
		static_cast<u32>(watchUiBone),
		hasWatchUiMesh ? 1 : 0,
		settings.forceSkipGlassDraw ? 1 : 0,
		static_cast<u32>(settings.debugLcdPass));

	u32 glassChildCount = 0;
	u32 glassSuppressedCount = 0;

	for (u32 childIdx = 0; childIdx < kinematics.children.size(); ++childIdx)
	{
		dxRender_Visual* child = kinematics.children[childIdx];
		if (child == nullptr || childIdx >= g_watchesShaderBackup.size())
		{
			continue;
		}

		const SWatchesChildShaderBackup& backup = g_watchesShaderBackup[childIdx];
		EGlassDetectReason glassReason = EGlassDetectReason::None;
		const bool isGlassChildMesh = IsGlassChildMesh(backup, settings, child, glassReason);
		const bool isWatchUi = IsWatchUiChild(kinematics, childIdx, watchUiBone);

		const char* textureName = backup.valid ? backup.texture.c_str() : "<none>";
		const char* shaderName = backup.shader.size() > 0 ? backup.shader.c_str() : "<none>";
		Msg("* [wristwatch] child[%u] visual='%s' shader='%s' tex='%s' watch_ui=%d glass=%d glass_reason=%s",
			childIdx,
			WristwatchVisualDebugName(child),
			shaderName,
			textureName,
			isWatchUi ? 1 : 0,
			isGlassChildMesh ? 1 : 0,
			GlassDetectReasonName(glassReason));

		if (isWatchUi)
		{
			child->shader.create(digitalShader, digitalTextures);
			Msg("* [wristwatch] child[%u] -> shader %s (watch_ui LCD pass)", childIdx, digitalShader);
			continue;
		}

		if (isGlassChildMesh)
		{
			++glassChildCount;
			if (hasWatchUiMesh && settings.forceSkipGlassDraw)
			{
				SuppressGlassChildDraw(child, backup, settings);
				++glassSuppressedCount;
				Msg("* [wristwatch] child[%u] -> glass suppressed (force_skip_glass)", childIdx);
			}
			else
			{
				BuildGlassTextureList(glassTextures);
				child->shader.create(glassShader, glassTextures);
				if (hasWatchUiMesh)
				{
					Msg("* [wristwatch] child[%u] -> shader %s (glass overlay on LCD, sorting 12)",
						childIdx,
						glassShader);
				}
				else
				{
					Msg("* [wristwatch] child[%u] -> shader %s (glass fallback, no watch_ui)", childIdx, glassShader);
				}
			}

			continue;
		}

		if (hasWatchUiMesh &&
			backup.valid &&
			ResourcePathMatches(backup.texture, settings.digitalTexture))
		{
			Msg("! [wristwatch] child[%u] shader='%s' tex='%s' shares watches texture but is not watch_ui or glass - possible pixel overlap",
				childIdx,
				shaderName,
				textureName);
		}

		if (backup.valid && backup.shader.size() > 0)
		{
			child->shader.create(backup.shader.c_str(), backup.texture.c_str());
			Msg("* [wristwatch] child[%u] -> shader %s (restore original)", childIdx, backup.shader.c_str());
		}
	}

	if (glassChildCount == 0 && settings.glassTexture.size() > 0)
	{
		Msg("! [wristwatch] no glass child detected (hidden=%s glass=%s mesh_substr='%s')",
			settings.shaderHidden.c_str(),
			settings.shaderGlass.c_str(),
			settings.glassMeshSubstr.c_str());
	}
	else
	{
		Msg("* [wristwatch] summary glass_children=%u glass_suppressed=%u",
			glassChildCount,
			glassSuppressedCount);
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
		if (IsWristwatchContentConfigured())
		{
			ApplyDigitalShaders(*kinematics);
		}
		else
		{
			RestoreOriginalShaders(*kinematics);
		}
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
