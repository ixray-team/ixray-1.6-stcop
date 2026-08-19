#pragma once

#include "../../../Include/xrRender/EditorRenderer.h"

#include <filesystem>
#include <mutex>

// Полное renderer-owned описание legacy particle effect. Данные не содержат
// shader handles и подходят для запуска CPU PAPI внутри render thread.
struct FTiramisuEditorParticleEffectDefinition
{
	xr_string Name;
	xr_string ShaderName;
	xr_string TextureName;
	xr_vector<u8> CompiledActions;
	xr_array<float, 2> FrameTexSize = {1.0f, 1.0f};
	xr_array<float, 3> VelocityScale = {0.0f, 0.0f, 0.0f};
	xr_array<float, 3> AlignToPathDefaultRotation = {
		-1.57079632679f,
		0.0f,
		0.0f
	};
	u32 Flags = 0;
	u32 MaxParticles = 0;
	s32 FrameDimensionX = 1;
	s32 FrameCount = 1;
	float FrameSpeed = 0.0f;
	float TimeLimit = -1.0f;

	[[nodiscard]] bool IsSimulatable() const noexcept
	{
		return MaxParticles != 0 && !CompiledActions.empty();
	}
};

struct FTiramisuEditorParticleGroupEntry
{
	xr_string EffectName;
	xr_string OnPlayChildName;
	xr_string OnBirthChildName;
	xr_string OnDeathChildName;
	float StartTime = 0.0f;
	float StopTime = 0.0f;
	u32 Flags = 0;
};

// Копируемое описание расписания particle group без legacy CPGDef pointers.
struct FTiramisuEditorParticleGroupDefinition
{
	xr_string Name;
	xr_vector<FTiramisuEditorParticleGroupEntry> Entries;
	u32 Flags = 0;
	float TimeLimit = 0.0f;
};

// Загружает legacy particle assets как renderer-owned read-only каталог.
// Класс не создаёт legacy shaders и не зависит от editor CRender/CPSLibrary.
class TiramisuEditorParticleLibrary final
{
public:
	[[nodiscard]] bool Reload(
		const std::filesystem::path& CompiledLibrary,
		const std::filesystem::path& LooseAssetsRoot
	);

	void CopySnapshot(FEditorParticleLibrarySnapshot& OutSnapshot) const;
	[[nodiscard]] bool CopyEffectDefinition(
		xr_string_view Name,
		FTiramisuEditorParticleEffectDefinition& OutDefinition
	) const;
	[[nodiscard]] bool CopyGroupDefinition(
		xr_string_view Name,
		FTiramisuEditorParticleGroupDefinition& OutDefinition
	) const;

private:
	mutable std::mutex Mutex;
	FEditorParticleLibrarySnapshot Snapshot;
	xr_vector<FTiramisuEditorParticleEffectDefinition> EffectDefinitions;
	xr_vector<FTiramisuEditorParticleGroupDefinition> GroupDefinitions;
};
