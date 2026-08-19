#pragma once

#include "../../../xrCore/xrCore.h"
#include "../../../xrCore/FS.h"

// Полностью декодированный bone track. Декодирование выполняет background
// loader один раз; render thread затем только интерполирует соседние keys.
struct FTiramisuEditorOgfMotionTrack
{
	xr_vector<Fquaternion> Rotations;
	xr_vector<Fvector> Translations;
};

struct FTiramisuEditorOgfMotionClip
{
	xr_string Name;
	u32 FrameCount = 0;
	float Speed = 1.0f;
	bool StopAtEnd = false;
	bool Fx = false;
	xr_vector<FTiramisuEditorOgfMotionTrack> BoneTracks;

	[[nodiscard]] float DurationSeconds() const noexcept;
};

struct FTiramisuEditorOgfMotionSet
{
	xr_vector<FTiramisuEditorOgfMotionClip> Clips;
	xr_string Diagnostic;

	[[nodiscard]] const FTiramisuEditorOgfMotionClip* FindClip(
		xr_string_view Name
	) const noexcept;
};

// OGF и OMF используют одинаковые OGF_S_SMPARAMS/OGF_S_MOTIONS chunks.
// BoneNames задаёт model-order, а loader применяет remap из SMPARAMS.
[[nodiscard]] bool LoadTiramisuEditorOgfMotions(
	IReader& Reader,
	xr_span<const xr_string> BoneNames,
	FTiramisuEditorOgfMotionSet& OutMotions
);

// Читает старый строковый OGF_S_MOTION_REFS и versioned REFS2. Имена
// возвращаются без добавления `.omf`, как они сохранены в исходном OGF.
[[nodiscard]] bool LoadTiramisuEditorOgfMotionReferences(
	IReader& Reader,
	xr_vector<xr_string>& OutReferences,
	xr_string* OutDiagnostic = nullptr
);

// Семплирует clip в local matrices в model bone order. Looping и speed
// соответствуют legacy cycle semantics; StopAtEnd фиксирует последний key.
[[nodiscard]] bool SampleTiramisuEditorOgfMotion(
	const FTiramisuEditorOgfMotionSet& Motions,
	xr_string_view ClipName,
	float TimeSeconds,
	xr_vector<Fmatrix>& OutLocalPose,
	xr_string* OutDiagnostic = nullptr
);
