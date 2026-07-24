#pragma once

#include "TiramisuEditorTypes.h"

#include <cstdint>

// Submits the active native scene document. Until legacy level auto-import is
// complete, an unopened native document temporarily falls back to EScene.
// The target LevelEditor composition root does not render legacy objects.
[[nodiscard]] bool SubmitEditorSceneToEditorRenderer(
	u32 ViewportId);

// Compatibility name for code outside LevelEditor while the import-only
// transition is in progress.
[[nodiscard]] bool SubmitLegacySceneToEditorRenderer(u32 ViewportId);
