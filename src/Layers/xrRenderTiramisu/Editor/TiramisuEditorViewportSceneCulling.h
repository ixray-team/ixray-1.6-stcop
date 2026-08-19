#pragma once

#include "../../../Include/xrRender/EditorRenderer.h"

// Консервативно проверяет canonical projector box декали против frustum камеры.
// Функция renderer-owned, не использует NRI и пригодна для CPU reference tests.
[[nodiscard]] bool IsTiramisuEditorDecalVisible(
	const FEditorDecalInstance& Decal,
	const FEditorViewportCamera& Camera
) noexcept;
