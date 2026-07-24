#pragma once

#include "EditorRenderBackend.h"

// Returns the installed implementation or a safe unavailable implementation.
[[nodiscard]] ECORE_API IMaterialPreviewRenderer& GetMaterialPreviewRenderer() noexcept;

// The caller owns Renderer. It must destroy every preview handle before it
// restores the previous implementation or destroys Renderer.
[[nodiscard]] ECORE_API IMaterialPreviewRenderer* InstallMaterialPreviewRenderer(
	IMaterialPreviewRenderer* Renderer) noexcept;
