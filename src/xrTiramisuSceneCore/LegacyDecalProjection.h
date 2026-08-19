#pragma once

#include "TiramisuSceneTypes.h"

namespace Tiramisu::Scene
{
// Renderer-neutral вершина старого Wallmark, достаточная для восстановления
// канонического projective decal volume.
struct FLegacyDecalVertex
{
	xr_array<float, 3> Position = {};
	xr_array<float, 2> TexCoord = {};
};

// Результат односторонней миграции Wallmark. DiagnosticCode пуст только при
// корректном, конечном и невырожденном projector transform.
struct FLegacyDecalProjectionResult
{
	xr_array<float, 16> LocalToWorld = {
		1.0f, 0.0f, 0.0f, 0.0f,
		0.0f, 1.0f, 0.0f, 0.0f,
		0.0f, 0.0f, 1.0f, 0.0f,
		0.0f, 0.0f, 0.0f, 1.0f
	};
	xr_string DiagnosticCode;

	[[nodiscard]] bool Succeeded() const noexcept
	{
		return DiagnosticCode.empty();
	}
};

// Восстанавливает положение и ориентацию projective decal из уже clipped
// legacy-геометрии и UV. Width/height сохраняют авторский размер Wallmark.
[[nodiscard]] FLegacyDecalProjectionResult BuildLegacyDecalProjection(
	const xr_vector<FLegacyDecalVertex>& Vertices,
	float Width,
	float Height
);
} // namespace Tiramisu::Scene
