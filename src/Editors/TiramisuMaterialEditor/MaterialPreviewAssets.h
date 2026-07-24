#pragma once

#include "TiramisuMaterialEditorTypes.h"

#include <string>
#include <string_view>

namespace Tiramisu::Editor
{
// Преобразует game-data reference texture в путь внутри смонтированного editor root.
[[nodiscard]] xr_string NormalizeMaterialPreviewTexturePath(
	xr_string_view AssetPath
);

// Именованное environment разрешается в обычный TextureCube через тот же descriptor path.
[[nodiscard]] xr_string_view MaterialPreviewEnvironmentAsset(
	xr_string_view Environment
) noexcept;
} // namespace Tiramisu::Editor
