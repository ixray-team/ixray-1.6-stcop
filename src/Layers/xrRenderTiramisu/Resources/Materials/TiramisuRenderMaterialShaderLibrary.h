#pragma once

#include "TiramisuRenderTypes.h"

#include <TiramisuMaterialShaderLibrary.h>

#include <optional>

// Собирает HLSL implementation с engine template и shader permutations.
class TiramisuRenderMaterialShaderLibrary
{
public:
	TiramisuRenderMaterialShaderLibrary();
	~TiramisuRenderMaterialShaderLibrary();

	[[nodiscard]] bool IsAvailable() const noexcept { return Library.has_value(); }
	// Разрешает уже собранные programs и source assets без runtime-компиляции в cooked mode.
	[[nodiscard]] xr_optional<FMaterialShaderProgramView>
	Find_RenderThread(const FMaterialAssetId& MaterialId, EMaterialPass Pass) const;
	[[nodiscard]] const FResolvedMaterialInstance*
	ResolveMaterial_RenderThread(
		const FMaterialAssetId& MaterialId
	) const;
	[[nodiscard]] const FMaterialAsset*
	ResolveMaster_RenderThread(
		const FMaterialAssetId& MaterialId
	) const;

private:
	xr_optional<TiramisuMaterialShaderLibrary> Library;
};
