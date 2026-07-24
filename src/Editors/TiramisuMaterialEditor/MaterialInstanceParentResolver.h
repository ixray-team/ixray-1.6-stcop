#pragma once

#include "TiramisuMaterialEditorTypes.h"

#include <MaterialAsset.h>

#include <filesystem>
#include <vector>

namespace Tiramisu::Editor
{
// Master, effective parent и current instance после разрешения parent chain.
struct FMaterialInstanceParentResolution
{
    FMaterialAsset Master;
    FResolvedMaterialInstance Parent;
    FResolvedMaterialInstance Instance;
    // Canonical dependencies resolved parent chain используются для точечного reload
    // без наблюдения за посторонними assets в material root.
    xr_vector<std::filesystem::path> AssetDependencies;
    xr_vector<FMaterialDiagnostic> Diagnostics;

    [[nodiscard]] bool Succeeded() const noexcept;
};

// Строит тот же parent graph, что cooker, но считает current in-memory instance
// авторитетнее disk copy. Возвращает effective parent и instance для live preview.
[[nodiscard]] FMaterialInstanceParentResolution ResolveMaterialInstanceParent(
    const std::filesystem::path& MaterialRoot,
    const FMaterialInstanceAsset& CurrentInstance);
} // namespace Tiramisu::Editor
