#pragma once

#include "TiramisuMaterialEditorTypes.h"

#include "MaterialEditorDocument.h"

#include <filesystem>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace Tiramisu::Editor
{
// UI-независимый документ MaterialInstance с inheritance, undo/redo и recovery.
class TiramisuMaterialInstanceEditorDocument
{
public:
    TiramisuMaterialInstanceEditorDocument();

    // Создаёт или открывает instance и сбрасывает history/save point.
    void NewInstance(xr_string Parent = {});
    void OpenInstance(FMaterialInstanceAsset Instance);

    // Выполняет безопасный file/recovery lifecycle instance asset.
    [[nodiscard]] FMaterialEditorOperationResult OpenInstanceJson(
        xr_string_view JsonText, xr_string_view SourcePath = {});
    [[nodiscard]] FMaterialEditorOperationResult OpenInstanceFile(
        const std::filesystem::path& Path);
    [[nodiscard]] FMaterialEditorOperationResult SaveInstanceFile(
        const std::filesystem::path& Path);
    [[nodiscard]] FMaterialEditorOperationResult SaveRecoveryFile(
        const std::filesystem::path& RecoveryPath) const;
    [[nodiscard]] FMaterialEditorOperationResult OpenRecoveryFile(
        const std::filesystem::path& RecoveryPath,
        const std::filesystem::path& OriginalPath = {});

    [[nodiscard]] const FMaterialInstanceAsset& GetInstance() const noexcept
    {
        return InstanceAsset;
    }
    [[nodiscard]] const xr_optional<FMaterialAsset>& GetParentMaterial() const noexcept
    {
        return ParentMaterial;
    }
    [[nodiscard]] const xr_optional<FResolvedMaterialInstance>&
        GetParentResolution() const noexcept
    {
        return ParentResolution;
    }

    // Устанавливает resolved parent, используемый inspector и validation overrides.
    void SetParentMaterial(FMaterialAsset Material);
    [[nodiscard]] FMaterialEditorOperationResult SetParentResolution(
        FMaterialAsset Material,
        FResolvedMaterialInstance Resolution);
    void ClearParentMaterial() noexcept;
    [[nodiscard]] const FMaterialValue* GetInheritedValue(
        const FMaterialParameterId& ParameterId, bool Static) const noexcept;
    [[nodiscard]] FMaterialStaticParameterSet
        GetEffectiveStaticParameters() const;

    // Меняет parent/runtime/static overrides только после type validation.
    [[nodiscard]] bool SetName(xr_string Name);
    [[nodiscard]] FMaterialEditorOperationResult SetParent(xr_string Parent);
    [[nodiscard]] FMaterialEditorOperationResult SetOverride(
        const FMaterialParameterId& ParameterId,
        FMaterialValue Value, bool Static);
    [[nodiscard]] FMaterialEditorOperationResult RemoveOverride(
        const FMaterialParameterId& ParameterId);
    [[nodiscard]] FMaterialEditorOperationResult ValidateOverrides() const;

    // Сериализует source instance либо его flattened представление для preview/cooker.
    [[nodiscard]] xr_string SerializeInstance() const;
    [[nodiscard]] xr_string SerializeFlattenedInstance() const;
    // Undo/redo и dirty state используют snapshots instance asset.
    [[nodiscard]] bool CanUndo() const noexcept { return !UndoHistory.empty(); }
    [[nodiscard]] bool CanRedo() const noexcept { return !RedoHistory.empty(); }
    [[nodiscard]] bool Undo();
    [[nodiscard]] bool Redo();
    [[nodiscard]] bool IsDirty() const;
    void MarkSaved();

private:
    void RecordMutation();
    static void AddDiagnostic(FMaterialEditorOperationResult& Result,
        xr_string_view Code, xr_string Message);

    static constexpr size_t MaxHistoryEntries = 128;

    FMaterialInstanceAsset InstanceAsset;
    xr_optional<FMaterialAsset> ParentMaterial;
    xr_optional<FResolvedMaterialInstance> ParentResolution;
    xr_vector<FMaterialInstanceAsset> UndoHistory;
    xr_vector<FMaterialInstanceAsset> RedoHistory;
    xr_string SavedInstance;
};
} // namespace Tiramisu::Editor
