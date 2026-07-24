#pragma once

#include "TiramisuMaterialEditorTypes.h"

#include <MaterialAsset.h>
#include <MaterialGraphSchema.h>

#include <cstddef>
#include <filesystem>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace Tiramisu::Editor
{
// Результат editor-команды с типизированными diagnostics.
struct FMaterialEditorOperationResult
{
    xr_vector<FMaterialDiagnostic> Diagnostics;

    [[nodiscard]] bool Succeeded() const noexcept;
};

// UI-независимый документ master material. FMaterialGraph остаётся единственной
// semantic model для editor, cooker и runtime compiler.
class TiramisuMaterialEditorDocument
{
public:
    TiramisuMaterialEditorDocument();

    // Создаёт или открывает semantic document и сбрасывает history/save point.
    void NewMaterial();
    void NewGraph();
    void OpenGraph(FMaterialGraph Graph);
    void OpenMaterial(FMaterialAsset Asset);

    // Выполняет безопасный file/recovery lifecycle через атомарную запись.
    [[nodiscard]] FMaterialEditorOperationResult OpenMaterialJson(
        xr_string_view JsonText, xr_string_view SourcePath = {});
    [[nodiscard]] FMaterialEditorOperationResult OpenMaterialFile(
        const std::filesystem::path& Path);
    [[nodiscard]] FMaterialEditorOperationResult SaveMaterialFile(
        const std::filesystem::path& Path);
    [[nodiscard]] FMaterialEditorOperationResult SaveRecoveryFile(
        const std::filesystem::path& RecoveryPath) const;
    [[nodiscard]] FMaterialEditorOperationResult OpenRecoveryFile(
        const std::filesystem::path& RecoveryPath,
        const std::filesystem::path& OriginalPath = {});

    [[nodiscard]] const FMaterialAsset& GetMaterial() const noexcept { return MaterialAsset; }
    [[nodiscard]] const FMaterialGraph& GetGraph() const noexcept
    {
        return MaterialAsset.Implementation.Graph;
    }

    // Изменяет master properties и parameter schema с записью в undo history.
    [[nodiscard]] bool SetMaterialName(xr_string Name);
    [[nodiscard]] bool SetMaterialDomain(EMaterialDomain Domain);
    [[nodiscard]] bool SetMaterialBlendMode(EMaterialBlendMode BlendMode);
    [[nodiscard]] bool SetMaterialShadingModel(EMaterialShadingModel ShadingModel);
    [[nodiscard]] bool SetMaterialTwoSided(bool TwoSided);
    [[nodiscard]] bool SetMaterialHlslTemplate(xr_string HlslTemplate);
    [[nodiscard]] FMaterialEditorOperationResult AddParameter(
        FMaterialParameterDefinition Definition);
    [[nodiscard]] FMaterialEditorOperationResult UpdateParameter(
        const FMaterialParameterId& ParameterId,
        FMaterialParameterDefinition Definition);
    [[nodiscard]] FMaterialEditorOperationResult RemoveParameter(
        const FMaterialParameterId& ParameterId);

    // Редактирует graph только через schema-validated операции.
    [[nodiscard]] FMaterialEditorOperationResult AddNode(xr_string_view Type,
        FMaterialNodeId NodeId, FFloat2 Position = {0.0f, 0.0f},
        EMaterialValueType ValueType = EMaterialValueType::Invalid);
    [[nodiscard]] FMaterialEditorOperationResult RemoveNode(const FMaterialNodeId& NodeId);
    [[nodiscard]] FMaterialEditorOperationResult Connect(xr_string LinkId,
        const FMaterialPinId& FromPin, const FMaterialPinId& ToPin);
    [[nodiscard]] FMaterialEditorOperationResult Disconnect(xr_string_view LinkId);
    [[nodiscard]] FMaterialEditorOperationResult SetNodeProperty(
        const FMaterialNodeId& NodeId, xr_string_view PropertyName,
        FMaterialValue Value);
    [[nodiscard]] FMaterialEditorOperationResult CopyNodes(
        xr_span<const FMaterialNodeId> NodeIds, xr_string& ClipboardJson) const;
    [[nodiscard]] FMaterialEditorOperationResult PasteNodes(
        xr_string_view ClipboardJson, FFloat2 PositionOffset,
        xr_vector<FMaterialNodeId>& PastedNodeIds);

    // Presentation может обновлять позиции nodes без заполнения undo history.
    [[nodiscard]] bool SetNodePosition(const FMaterialNodeId& NodeId,
        FFloat2 Position, bool RecordUndo);

    // Компилирует или сериализует текущее in-memory состояние без скрытого file I/O.
    [[nodiscard]] FMaterialGraphCompileResult Compile(
        const FMaterialGraphCompileOptions& Options = {}) const;
    [[nodiscard]] xr_string Serialize() const;
    [[nodiscard]] xr_string SerializeMaterial() const;

    // Undo/redo и dirty state сравниваются с последней сохранённой сериализацией.
    [[nodiscard]] bool CanUndo() const noexcept { return !UndoHistory.empty(); }
    [[nodiscard]] bool CanRedo() const noexcept { return !RedoHistory.empty(); }
    [[nodiscard]] bool Undo();
    [[nodiscard]] bool Redo();

    [[nodiscard]] bool IsDirty() const;
    void MarkSaved();

private:
    void RecordMutation();
    [[nodiscard]] bool IsGraphImplementation() const noexcept;
    [[nodiscard]] bool RequireGraph(FMaterialEditorOperationResult& Result) const;
    [[nodiscard]] bool IsParameterReferenced(
        const FMaterialParameterId& ParameterId) const;
    [[nodiscard]] bool ValidateParameterDefinition(
        const FMaterialParameterDefinition& Definition,
        FMaterialEditorOperationResult& Result) const;
    static void AddDiagnostic(FMaterialEditorOperationResult& Result,
        xr_string_view Code, xr_string Message,
        FMaterialNodeId Node = {}, FMaterialPinId Pin = {});

    static constexpr size_t MaxHistoryEntries = 128;

    FMaterialAsset MaterialAsset;
    xr_vector<FMaterialAsset> UndoHistory;
    xr_vector<FMaterialAsset> RedoHistory;
    xr_string SavedMaterial;
};
} // namespace Tiramisu::Editor
