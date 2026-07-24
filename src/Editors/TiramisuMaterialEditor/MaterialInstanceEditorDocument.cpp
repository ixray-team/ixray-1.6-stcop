#include "MaterialInstanceEditorDocument.h"
#include "MaterialEditorFileIO.h"

#include <fstream>
#include <iterator>
#include <ranges>
#include <utility>

namespace Tiramisu::Editor
{

TiramisuMaterialInstanceEditorDocument::TiramisuMaterialInstanceEditorDocument()
{
    NewInstance();
}

void TiramisuMaterialInstanceEditorDocument::NewInstance(xr_string Parent)
{
    InstanceAsset = {};
    InstanceAsset.Id.Value = GenerateMaterialGuid();
    InstanceAsset.Name = "New Material Instance";
    InstanceAsset.Parent = std::move(Parent);
    ClearParentMaterial();
    UndoHistory.clear();
    RedoHistory.clear();
    MarkSaved();
}

void TiramisuMaterialInstanceEditorDocument::OpenInstance(FMaterialInstanceAsset Instance)
{
    InstanceAsset = std::move(Instance);
    ClearParentMaterial();
    UndoHistory.clear();
    RedoHistory.clear();
    MarkSaved();
}

FMaterialEditorOperationResult TiramisuMaterialInstanceEditorDocument::OpenInstanceJson(
    const xr_string_view JsonText, const xr_string_view SourcePath)
{
    FMaterialEditorOperationResult Result;
    FMaterialInstanceParseResult Parsed = ParseMaterialInstanceJson(JsonText, SourcePath);
    Result.Diagnostics = Parsed.Diagnostics;
    if (Parsed.Succeeded())
    {
        const bool Migrated = std::ranges::any_of(Result.Diagnostics,
            [](const FMaterialDiagnostic& Diagnostic)
            { return Diagnostic.Code.starts_with("asset.migrated_"); });
        OpenInstance(std::move(Parsed.Value));
        if (Migrated)
            SavedInstance.clear();
    }
    return Result;
}

FMaterialEditorOperationResult TiramisuMaterialInstanceEditorDocument::OpenInstanceFile(
    const std::filesystem::path& Path)
{
    FMaterialEditorOperationResult Result;
    std::ifstream Input(Path, std::ios::binary);
    if (!Input)
    {
        AddDiagnostic(Result, "editor.instance_open_failed",
            "Cannot open material instance '" + ToXrString(Path.string()) + "'.");
        return Result;
    }
    const std::string Text{std::istreambuf_iterator<char>(Input),
        std::istreambuf_iterator<char>()};
    const xr_string JsonText = ToXrString(Text);
    return OpenInstanceJson(JsonText, Path.generic_string());
}

FMaterialEditorOperationResult TiramisuMaterialInstanceEditorDocument::SaveInstanceFile(
    const std::filesystem::path& Path)
{
    FMaterialEditorOperationResult Result;
    const xr_string JsonText = SerializeInstance();
    FMaterialInstanceParseResult Validation =
        ParseMaterialInstanceJson(JsonText, Path.generic_string());
    Result.Diagnostics.insert(Result.Diagnostics.end(),
        Validation.Diagnostics.begin(), Validation.Diagnostics.end());
    if (!Validation.Succeeded())
        return Result;

    const FAtomicTextFileWriteResult WriteResult = WriteTextFileAtomically(Path, JsonText);
    if (!WriteResult.Success)
    {
        AddDiagnostic(Result, "editor.instance_save_failed",
            "Cannot save material instance '" + ToXrString(Path.string()) + "': " + WriteResult.Error);
        return Result;
    }

    InstanceAsset.SourcePath = Path.generic_string();
    MarkSaved();
    return Result;
}

FMaterialEditorOperationResult TiramisuMaterialInstanceEditorDocument::SaveRecoveryFile(
    const std::filesystem::path& RecoveryPath) const
{
    FMaterialEditorOperationResult Result = ValidateOverrides();
    if (!Result.Succeeded())
        return Result;

    const xr_string JsonText = SerializeInstance();
    FMaterialInstanceParseResult Validation = ParseMaterialInstanceJson(
        JsonText, InstanceAsset.SourcePath);
    Result.Diagnostics.insert(Result.Diagnostics.end(),
        Validation.Diagnostics.begin(), Validation.Diagnostics.end());
    if (!Validation.Succeeded())
        return Result;

    const FAtomicTextFileWriteResult WriteResult =
        WriteTextFileAtomically(RecoveryPath, JsonText);
    if (!WriteResult.Success)
        AddDiagnostic(Result, "editor.instance_autosave_failed",
            "Cannot write material instance recovery file '" +
                ToXrString(RecoveryPath.string()) + "': " + WriteResult.Error);
    return Result;
}

FMaterialEditorOperationResult TiramisuMaterialInstanceEditorDocument::OpenRecoveryFile(
    const std::filesystem::path& RecoveryPath,
    const std::filesystem::path& OriginalPath)
{
    FMaterialEditorOperationResult Result;
    std::ifstream Input(RecoveryPath, std::ios::binary);
    if (!Input)
    {
        AddDiagnostic(Result, "editor.instance_recovery_open_failed",
            "Cannot open material instance recovery file '" +
                ToXrString(RecoveryPath.string()) + "'.");
        return Result;
    }
    const std::string Text{std::istreambuf_iterator<char>(Input),
        std::istreambuf_iterator<char>()};
    const xr_string JsonText = ToXrString(Text);
    Result = OpenInstanceJson(JsonText, OriginalPath.generic_string());
    if (Result.Succeeded())
        SavedInstance.clear();
    return Result;
}

void TiramisuMaterialInstanceEditorDocument::SetParentMaterial(FMaterialAsset Material)
{
    FResolvedMaterialInstance Resolution;
    Resolution.MasterId = Material.Id;
    Resolution.Domain = Material.Domain;
    Resolution.BlendMode = Material.BlendMode;
    Resolution.ShadingModel = Material.ShadingModel;
    Resolution.TwoSided = Material.TwoSided;
    for (const FMaterialParameterDefinition& Parameter : Material.Parameters)
    {
        if (Parameter.IsStatic())
            Resolution.StaticParameters.emplace(Parameter.Id, Parameter.DefaultValue);
        else
            Resolution.Parameters.emplace(Parameter.Id, Parameter.DefaultValue);
    }
    ParentResolution = std::move(Resolution);
    ParentMaterial = std::move(Material);
}

FMaterialEditorOperationResult
TiramisuMaterialInstanceEditorDocument::SetParentResolution(
    FMaterialAsset Material, FResolvedMaterialInstance Resolution)
{
    FMaterialEditorOperationResult Result;
    if (!Material.Id.IsValid() || Resolution.MasterId != Material.Id)
    {
        AddDiagnostic(Result, "editor.instance_parent_resolution_mismatch",
            "Resolved parent does not refer to the supplied master material.");
        return Result;
    }
    ParentMaterial = std::move(Material);
    ParentResolution = std::move(Resolution);
    return Result;
}

void TiramisuMaterialInstanceEditorDocument::ClearParentMaterial() noexcept
{
    ParentMaterial.reset();
    ParentResolution.reset();
}

const FMaterialValue* TiramisuMaterialInstanceEditorDocument::GetInheritedValue(
    const FMaterialParameterId& ParameterId, const bool Static) const noexcept
{
    if (!ParentResolution)
        return nullptr;
    const FMaterialParameterMap& Parameters = Static
        ? ParentResolution->StaticParameters : ParentResolution->Parameters;
    const auto Value = Parameters.find(ParameterId);
    return Value == Parameters.end() ? nullptr : &Value->second;
}

FMaterialStaticParameterSet
TiramisuMaterialInstanceEditorDocument::GetEffectiveStaticParameters() const
{
    FMaterialStaticParameterSet Parameters = ParentResolution
        ? ParentResolution->StaticParameters : FMaterialStaticParameterSet{};
    for (const auto& [Parameter, Value] : InstanceAsset.StaticOverrides)
        Parameters[Parameter] = Value;
    return Parameters;
}

bool TiramisuMaterialInstanceEditorDocument::SetName(xr_string Name)
{
    if (InstanceAsset.Name == Name)
        return false;
    RecordMutation();
    InstanceAsset.Name = std::move(Name);
    return true;
}

FMaterialEditorOperationResult TiramisuMaterialInstanceEditorDocument::SetParent(xr_string Parent)
{
    FMaterialEditorOperationResult Result;
    if (Parent.empty())
    {
        AddDiagnostic(Result, "editor.instance_parent_missing",
            "A material instance parent cannot be empty.");
        return Result;
    }
    if ((!InstanceAsset.Overrides.empty() || !InstanceAsset.StaticOverrides.empty()) &&
        InstanceAsset.Parent != Parent)
    {
        AddDiagnostic(Result, "editor.instance_parent_has_overrides",
            "Remove overrides before changing the material instance parent.");
        return Result;
    }
    if (InstanceAsset.Parent == Parent)
        return Result;

    RecordMutation();
    InstanceAsset.Parent = std::move(Parent);
    ClearParentMaterial();
    return Result;
}

FMaterialEditorOperationResult TiramisuMaterialInstanceEditorDocument::SetOverride(
    const FMaterialParameterId& ParameterId, FMaterialValue Value, const bool Static)
{
    FMaterialEditorOperationResult Result;
    if (!ParentMaterial)
    {
        AddDiagnostic(Result, "editor.instance_parent_schema_missing",
            "Load the parent master material before editing overrides.");
        return Result;
    }
    const FMaterialParameterDefinition* Definition =
        ParentMaterial->FindParameter(ParameterId);
    if (!Definition)
    {
        AddDiagnostic(Result, "editor.instance_unknown_parameter",
            "The parent master material does not declare this parameter.");
        return Result;
    }
    if (Definition->IsStatic() != Static)
    {
        AddDiagnostic(Result, "editor.instance_override_storage_mismatch",
            Static ? "A runtime parameter cannot be stored as a static override."
                   : "A static parameter cannot be stored as a runtime override.");
        return Result;
    }
    if (!ValueMatchesParameterType(Value, Definition->Type))
    {
        AddDiagnostic(Result, "editor.instance_override_type_mismatch",
            "Override value does not match the parent parameter type.");
        return Result;
    }

    FMaterialParameterMap& Overrides = Static
        ? InstanceAsset.StaticOverrides : InstanceAsset.Overrides;
    const auto Existing = Overrides.find(ParameterId);
    if (Existing != Overrides.end() && Existing->second == Value)
        return Result;

    RecordMutation();
    Overrides[ParameterId] = std::move(Value);
    return Result;
}

FMaterialEditorOperationResult TiramisuMaterialInstanceEditorDocument::RemoveOverride(
    const FMaterialParameterId& ParameterId)
{
    FMaterialEditorOperationResult Result;
    if (!InstanceAsset.Overrides.contains(ParameterId) &&
        !InstanceAsset.StaticOverrides.contains(ParameterId))
    {
        AddDiagnostic(Result, "editor.instance_override_missing",
            "The material instance has no override for this parameter.");
        return Result;
    }

    RecordMutation();
    InstanceAsset.Overrides.erase(ParameterId);
    InstanceAsset.StaticOverrides.erase(ParameterId);
    return Result;
}

FMaterialEditorOperationResult TiramisuMaterialInstanceEditorDocument::ValidateOverrides() const
{
    FMaterialEditorOperationResult Result;
    if (InstanceAsset.Parent.empty())
    {
        AddDiagnostic(Result, "editor.instance_parent_missing",
            "A material instance parent cannot be empty.");
        return Result;
    }
    if (!ParentMaterial &&
        (!InstanceAsset.Overrides.empty() || !InstanceAsset.StaticOverrides.empty()))
    {
        AddDiagnostic(Result, "editor.instance_parent_schema_missing",
            "Load the parent master material to validate overrides before saving.");
        return Result;
    }
    if (!ParentMaterial)
        return Result;

    const auto ValidateMap = [this, &Result](const FMaterialParameterMap& Overrides,
                                 const bool Static)
    {
        for (const auto& [Id, Value] : Overrides)
        {
            const FMaterialParameterDefinition* Definition = ParentMaterial->FindParameter(Id);
            if (!Definition)
            {
                AddDiagnostic(Result, "editor.instance_unknown_parameter",
                    "Override references a parameter missing from the parent master.");
            }
            else if (Definition->IsStatic() != Static)
            {
                AddDiagnostic(Result, "editor.instance_override_storage_mismatch",
                    "Override is stored in the wrong runtime/static map.");
            }
            else if (!ValueMatchesParameterType(Value, Definition->Type))
            {
                AddDiagnostic(Result, "editor.instance_override_type_mismatch",
                    "Override value does not match the parent parameter type.");
            }
        }
    };
    ValidateMap(InstanceAsset.Overrides, false);
    ValidateMap(InstanceAsset.StaticOverrides, true);
    return Result;
}

xr_string TiramisuMaterialInstanceEditorDocument::SerializeInstance() const
{
    return SerializeMaterialInstanceJson(InstanceAsset);
}

xr_string TiramisuMaterialInstanceEditorDocument::SerializeFlattenedInstance() const
{
    if (!ParentResolution)
        return SerializeInstance();

    FMaterialInstanceAsset Flattened = InstanceAsset;
    Flattened.Parent = ParentResolution->MasterId.Value;
    Flattened.Overrides = ParentResolution->Parameters;
    Flattened.StaticOverrides = ParentResolution->StaticParameters;
    for (const auto& [Parameter, Value] : InstanceAsset.Overrides)
        Flattened.Overrides[Parameter] = Value;
    for (const auto& [Parameter, Value] : InstanceAsset.StaticOverrides)
        Flattened.StaticOverrides[Parameter] = Value;
    return SerializeMaterialInstanceJson(Flattened);
}

bool TiramisuMaterialInstanceEditorDocument::Undo()
{
    if (UndoHistory.empty())
        return false;
    const xr_string PreviousParent = InstanceAsset.Parent;
    RedoHistory.push_back(std::move(InstanceAsset));
    InstanceAsset = std::move(UndoHistory.back());
    UndoHistory.pop_back();
    if (InstanceAsset.Parent != PreviousParent)
        ClearParentMaterial();
    return true;
}

bool TiramisuMaterialInstanceEditorDocument::Redo()
{
    if (RedoHistory.empty())
        return false;
    const xr_string PreviousParent = InstanceAsset.Parent;
    UndoHistory.push_back(std::move(InstanceAsset));
    InstanceAsset = std::move(RedoHistory.back());
    RedoHistory.pop_back();
    if (InstanceAsset.Parent != PreviousParent)
        ClearParentMaterial();
    return true;
}

bool TiramisuMaterialInstanceEditorDocument::IsDirty() const
{
    return SerializeInstance() != SavedInstance;
}

void TiramisuMaterialInstanceEditorDocument::MarkSaved()
{
    SavedInstance = SerializeInstance();
}

void TiramisuMaterialInstanceEditorDocument::RecordMutation()
{
    if (UndoHistory.size() == MaxHistoryEntries)
        UndoHistory.erase(UndoHistory.begin());
    UndoHistory.push_back(InstanceAsset);
    RedoHistory.clear();
}

void TiramisuMaterialInstanceEditorDocument::AddDiagnostic(
    FMaterialEditorOperationResult& Result, const xr_string_view Code,
    xr_string Message)
{
    Result.Diagnostics.push_back({EMaterialDiagnosticSeverity::Error,
        xr_string(Code), std::move(Message), {}, {}});
}
} // namespace Tiramisu::Editor
