#include "SceneConversionDump.h"
#include "SceneJsonHelpers.h"

#include <nlohmann/json.hpp>

#include <limits>

namespace Tiramisu::Scene
{
namespace
{
using Json = nlohmann::json;

bool ReadString(const Json& Object, const char* Name, xr_string& Result)
{
    const auto Found = Object.find(Name);
    if (Found == Object.end() || !Found->is_string())
        return false;
    Result = Found->get<xr_string>();
    return true;
}

bool ReadUint32(const Json& Object, const char* Name, u32& Result)
{
    const auto Found = Object.find(Name);
    if (Found == Object.end() || !Found->is_number_unsigned())
        return false;
    const u64 Value = Found->get<u64>();
    if (Value > std::numeric_limits<u32>::max())
        return false;
    Result = static_cast<u32>(Value);
    return true;
}
} // namespace

xr_string_view ToString(const ESceneConversionStatus Status) noexcept
{
    return Status == ESceneConversionStatus::Succeeded
        ? "succeeded" : "failed";
}

xr_string SerializeSceneConversionDumpJson(
    const FSceneConversionDump& Dump)
{
    Json Root = {
        {"asset_version", Dump.Version},
        {"status", ToString(Dump.Status)},
        {"importer", Dump.Importer},
        {"importer_version", Dump.ImporterVersion},
        {"source_type", Dump.SourceType},
        {"source_path", Dump.SourcePath},
        {"source_hash", Dump.SourceHash},
        {"target_path", Dump.TargetPath},
        {"target_payload_path", Dump.TargetPayloadPath},
        {"target_asset_guid", Dump.TargetAssetId},
        {"counts", {
            {"meshes", Dump.MeshCount},
            {"vertices", Dump.VertexCount},
            {"indices", Dump.IndexCount},
            {"components", Dump.ComponentCount},
            {"created_material_instances",
                Dump.CreatedMaterialInstances},
            {"reused_material_instances",
                Dump.ReusedMaterialInstances}}}};
    Root["asset_mappings"] = Json::array();
    for (const FSceneConversionAssetMapping& Mapping :
        Dump.AssetMappings)
    {
        Root["asset_mappings"].push_back({
            {"source", Mapping.Source},
            {"target", Mapping.Target},
            {"dump", Mapping.Dump},
            {"asset_guid", Mapping.AssetId},
            {"target_payload", Mapping.TargetPayload}});
    }
    Root["material_mappings"] = Json::array();
    for (const FSceneConversionMaterialMapping& Mapping :
        Dump.MaterialMappings)
    {
        Root["material_mappings"].push_back({
            {"surface", Mapping.Surface},
            {"source_key", Mapping.SourceKey},
            {"material_instance", Mapping.MaterialInstance},
            {"two_sided", Mapping.TwoSided},
            {"created", Mapping.Created}});
    }
    Root["diagnostics"] = Json::array();
    for (const FSceneConversionDiagnostic& Diagnostic : Dump.Diagnostics)
    {
        Root["diagnostics"].push_back({
            {"severity", Diagnostic.Severity},
            {"code", Diagnostic.Code},
            {"message", Diagnostic.Message}});
    }
    return Root.dump(2);
}

FSceneConversionDumpParseResult ParseSceneConversionDumpJson(
    const xr_string_view JsonText)
{
    FSceneConversionDumpParseResult Result;
    const Json Root = Json::parse(JsonText, nullptr, false);
    if (Root.is_discarded() || !Root.is_object())
    {
        Result.Diagnostic = "Conversion dump must contain a JSON object";
        return Result;
    }
    xr_string Status;
    bool Valid =
        ReadUint32(Root, "asset_version", Result.Value.Version) &&
        ReadString(Root, "status", Status) &&
        ReadString(Root, "importer", Result.Value.Importer) &&
        ReadUint32(Root, "importer_version",
            Result.Value.ImporterVersion) &&
        ReadString(Root, "source_type", Result.Value.SourceType) &&
        ReadString(Root, "source_path", Result.Value.SourcePath) &&
        ReadString(Root, "source_hash", Result.Value.SourceHash) &&
        ReadString(Root, "target_path", Result.Value.TargetPath) &&
        ReadString(Root, "target_asset_guid",
            Result.Value.TargetAssetId);
    if (Result.Value.Version == SceneConversionDumpVersion)
    {
        Valid = Valid && ReadString(Root, "target_payload_path",
            Result.Value.TargetPayloadPath);
    }
    if (Status == "succeeded")
        Result.Value.Status = ESceneConversionStatus::Succeeded;
    else if (Status == "failed")
        Result.Value.Status = ESceneConversionStatus::Failed;
    else
        Valid = false;

    const auto Counts = Root.find("counts");
    Valid = Valid && Counts != Root.end() && Counts->is_object() &&
        ReadUint32(*Counts, "meshes", Result.Value.MeshCount) &&
        ReadUint32(*Counts, "vertices", Result.Value.VertexCount) &&
        ReadUint32(*Counts, "indices", Result.Value.IndexCount) &&
        ReadUint32(*Counts, "components", Result.Value.ComponentCount) &&
        ReadUint32(*Counts, "created_material_instances",
            Result.Value.CreatedMaterialInstances) &&
        ReadUint32(*Counts, "reused_material_instances",
            Result.Value.ReusedMaterialInstances);

    const auto Assets = Root.find("asset_mappings");
    Valid = Valid && Assets != Root.end() && Assets->is_array();
    if (Valid)
    {
        for (const Json& Item : *Assets)
        {
            FSceneConversionAssetMapping Mapping;
            if (!Item.is_object() ||
                !ReadString(Item, "source", Mapping.Source) ||
                !ReadString(Item, "target", Mapping.Target) ||
                !ReadString(Item, "dump", Mapping.Dump) ||
                !ReadString(Item, "asset_guid", Mapping.AssetId))
            {
                Valid = false;
                break;
            }
            const auto Payload = Item.find("target_payload");
            if (Payload != Item.end())
            {
                if (!Payload->is_string())
                {
                    Valid = false;
                    break;
                }
                Mapping.TargetPayload = Payload->get<xr_string>();
            }
            Result.Value.AssetMappings.push_back(std::move(Mapping));
        }
    }
    const auto Mappings = Root.find("material_mappings");
    Valid = Valid && Mappings != Root.end() && Mappings->is_array();
    if (Valid)
    {
        for (const Json& Item : *Mappings)
        {
            FSceneConversionMaterialMapping Mapping;
            const auto TwoSided = Item.find("two_sided");
            const auto Created = Item.find("created");
            const bool ItemValid = Item.is_object() &&
                ReadString(Item, "surface", Mapping.Surface) &&
                ReadString(Item, "source_key", Mapping.SourceKey) &&
                ReadString(Item, "material_instance",
                    Mapping.MaterialInstance) &&
                TwoSided != Item.end() && TwoSided->is_boolean() &&
                Created != Item.end() && Created->is_boolean();
            if (!ItemValid)
            {
                Valid = false;
                break;
            }
            Mapping.TwoSided = TwoSided->get<bool>();
            Mapping.Created = Created->get<bool>();
            Result.Value.MaterialMappings.push_back(std::move(Mapping));
        }
    }
    const auto Diagnostics = Root.find("diagnostics");
    Valid = Valid && Diagnostics != Root.end() &&
        Diagnostics->is_array();
    if (Valid)
    {
        for (const Json& Item : *Diagnostics)
        {
            FSceneConversionDiagnostic Diagnostic;
            if (!Item.is_object() ||
                !ReadString(Item, "severity", Diagnostic.Severity) ||
                !ReadString(Item, "code", Diagnostic.Code) ||
                !ReadString(Item, "message", Diagnostic.Message))
            {
                Valid = false;
                break;
            }
            Result.Value.Diagnostics.push_back(std::move(Diagnostic));
        }
    }
    if (!Valid ||
        (Result.Value.Version != SceneConversionDumpVersion &&
            Result.Value.Version != LegacySceneConversionDumpVersion))
        Result.Diagnostic = "Conversion dump has an invalid or unsupported schema";
    return Result;
}
} // namespace Tiramisu::Scene
