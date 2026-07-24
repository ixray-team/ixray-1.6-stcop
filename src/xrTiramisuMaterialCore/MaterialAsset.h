#pragma once

#include "MaterialGraph.h"

#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

enum class EMaterialImplementationType : u8
{
    Hlsl,
    Graph
};

// Описание реализации master material: ручной HLSL include или expression graph.
struct FMaterialImplementation
{
    EMaterialImplementationType Type = EMaterialImplementationType::Hlsl;
    xr_string Source;
    FMaterialGraph Graph;
};

// Source-представление master material со стабильным GUID и shader contract.
struct FMaterialAsset
{
    u32 Version = MaterialAssetVersion;
    FMaterialAssetId Id;
    xr_string Name;
    xr_string SourcePath;
    EMaterialDomain Domain = EMaterialDomain::Surface;
    EMaterialBlendMode BlendMode = EMaterialBlendMode::Opaque;
    EMaterialShadingModel ShadingModel = EMaterialShadingModel::DefaultLit;
    bool TwoSided = false;
    xr_string HlslTemplate;
    FMaterialImplementation Implementation;
    xr_vector<FMaterialParameterDefinition> Parameters;
    xr_vector<xr_string> Dependencies;

    [[nodiscard]] const FMaterialParameterDefinition* FindParameter(const FMaterialParameterId& Parameter) const noexcept;
};

// Source-представление instance с parent reference и parameter overrides.
struct FMaterialInstanceAsset
{
    u32 Version = MaterialAssetVersion;
    FMaterialAssetId Id;
    xr_string Name;
    xr_string SourcePath;
    xr_string Parent;
    FMaterialParameterMap Overrides;
    FMaterialStaticParameterSet StaticOverrides;
};

template <typename T>
// Типизированный результат разбора material JSON без исключений на границе API.
struct TMaterialParseResult
{
    T Value;
    xr_vector<FMaterialDiagnostic> Diagnostics;

    [[nodiscard]] bool Succeeded() const noexcept
    {
        for (const FMaterialDiagnostic& Diagnostic : Diagnostics)
            if (Diagnostic.Severity == EMaterialDiagnosticSeverity::Error)
                return false;
        return true;
    }
};

using FMaterialAssetParseResult = TMaterialParseResult<FMaterialAsset>;
using FMaterialInstanceParseResult = TMaterialParseResult<FMaterialInstanceAsset>;

// Парсит source assets; ошибки возвращаются diagnostics, а не исключениями.
[[nodiscard]] FMaterialAssetParseResult ParseMaterialAssetJson(
    xr_string_view JsonText, xr_string_view SourcePath = {});
[[nodiscard]] FMaterialInstanceParseResult ParseMaterialInstanceJson(
    xr_string_view JsonText, xr_string_view SourcePath = {});
[[nodiscard]] xr_string SerializeMaterialAssetJson(const FMaterialAsset& Asset);
[[nodiscard]] xr_string SerializeMaterialInstanceJson(const FMaterialInstanceAsset& Instance);

// Flattened instance после разрешения parent chain и наследования overrides.
struct FResolvedMaterialInstance
{
    FMaterialHandle MasterHandle;
    FMaterialAssetId MasterId;
    EMaterialDomain Domain = EMaterialDomain::Surface;
    EMaterialBlendMode BlendMode = EMaterialBlendMode::Opaque;
    EMaterialShadingModel ShadingModel = EMaterialShadingModel::DefaultLit;
    bool TwoSided = false;
    FMaterialParameterMap Parameters;
    FMaterialStaticParameterSet StaticParameters;
    xr_vector<FMaterialAssetId> ParentChain;
};

// Результат flattening instance вместе с диагностикой циклов и типов.
struct FMaterialResolveResult
{
    FResolvedMaterialInstance Value;
    xr_vector<FMaterialDiagnostic> Diagnostics;

    [[nodiscard]] bool Succeeded() const noexcept;
};

// Результат регистрации asset в библиотеке с generation-counted handle.
struct FMaterialRegistrationResult
{
    FMaterialHandle Handle;
    xr_vector<FMaterialDiagnostic> Diagnostics;

    [[nodiscard]] bool Succeeded() const noexcept;
};

// Общая библиотека master/instance assets для renderer, cooker, editor и тестов.
class TiramisuMaterialLibrary
{
public:
    // Регистрирует, перезагружает и разрешает assets через стабильные handles.
    [[nodiscard]] FMaterialRegistrationResult RegisterMaster(FMaterialAsset Asset);
    [[nodiscard]] FMaterialRegistrationResult RegisterInstance(FMaterialInstanceAsset Instance);
    [[nodiscard]] bool ReloadMaster(FMaterialHandle Handle, FMaterialAsset Asset);
    [[nodiscard]] bool ReloadInstance(FMaterialHandle Handle, FMaterialInstanceAsset Instance);
    [[nodiscard]] bool RemoveMaster(FMaterialHandle Handle);
    [[nodiscard]] bool RemoveInstance(FMaterialHandle Handle);

    [[nodiscard]] const FMaterialAsset* GetMaster(FMaterialHandle Handle) const noexcept;
    [[nodiscard]] const FMaterialInstanceAsset* GetInstance(FMaterialHandle Handle) const noexcept;
    [[nodiscard]] xr_optional<FMaterialHandle> FindMaster(xr_string_view IdOrPath) const;
    [[nodiscard]] xr_optional<FMaterialHandle> FindInstance(xr_string_view IdOrPath) const;
    [[nodiscard]] FMaterialResolveResult Resolve(xr_string_view MasterOrInstance) const;
    [[nodiscard]] TiramisuMaterialInstanceDynamic CreateDynamic(const FResolvedMaterialInstance& Resolved) const;

private:
    bool ResolveRecursive(xr_string_view Reference, xr_vector<xr_string>& ActiveReferences,
        FResolvedMaterialInstance& Resolved, xr_vector<FMaterialDiagnostic>& Diagnostics) const;
    void IndexMaster(const FMaterialAsset& Asset, FMaterialHandle Handle);
    void IndexInstance(const FMaterialInstanceAsset& Instance, FMaterialHandle Handle);
    void RemoveMasterIndices(const FMaterialAsset& Asset);
    void RemoveInstanceIndices(const FMaterialInstanceAsset& Instance);

    TGenerationPool<FMaterialAsset> Masters;
    TGenerationPool<FMaterialInstanceAsset> Instances;
    xr_hash_map<xr_string, FMaterialHandle> MasterReferences;
    xr_hash_map<xr_string, FMaterialHandle> InstanceReferences;
};
