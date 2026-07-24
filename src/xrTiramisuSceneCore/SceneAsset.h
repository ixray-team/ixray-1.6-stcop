#pragma once

#include "TiramisuSceneTypes.h"

#include <array>
#include <cstdint>
#include <filesystem>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace Tiramisu::Scene
{
inline constexpr u32 LegacyInlineStaticMeshAssetVersion = 1;
inline constexpr u32 StaticMeshAssetVersion = 2;
inline constexpr u32 StaticMeshBinaryVersion = 1;
inline constexpr u32 StaticMeshBinaryVertexStride = 60;
inline constexpr u32 StaticMeshBinaryIndexStride = 4;
inline constexpr u32 LegacyStaticMeshOnlyRenderSceneAssetVersion = 1;
inline constexpr u32 RenderSceneAssetVersion = 2;

enum class ESceneDiagnosticSeverity : u8
{
    Warning,
    Error
};

// Диагностика scene asset loader с кодом, path и severity.
struct FSceneDiagnostic
{
    ESceneDiagnosticSeverity Severity = ESceneDiagnosticSeverity::Error;
    xr_string Code;
    xr_string Message;
    xr_string Source;
};

// Стабильный material slot static mesh asset с default material reference.
struct FStaticMeshMaterialSlot
{
    xr_string Name;
    // GUID/path master material или MaterialInstance. Новый scene format никогда не хранит
    // здесь имя legacy shaders.xr.
    xr_string Material;
    // Import metadata для legacy surfaces. У native materials значение обычно
    // наследуется из master asset.
    bool TwoSided = false;
};

// Canonical вершина нового static mesh формата.
struct FStaticMeshVertex
{
    xr_array<float, 3> Position = {};
    xr_array<float, 3> Normal = {0.0f, 1.0f, 0.0f};
    xr_array<float, 4> Tangent = {1.0f, 0.0f, 0.0f, 1.0f};
    xr_array<float, 2> TexCoord0 = {};
    xr_array<float, 2> TexCoord1 = {};
    u32 Color = 0xffffffffu;
};

// Диапазон индексов одного material slot внутри static mesh.
struct FStaticMeshSection
{
    u32 FirstIndex = 0;
    u32 IndexCount = 0;
    u32 MaterialSlot = 0;
};

// Метаданные внешнего binary payload с layout, counts и hash.
struct FStaticMeshGeometryStorage
{
    // Relative to the metadata JSON. Native v2 assets keep bulk geometry out
    // of JSON so large meshes remain compact and fast to load.
    xr_string File;
    u32 BinaryVersion = StaticMeshBinaryVersion;
    u32 VertexStride = StaticMeshBinaryVertexStride;
    u32 IndexStride = StaticMeshBinaryIndexStride;
    u32 VertexCount = 0;
    u32 IndexCount = 0;
    // Lowercase 64-bit FNV-1a of the binary payload after its header.
    xr_string ContentHash;
};

// Текстовое описание StaticMesh и загруженная canonical geometry.
struct FStaticMeshAsset
{
    u32 Version = StaticMeshAssetVersion;
    xr_string Id;
    xr_string Name;
    xr_string SourcePath;
    FStaticMeshGeometryStorage Geometry;
    xr_vector<FStaticMeshMaterialSlot> MaterialSlots;
    xr_vector<FStaticMeshVertex> Vertices;
    xr_vector<u32> Indices;
    xr_vector<FStaticMeshSection> Sections;
};

// Per-component замена material slot без дублирования geometry asset.
struct FStaticMeshMaterialOverride
{
    u32 MaterialSlot = 0;
    // Explicit master/MaterialInstance GUID or path. Legacy shader names are
    // resolved during import and never stored in a native render scene.
    xr_string Material;
    bool TwoSided = false;
};

// Экземпляр StaticMesh в RenderScene с transform и overrides.
struct FStaticMeshComponent
{
    xr_string Id;
    xr_string Name;
    xr_string StaticMesh;
    xr_array<float, 16> LocalToWorld = {
        1.0f, 0.0f, 0.0f, 0.0f,
        0.0f, 1.0f, 0.0f, 0.0f,
        0.0f, 0.0f, 1.0f, 0.0f,
        0.0f, 0.0f, 0.0f, 1.0f};
    bool Visible = true;
    xr_vector<FStaticMeshMaterialOverride> MaterialOverrides;
};

enum class ELightType : u8
{
    Directional,
    Point,
    Spot
};

// Native Directional, Point или Spot light нового scene format.
struct FLightComponent
{
    xr_string Id;
    xr_string Name;
    ELightType Type = ELightType::Point;
    xr_array<float, 16> LocalToWorld = {
        1.0f, 0.0f, 0.0f, 0.0f,
        0.0f, 1.0f, 0.0f, 0.0f,
        0.0f, 0.0f, 1.0f, 0.0f,
        0.0f, 0.0f, 0.0f, 1.0f};
    // Linear HDR RGB multiplier. Physical/legacy unit conversion is handled
    // by importers rather than encoded in the renderer-neutral scene format.
    xr_array<float, 3> Color = {1.0f, 1.0f, 1.0f};
    float Intensity = 1.0f;
    float Range = 10.0f;
    float InnerConeAngleDegrees = 20.0f;
    float OuterConeAngleDegrees = 45.0f;
    bool Visible = true;
    bool CastShadows = true;
};

// Текстовый native scene asset из компонентов и общих настроек.
struct FRenderSceneAsset
{
    u32 Version = RenderSceneAssetVersion;
    xr_string Id;
    xr_string Name;
    xr_string SourcePath;
    xr_vector<FStaticMeshComponent> StaticMeshComponents;
    xr_vector<FLightComponent> LightComponents;
};

template <typename T>
// Типизированный результат загрузки scene asset с diagnostics.
struct TSceneAssetParseResult
{
    T Value;
    xr_vector<FSceneDiagnostic> Diagnostics;

    [[nodiscard]] bool Succeeded() const noexcept;
};

using FStaticMeshAssetParseResult =
    TSceneAssetParseResult<FStaticMeshAsset>;
using FRenderSceneAssetParseResult =
    TSceneAssetParseResult<FRenderSceneAsset>;

// Результат атомарной записи парных StaticMesh JSON/BIN файлов.
struct FStaticMeshAssetWriteResult
{
    std::filesystem::path MetadataPath;
    std::filesystem::path GeometryPath;
    xr_vector<FSceneDiagnostic> Diagnostics;

    [[nodiscard]] bool Succeeded() const noexcept;
};

// RenderScene после загрузки и дедупликации всех referenced meshes.
struct FResolvedRenderScene
{
    FRenderSceneAsset Scene;
    // Keyed by the exact component reference stored in the scene. Asset GUIDs
    // remain available in each value and are used for stable renderer IDs.
    xr_hash_map<xr_string, FStaticMeshAsset> StaticMeshes;
};

// Результат полного разрешения RenderScene и её зависимостей.
struct FResolvedRenderSceneResult
{
    FResolvedRenderScene Value;
    xr_vector<FSceneDiagnostic> Diagnostics;

    [[nodiscard]] bool Succeeded() const noexcept;
};

// Читает и пишет metadata JSON без загрузки внешнего geometry payload.
[[nodiscard]] FStaticMeshAssetParseResult ParseStaticMeshAssetJson(
    xr_string_view JsonText, xr_string_view SourcePath = {});
[[nodiscard]] FRenderSceneAssetParseResult ParseRenderSceneAssetJson(
    xr_string_view JsonText, xr_string_view SourcePath = {});
[[nodiscard]] xr_string SerializeStaticMeshAssetJson(
    const FStaticMeshAsset& Asset);
[[nodiscard]] xr_string SerializeRenderSceneAssetJson(
    const FRenderSceneAsset& Asset);

// Writes a v2 metadata JSON plus a versioned binary geometry payload. Existing
// v1 inline-geometry JSON remains readable for one-way prototype migration.
// Атомарно сохраняет JSON/BIN пару и проверяет hash/strides/counts при загрузке.
[[nodiscard]] FStaticMeshAssetWriteResult SaveStaticMeshAsset(
    const std::filesystem::path& MetadataPath,
    const FStaticMeshAsset& Asset);
[[nodiscard]] FStaticMeshAssetParseResult LoadStaticMeshAsset(
    const std::filesystem::path& MetadataPath);
[[nodiscard]] std::filesystem::path MakeStaticMeshGeometryPath(
    const std::filesystem::path& MetadataPath);

// Загружает scene и все referenced static-mesh source assets. Относительные references
// разрешаются от scene file; absolute paths допустимы для editor tools, но cooker
// не должен записывать их в готовый asset.
// Разрешает RenderScene и дедуплицирует referenced StaticMesh assets по path/GUID.
[[nodiscard]] FResolvedRenderSceneResult LoadRenderSceneAsset(
    const std::filesystem::path& ScenePath);

// Общие операции над scene GUID, light types и deterministic mesh revision.
[[nodiscard]] bool IsValidSceneStableId(xr_string_view Value) noexcept;
[[nodiscard]] u64 StableSceneIdHash(xr_string_view Value) noexcept;
[[nodiscard]] xr_string_view ToString(ELightType Type) noexcept;
[[nodiscard]] bool TryParseLightType(
    xr_string_view Value, ELightType& OutType) noexcept;
[[nodiscard]] u64 CalculateStaticMeshRevision(
    const FStaticMeshAsset& Asset) noexcept;

template <typename T>
bool TSceneAssetParseResult<T>::Succeeded() const noexcept
{
    for (const FSceneDiagnostic& Diagnostic : Diagnostics)
    {
        if (Diagnostic.Severity == ESceneDiagnosticSeverity::Error)
            return false;
    }
    return true;
}
} // namespace Tiramisu::Scene
