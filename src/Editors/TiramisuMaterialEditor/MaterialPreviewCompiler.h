#pragma once

#include "TiramisuMaterialEditorTypes.h"

#include <MaterialAsset.h>
#include <MaterialParameterLayout.h>
#include <MaterialPass.h>
#include <TiramisuMaterialShaderCompiler.h>

#include <filesystem>
#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

namespace Tiramisu::Editor
{
// Владеющий запрос background-компиляции material preview или viewport pass.
struct FMaterialPreviewCompileRequest
{
    EMaterialShaderBackend Backend = EMaterialShaderBackend::D3D12;
    // Владеющие строки позволяют безопасно переместить запрос в background compiler
    // независимо от lifetime editor document.
    xr_string MaterialJson;
    xr_string MaterialInstanceJson;
    xr_string GeneratedHlsl;
    xr_string TemplateSource;
    xr_string VertexFactorySource;
    // Универсальный pass source используют preview и основной viewport; старое поле
    // PreviewPassSource сохранено только для поэтапной миграции callers.
    EMaterialPass Pass = EMaterialPass::Validation;
    xr_string PassSource;
    xr_string PreviewPassSource;
    xr_string RenderPassSignature = "material_preview:rgba8:d32";
    xr_string CompilerOptions = "editor_preview_v2";
    // Содержимое transitive includes входит в deterministic key; реальные includes
    // DXC по-прежнему разрешает через IncludeDirectories.
    xr_vector<xr_string> DependencySources;
    xr_vector<std::filesystem::path> IncludeDirectories;
    bool Debug = false;
};

// NRI-независимый результат. Renderer патчит resource indices под свой descriptor heap
// до создания или обновления preview material instance.
struct FMaterialPreviewCompileResult
{
    FMaterialAssetId MaterialId;
    FResolvedMaterialInstance ResolvedMaterial;
    FMaterialPackedParameterBlock ParameterBlock;
    xr_vector<u8> VertexBytecode;
    xr_vector<u8> PixelBytecode;
    u64 PipelineKey = 0;
    xr_vector<FMaterialDiagnostic> Diagnostics;

    [[nodiscard]] bool Succeeded() const noexcept;
};

// Счётчики общего cache editor shader compilation.
struct FMaterialPreviewShaderCacheStatistics
{
    size_t RequestCount = 0;
    size_t HitCount = 0;
    size_t CompileCount = 0;
    size_t EntryCount = 0;
};

// Общий compiler frontend редактора выполняет то же разрешение instances, packing,
// template assembly и DXC, что cooker. GPU pipeline создаёт renderer.
[[nodiscard]] FMaterialPreviewCompileResult CompileMaterialPreview(
    const FMaterialPreviewCompileRequest& Request);

// Runtime values исключены из permutation key, поэтому одинаковые master/static/pass/API
// запросы разделяют один in-flight DXC compile.
[[nodiscard]] FMaterialPreviewShaderCacheStatistics
GetMaterialPreviewShaderCacheStatistics();
void ResetMaterialPreviewShaderCacheForTests();
} // namespace Tiramisu::Editor
