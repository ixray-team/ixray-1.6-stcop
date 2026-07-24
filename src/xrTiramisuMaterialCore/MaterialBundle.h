#pragma once

#include "MaterialPass.h"
#include "MaterialTypes.h"

#include <cstdint>
#include <span>
#include <string>
#include <vector>

constexpr u32 MaterialBundleVersion = 2;

enum class EMaterialBundleRecordType : u8
{
    Master,
    FlattenedInstance
};

enum class EMaterialShaderBlobFormat : u8
{
    Dxil,
    SpirV
};

// Одна flattened material-запись в версионированном cooked bundle.
struct FMaterialBundleRecord
{
    EMaterialBundleRecordType Type = EMaterialBundleRecordType::Master;
    FMaterialAssetId Id;
    FMaterialAssetId MasterId;
    xr_string SourcePath;
    xr_string AssetPayload;
    xr_string GeneratedHlsl;
    xr_vector<xr_string> Dependencies;
};

// DXIL или SPIR-V blob одной material permutation и стадии.
struct FMaterialShaderBlob
{
    FMaterialAssetId MaterialId;
    u64 PipelineKey = 0;
    EMaterialShaderBlobFormat Format = EMaterialShaderBlobFormat::Dxil;
    xr_string EntryPoint;
    xr_vector<u8> Bytecode;
    EMaterialPass Pass = EMaterialPass::Validation;
    EMaterialShaderStage Stage = EMaterialShaderStage::Pixel;
    xr_string VertexFactory = "material_validation";
    xr_string RenderPassSignature = "validation:rgba8";
};

// Cooked-набор материалов, зависимостей и готовых shader blobs.
struct FMaterialBundle
{
    u32 Version = MaterialBundleVersion;
    bool CompleteShaderSet = false;
    xr_vector<FMaterialBundleRecord> Records;
    xr_vector<FMaterialShaderBlob> ShaderBlobs;
};

// Результат детерминированной сериализации material bundle.
struct FMaterialBundleWriteResult
{
    xr_vector<u8> Data;
    xr_vector<FMaterialDiagnostic> Diagnostics;

    [[nodiscard]] bool Succeeded() const noexcept;
};

// Результат проверки и чтения material bundle.
struct FMaterialBundleReadResult
{
    FMaterialBundle Value;
    xr_vector<FMaterialDiagnostic> Diagnostics;

    [[nodiscard]] bool Succeeded() const noexcept;
};

// Сериализует cooked bundle детерминированно и проверяет его границы при чтении.
[[nodiscard]] FMaterialBundleWriteResult SerializeMaterialBundle(const FMaterialBundle& Bundle);
[[nodiscard]] FMaterialBundleReadResult DeserializeMaterialBundle(xr_span<const u8> Data);
