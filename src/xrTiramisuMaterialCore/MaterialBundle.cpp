#include "MaterialBundle.h"

#include <algorithm>
#include <array>
#include <cstring>
#include <limits>
#include <ranges>
#include <stdexcept>
#include <tuple>

namespace
{
constexpr xr_array<u8, 8> BundleMagic = {'X', 'R', 'M', 'A', 'T', 'B', 'N', 'D'};
constexpr u64 FnvOffset = 14695981039346656037ull;
constexpr u64 FnvPrime = 1099511628211ull;
constexpr u32 MaximumCollectionSize = 1'000'000;
constexpr u32 MaximumStringSize = 64 * 1024 * 1024;

bool HasErrors(const xr_vector<FMaterialDiagnostic>& Diagnostics)
{
    return std::ranges::any_of(Diagnostics,
        [](const FMaterialDiagnostic& Diagnostic)
        { return Diagnostic.Severity == EMaterialDiagnosticSeverity::Error; });
}

void AddError(xr_vector<FMaterialDiagnostic>& Diagnostics, const xr_string_view Code, const xr_string& Message)
{
    Diagnostics.push_back({EMaterialDiagnosticSeverity::Error, xr_string(Code), Message, {}, {}});
}

u64 Checksum(const xr_span<const u8> Data)
{
    u64 Hash = FnvOffset;
    for (const u8 Byte : Data)
    {
        Hash ^= Byte;
        Hash *= FnvPrime;
    }
    return Hash;
}

class FBinaryWriter
{
public:
    void WriteU8(const u8 Value) { Data.push_back(Value); }

    void WriteU32(const u32 Value)
    {
        for (u32 Shift = 0; Shift < 32; Shift += 8)
            Data.push_back(static_cast<u8>((Value >> Shift) & 0xffu));
    }

    void WriteU64(const u64 Value)
    {
        for (u32 Shift = 0; Shift < 64; Shift += 8)
            Data.push_back(static_cast<u8>((Value >> Shift) & 0xffu));
    }

    void WriteString(const xr_string_view Value)
    {
        if (Value.size() > std::numeric_limits<u32>::max())
            throw std::runtime_error("String is too large for a material bundle.");
        WriteU32(static_cast<u32>(Value.size()));
        Data.insert(Data.end(), Value.begin(), Value.end());
    }

    void WriteBytes(const xr_span<const u8> Value)
    {
        if (Value.size() > std::numeric_limits<u32>::max())
            throw std::runtime_error("Bytecode is too large for a material bundle.");
        WriteU32(static_cast<u32>(Value.size()));
        Data.insert(Data.end(), Value.begin(), Value.end());
    }

    xr_vector<u8> Data;
};

class FBinaryReader
{
public:
    explicit FBinaryReader(const xr_span<const u8> InData) : Data(InData) {}

    u8 ReadU8()
    {
        Require(1);
        return Data[Position++];
    }

    u32 ReadU32()
    {
        Require(4);
        u32 Value = 0;
        for (u32 Shift = 0; Shift < 32; Shift += 8)
            Value |= static_cast<u32>(Data[Position++]) << Shift;
        return Value;
    }

    u64 ReadU64()
    {
        Require(8);
        u64 Value = 0;
        for (u32 Shift = 0; Shift < 64; Shift += 8)
            Value |= static_cast<u64>(Data[Position++]) << Shift;
        return Value;
    }

    xr_string ReadString()
    {
        const u32 Size = ReadU32();
        if (Size > MaximumStringSize)
            throw std::runtime_error("Material bundle string exceeds the safety limit.");
        Require(Size);
        const char* Begin = reinterpret_cast<const char*>(Data.data() + Position);
        Position += Size;
        return {Begin, Size};
    }

    xr_vector<u8> ReadBytes()
    {
        const u32 Size = ReadU32();
        if (Size > MaximumStringSize)
            throw std::runtime_error("Material shader blob exceeds the safety limit.");
        Require(Size);
        xr_vector<u8> Result(Data.begin() + Position, Data.begin() + Position + Size);
        Position += Size;
        return Result;
    }

    [[nodiscard]] size_t GetPosition() const noexcept { return Position; }

private:
    void Require(const size_t Size) const
    {
        if (Size > Data.size() - Position)
            throw std::runtime_error("Material bundle is truncated.");
    }

    xr_span<const u8> Data;
    size_t Position = 0;
};
} // namespace

bool FMaterialBundleWriteResult::Succeeded() const noexcept
{
    return !Data.empty() && !HasErrors(Diagnostics);
}

bool FMaterialBundleReadResult::Succeeded() const noexcept
{
    return !HasErrors(Diagnostics);
}

FMaterialBundleWriteResult SerializeMaterialBundle(const FMaterialBundle& Bundle)
{
    FMaterialBundleWriteResult Result;
    if (Bundle.Version != MaterialBundleVersion)
    {
        AddError(Result.Diagnostics, "bundle.unsupported_version", "Cannot write unsupported material bundle version.");
        return Result;
    }
    if (Bundle.CompleteShaderSet && Bundle.ShaderBlobs.empty())
    {
        AddError(Result.Diagnostics, "bundle.missing_shader_blobs", "A complete material bundle cannot have an empty shader set.");
        return Result;
    }

    try
    {
        xr_vector<FMaterialBundleRecord> Records = Bundle.Records;
        std::ranges::sort(Records,
            [](const FMaterialBundleRecord& Left, const FMaterialBundleRecord& Right)
            { return std::tie(Left.Id.Value, Left.Type, Left.SourcePath) < std::tie(Right.Id.Value, Right.Type, Right.SourcePath); });
        xr_vector<FMaterialShaderBlob> Blobs = Bundle.ShaderBlobs;
        std::ranges::sort(Blobs,
            [](const FMaterialShaderBlob& Left, const FMaterialShaderBlob& Right)
            { return std::tie(Left.MaterialId.Value, Left.PipelineKey, Left.Format,
                         Left.Pass, Left.Stage, Left.EntryPoint, Left.VertexFactory,
                         Left.RenderPassSignature) <
                std::tie(Right.MaterialId.Value, Right.PipelineKey, Right.Format,
                         Right.Pass, Right.Stage, Right.EntryPoint, Right.VertexFactory,
                         Right.RenderPassSignature); });

        if (Records.size() > MaximumCollectionSize || Blobs.size() > MaximumCollectionSize)
            throw std::runtime_error("Material bundle collection exceeds the safety limit.");

        FBinaryWriter Writer;
        Writer.Data.insert(Writer.Data.end(), BundleMagic.begin(), BundleMagic.end());
        Writer.WriteU32(Bundle.Version);
        Writer.WriteU32(Bundle.CompleteShaderSet ? 1u : 0u);
        Writer.WriteU32(static_cast<u32>(Records.size()));
        Writer.WriteU32(static_cast<u32>(Blobs.size()));
        for (FMaterialBundleRecord& Record : Records)
        {
            std::ranges::sort(Record.Dependencies);
            Record.Dependencies.erase(std::ranges::unique(Record.Dependencies).begin(), Record.Dependencies.end());
            Writer.WriteU8(static_cast<u8>(Record.Type));
            Writer.WriteString(Record.Id.Value);
            Writer.WriteString(Record.MasterId.Value);
            Writer.WriteString(Record.SourcePath);
            Writer.WriteString(Record.AssetPayload);
            Writer.WriteString(Record.GeneratedHlsl);
            Writer.WriteU32(static_cast<u32>(Record.Dependencies.size()));
            for (const xr_string& Dependency : Record.Dependencies)
                Writer.WriteString(Dependency);
        }
        for (const FMaterialShaderBlob& Blob : Blobs)
        {
            Writer.WriteString(Blob.MaterialId.Value);
            Writer.WriteU64(Blob.PipelineKey);
            Writer.WriteU8(static_cast<u8>(Blob.Format));
            if (!FindMaterialPassDefinition(Blob.Pass))
                throw std::runtime_error("Material shader blob pass is invalid.");
            if (Blob.Stage != EMaterialShaderStage::Vertex && Blob.Stage != EMaterialShaderStage::Pixel)
                throw std::runtime_error("Material shader blob stage is invalid.");
            if (Blob.VertexFactory.empty() || Blob.RenderPassSignature.empty())
                throw std::runtime_error("Material shader blob has incomplete pass metadata.");
            Writer.WriteU8(static_cast<u8>(Blob.Pass));
            Writer.WriteU8(static_cast<u8>(Blob.Stage));
            Writer.WriteString(Blob.EntryPoint);
            Writer.WriteString(Blob.VertexFactory);
            Writer.WriteString(Blob.RenderPassSignature);
            Writer.WriteBytes(Blob.Bytecode);
        }
        Writer.WriteU64(Checksum(Writer.Data));
        Result.Data = std::move(Writer.Data);
    }
    catch (const std::exception& Error)
    {
        AddError(Result.Diagnostics, "bundle.write_failed", Error.what());
    }
    return Result;
}

FMaterialBundleReadResult DeserializeMaterialBundle(const xr_span<const u8> Data)
{
    FMaterialBundleReadResult Result;
    if (Data.size() < BundleMagic.size() + 4 * sizeof(u32) + sizeof(u64))
    {
        AddError(Result.Diagnostics, "bundle.too_small", "Material bundle is too small.");
        return Result;
    }

    try
    {
        const xr_span Payload = Data.first(Data.size() - sizeof(u64));
        FBinaryReader ChecksumReader(Data.last(sizeof(u64)));
        if (Checksum(Payload) != ChecksumReader.ReadU64())
            throw std::runtime_error("Material bundle checksum mismatch.");

        FBinaryReader Reader(Payload);
        for (const u8 Expected : BundleMagic)
            if (Reader.ReadU8() != Expected)
                throw std::runtime_error("Material bundle magic is invalid.");
        Result.Value.Version = Reader.ReadU32();
        if (Result.Value.Version != MaterialBundleVersion)
            throw std::runtime_error("Material bundle version is not supported.");
        Result.Value.CompleteShaderSet = Reader.ReadU32() != 0;
        const u32 RecordCount = Reader.ReadU32();
        const u32 BlobCount = Reader.ReadU32();
        if (RecordCount > MaximumCollectionSize || BlobCount > MaximumCollectionSize)
            throw std::runtime_error("Material bundle collection exceeds the safety limit.");

        Result.Value.Records.reserve(RecordCount);
        for (u32 Index = 0; Index < RecordCount; ++Index)
        {
            FMaterialBundleRecord& Record = Result.Value.Records.emplace_back();
            Record.Type = static_cast<EMaterialBundleRecordType>(Reader.ReadU8());
            if (Record.Type != EMaterialBundleRecordType::Master && Record.Type != EMaterialBundleRecordType::FlattenedInstance)
                throw std::runtime_error("Material bundle record type is invalid.");
            Record.Id.Value = Reader.ReadString();
            Record.MasterId.Value = Reader.ReadString();
            Record.SourcePath = Reader.ReadString();
            Record.AssetPayload = Reader.ReadString();
            Record.GeneratedHlsl = Reader.ReadString();
            const u32 DependencyCount = Reader.ReadU32();
            if (DependencyCount > MaximumCollectionSize)
                throw std::runtime_error("Material dependency table exceeds the safety limit.");
            Record.Dependencies.reserve(DependencyCount);
            for (u32 Dependency = 0; Dependency < DependencyCount; ++Dependency)
                Record.Dependencies.push_back(Reader.ReadString());
        }
        Result.Value.ShaderBlobs.reserve(BlobCount);
        for (u32 Index = 0; Index < BlobCount; ++Index)
        {
            FMaterialShaderBlob& Blob = Result.Value.ShaderBlobs.emplace_back();
            Blob.MaterialId.Value = Reader.ReadString();
            Blob.PipelineKey = Reader.ReadU64();
            Blob.Format = static_cast<EMaterialShaderBlobFormat>(Reader.ReadU8());
            if (Blob.Format != EMaterialShaderBlobFormat::Dxil && Blob.Format != EMaterialShaderBlobFormat::SpirV)
                throw std::runtime_error("Material shader blob format is invalid.");
            Blob.Pass = static_cast<EMaterialPass>(Reader.ReadU8());
            if (!FindMaterialPassDefinition(Blob.Pass))
                throw std::runtime_error("Material shader blob pass is invalid.");
            Blob.Stage = static_cast<EMaterialShaderStage>(Reader.ReadU8());
            if (Blob.Stage != EMaterialShaderStage::Vertex && Blob.Stage != EMaterialShaderStage::Pixel)
                throw std::runtime_error("Material shader blob stage is invalid.");
            Blob.EntryPoint = Reader.ReadString();
            Blob.VertexFactory = Reader.ReadString();
            Blob.RenderPassSignature = Reader.ReadString();
            if (Blob.VertexFactory.empty() || Blob.RenderPassSignature.empty())
                throw std::runtime_error("Material shader blob has incomplete pass metadata.");
            Blob.Bytecode = Reader.ReadBytes();
        }
        if (Reader.GetPosition() != Payload.size())
            throw std::runtime_error("Material bundle contains unexpected trailing data.");
        if (Result.Value.CompleteShaderSet && Result.Value.ShaderBlobs.empty())
            throw std::runtime_error("Complete material bundle has no shader blobs.");
    }
    catch (const std::exception& Error)
    {
        Result.Value = {};
        AddError(Result.Diagnostics, "bundle.read_failed", Error.what());
    }
    return Result;
}
