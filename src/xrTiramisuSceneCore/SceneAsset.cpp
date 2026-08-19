#include "SceneAsset.h"
#include "SceneJsonHelpers.h"

#include <nlohmann/json.hpp>

namespace Tiramisu::Scene
{
namespace
{
using Json = nlohmann::json;

constexpr u64 FnvOffset = 14695981039346656037ull;
constexpr u64 FnvPrime = 1099511628211ull;
constexpr size_t StaticMeshBinaryHeaderSize = 72;
constexpr xr_array<u8, 8> StaticMeshBinaryMagic =
	{
		'T', 'R', 'M', 'S', 'H', 'B', 'I', 'N'
};

constexpr u32 StaticMeshBinaryEndianTag = 0x01020304u;

void AddError(xr_vector<FSceneDiagnostic>& Diagnostics, const xr_string_view Code, xr_string Message, const xr_string_view Source)
{
	Diagnostics.push_back({ESceneDiagnosticSeverity::Error, xr_string(Code), std::move(Message), xr_string(Source)});
}

[[nodiscard]] bool HasErrors(const xr_vector<FSceneDiagnostic>& Diagnostics) noexcept
{
	return std::ranges::any_of(Diagnostics, [](const FSceneDiagnostic& Diagnostic)
							   { return Diagnostic.Severity == ESceneDiagnosticSeverity::Error; });
}

[[nodiscard]] bool ReadString(const Json& Object, const char* Name, xr_string& Result)
{
	const auto Found = Object.find(Name);
	if (Found == Object.end() || !Found->is_string())
	{
		return false;
	}
	Result = Found->get<xr_string>();
	return true;
}

[[nodiscard]] bool ReadUint32(const Json& Object, const char* Name, u32& Result)
{
	const auto Found = Object.find(Name);
	if (Found == Object.end() || !Found->is_number_unsigned())
	{
		return false;
	}
	const u64 Value = Found->get<u64>();
	if (Value > std::numeric_limits<u32>::max())
	{
		return false;
	}
	Result = static_cast<u32>(Value);
	return true;
}

[[nodiscard]] bool IsLowerHexHash(const xr_string_view Value) noexcept
{
	return Value.size() == 16 && std::ranges::all_of(Value, [](const char Character)
													 { return (Character >= '0' && Character <= '9') || (Character >= 'a' && Character <= 'f'); });
}

[[nodiscard]] bool ParseHexHash(const xr_string_view Text, u64& Value) noexcept
{
	if (!IsLowerHexHash(Text))
	{
		return false;
	}
	Value = 0;
	for (const char Character : Text)
	{
		Value <<= 4;
		Value |= Character <= '9' ? static_cast<u64>(Character - '0') : static_cast<u64>(Character - 'a' + 10);
	}
	return true;
}

[[nodiscard]] xr_string FormatHexHash(const u64 Value)
{
	std::ostringstream Text;
	Text << std::hex << std::setfill('0') << std::setw(16) << Value;
	return Text.str();
}

template <size_t Size>
[[nodiscard]] bool ReadFloatArray(const Json& Value, xr_array<float, Size>& Result)
{
	if (!Value.is_array() || Value.size() != Size)
	{
		return false;
	}
	for (size_t Index = 0; Index < Size; ++Index)
	{
		if (!Value[Index].is_number())
		{
			return false;
		}
		const double Component = Value[Index].get<double>();
		if (!std::isfinite(Component) || Component < -std::numeric_limits<float>::max() || Component > std::numeric_limits<float>::max())
		{
			return false;
		}
		Result[Index] = static_cast<float>(Component);
	}
	return true;
}

template <size_t Size>
[[nodiscard]] bool IsFinite(const xr_array<float, Size>& Values) noexcept
{
	return std::ranges::all_of(Values, [](const float Value)
							   { return std::isfinite(Value); });
}

void HashBytes(u64& Hash, const void* Data, const size_t Size) noexcept
{
	const auto* Bytes = static_cast<const u8*>(Data);
	for (size_t Index = 0; Index < Size; ++Index)
	{
		Hash ^= Bytes[Index];
		Hash *= FnvPrime;
	}
}

void HashString(u64& Hash, const xr_string_view Value) noexcept
{
	HashBytes(Hash, Value.data(), Value.size());
	constexpr u8 Separator = 0xff;
	HashBytes(Hash, &Separator, sizeof(Separator));
}

[[nodiscard]] xr_string ReadText(const std::filesystem::path& Path)
{
	std::ifstream Input(Path, std::ios::binary);
	if (!Input)
	{
		return {};
	}
	const std::string Text{std::istreambuf_iterator<char>(Input), std::istreambuf_iterator<char>()};
	return xr_string(Text);
}

[[nodiscard]] xr_vector<u8> ReadBinary(const std::filesystem::path& Path)
{
	std::ifstream Input(Path, std::ios::binary | std::ios::ate);
	if (!Input)
	{
		return {};
	}
	const std::streamoff Size = Input.tellg();
	if (Size <= 0 || static_cast<u64>(Size) > std::numeric_limits<size_t>::max())
	{
		return {};
	}
	xr_vector<u8> Result(static_cast<size_t>(Size));
	Input.seekg(0, std::ios::beg);
	Input.read(reinterpret_cast<char*>(Result.data()), Size);
	if (!Input)
	{
		return {};
	}
	return Result;
}

void AppendDiagnostics(xr_vector<FSceneDiagnostic>& Destination, xr_vector<FSceneDiagnostic> Source)
{
	Destination.insert(Destination.end(), std::make_move_iterator(Source.begin()), std::make_move_iterator(Source.end()));
}

[[nodiscard]] std::filesystem::path ResolveReferencePath(const std::filesystem::path& ScenePath, const xr_string_view Reference)
{
	std::filesystem::path Path(Reference);
	if (Path.is_relative())
	{
		Path = ScenePath.parent_path() / Path;
	}
	return Path.lexically_normal();
}

void AppendUint32(xr_vector<u8>& Bytes, const u32 Value)
{
	for (u32 Shift = 0; Shift < 32; Shift += 8)
	{
		Bytes.push_back(static_cast<u8>(Value >> Shift));
	}
}

void AppendUint64(xr_vector<u8>& Bytes, const u64 Value)
{
	for (u32 Shift = 0; Shift < 64; Shift += 8)
	{
		Bytes.push_back(static_cast<u8>(Value >> Shift));
	}
}

void AppendFloat(xr_vector<u8>& Bytes, const float Value)
{
	AppendUint32(Bytes, std::bit_cast<u32>(Value));
}

void WriteUint32(xr_vector<u8>& Bytes, const size_t Offset, const u32 Value)
{
	for (u32 Shift = 0; Shift < 32; Shift += 8)
	{
		Bytes[Offset + Shift / 8] = static_cast<u8>(Value >> Shift);
	}
}

void WriteUint64(xr_vector<u8>& Bytes, const size_t Offset, const u64 Value)
{
	for (u32 Shift = 0; Shift < 64; Shift += 8)
	{
		Bytes[Offset + Shift / 8] = static_cast<u8>(Value >> Shift);
	}
}

[[nodiscard]] bool ReadUint32(const xr_vector<u8>& Bytes, const size_t Offset, u32& Value) noexcept
{
	if (Offset > Bytes.size() || Bytes.size() - Offset < sizeof(Value))
	{
		return false;
	}
	Value = 0;
	for (u32 Shift = 0; Shift < 32; Shift += 8)
	{
		Value |= static_cast<u32>(Bytes[Offset + Shift / 8]) << Shift;
	}
	return true;
}

[[nodiscard]] bool ReadUint64(const xr_vector<u8>& Bytes, const size_t Offset, u64& Value) noexcept
{
	if (Offset > Bytes.size() || Bytes.size() - Offset < sizeof(Value))
	{
		return false;
	}
	Value = 0;
	for (u32 Shift = 0; Shift < 64; Shift += 8)
	{
		Value |= static_cast<u64>(Bytes[Offset + Shift / 8]) << Shift;
	}
	return true;
}

[[nodiscard]] bool ReadFloat(const xr_vector<u8>& Bytes, size_t& Offset, float& Value) noexcept
{
	u32 Bits = 0;
	if (!ReadUint32(Bytes, Offset, Bits))
	{
		return false;
	}
	Offset += sizeof(Bits);
	Value = std::bit_cast<float>(Bits);
	return true;
}

[[nodiscard]] u64 HashRange(const xr_vector<u8>& Bytes, const size_t Offset) noexcept
{
	u64 Hash = FnvOffset;
	if (Offset < Bytes.size())
	{
		HashBytes(Hash, Bytes.data() + Offset, Bytes.size() - Offset);
	}
	return Hash;
}

[[nodiscard]] bool BuildStaticMeshBinary(const FStaticMeshAsset& Asset, xr_vector<u8>& Bytes, u64& PayloadHash)
{
	if (Asset.Vertices.empty() || Asset.Indices.empty() || Asset.Vertices.size() > std::numeric_limits<u32>::max() || Asset.Indices.size() > std::numeric_limits<u32>::max())
	{
		return false;
	}

	const u64 VertexBytes = static_cast<u64>(Asset.Vertices.size()) * StaticMeshBinaryVertexStride;
	const u64 IndexBytes = static_cast<u64>(Asset.Indices.size()) * StaticMeshBinaryIndexStride;
	const u64 FileSize = StaticMeshBinaryHeaderSize + VertexBytes + IndexBytes;

	if (FileSize > std::numeric_limits<size_t>::max())
	{
		return false;
	}

	Bytes.assign(StaticMeshBinaryHeaderSize, 0);
	Bytes.reserve(static_cast<size_t>(FileSize));
	for (const FStaticMeshVertex& Vertex : Asset.Vertices)
	{
		for (const float Value : Vertex.Position)
		{
			AppendFloat(Bytes, Value);
		}
		for (const float Value : Vertex.Normal)
		{
			AppendFloat(Bytes, Value);
		}
		for (const float Value : Vertex.Tangent)
		{
			AppendFloat(Bytes, Value);
		}
		for (const float Value : Vertex.TexCoord0)
		{
			AppendFloat(Bytes, Value);
		}
		for (const float Value : Vertex.TexCoord1)
		{
			AppendFloat(Bytes, Value);
		}
		AppendUint32(Bytes, Vertex.Color);
	}
	for (const u32 Index : Asset.Indices)
	{
		AppendUint32(Bytes, Index);
	}
	if (Bytes.size() != FileSize)
	{
		return false;
	}

	PayloadHash = HashRange(Bytes, StaticMeshBinaryHeaderSize);
	std::copy(StaticMeshBinaryMagic.begin(), StaticMeshBinaryMagic.end(), Bytes.begin());
	WriteUint32(Bytes, 8, StaticMeshBinaryVersion);
	WriteUint32(Bytes, 12, StaticMeshBinaryEndianTag);
	WriteUint32(Bytes, 16, static_cast<u32>(StaticMeshBinaryHeaderSize));
	WriteUint32(Bytes, 20, StaticMeshBinaryVertexStride);
	WriteUint32(Bytes, 24, StaticMeshBinaryIndexStride);
	WriteUint32(Bytes, 28, static_cast<u32>(Asset.Vertices.size()));
	WriteUint32(Bytes, 32, static_cast<u32>(Asset.Indices.size()));
	WriteUint32(Bytes, 36, 0);
	WriteUint64(Bytes, 40, StaticMeshBinaryHeaderSize);
	WriteUint64(Bytes, 48, StaticMeshBinaryHeaderSize + VertexBytes);
	WriteUint64(Bytes, 56, FileSize);
	WriteUint64(Bytes, 64, PayloadHash);
	return true;
}

[[nodiscard]] bool DecodeStaticMeshBinary(const xr_vector<u8>& Bytes, FStaticMeshAssetParseResult& Result)
{
	if (Bytes.size() < StaticMeshBinaryHeaderSize || !std::equal(StaticMeshBinaryMagic.begin(), StaticMeshBinaryMagic.end(), Bytes.begin()))
	{
		AddError(Result.Diagnostics, "static_mesh.binary_invalid_magic", "Static-mesh binary payload has an invalid header.", Result.Value.SourcePath);
		return false;
	}

	u32 Version = 0;
	u32 EndianTag = 0;
	u32 HeaderSize = 0;
	u32 VertexStride = 0;
	u32 IndexStride = 0;
	u32 VertexCount = 0;
	u32 IndexCount = 0;
	u32 Reserved = 0;
	u64 VertexOffset = 0;
	u64 IndexOffset = 0;
	u64 FileSize = 0;
	u64 StoredHash = 0;
	const bool HeaderValid = ReadUint32(Bytes, 8, Version) && ReadUint32(Bytes, 12, EndianTag) && ReadUint32(Bytes, 16, HeaderSize) && ReadUint32(Bytes, 20, VertexStride) && ReadUint32(Bytes, 24, IndexStride) && ReadUint32(Bytes, 28, VertexCount) && ReadUint32(Bytes, 32, IndexCount) && ReadUint32(Bytes, 36, Reserved) && ReadUint64(Bytes, 40, VertexOffset) && ReadUint64(Bytes, 48, IndexOffset) && ReadUint64(Bytes, 56, FileSize) && ReadUint64(Bytes, 64, StoredHash);
	const u64 ExpectedIndexOffset = StaticMeshBinaryHeaderSize + static_cast<u64>(VertexCount) * StaticMeshBinaryVertexStride;
	const u64 ExpectedFileSize = ExpectedIndexOffset + static_cast<u64>(IndexCount) * StaticMeshBinaryIndexStride;
	u64 MetadataHash = 0;
	const bool MetadataHashValid = ParseHexHash(Result.Value.Geometry.ContentHash, MetadataHash);
	if (!HeaderValid || Version != StaticMeshBinaryVersion || EndianTag != StaticMeshBinaryEndianTag || HeaderSize != StaticMeshBinaryHeaderSize || VertexStride != StaticMeshBinaryVertexStride || IndexStride != StaticMeshBinaryIndexStride || Reserved != 0 || VertexCount == 0 || IndexCount == 0 || IndexCount % 3 != 0 || VertexOffset != StaticMeshBinaryHeaderSize || IndexOffset != ExpectedIndexOffset || FileSize != ExpectedFileSize || FileSize != Bytes.size() || VertexCount != Result.Value.Geometry.VertexCount || IndexCount != Result.Value.Geometry.IndexCount || Version != Result.Value.Geometry.BinaryVersion || VertexStride != Result.Value.Geometry.VertexStride || IndexStride != Result.Value.Geometry.IndexStride || !MetadataHashValid || StoredHash != MetadataHash || StoredHash != HashRange(Bytes, StaticMeshBinaryHeaderSize))
	{
		AddError(Result.Diagnostics, "static_mesh.binary_header_mismatch", "Static-mesh binary header, size or content hash does not match the metadata JSON.", Result.Value.SourcePath);
		return false;
	}

	Result.Value.Vertices.resize(VertexCount);
	size_t Offset = static_cast<size_t>(VertexOffset);
	for (FStaticMeshVertex& Vertex : Result.Value.Vertices)
	{
		for (float& Value : Vertex.Position)
		{
			if (!ReadFloat(Bytes, Offset, Value))
			{
				return false;
			}
		}
		for (float& Value : Vertex.Normal)
		{
			if (!ReadFloat(Bytes, Offset, Value))
			{
				return false;
			}
		}
		for (float& Value : Vertex.Tangent)
		{
			if (!ReadFloat(Bytes, Offset, Value))
			{
				return false;
			}
		}
		for (float& Value : Vertex.TexCoord0)
		{
			if (!ReadFloat(Bytes, Offset, Value))
			{
				return false;
			}
		}
		for (float& Value : Vertex.TexCoord1)
		{
			if (!ReadFloat(Bytes, Offset, Value))
			{
				return false;
			}
		}
		if (!ReadUint32(Bytes, Offset, Vertex.Color))
		{
			return false;
		}
		Offset += sizeof(Vertex.Color);
	}
	if (Offset != IndexOffset)
	{
		return false;
	}
	Result.Value.Indices.resize(IndexCount);
	for (u32& Index : Result.Value.Indices)
	{
		if (!ReadUint32(Bytes, Offset, Index))
		{
			return false;
		}
		Offset += sizeof(Index);
	}
	return Offset == Bytes.size();
}

[[nodiscard]] bool WriteFileAtomically(const std::filesystem::path& Path, const void* Data, const size_t Size, xr_string& ErrorMessage)
{
	std::error_code Error;
	std::filesystem::create_directories(Path.parent_path(), Error);
	if (Error)
	{
		ErrorMessage = "Cannot create asset directory: " + Error.message();
		return false;
	}
	std::filesystem::path Temporary = Path;
	Temporary += ".tmp-" + std::to_string(std::chrono::steady_clock::now().time_since_epoch().count());
	{
		std::ofstream Output(Temporary, std::ios::binary | std::ios::trunc);
		if (!Output)
		{
			ErrorMessage = "Cannot open temporary asset file.";
			return false;
		}
		Output.write(static_cast<const char*>(Data), static_cast<std::streamsize>(Size));
		Output.flush();
		if (!Output)
		{
			Output.close();
			std::filesystem::remove(Temporary, Error);
			ErrorMessage = "Cannot write temporary asset file.";
			return false;
		}
	}
	std::filesystem::rename(Temporary, Path, Error);
	if (Error)
	{
		std::error_code RemoveError;
		std::filesystem::remove(Path, RemoveError);
		Error.clear();
		std::filesystem::rename(Temporary, Path, Error);
	}
	if (Error)
	{
		std::error_code RemoveError;
		std::filesystem::remove(Temporary, RemoveError);
		ErrorMessage = "Cannot publish asset file: " + Error.message();
		return false;
	}
	return true;
}

void ValidateStaticMesh(FStaticMeshAssetParseResult& Result, const bool RequireResidentGeometry)
{
	FStaticMeshAsset& Asset = Result.Value;
	const xr_string& Source = Asset.SourcePath;
	if (Asset.Version != StaticMeshAssetVersion && Asset.Version != LegacyInlineStaticMeshAssetVersion)
	{
		AddError(Result.Diagnostics, "static_mesh.unsupported_version", "Unsupported static-mesh asset version " + std::to_string(Asset.Version) + ".", Source);
	}
	if (!IsValidSceneStableId(Asset.Id))
	{
		AddError(Result.Diagnostics, "static_mesh.invalid_guid", "Static-mesh GUID is missing or invalid.", Source);
	}
	if (Asset.Name.empty())
	{
		AddError(Result.Diagnostics, "static_mesh.missing_name", "Static-mesh name is missing.", Source);
	}
	if (Asset.MaterialSlots.empty())
	{
		AddError(Result.Diagnostics, "static_mesh.missing_material_slots", "Static mesh must contain at least one material slot.", Source);
	}
	for (size_t Index = 0; Index < Asset.MaterialSlots.size(); ++Index)
	{
		const FStaticMeshMaterialSlot& Slot = Asset.MaterialSlots[Index];
		if (Slot.Name.empty() || Slot.Material.empty())
		{
			AddError(Result.Diagnostics, "static_mesh.invalid_material_slot", "Material slot " + std::to_string(Index) + " must have a name and explicit material reference.", Source);
		}
	}
	if (Asset.Version == StaticMeshAssetVersion)
	{
		const FStaticMeshGeometryStorage& Geometry = Asset.Geometry;
		if (Geometry.File.empty() || Geometry.BinaryVersion != StaticMeshBinaryVersion || Geometry.VertexStride != StaticMeshBinaryVertexStride || Geometry.IndexStride != StaticMeshBinaryIndexStride || Geometry.VertexCount == 0 || Geometry.IndexCount == 0 || Geometry.IndexCount % 3 != 0 || !IsLowerHexHash(Geometry.ContentHash))
		{
			AddError(Result.Diagnostics, "static_mesh.invalid_geometry_metadata", "Static-mesh v2 geometry metadata is missing or invalid.", Source);
		}
	}
	const bool GeometryResident = !Asset.Vertices.empty() || !Asset.Indices.empty();
	if (RequireResidentGeometry && (!GeometryResident || Asset.Vertices.empty() || Asset.Indices.empty() || Asset.Indices.size() % 3 != 0))
	{
		AddError(Result.Diagnostics, "static_mesh.invalid_topology", "Static mesh must contain vertices and triangle-list indices.", Source);
	}
	if (GeometryResident && Asset.Version == StaticMeshAssetVersion && (Asset.Vertices.size() != Asset.Geometry.VertexCount || Asset.Indices.size() != Asset.Geometry.IndexCount))
	{
		AddError(Result.Diagnostics, "static_mesh.geometry_count_mismatch", "Resident geometry counts do not match metadata.", Source);
	}
	if (GeometryResident)
	{
		for (const FStaticMeshVertex& Vertex : Asset.Vertices)
		{
			if (!IsFinite(Vertex.Position) || !IsFinite(Vertex.Normal) || !IsFinite(Vertex.Tangent) || !IsFinite(Vertex.TexCoord0) || !IsFinite(Vertex.TexCoord1))
			{
				AddError(Result.Diagnostics, "static_mesh.non_finite_vertex", "Static mesh contains a non-finite vertex.", Source);
				break;
			}
		}
		for (const u32 Index : Asset.Indices)
		{
			if (Index >= Asset.Vertices.size())
			{
				AddError(Result.Diagnostics, "static_mesh.index_out_of_range", "Static mesh index is outside the vertex array.", Source);
				break;
			}
		}
	}
	const u64 AvailableIndexCount = GeometryResident ? Asset.Indices.size() : Asset.Geometry.IndexCount;
	u64 ExpectedFirstIndex = 0;
	for (const FStaticMeshSection& Section : Asset.Sections)
	{
		const u64 End = static_cast<u64>(Section.FirstIndex) + Section.IndexCount;
		if (Section.FirstIndex != ExpectedFirstIndex || Section.IndexCount == 0 || Section.IndexCount % 3 != 0 || End > AvailableIndexCount || Section.MaterialSlot >= Asset.MaterialSlots.size())
		{
			AddError(Result.Diagnostics, "static_mesh.invalid_section", "Static-mesh sections must be contiguous triangle ranges with valid material slots.", Source);
			break;
		}
		ExpectedFirstIndex = End;
	}
	if (ExpectedFirstIndex != AvailableIndexCount)
	{
		AddError(Result.Diagnostics, "static_mesh.incomplete_sections", "Static-mesh sections do not cover the complete index buffer.", Source);
	}
}

void ValidateRenderScene(FRenderSceneAssetParseResult& Result)
{
	FRenderSceneAsset& Asset = Result.Value;
	const xr_string& Source = Asset.SourcePath;
	if (Asset.Version != RenderSceneAssetVersion &&
		Asset.Version != LightRenderSceneAssetVersion &&
		Asset.Version != LegacyStaticMeshOnlyRenderSceneAssetVersion)
	{
		AddError(Result.Diagnostics, "scene.unsupported_version", "Unsupported render-scene asset version " + std::to_string(Asset.Version) + ".", Source);
	}
	if (!IsValidSceneStableId(Asset.Id))
	{
		AddError(Result.Diagnostics, "scene.invalid_guid", "Render-scene GUID is missing or invalid.", Source);
	}
	if (Asset.Name.empty())
	{
		AddError(Result.Diagnostics, "scene.missing_name", "Render-scene name is missing.", Source);
	}
	xr_hash_set<xr_string> ObjectIds;
	for (const FStaticMeshComponent& Component : Asset.StaticMeshComponents)
	{
		if (!IsValidSceneStableId(Component.Id) || !ObjectIds.insert(Component.Id).second)
		{
			AddError(Result.Diagnostics, "scene.invalid_component_guid", "Static-mesh component GUID is invalid or duplicated.", Source);
		}
		if (Component.Name.empty() || Component.StaticMesh.empty())
		{
			AddError(Result.Diagnostics, "scene.invalid_component", "Static-mesh component must have a name and mesh reference.", Source);
		}
		if (!IsFinite(Component.LocalToWorld))
		{
			AddError(Result.Diagnostics, "scene.non_finite_transform", "Static-mesh component contains a non-finite transform.", Source);
		}
		xr_hash_set<u32> OverrideSlots;
		for (const FStaticMeshMaterialOverride& Override : Component.MaterialOverrides)
		{
			if (Override.Material.empty() || !OverrideSlots.insert(Override.MaterialSlot).second)
			{
				AddError(Result.Diagnostics, "scene.invalid_material_override", "Static-mesh component material overrides must have unique slots and explicit material references.", Source);
			}
		}
	}
	if (Asset.Version == LegacyStaticMeshOnlyRenderSceneAssetVersion && !Asset.LightComponents.empty())
	{
		AddError(Result.Diagnostics, "scene.light_requires_version_2", "Light components require render-scene asset version 2.", Source);
	}
	if (Asset.Version < RenderSceneAssetVersion &&
		!Asset.DecalComponents.empty())
	{
		AddError(
			Result.Diagnostics,
			"scene.decal_requires_version_3",
			"Decal components require render-scene asset version 3.",
			Source
		);
	}
	for (const FLightComponent& Light : Asset.LightComponents)
	{
		if (!IsValidSceneStableId(Light.Id) || !ObjectIds.insert(Light.Id).second)
		{
			AddError(Result.Diagnostics, "scene.invalid_light_guid", "Light component GUID is invalid or duplicated by another scene object.", Source);
		}
		if (Light.Name.empty())
		{
			AddError(Result.Diagnostics, "scene.invalid_light", "Light component must have a name.", Source);
		}
		if (!IsFinite(Light.LocalToWorld))
		{
			AddError(Result.Diagnostics, "scene.non_finite_light_transform", "Light component contains a non-finite transform.", Source);
		}
		if (!IsFinite(Light.Color) || std::ranges::any_of(Light.Color, [](const float Value)
														  { return Value < 0.0f; }) ||
			!std::isfinite(Light.Intensity) || Light.Intensity < 0.0f)
		{
			AddError(Result.Diagnostics, "scene.invalid_light_radiometry", "Light color and intensity must contain finite non-negative values.", Source);
		}
		if ((Light.Type == ELightType::Point || Light.Type == ELightType::Spot) && (!std::isfinite(Light.Range) || Light.Range <= 0.0f))
		{
			AddError(Result.Diagnostics, "scene.invalid_light_range", "Point and spot lights require a finite positive range.", Source);
		}
		if (Light.Type == ELightType::Spot && (!std::isfinite(Light.InnerConeAngleDegrees) || !std::isfinite(Light.OuterConeAngleDegrees) || Light.InnerConeAngleDegrees < 0.0f || Light.OuterConeAngleDegrees <= 0.0f || Light.InnerConeAngleDegrees > Light.OuterConeAngleDegrees || Light.OuterConeAngleDegrees >= 90.0f))
		{
			AddError(Result.Diagnostics, "scene.invalid_light_cone", "Spot-light cone angles must satisfy 0 <= inner <= outer < 90 degrees.", Source);
		}
	}
	for (const FDecalComponent& Decal : Asset.DecalComponents)
	{
		if (!IsValidSceneStableId(Decal.Id) ||
			!ObjectIds.insert(Decal.Id).second)
		{
			AddError(
				Result.Diagnostics,
				"scene.invalid_decal_guid",
				"Decal component GUID is invalid or duplicated by another scene object.",
				Source
			);
		}
		if (Decal.Name.empty() || Decal.Material.empty())
		{
			AddError(
				Result.Diagnostics,
				"scene.invalid_decal",
				"Decal component must have a name and explicit material reference.",
				Source
			);
		}
		if (!IsFinite(Decal.LocalToWorld))
		{
			AddError(
				Result.Diagnostics,
				"scene.non_finite_decal_transform",
				"Decal component contains a non-finite transform.",
				Source
			);
		}
	}
}
} // namespace

bool FResolvedRenderSceneResult::Succeeded() const noexcept
{
	return !HasErrors(Diagnostics) && !Value.Scene.Id.empty();
}

bool FStaticMeshAssetWriteResult::Succeeded() const noexcept
{
	return !HasErrors(Diagnostics) && !MetadataPath.empty() && !GeometryPath.empty();
}

bool IsValidSceneStableId(const xr_string_view Value) noexcept
{
	if (Value.empty())
	{
		return false;
	}
	for (const char Character : Value)
	{
		const unsigned char Byte = static_cast<unsigned char>(Character);
		if (!std::isalnum(Byte) && Character != '-' && Character != '_' && Character != '.' && Character != '/' && Character != ':')
		{
			return false;
		}
	}
	return true;
}

u64 StableSceneIdHash(const xr_string_view Value) noexcept
{
	u64 Hash = FnvOffset;
	HashString(Hash, "tiramisu.scene");
	HashString(Hash, Value);
	return Hash == 0 ? 1 : Hash;
}

xr_string_view ToString(const ELightType Type) noexcept
{
	switch (Type)
	{
		case ELightType::Directional:
			return "directional";
		case ELightType::Point:
			return "point";
		case ELightType::Spot:
			return "spot";
	}
	return "point";
}

bool TryParseLightType(const xr_string_view Value, ELightType& OutType) noexcept
{
	if (Value == "directional")
	{
		OutType = ELightType::Directional;
		return true;
	}
	if (Value == "point")
	{
		OutType = ELightType::Point;
		return true;
	}
	if (Value == "spot")
	{
		OutType = ELightType::Spot;
		return true;
	}
	return false;
}

u64 CalculateStaticMeshRevision(const FStaticMeshAsset& Asset) noexcept
{
	u64 Hash = FnvOffset;
	HashString(Hash, Asset.Id);
	for (const FStaticMeshMaterialSlot& Slot : Asset.MaterialSlots)
	{
		HashString(Hash, Slot.Name);
		HashString(Hash, Slot.Material);
		HashBytes(Hash, &Slot.TwoSided, sizeof(Slot.TwoSided));
	}
	for (const FStaticMeshVertex& Vertex : Asset.Vertices)
	{
		HashBytes(Hash, Vertex.Position.data(), Vertex.Position.size() * sizeof(float));
		HashBytes(Hash, Vertex.Normal.data(), Vertex.Normal.size() * sizeof(float));
		HashBytes(Hash, Vertex.Tangent.data(), Vertex.Tangent.size() * sizeof(float));
		HashBytes(Hash, Vertex.TexCoord0.data(), Vertex.TexCoord0.size() * sizeof(float));
		HashBytes(Hash, Vertex.TexCoord1.data(), Vertex.TexCoord1.size() * sizeof(float));
		HashBytes(Hash, &Vertex.Color, sizeof(Vertex.Color));
	}
	HashBytes(Hash, Asset.Indices.data(), Asset.Indices.size() * sizeof(u32));
	for (const FStaticMeshSection& Section : Asset.Sections)
	{
		HashBytes(Hash, &Section.FirstIndex, sizeof(Section.FirstIndex));
		HashBytes(Hash, &Section.IndexCount, sizeof(Section.IndexCount));
		HashBytes(Hash, &Section.MaterialSlot, sizeof(Section.MaterialSlot));
	}
	return Hash == 0 ? 1 : Hash;
}

FStaticMeshAssetParseResult ParseStaticMeshAssetJson(const xr_string_view JsonText, const xr_string_view SourcePath)
{
	FStaticMeshAssetParseResult Result;
	Result.Value.SourcePath = SourcePath;
	try
	{
		const Json Root = Json::parse(JsonText, nullptr, false);
		if (Root.is_discarded() || !Root.is_object())
		{
			AddError(Result.Diagnostics, "static_mesh.invalid_json", "Static-mesh asset must contain a JSON object.", SourcePath);
			return Result;
		}
		if (!ReadUint32(Root, "asset_version", Result.Value.Version))
		{
			AddError(Result.Diagnostics, "static_mesh.invalid_version", "asset_version must be an unsigned integer.", SourcePath);
		}
		if (!ReadString(Root, "guid", Result.Value.Id))
		{
			Result.Value.Id.clear();
		}
		if (!ReadString(Root, "name", Result.Value.Name))
		{
			Result.Value.Name.clear();
		}

		if (Result.Value.Version == StaticMeshAssetVersion)
		{
			const auto Geometry = Root.find("geometry");
			xr_string VertexFormat;
			xr_string IndexFormat;
			const bool GeometryValid = Geometry != Root.end() && Geometry->is_object() && ReadString(*Geometry, "file", Result.Value.Geometry.File) && ReadUint32(*Geometry, "binary_version", Result.Value.Geometry.BinaryVersion) && ReadString(*Geometry, "vertex_format", VertexFormat) && ReadUint32(*Geometry, "vertex_stride", Result.Value.Geometry.VertexStride) && ReadUint32(*Geometry, "vertex_count", Result.Value.Geometry.VertexCount) && ReadString(*Geometry, "index_format", IndexFormat) && ReadUint32(*Geometry, "index_stride", Result.Value.Geometry.IndexStride) && ReadUint32(*Geometry, "index_count", Result.Value.Geometry.IndexCount) && ReadString(*Geometry, "content_hash", Result.Value.Geometry.ContentHash) && VertexFormat == "P3F_N3F_T4F_UV0_2F_UV1_2F_RGBA8" && IndexFormat == "uint32";
			if (!GeometryValid)
			{
				AddError(Result.Diagnostics, "static_mesh.invalid_geometry_fields", "Static-mesh v2 geometry must describe the binary file, formats, strides, counts and content hash.", SourcePath);
			}
		}

		const auto Materials = Root.find("material_slots");
		if (Materials != Root.end() && Materials->is_array())
		{
			for (const Json& Item : *Materials)
			{
				FStaticMeshMaterialSlot Slot;
				if (Item.is_object())
				{
					(void)ReadString(Item, "name", Slot.Name);
					(void)ReadString(Item, "material", Slot.Material);
					if (Item.contains("two_sided"))
					{
						if (!Item["two_sided"].is_boolean())
						{
							AddError(Result.Diagnostics, "static_mesh.invalid_material_slot", "Material slot two_sided must be boolean.", SourcePath);
						}
						else
						{
							Slot.TwoSided = Item["two_sided"].get<bool>();
						}
					}
				}
				Result.Value.MaterialSlots.push_back(std::move(Slot));
			}
		}

		const auto Vertices = Root.find("vertices");
		if (Result.Value.Version == LegacyInlineStaticMeshAssetVersion && Vertices != Root.end() && Vertices->is_array())
		{
			for (const Json& Item : *Vertices)
			{
				FStaticMeshVertex Vertex;
				bool Valid = Item.is_object();
				Valid = Valid && Item.contains("position") && ReadFloatArray(Item["position"], Vertex.Position);
				if (Item.contains("normal"))
				{
					Valid = Valid && ReadFloatArray(Item["normal"], Vertex.Normal);
				}
				if (Item.contains("tangent"))
				{
					Valid = Valid && ReadFloatArray(Item["tangent"], Vertex.Tangent);
				}
				if (Item.contains("uv0"))
				{
					Valid = Valid && ReadFloatArray(Item["uv0"], Vertex.TexCoord0);
				}
				if (Item.contains("uv1"))
				{
					Valid = Valid && ReadFloatArray(Item["uv1"], Vertex.TexCoord1);
				}
				if (Item.contains("color"))
				{
					u32 Color = 0;
					Valid = Valid && ReadUint32(Item, "color", Color);
					Vertex.Color = Color;
				}
				if (!Valid)
				{
					AddError(Result.Diagnostics, "static_mesh.invalid_vertex", "Every vertex must contain finite typed attributes.", SourcePath);
				}
				Result.Value.Vertices.push_back(Vertex);
			}
		}

		const auto Indices = Root.find("indices");
		if (Result.Value.Version == LegacyInlineStaticMeshAssetVersion && Indices != Root.end() && Indices->is_array())
		{
			for (const Json& Value : *Indices)
			{
				if (!Value.is_number_unsigned() || Value.get<u64>() > std::numeric_limits<u32>::max())
				{
					AddError(Result.Diagnostics, "static_mesh.invalid_index", "Every static-mesh index must be uint32.", SourcePath);
					Result.Value.Indices.push_back(0);
				}
				else
				{
					Result.Value.Indices.push_back(Value.get<u32>());
				}
			}
		}

		const auto Sections = Root.find("sections");
		if (Sections != Root.end() && Sections->is_array())
		{
			for (const Json& Item : *Sections)
			{
				FStaticMeshSection Section;
				const bool Valid = Item.is_object() && ReadUint32(Item, "first_index", Section.FirstIndex) && ReadUint32(Item, "index_count", Section.IndexCount) && ReadUint32(Item, "material_slot", Section.MaterialSlot);
				if (!Valid)
				{
					AddError(Result.Diagnostics, "static_mesh.invalid_section_fields", "Every section must contain uint32 first_index, index_count and material_slot.", SourcePath);
				}
				Result.Value.Sections.push_back(Section);
			}
		}
		ValidateStaticMesh(Result, Result.Value.Version == LegacyInlineStaticMeshAssetVersion);
	}
	catch (const std::exception& Error)
	{
		AddError(Result.Diagnostics, "static_mesh.invalid_json", Error.what(), SourcePath);
	}
	return Result;
}

FRenderSceneAssetParseResult ParseRenderSceneAssetJson(const xr_string_view JsonText, const xr_string_view SourcePath)
{
	FRenderSceneAssetParseResult Result;
	Result.Value.SourcePath = SourcePath;
	try
	{
		const Json Root = Json::parse(JsonText, nullptr, false);
		if (Root.is_discarded() || !Root.is_object())
		{
			AddError(Result.Diagnostics, "scene.invalid_json", "Render-scene asset must contain a JSON object.", SourcePath);
			return Result;
		}
		if (!ReadUint32(Root, "asset_version", Result.Value.Version))
		{
			AddError(Result.Diagnostics, "scene.invalid_version", "asset_version must be an unsigned integer.", SourcePath);
		}
		if (!ReadString(Root, "guid", Result.Value.Id))
		{
			Result.Value.Id.clear();
		}
		if (!ReadString(Root, "name", Result.Value.Name))
		{
			Result.Value.Name.clear();
		}

		const auto Components = Root.find("static_mesh_components");
		if (Components != Root.end() && !Components->is_array())
		{
			AddError(Result.Diagnostics, "scene.invalid_component_array", "static_mesh_components must be an array.", SourcePath);
		}
		else if (Components != Root.end())
		{
			for (const Json& Item : *Components)
			{
				FStaticMeshComponent Component;
				bool Valid = Item.is_object();
				Valid = Valid && ReadString(Item, "guid", Component.Id);
				Valid = Valid && ReadString(Item, "name", Component.Name);
				Valid = Valid && ReadString(Item, "static_mesh", Component.StaticMesh);
				if (Item.contains("transform"))
				{
					Valid = Valid && ReadFloatArray(Item["transform"], Component.LocalToWorld);
				}
				if (Item.contains("visible"))
				{
					if (!Item["visible"].is_boolean())
					{
						Valid = false;
					}
					else
					{
						Component.Visible = Item["visible"].get<bool>();
					}
				}
				if (Item.contains("material_overrides"))
				{
					const Json& Overrides = Item["material_overrides"];
					if (!Overrides.is_array())
					{
						Valid = false;
					}
					else
					{
						for (const Json& OverrideItem : Overrides)
						{
							FStaticMeshMaterialOverride Override;
							bool OverrideValid = OverrideItem.is_object() && ReadUint32(OverrideItem, "material_slot", Override.MaterialSlot) && ReadString(OverrideItem, "material", Override.Material);
							if (OverrideItem.contains("two_sided"))
							{
								OverrideValid = OverrideValid && OverrideItem["two_sided"].is_boolean();
								if (OverrideItem["two_sided"].is_boolean())
								{
									Override.TwoSided = OverrideItem["two_sided"].get<bool>();
								}
							}
							if (!OverrideValid)
							{
								Valid = false;
							}
							Component.MaterialOverrides.push_back(std::move(Override));
						}
					}
				}
				if (!Valid)
				{
					AddError(Result.Diagnostics, "scene.invalid_component_fields", "Every static-mesh component must contain typed guid, name, static_mesh, transform and visible fields.", SourcePath);
				}
				Result.Value.StaticMeshComponents.push_back(std::move(Component));
			}
		}
		const auto Lights = Root.find("light_components");
		if (Lights != Root.end() && !Lights->is_array())
		{
			AddError(Result.Diagnostics, "scene.invalid_light_array", "light_components must be an array.", SourcePath);
		}
		else if (Lights != Root.end())
		{
			for (const Json& Item : *Lights)
			{
				FLightComponent Light;
				xr_string Type;
				bool Valid = Item.is_object();
				Valid = Valid && ReadString(Item, "guid", Light.Id);
				Valid = Valid && ReadString(Item, "name", Light.Name);
				Valid = Valid && ReadString(Item, "type", Type);
				Valid = Valid && TryParseLightType(Type, Light.Type);
				if (Item.contains("transform"))
				{
					Valid = Valid && ReadFloatArray(Item["transform"], Light.LocalToWorld);
				}
				else
				{
					Valid = false;
				}
				if (Item.contains("color"))
				{
					Valid = Valid && ReadFloatArray(Item["color"], Light.Color);
				}
				else
				{
					Valid = false;
				}
				const auto ReadFloat = [&](const char* Name, float& Value)
				{
					const auto Found = Item.find(Name);
					if (Found == Item.end() || !Found->is_number())
					{
						return false;
					}
					const double Parsed = Found->get<double>();
					if (!std::isfinite(Parsed) || Parsed < -std::numeric_limits<float>::max() || Parsed > std::numeric_limits<float>::max())
					{
						return false;
					}
					Value = static_cast<float>(Parsed);
					return true;
				};
				Valid = Valid && ReadFloat("intensity", Light.Intensity);
				Valid = Valid && ReadFloat("range", Light.Range);
				Valid = Valid && ReadFloat("inner_cone_degrees", Light.InnerConeAngleDegrees);
				Valid = Valid && ReadFloat("outer_cone_degrees", Light.OuterConeAngleDegrees);
				if (!Item.contains("visible") || !Item["visible"].is_boolean())
				{
					Valid = false;
				}
				else
				{
					Light.Visible = Item["visible"].get<bool>();
				}
				if (!Item.contains("cast_shadows") || !Item["cast_shadows"].is_boolean())
				{
					Valid = false;
				}
				else
				{
					Light.CastShadows = Item["cast_shadows"].get<bool>();
				}
				if (!Valid)
				{
					AddError(Result.Diagnostics, "scene.invalid_light_fields", "Every light component must contain typed guid, name, type, transform, color, intensity, range, cone angles, visible and cast_shadows fields.", SourcePath);
				}
				Result.Value.LightComponents.push_back(std::move(Light));
			}
		}
		const auto Decals = Root.find("decal_components");
		if (Decals != Root.end() && !Decals->is_array())
		{
			AddError(
				Result.Diagnostics,
				"scene.invalid_decal_array",
				"decal_components must be an array.",
				SourcePath
			);
		}
		else if (Decals != Root.end())
		{
			for (const Json& Item : *Decals)
			{
				FDecalComponent Decal;
				bool Valid = Item.is_object();
				Valid = Valid && ReadString(Item, "guid", Decal.Id);
				Valid = Valid && ReadString(Item, "name", Decal.Name);
				Valid = Valid && ReadString(Item, "material", Decal.Material);
				Valid = Valid && Item.contains("transform") &&
					ReadFloatArray(Item["transform"], Decal.LocalToWorld);
				const auto SortOrder = Item.find("sort_order");
				if (SortOrder == Item.end() ||
					!SortOrder->is_number_integer())
				{
					Valid = false;
				}
				else
				{
					const s64 Value = SortOrder->get<s64>();
					Valid = Valid &&
						Value >= std::numeric_limits<s32>::min() &&
						Value <= std::numeric_limits<s32>::max();
					if (Valid)
					{
						Decal.SortOrder = static_cast<s32>(Value);
					}
				}
				if (!Item.contains("visible") ||
					!Item["visible"].is_boolean())
				{
					Valid = false;
				}
				else
				{
					Decal.Visible = Item["visible"].get<bool>();
				}
				if (!Valid)
				{
					AddError(
						Result.Diagnostics,
						"scene.invalid_decal_fields",
						"Every decal component must contain typed guid, name, material, transform, sort_order and visible fields.",
						SourcePath
					);
				}
				Result.Value.DecalComponents.push_back(
					std::move(Decal)
				);
			}
		}
		ValidateRenderScene(Result);
	}
	catch (const std::exception& Error)
	{
		AddError(Result.Diagnostics, "scene.invalid_json", Error.what(), SourcePath);
	}
	return Result;
}

xr_string SerializeStaticMeshAssetJson(const FStaticMeshAsset& Asset)
{
	Json Root = {
		{"asset_version", Asset.Version},
		{"guid", Asset.Id},
		{"name", Asset.Name}
	};
	Root["material_slots"] = Json::array();
	for (const FStaticMeshMaterialSlot& Slot : Asset.MaterialSlots)
	{
		Root["material_slots"].push_back({{"name", Slot.Name}, {"material", Slot.Material}, {"two_sided", Slot.TwoSided}});
	}
	if (Asset.Version == LegacyInlineStaticMeshAssetVersion)
	{
		Root["vertices"] = Json::array();
		for (const FStaticMeshVertex& Vertex : Asset.Vertices)
		{
			Root["vertices"].push_back({{"position", Vertex.Position}, {"normal", Vertex.Normal}, {"tangent", Vertex.Tangent}, {"uv0", Vertex.TexCoord0}, {"uv1", Vertex.TexCoord1}, {"color", Vertex.Color}});
		}
		Root["indices"] = Asset.Indices;
	}
	else
	{
		Root["geometry"] = {
			{"file", Asset.Geometry.File},
			{"binary_version", Asset.Geometry.BinaryVersion},
			{"vertex_format", "P3F_N3F_T4F_UV0_2F_UV1_2F_RGBA8"},
			{"vertex_stride", Asset.Geometry.VertexStride},
			{"vertex_count", Asset.Geometry.VertexCount},
			{"index_format", "uint32"},
			{"index_stride", Asset.Geometry.IndexStride},
			{"index_count", Asset.Geometry.IndexCount},
			{"content_hash", Asset.Geometry.ContentHash}
		};
	}
	Root["sections"] = Json::array();
	for (const FStaticMeshSection& Section : Asset.Sections)
	{
		Root["sections"].push_back({{"first_index", Section.FirstIndex}, {"index_count", Section.IndexCount}, {"material_slot", Section.MaterialSlot}});
	}
	return Root.dump(2);
}

xr_string SerializeRenderSceneAssetJson(const FRenderSceneAsset& Asset)
{
	Json Root = {
		{"asset_version", Asset.Version},
		{"guid", Asset.Id},
		{"name", Asset.Name}
	};
	Root["static_mesh_components"] = Json::array();
	for (const FStaticMeshComponent& Component : Asset.StaticMeshComponents)
	{
		Json SerializedComponent = {
			{"guid", Component.Id},
			{"name", Component.Name},
			{"static_mesh", Component.StaticMesh},
			{"transform", Component.LocalToWorld},
			{"visible", Component.Visible}
		};
		SerializedComponent["material_overrides"] = Json::array();
		for (const FStaticMeshMaterialOverride& Override : Component.MaterialOverrides)
		{
			SerializedComponent["material_overrides"].push_back({{"material_slot", Override.MaterialSlot}, {"material", Override.Material}, {"two_sided", Override.TwoSided}});
		}
		Root["static_mesh_components"].push_back(std::move(SerializedComponent));
	}
	Root["light_components"] = Json::array();
	for (const FLightComponent& Light : Asset.LightComponents)
	{
		Root["light_components"].push_back({{"guid", Light.Id}, {"name", Light.Name}, {"type", ToString(Light.Type)}, {"transform", Light.LocalToWorld}, {"color", Light.Color}, {"intensity", Light.Intensity}, {"range", Light.Range}, {"inner_cone_degrees", Light.InnerConeAngleDegrees}, {"outer_cone_degrees", Light.OuterConeAngleDegrees}, {"visible", Light.Visible}, {"cast_shadows", Light.CastShadows}});
	}
	Root["decal_components"] = Json::array();
	for (const FDecalComponent& Decal : Asset.DecalComponents)
	{
		Root["decal_components"].push_back({
			{"guid", Decal.Id},
			{"name", Decal.Name},
			{"material", Decal.Material},
			{"transform", Decal.LocalToWorld},
			{"sort_order", Decal.SortOrder},
			{"visible", Decal.Visible}
		});
	}
	return Root.dump(2);
}

std::filesystem::path MakeStaticMeshGeometryPath(const std::filesystem::path& MetadataPath)
{
	std::filesystem::path Result = MetadataPath;
	Result.replace_extension(".bin");
	return Result.lexically_normal();
}

FStaticMeshAssetWriteResult SaveStaticMeshAsset(const std::filesystem::path& MetadataPath, const FStaticMeshAsset& Asset)
{
	FStaticMeshAssetWriteResult Result;
	Result.MetadataPath = MetadataPath.lexically_normal();
	Result.GeometryPath = MakeStaticMeshGeometryPath(Result.MetadataPath);
	if (Result.MetadataPath.empty() || Result.MetadataPath == Result.GeometryPath)
	{
		AddError(Result.Diagnostics, "static_mesh.invalid_target_path", "Static-mesh metadata and binary paths are invalid.", Result.MetadataPath.generic_string());
		Result.MetadataPath.clear();
		Result.GeometryPath.clear();
		return Result;
	}

	FStaticMeshAsset Prepared = Asset;
	Prepared.Version = StaticMeshAssetVersion;
	Prepared.SourcePath = Result.MetadataPath.generic_string();
	xr_vector<u8> Binary;
	u64 PayloadHash = 0;
	if (!BuildStaticMeshBinary(Prepared, Binary, PayloadHash))
	{
		AddError(Result.Diagnostics, "static_mesh.binary_build_failed", "Static-mesh geometry is empty, too large or cannot be encoded.", Prepared.SourcePath);
		Result.MetadataPath.clear();
		Result.GeometryPath.clear();
		return Result;
	}
	Prepared.Geometry.File = Result.GeometryPath.filename().generic_string();
	Prepared.Geometry.BinaryVersion = StaticMeshBinaryVersion;
	Prepared.Geometry.VertexStride = StaticMeshBinaryVertexStride;
	Prepared.Geometry.IndexStride = StaticMeshBinaryIndexStride;
	Prepared.Geometry.VertexCount = static_cast<u32>(Prepared.Vertices.size());
	Prepared.Geometry.IndexCount = static_cast<u32>(Prepared.Indices.size());
	Prepared.Geometry.ContentHash = FormatHexHash(PayloadHash);

	FStaticMeshAssetParseResult Validation;
	Validation.Value = Prepared;
	ValidateStaticMesh(Validation, true);
	if (!Validation.Succeeded())
	{
		Result.Diagnostics = std::move(Validation.Diagnostics);
		Result.MetadataPath.clear();
		Result.GeometryPath.clear();
		return Result;
	}

	xr_string WriteError;
	if (!WriteFileAtomically(Result.GeometryPath, Binary.data(), Binary.size(), WriteError))
	{
		AddError(Result.Diagnostics, "static_mesh.binary_write_failed", std::move(WriteError), Prepared.SourcePath);
		Result.MetadataPath.clear();
		Result.GeometryPath.clear();
		return Result;
	}
	const xr_string JsonText = SerializeStaticMeshAssetJson(Prepared);
	if (!WriteFileAtomically(Result.MetadataPath, JsonText.data(), JsonText.size(), WriteError))
	{
		AddError(Result.Diagnostics, "static_mesh.metadata_write_failed", std::move(WriteError), Prepared.SourcePath);
		Result.MetadataPath.clear();
		Result.GeometryPath.clear();
		return Result;
	}
	return Result;
}

FStaticMeshAssetParseResult LoadStaticMeshAsset(const std::filesystem::path& MetadataPath)
{
	const std::filesystem::path Normalized = MetadataPath.lexically_normal();
	const xr_string JsonText = ReadText(Normalized);
	if (JsonText.empty())
	{
		FStaticMeshAssetParseResult Result;
		Result.Value.SourcePath = Normalized.generic_string();
		AddError(Result.Diagnostics, "static_mesh.read_failed", "Cannot read static-mesh metadata JSON.", Result.Value.SourcePath);
		return Result;
	}
	FStaticMeshAssetParseResult Result = ParseStaticMeshAssetJson(JsonText, Normalized.generic_string());
	if (!Result.Succeeded() || Result.Value.Version == LegacyInlineStaticMeshAssetVersion)
	{
		return Result;
	}

	const std::filesystem::path GeometryReference(Result.Value.Geometry.File.c_str());
	if (GeometryReference.is_absolute() || GeometryReference.filename() != GeometryReference)
	{
		AddError(Result.Diagnostics, "static_mesh.invalid_geometry_path", "Static-mesh binary payload must be a file next to its metadata.", Result.Value.SourcePath);
		return Result;
	}
	const std::filesystem::path GeometryPath = (Normalized.parent_path() / GeometryReference).lexically_normal();
	const xr_vector<u8> Binary = ReadBinary(GeometryPath);
	if (Binary.empty())
	{
		AddError(Result.Diagnostics, "static_mesh.binary_read_failed", "Cannot read static-mesh binary payload '" + GeometryReference.generic_string() + "'.", Result.Value.SourcePath);
		return Result;
	}
	const size_t DiagnosticsBefore = Result.Diagnostics.size();
	if (!DecodeStaticMeshBinary(Binary, Result))
	{
		if (Result.Diagnostics.size() == DiagnosticsBefore)
		{
			AddError(Result.Diagnostics, "static_mesh.binary_decode_failed", "Static-mesh binary payload is truncated or malformed.", Result.Value.SourcePath);
		}
		return Result;
	}
	ValidateStaticMesh(Result, true);
	return Result;
}

FResolvedRenderSceneResult LoadRenderSceneAsset(const std::filesystem::path& ScenePath)
{
	FResolvedRenderSceneResult Result;
	const xr_string JsonText = ReadText(ScenePath);
	if (JsonText.empty())
	{
		AddError(Result.Diagnostics, "scene.read_failed", "Cannot read render-scene asset.", ScenePath.generic_string());
		return Result;
	}

	FRenderSceneAssetParseResult Parsed = ParseRenderSceneAssetJson(JsonText, ScenePath.generic_string());
	Result.Value.Scene = std::move(Parsed.Value);
	AppendDiagnostics(Result.Diagnostics, std::move(Parsed.Diagnostics));
	if (HasErrors(Result.Diagnostics))
	{
		return Result;
	}

	for (const FStaticMeshComponent& Component : Result.Value.Scene.StaticMeshComponents)
	{
		if (Result.Value.StaticMeshes.contains(Component.StaticMesh))
		{
			continue;
		}
		const std::filesystem::path MeshPath = ResolveReferencePath(ScenePath, Component.StaticMesh);
		FStaticMeshAssetParseResult Mesh = LoadStaticMeshAsset(MeshPath);
		const bool MeshValid = Mesh.Succeeded();
		AppendDiagnostics(Result.Diagnostics, std::move(Mesh.Diagnostics));
		if (MeshValid)
		{
			Result.Value.StaticMeshes.emplace(Component.StaticMesh, std::move(Mesh.Value));
		}
	}

	for (const FStaticMeshComponent& Component : Result.Value.Scene.StaticMeshComponents)
	{
		const auto Mesh = Result.Value.StaticMeshes.find(Component.StaticMesh);
		if (Mesh == Result.Value.StaticMeshes.end())
		{
			continue;
		}
		for (const FStaticMeshMaterialOverride& Override : Component.MaterialOverrides)
		{
			if (Override.MaterialSlot >= Mesh->second.MaterialSlots.size())
			{
				AddError(Result.Diagnostics, "scene.material_override_out_of_range", "Component '" + Component.Name + "' overrides a material slot outside static mesh '" + Component.StaticMesh + "'.", ScenePath.generic_string());
			}
		}
	}
	return Result;
}
} // namespace Tiramisu::Scene
