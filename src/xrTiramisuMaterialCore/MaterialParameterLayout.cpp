#include "MaterialParameterLayout.h"

#include <algorithm>
#include <cctype>
#include <cstring>
#include <limits>
#include <map>
#include <ranges>
#include <set>
#include <sstream>
#include <type_traits>

namespace
{
constexpr u64 FnvOffset = 14695981039346656037ull;
constexpr u64 FnvPrime = 1099511628211ull;

bool HasErrors(const xr_vector<FMaterialDiagnostic>& Diagnostics)
{
	return std::ranges::any_of(Diagnostics, [](const FMaterialDiagnostic& Diagnostic)
							   { return Diagnostic.Severity == EMaterialDiagnosticSeverity::Error; });
}

void AddError(xr_vector<FMaterialDiagnostic>& Diagnostics, xr_string Code, xr_string Message)
{
	Diagnostics.push_back({EMaterialDiagnosticSeverity::Error, std::move(Code), std::move(Message), {}, {}});
}

u32 AlignUp(const u32 Value, const u32 Alignment)
{
	return (Value + Alignment - 1u) & ~(Alignment - 1u);
}

u32 ParameterSize(const EMaterialParameterType Type)
{
	switch (Type)
	{
		case EMaterialParameterType::Scalar:
		case EMaterialParameterType::Texture2D:
		case EMaterialParameterType::TextureCube:
		case EMaterialParameterType::SamplerPreset:
			return 4;
		case EMaterialParameterType::Float2:
			return 8;
		case EMaterialParameterType::Float3:
			return 12;
		case EMaterialParameterType::Float4:
		case EMaterialParameterType::Color:
			return 16;
		case EMaterialParameterType::StaticBool:
		case EMaterialParameterType::StaticEnum:
			return 0;
	}
	return 0;
}

xr_string HlslFieldName(const FMaterialParameterId& Id)
{
	xr_string Result = "P_";
	for (const char Character : Id.Value)
	{
		Result += std::isalnum(static_cast<unsigned char>(Character)) ? Character : '_';
	}
	return Result;
}

xr_string_view HlslType(const EMaterialParameterType Type)
{
	switch (Type)
	{
		case EMaterialParameterType::Scalar:
			return "float";
		case EMaterialParameterType::Float2:
			return "float2";
		case EMaterialParameterType::Float3:
			return "float3";
		case EMaterialParameterType::Float4:
		case EMaterialParameterType::Color:
			return "float4";
		case EMaterialParameterType::Texture2D:
		case EMaterialParameterType::TextureCube:
		case EMaterialParameterType::SamplerPreset:
			return "uint";
		case EMaterialParameterType::StaticBool:
		case EMaterialParameterType::StaticEnum:
			return {};
	}
	return {};
}

void HashBytes(u64& Hash, const void* Data, const size_t Size)
{
	const auto* Bytes = static_cast<const u8*>(Data);
	for (size_t Index = 0; Index < Size; ++Index)
	{
		Hash ^= Bytes[Index];
		Hash *= FnvPrime;
	}
}

void HashString(u64& Hash, const xr_string_view Value)
{
	HashBytes(Hash, Value.data(), Value.size());
	constexpr u8 Separator = 0xff;
	HashBytes(Hash, &Separator, sizeof(Separator));
}

u64 LayoutHash(const FMaterialParameterLayout& Layout)
{
	u64 Hash = FnvOffset;
	HashBytes(Hash, &Layout.Version, sizeof(Layout.Version));
	HashBytes(Hash, &Layout.ByteSize, sizeof(Layout.ByteSize));
	for (const FMaterialParameterLayoutField& Field : Layout.Fields)
	{
		HashString(Hash, Field.Id.Value);
		const u8 Type = static_cast<u8>(Field.Type);
		HashBytes(Hash, &Type, sizeof(Type));
		HashBytes(Hash, &Field.Offset, sizeof(Field.Offset));
		HashBytes(Hash, &Field.Size, sizeof(Field.Size));
	}
	return Hash;
}

template <typename T>
void WriteValue(xr_vector<u8>& Data, const u32 Offset, const T& Value)
{
	static_assert(std::is_trivially_copyable_v<T>);
	std::memcpy(Data.data() + Offset, &Value, sizeof(T));
}

bool WriteParameterValue(FMaterialPackedParameterBlock& Block, const FMaterialParameterLayoutField& Field, const FMaterialValue& Value, xr_vector<FMaterialDiagnostic>& Diagnostics)
{
	if (!ValueMatchesParameterType(Value, Field.Type))
	{
		AddError(Diagnostics, "parameter_pack.type_mismatch", "Parameter '" + Field.Id.Value + "' does not match its material layout type.");
		return false;
	}

	switch (Field.Type)
	{
		case EMaterialParameterType::Scalar:
			WriteValue(Block.Data, Field.Offset, std::get<float>(Value));
			break;
		case EMaterialParameterType::Float2:
			WriteValue(Block.Data, Field.Offset, std::get<FFloat2>(Value));
			break;
		case EMaterialParameterType::Float3:
			WriteValue(Block.Data, Field.Offset, std::get<FFloat3>(Value));
			break;
		case EMaterialParameterType::Float4:
		case EMaterialParameterType::Color:
			WriteValue(Block.Data, Field.Offset, std::get<FFloat4>(Value));
			break;
		case EMaterialParameterType::Texture2D:
		case EMaterialParameterType::TextureCube:
		case EMaterialParameterType::SamplerPreset:
		{
			const xr_string& AssetPath = std::get<xr_string>(Value);
			if (AssetPath.empty())
			{
				AddError(Diagnostics, "parameter_pack.empty_resource", "Resource parameter '" + Field.Id.Value + "' has an empty asset path.");
				return false;
			}
			constexpr u32 InvalidIndex = FDescriptorHeapIndex::Invalid;
			WriteValue(Block.Data, Field.Offset, InvalidIndex);
			Block.Resources.push_back({Field.Id, Field.Type, Field.Offset, AssetPath});
			break;
		}
		case EMaterialParameterType::StaticBool:
		case EMaterialParameterType::StaticEnum:
			AddError(Diagnostics, "parameter_pack.static_field", "Static parameters cannot be stored in a GPU parameter block.");
			return false;
	}
	return true;
}
} // namespace

const FMaterialParameterLayoutField* FMaterialParameterLayout::Find(const FMaterialParameterId& Id) const noexcept
{
	const auto Found = std::ranges::lower_bound(Fields, Id, {}, &FMaterialParameterLayoutField::Id);
	return Found != Fields.end() && Found->Id == Id ? &*Found : nullptr;
}

bool FMaterialParameterLayoutResult::Succeeded() const noexcept
{
	return Value.StableHash != 0 && !HasErrors(Diagnostics);
}

bool FMaterialParameterPackResult::Succeeded() const noexcept
{
	return !Value.Data.empty() && !HasErrors(Diagnostics);
}

FMaterialParameterLayoutResult BuildMaterialParameterLayout(
	const xr_span<const FMaterialParameterDefinition> Definitions
)
{
	FMaterialParameterLayoutResult Result;
	xr_vector<const FMaterialParameterDefinition*> Sorted;
	Sorted.reserve(Definitions.size());
	xr_set<FMaterialParameterId> UniqueIds;
	xr_set<xr_string> UniqueHlslNames;
	for (const FMaterialParameterDefinition& Definition : Definitions)
	{
		if (!Definition.Id.IsValid() || !IsValidStableId(Definition.Id.Value))
		{
			AddError(Result.Diagnostics, "parameter_layout.invalid_id", "Material parameter has an invalid stable id.");
		}
		if (!UniqueIds.insert(Definition.Id).second)
		{
			AddError(Result.Diagnostics, "parameter_layout.duplicate_id", "Material parameter id '" + Definition.Id.Value + "' is duplicated.");
		}
		if (!ValueMatchesParameterType(Definition.DefaultValue, Definition.Type))
		{
			AddError(Result.Diagnostics, "parameter_layout.invalid_default", "Default value for parameter '" + Definition.Id.Value + "' has the wrong type.");
		}
		if (!Definition.IsStatic())
		{
			if (!UniqueHlslNames.insert(HlslFieldName(Definition.Id)).second)
			{
				AddError(Result.Diagnostics, "parameter_layout.hlsl_name_collision", "Parameter '" + Definition.Id.Value + "' collides with another generated HLSL field name.");
			}
			Sorted.push_back(&Definition);
		}
	}
	if (HasErrors(Result.Diagnostics))
	{
		return Result;
	}

	std::ranges::sort(Sorted, {}, [](const FMaterialParameterDefinition* Definition)
					  { return Definition->Id; });
	u32 Offset = 0;
	for (const FMaterialParameterDefinition* Definition : Sorted)
	{
		const u32 Size = ParameterSize(Definition->Type);
		if (Size == 0)
		{
			AddError(Result.Diagnostics, "parameter_layout.unsupported_type", "Runtime parameter '" + Definition->Id.Value + "' has an unsupported GPU type.");
			continue;
		}
		Offset = AlignUp(Offset, 4);
		if (Offset > std::numeric_limits<u32>::max() - Size)
		{
			AddError(Result.Diagnostics, "parameter_layout.too_large", "Material parameter layout exceeds 32-bit offsets.");
			break;
		}
		Result.Value.Fields.push_back({Definition->Id, Definition->Type, Offset, Size});
		Offset += Size;
	}
	if (HasErrors(Result.Diagnostics))
	{
		return Result;
	}

	Result.Value.ByteSize = std::max(MaterialParameterBlockAlignment, AlignUp(Offset, MaterialParameterBlockAlignment));
	Result.Value.StableHash = LayoutHash(Result.Value);
	return Result;
}

FMaterialParameterPackResult PackMaterialParameters(const FMaterialParameterLayout& Layout, const xr_span<const FMaterialParameterDefinition> Definitions, const FMaterialParameterMap& Values)
{
	FMaterialParameterPackResult Result;
	Result.Value.LayoutHash = Layout.StableHash;
	if (Layout.Version != MaterialParameterLayoutVersion || Layout.StableHash == 0 ||
		Layout.ByteSize < MaterialParameterBlockAlignment || Layout.ByteSize % MaterialParameterBlockAlignment != 0)
	{
		AddError(Result.Diagnostics, "parameter_pack.invalid_layout", "Material parameter layout is invalid or unsupported.");
		return Result;
	}
	Result.Value.Data.resize(Layout.ByteSize);

	xr_map<FMaterialParameterId, const FMaterialParameterDefinition*> ById;
	for (const FMaterialParameterDefinition& Definition : Definitions)
	{
		ById.emplace(Definition.Id, &Definition);
	}
	for (const auto& [Id, Value] : Values)
	{
		const auto Found = ById.find(Id);
		if (Found == ById.end())
		{
			AddError(Result.Diagnostics, "parameter_pack.unknown_parameter", "Packed values contain unknown parameter '" + Id.Value + "'.");
		}
		else if (Found->second->IsStatic())
		{
			AddError(Result.Diagnostics, "parameter_pack.static_parameter", "Static parameter '" + Id.Value + "' cannot be packed as runtime data.");
		}
		else if (!ValueMatchesParameterType(Value, Found->second->Type))
		{
			AddError(Result.Diagnostics, "parameter_pack.type_mismatch", "Packed value for parameter '" + Id.Value + "' has the wrong type.");
		}
	}
	if (HasErrors(Result.Diagnostics))
	{
		return Result;
	}

	for (const FMaterialParameterLayoutField& Field : Layout.Fields)
	{
		const auto Definition = ById.find(Field.Id);
		if (Definition == ById.end() || Definition->second->IsStatic() || Definition->second->Type != Field.Type ||
			Field.Offset + Field.Size > Result.Value.Data.size())
		{
			AddError(Result.Diagnostics, "parameter_pack.layout_mismatch", "Material layout field '" + Field.Id.Value + "' does not match its parameter definition.");
			continue;
		}
		const auto Override = Values.find(Field.Id);
		const FMaterialValue& Value = Override == Values.end() ? Definition->second->DefaultValue : Override->second;
		WriteParameterValue(Result.Value, Field, Value, Result.Diagnostics);
	}
	return Result;
}

FMaterialParameterPackResult PatchMaterialParameterResources(
	const FMaterialPackedParameterBlock& Source, const FMaterialResourceIndexResolver& Resolver
)
{
	if (!Resolver)
	{
		FMaterialParameterPackResult Result;
		Result.Value = Source;
		AddError(Result.Diagnostics, "parameter_patch.missing_resolver", "Material resource index resolver is not set.");
		return Result;
	}

	const FMaterialResourceReferenceIndexResolver ReferenceResolver =
		[&Resolver](const FMaterialParameterResourceReference& Reference)
	{
		return Resolver(Reference.Type, Reference.AssetPath);
	};
	return PatchMaterialParameterResources(Source, ReferenceResolver);
}

FMaterialParameterPackResult PatchMaterialParameterResources(
	const FMaterialPackedParameterBlock& Source,
	const FMaterialResourceReferenceIndexResolver& Resolver
)
{
	FMaterialParameterPackResult Result;
	Result.Value = Source;
	if (!Resolver)
	{
		AddError(Result.Diagnostics, "parameter_patch.missing_resolver", "Material resource index resolver is not set.");
		return Result;
	}
	for (const FMaterialParameterResourceReference& Reference : Source.Resources)
	{
		if (Reference.Offset > Result.Value.Data.size() || Result.Value.Data.size() - Reference.Offset < sizeof(u32))
		{
			AddError(Result.Diagnostics, "parameter_patch.invalid_offset", "Resource parameter '" + Reference.Parameter.Value + "' points outside the packed block.");
			continue;
		}
		const xr_optional<FDescriptorHeapIndex> Index = Resolver(Reference);
		if (!Index || !Index->IsValid())
		{
			AddError(Result.Diagnostics, "parameter_patch.unresolved_resource", "Resource parameter '" + Reference.Parameter.Value + "' could not be resolved.");
			continue;
		}
		WriteValue(Result.Value.Data, Reference.Offset, Index->Value);
	}
	return Result;
}

xr_string MaterialParameterHlslFieldName(const FMaterialParameterId& Id)
{
	return HlslFieldName(Id);
}

xr_string GenerateMaterialParameterHlsl(const FMaterialParameterLayout& Layout)
{
	if (Layout.Version != MaterialParameterLayoutVersion || Layout.StableHash == 0 ||
		Layout.ByteSize < MaterialParameterBlockAlignment || Layout.ByteSize % MaterialParameterBlockAlignment != 0)
	{
		return {};
	}

	std::ostringstream Hlsl;
	Hlsl << "#define MATERIAL_PARAMETER_BLOCK_SIZE " << Layout.ByteSize << "u\n";
	Hlsl << "#define MATERIAL_PARAMETER_LAYOUT_HASH_LOW "
		 << static_cast<u32>(Layout.StableHash) << "u\n";
	Hlsl << "#define MATERIAL_PARAMETER_LAYOUT_HASH_HIGH "
		 << static_cast<u32>(Layout.StableHash >> 32u) << "u\n";
	Hlsl << "struct MaterialParameters\n{\n";
	Hlsl << "    uint MaterialSamplerIndex;\n";
	for (const FMaterialParameterLayoutField& Field : Layout.Fields)
	{
		const xr_string_view Type = HlslType(Field.Type);
		if (Type.empty())
		{
			return {};
		}
		Hlsl << "    " << Type << ' ' << HlslFieldName(Field.Id) << ";\n";
	}
	Hlsl << "};\n";
	Hlsl << "MaterialParameters LoadMaterialParameters(ByteAddressBuffer MaterialData, uint DataOffset, uint DefaultSamplerIndex)\n{\n";
	Hlsl << "    MaterialParameters Result;\n";
	Hlsl << "    Result.MaterialSamplerIndex = DefaultSamplerIndex;\n";
	for (const FMaterialParameterLayoutField& Field : Layout.Fields)
	{
		Hlsl << "    Result." << HlslFieldName(Field.Id) << " = ";
		switch (Field.Type)
		{
			case EMaterialParameterType::Scalar:
				Hlsl << "asfloat(MaterialData.Load(DataOffset + " << Field.Offset << "u));\n";
				break;
			case EMaterialParameterType::Float2:
				Hlsl << "asfloat(MaterialData.Load2(DataOffset + " << Field.Offset << "u));\n";
				break;
			case EMaterialParameterType::Float3:
				Hlsl << "asfloat(MaterialData.Load3(DataOffset + " << Field.Offset << "u));\n";
				break;
			case EMaterialParameterType::Float4:
			case EMaterialParameterType::Color:
				Hlsl << "asfloat(MaterialData.Load4(DataOffset + " << Field.Offset << "u));\n";
				break;
			case EMaterialParameterType::Texture2D:
			case EMaterialParameterType::TextureCube:
			case EMaterialParameterType::SamplerPreset:
				Hlsl << "MaterialData.Load(DataOffset + " << Field.Offset << "u);\n";
				break;
			case EMaterialParameterType::StaticBool:
			case EMaterialParameterType::StaticEnum:
				return {};
		}
	}
	Hlsl << "    return Result;\n}\n";
	return Hlsl.str();
}
