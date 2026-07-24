#include "MaterialTypes.h"

#include <array>
#include <bit>
#include <cctype>
#include <cmath>
#include <limits>
#include <random>
#include <type_traits>
#include <utility>

namespace
{
constexpr u64 FnvOffset = 14695981039346656037ull;
constexpr u64 FnvPrime = 1099511628211ull;

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

void HashValue(u64& Hash, const FMaterialValue& Value) noexcept
{
	const u8 Type = static_cast<u8>(Value.index());
	HashBytes(Hash, &Type, sizeof(Type));

	std::visit(
		[&Hash](const auto& Item)
		{
			using TValue = std::decay_t<decltype(Item)>;
			if constexpr (std::is_same_v<TValue, std::monostate>)
			{
				return;
			}
			else if constexpr (std::is_same_v<TValue, xr_string>)
			{
				HashString(Hash, Item);
			}
			else
			{
				HashBytes(Hash, &Item, sizeof(Item));
			}
		},
		Value
	);
}
} // namespace

bool FMaterialParameterDefinition::IsStatic() const noexcept
{
	return Type == EMaterialParameterType::StaticBool || Type == EMaterialParameterType::StaticEnum;
}

u64 FMaterialPipelineKey::StableHash() const noexcept
{
	u64 Hash = FnvOffset;
	HashString(Hash, MasterMaterial.Value);
	for (const auto& [Id, Value] : StaticParameters)
	{
		HashString(Hash, Id.Value);
		HashValue(Hash, Value);
	}
	HashString(Hash, VertexFactory);
	HashString(Hash, RenderPassSignature);
	HashString(Hash, Backend);
	HashString(Hash, ShaderModel);
	HashString(Hash, CompilerOptions);
	return Hash;
}

TiramisuMaterialInstanceDynamic::TiramisuMaterialInstanceDynamic(FMaterialAssetId InMaster, FMaterialParameterMap InRuntimeParameters, FMaterialStaticParameterSet InStaticParameters)
	: Master(std::move(InMaster)), RuntimeParameters(std::move(InRuntimeParameters)), StaticParameters(std::move(InStaticParameters))
{
}

EMaterialUpdateError TiramisuMaterialInstanceDynamic::SetParameter(
	const FMaterialParameterDefinition& Definition, FMaterialValue Value
)
{
	if (Definition.IsStatic())
	{
		return EMaterialUpdateError::StaticParameterIsImmutable;
	}
	if (!ValueMatchesParameterType(Value, Definition.Type))
	{
		return EMaterialUpdateError::TypeMismatch;
	}
	RuntimeParameters[Definition.Id] = std::move(Value);
	return EMaterialUpdateError::None;
}

EMaterialUpdateError TiramisuMaterialInstanceDynamic::SetStaticParameter(
	const FMaterialParameterDefinition&, FMaterialValue
)
{
	return EMaterialUpdateError::StaticParameterIsImmutable;
}

bool IsValidStableId(const xr_string_view Value) noexcept
{
	if (Value.empty())
	{
		return false;
	}
	for (const char Character : Value)
	{
		const unsigned char Byte = static_cast<unsigned char>(Character);
		if (!std::isalnum(Byte) && Character != '-' && Character != '_' && Character != '.' && Character != '/' &&
			Character != ':')
		{
			return false;
		}
	}
	return true;
}

static xr_string FormatGuid(xr_array<u8, 16> Bytes, const u8 Version)
{
	Bytes[6] = static_cast<u8>((Bytes[6] & 0x0fu) | (Version << 4u));
	Bytes[8] = static_cast<u8>((Bytes[8] & 0x3fu) | 0x80u);

	constexpr char Hex[] = "0123456789abcdef";
	xr_string Result;
	Result.reserve(36);
	for (size_t Index = 0; Index < Bytes.size(); ++Index)
	{
		if (Index == 4 || Index == 6 || Index == 8 || Index == 10)
		{
			Result.push_back('-');
		}
		Result.push_back(Hex[Bytes[Index] >> 4u]);
		Result.push_back(Hex[Bytes[Index] & 0x0fu]);
	}
	return Result;
}

xr_string GenerateMaterialGuid()
{
	xr_array<u8, 16> Bytes{};
	std::random_device Random;
	for (u8& Byte : Bytes)
	{
		Byte = static_cast<u8>(Random());
	}

	return FormatGuid(Bytes, 4u);
}

xr_string GenerateDeterministicMaterialGuid(const xr_string_view Namespace, const xr_string_view Name)
{
	u64 First = FnvOffset;
	HashString(First, Namespace);
	HashString(First, Name);

	u64 Second = FnvOffset ^ 0x9e3779b97f4a7c15ull;
	HashString(Second, Name);
	HashString(Second, Namespace);

	xr_array<u8, 16> Bytes{};
	for (size_t Index = 0; Index < 8; ++Index)
	{
		Bytes[Index] = static_cast<u8>(First >> (Index * 8u));
		Bytes[Index + 8] = static_cast<u8>(Second >> (Index * 8u));
	}
	return FormatGuid(Bytes, 5u);
}

bool ValueMatchesParameterType(const FMaterialValue& Value, const EMaterialParameterType Type) noexcept
{
	switch (Type)
	{
		case EMaterialParameterType::Scalar:
			return std::holds_alternative<float>(Value);
		case EMaterialParameterType::Float2:
			return std::holds_alternative<FFloat2>(Value);
		case EMaterialParameterType::Float3:
			return std::holds_alternative<FFloat3>(Value);
		case EMaterialParameterType::Float4:
		case EMaterialParameterType::Color:
			return std::holds_alternative<FFloat4>(Value);
		case EMaterialParameterType::Texture2D:
		case EMaterialParameterType::TextureCube:
		case EMaterialParameterType::SamplerPreset:
			return std::holds_alternative<xr_string>(Value);
		case EMaterialParameterType::StaticBool:
			return std::holds_alternative<bool>(Value);
		case EMaterialParameterType::StaticEnum:
			return std::holds_alternative<s32>(Value);
	}
	return false;
}

EMaterialValueType ToValueType(const EMaterialParameterType Type) noexcept
{
	switch (Type)
	{
		case EMaterialParameterType::Scalar:
			return EMaterialValueType::Float1;
		case EMaterialParameterType::Float2:
			return EMaterialValueType::Float2;
		case EMaterialParameterType::Float3:
			return EMaterialValueType::Float3;
		case EMaterialParameterType::Float4:
		case EMaterialParameterType::Color:
			return EMaterialValueType::Float4;
		case EMaterialParameterType::Texture2D:
			return EMaterialValueType::Texture2D;
		case EMaterialParameterType::TextureCube:
			return EMaterialValueType::TextureCube;
		case EMaterialParameterType::SamplerPreset:
			return EMaterialValueType::Sampler;
		case EMaterialParameterType::StaticBool:
			return EMaterialValueType::Bool;
		case EMaterialParameterType::StaticEnum:
			return EMaterialValueType::Integer;
	}
	return EMaterialValueType::Invalid;
}

FMaterialPermutationStatistics CalculateMaterialPermutationStatistics(const xr_span<const FMaterialParameterDefinition> Parameters) noexcept
{
	FMaterialPermutationStatistics Result;
	for (const FMaterialParameterDefinition& Parameter : Parameters)
	{
		u64 Factor = 1;
		if (Parameter.Type == EMaterialParameterType::StaticBool)
		{
			++Result.StaticBoolParameters;
			Factor = 2;
		}
		else if (Parameter.Type == EMaterialParameterType::StaticEnum)
		{
			++Result.StaticEnumParameters;
			if (!Parameter.Minimum || !Parameter.Maximum ||
				!std::isfinite(*Parameter.Minimum) || !std::isfinite(*Parameter.Maximum) ||
				std::floor(*Parameter.Minimum) != *Parameter.Minimum ||
				std::floor(*Parameter.Maximum) != *Parameter.Maximum ||
				*Parameter.Minimum > *Parameter.Maximum)
			{
				Result.Exact = false;
			}
			else
			{
				const double Count = static_cast<double>(*Parameter.Maximum) -
									 static_cast<double>(*Parameter.Minimum) + 1.0;
				if (Count > static_cast<double>(std::numeric_limits<u64>::max()))
				{
					Result.Overflow = true;
					Result.Exact = false;
					Factor = std::numeric_limits<u64>::max();
				}
				else
				{
					Factor = static_cast<u64>(Count);
				}
			}
		}
		else
		{
			continue;
		}

		if (Factor != 0 && Result.PermutationCount >
							   std::numeric_limits<u64>::max() / Factor)
		{
			Result.PermutationCount = std::numeric_limits<u64>::max();
			Result.Overflow = true;
			Result.Exact = false;
		}
		else
		{
			Result.PermutationCount *= Factor;
		}
	}
	return Result;
}

xr_string_view ToString(const EMaterialDomain Value) noexcept
{
	switch (Value)
	{
		case EMaterialDomain::Surface:
			return "surface";
		case EMaterialDomain::Decal:
			return "decal";
		case EMaterialDomain::UI:
			return "ui";
		case EMaterialDomain::PostProcess:
			return "post_process";
	}
	return "surface";
}

xr_string_view ToString(const EMaterialBlendMode Value) noexcept
{
	switch (Value)
	{
		case EMaterialBlendMode::Opaque:
			return "opaque";
		case EMaterialBlendMode::Masked:
			return "masked";
		case EMaterialBlendMode::Translucent:
			return "translucent";
		case EMaterialBlendMode::Additive:
			return "additive";
		case EMaterialBlendMode::Modulate:
			return "modulate";
	}
	return "opaque";
}

xr_string_view ToString(const EMaterialShadingModel Value) noexcept
{
	switch (Value)
	{
		case EMaterialShadingModel::DefaultLit:
			return "default_lit";
		case EMaterialShadingModel::Unlit:
			return "unlit";
		case EMaterialShadingModel::Foliage:
			return "foliage";
		case EMaterialShadingModel::Hair:
			return "hair";
	}
	return "default_lit";
}

xr_string_view ToString(const EMaterialParameterType Value) noexcept
{
	switch (Value)
	{
		case EMaterialParameterType::Scalar:
			return "scalar";
		case EMaterialParameterType::Float2:
			return "float2";
		case EMaterialParameterType::Float3:
			return "float3";
		case EMaterialParameterType::Float4:
			return "float4";
		case EMaterialParameterType::Color:
			return "color";
		case EMaterialParameterType::Texture2D:
			return "texture2d";
		case EMaterialParameterType::TextureCube:
			return "texture_cube";
		case EMaterialParameterType::SamplerPreset:
			return "sampler_preset";
		case EMaterialParameterType::StaticBool:
			return "static_bool";
		case EMaterialParameterType::StaticEnum:
			return "static_enum";
	}
	return "scalar";
}

xr_string_view ToString(const EMaterialValueType Value) noexcept
{
	switch (Value)
	{
		case EMaterialValueType::Invalid:
			return "invalid";
		case EMaterialValueType::Float1:
			return "float";
		case EMaterialValueType::Float2:
			return "float2";
		case EMaterialValueType::Float3:
			return "float3";
		case EMaterialValueType::Float4:
			return "float4";
		case EMaterialValueType::Bool:
			return "bool";
		case EMaterialValueType::Integer:
			return "int";
		case EMaterialValueType::Texture2D:
			return "texture2d";
		case EMaterialValueType::TextureCube:
			return "texture_cube";
		case EMaterialValueType::Sampler:
			return "sampler";
	}
	return "invalid";
}

xr_optional<EMaterialDomain> ParseMaterialDomain(const xr_string_view Value) noexcept
{
	if (Value == "surface")
	{
		return EMaterialDomain::Surface;
	}
	if (Value == "decal")
	{
		return EMaterialDomain::Decal;
	}
	if (Value == "ui")
	{
		return EMaterialDomain::UI;
	}
	if (Value == "post_process")
	{
		return EMaterialDomain::PostProcess;
	}
	return std::nullopt;
}

xr_optional<EMaterialBlendMode> ParseMaterialBlendMode(const xr_string_view Value) noexcept
{
	if (Value == "opaque")
	{
		return EMaterialBlendMode::Opaque;
	}
	if (Value == "masked")
	{
		return EMaterialBlendMode::Masked;
	}
	if (Value == "translucent")
	{
		return EMaterialBlendMode::Translucent;
	}
	if (Value == "additive")
	{
		return EMaterialBlendMode::Additive;
	}
	if (Value == "modulate")
	{
		return EMaterialBlendMode::Modulate;
	}
	return std::nullopt;
}

xr_optional<EMaterialShadingModel> ParseMaterialShadingModel(const xr_string_view Value) noexcept
{
	if (Value == "default_lit")
	{
		return EMaterialShadingModel::DefaultLit;
	}
	if (Value == "unlit")
	{
		return EMaterialShadingModel::Unlit;
	}
	if (Value == "foliage")
	{
		return EMaterialShadingModel::Foliage;
	}
	if (Value == "hair")
	{
		return EMaterialShadingModel::Hair;
	}
	return std::nullopt;
}

xr_optional<EMaterialParameterType> ParseMaterialParameterType(const xr_string_view Value) noexcept
{
	if (Value == "scalar")
	{
		return EMaterialParameterType::Scalar;
	}
	if (Value == "float2")
	{
		return EMaterialParameterType::Float2;
	}
	if (Value == "float3")
	{
		return EMaterialParameterType::Float3;
	}
	if (Value == "float4")
	{
		return EMaterialParameterType::Float4;
	}
	if (Value == "color")
	{
		return EMaterialParameterType::Color;
	}
	if (Value == "texture2d")
	{
		return EMaterialParameterType::Texture2D;
	}
	if (Value == "texture_cube")
	{
		return EMaterialParameterType::TextureCube;
	}
	if (Value == "sampler_preset")
	{
		return EMaterialParameterType::SamplerPreset;
	}
	if (Value == "static_bool")
	{
		return EMaterialParameterType::StaticBool;
	}
	if (Value == "static_enum")
	{
		return EMaterialParameterType::StaticEnum;
	}
	return std::nullopt;
}

xr_optional<EMaterialValueType> ParseMaterialValueType(const xr_string_view Value) noexcept
{
	if (Value == "float" || Value == "float1")
	{
		return EMaterialValueType::Float1;
	}
	if (Value == "float2")
	{
		return EMaterialValueType::Float2;
	}
	if (Value == "float3")
	{
		return EMaterialValueType::Float3;
	}
	if (Value == "float4" || Value == "color")
	{
		return EMaterialValueType::Float4;
	}
	if (Value == "bool")
	{
		return EMaterialValueType::Bool;
	}
	if (Value == "int")
	{
		return EMaterialValueType::Integer;
	}
	if (Value == "texture2d")
	{
		return EMaterialValueType::Texture2D;
	}
	if (Value == "texture_cube")
	{
		return EMaterialValueType::TextureCube;
	}
	if (Value == "sampler")
	{
		return EMaterialValueType::Sampler;
	}
	return std::nullopt;
}
