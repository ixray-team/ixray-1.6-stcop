#pragma once

#include "TiramisuMaterialCoreTypes.h"

#include <array>
#include <compare>
#include <cstddef>
#include <cstdint>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

constexpr u32 MaterialAssetVersion = 1;
constexpr u32 MaterialGraphVersion = 1;
constexpr u32 LegacyMaterialMapVersion = 1;

template <typename Tag>
// Строго типизированный стабильный идентификатор, исключающий смешение GUID разных сущностей.
struct TStableId
{
	xr_string Value;

	[[nodiscard]] bool IsValid() const noexcept { return !Value.empty(); }
	auto operator<=>(const TStableId&) const = default;
};

struct FMaterialAssetIdTag;
struct FMaterialParameterIdTag;
struct FMaterialNodeIdTag;
struct FMaterialPinIdTag;

using FMaterialAssetId = TStableId<FMaterialAssetIdTag>;
using FMaterialParameterId = TStableId<FMaterialParameterIdTag>;
using FMaterialNodeId = TStableId<FMaterialNodeIdTag>;
using FMaterialPinId = TStableId<FMaterialPinIdTag>;

enum class EMaterialDomain : u8
{
	Surface,
	Decal,
	UI,
	PostProcess
};

enum class EMaterialBlendMode : u8
{
	Opaque,
	Masked,
	Translucent,
	Additive,
	Modulate
};

enum class EMaterialShadingModel : u8
{
	DefaultLit,
	Unlit,
	Foliage,
	Hair
};

enum class EMaterialParameterType : u8
{
	Scalar,
	Float2,
	Float3,
	Float4,
	Color,
	Texture2D,
	TextureCube,
	SamplerPreset,
	StaticBool,
	StaticEnum
};

enum class EMaterialValueType : u8
{
	Invalid,
	Float1,
	Float2,
	Float3,
	Float4,
	Bool,
	Integer,
	Texture2D,
	TextureCube,
	Sampler
};

enum class EMaterialDiagnosticSeverity : u8
{
	Info,
	Warning,
	Error
};

using FFloat2 = xr_array<float, 2>;
using FFloat3 = xr_array<float, 3>;
using FFloat4 = xr_array<float, 4>;
using FMaterialValue = std::variant<std::monostate, float, FFloat2, FFloat3, FFloat4, bool, s32, xr_string>;

// Диагностика material subsystem с severity, code, asset и node context.
struct FMaterialDiagnostic
{
	EMaterialDiagnosticSeverity Severity = EMaterialDiagnosticSeverity::Error;
	xr_string Code;
	xr_string Message;
	FMaterialNodeId Node;
	FMaterialPinId Pin;
};

// Объявление material parameter со стабильным GUID, типом, default и UI metadata.
struct FMaterialParameterDefinition
{
	FMaterialParameterId Id;
	xr_string Name;
	EMaterialParameterType Type = EMaterialParameterType::Scalar;
	FMaterialValue DefaultValue = 0.0f;
	xr_string DisplayName;
	xr_string Category;
	xr_string Description;
	xr_optional<float> Minimum;
	xr_optional<float> Maximum;

	[[nodiscard]] bool IsStatic() const noexcept;
	auto operator<=>(const FMaterialParameterDefinition&) const = default;
};

using FMaterialParameterMap = xr_map<FMaterialParameterId, FMaterialValue>;
using FMaterialStaticParameterSet = xr_map<FMaterialParameterId, FMaterialValue>;

// Сводка количества static combinations и скомпилированных permutations.
struct FMaterialPermutationStatistics
{
	u64 PermutationCount = 1;
	size_t StaticBoolParameters = 0;
	size_t StaticEnumParameters = 0;
	bool Exact = true;
	bool Overflow = false;
};

// Детерминированный ключ pipeline из material, pass, vertex factory, API и static parameters.
struct FMaterialPipelineKey
{
	FMaterialAssetId MasterMaterial;
	FMaterialStaticParameterSet StaticParameters;
	xr_string VertexFactory;
	xr_string RenderPassSignature;
	xr_string Backend;
	xr_string ShaderModel;
	xr_string CompilerOptions;

	[[nodiscard]] u64 StableHash() const noexcept;
	auto operator<=>(const FMaterialPipelineKey&) const = default;
};

// Публичный generation-counted handle material asset в runtime library.
struct FMaterialHandle
{
	u32 Index = UINT32_MAX;
	u32 Generation = 0;

	[[nodiscard]] bool IsValid() const noexcept { return Index != UINT32_MAX && Generation != 0; }
	auto operator<=>(const FMaterialHandle&) const = default;
};

template <typename T>
// Пул объектов с защитой от устаревших handles через счётчик поколения.
class TGenerationPool
{
public:
	[[nodiscard]] FMaterialHandle Add(T Value)
	{
		if (!FreeSlots.empty())
		{
			const u32 Index = FreeSlots.back();
			FreeSlots.pop_back();
			FSlot& Slot = Slots[Index];
			Slot.Value = std::move(Value);
			return {Index, Slot.Generation};
		}

		FSlot& Slot = Slots.emplace_back();
		Slot.Value = std::move(Value);
		return {static_cast<u32>(Slots.size() - 1), Slot.Generation};
	}

	[[nodiscard]] T* Get(const FMaterialHandle Handle) noexcept
	{
		if (!IsAlive(Handle))
		{
			return nullptr;
		}
		return &*Slots[Handle.Index].Value;
	}

	[[nodiscard]] const T* Get(const FMaterialHandle Handle) const noexcept
	{
		if (!IsAlive(Handle))
		{
			return nullptr;
		}
		return &*Slots[Handle.Index].Value;
	}

	[[nodiscard]] bool Replace(const FMaterialHandle Handle, T Value)
	{
		T* Existing = Get(Handle);
		if (!Existing)
		{
			return false;
		}
		*Existing = std::move(Value);
		return true;
	}

	[[nodiscard]] bool Remove(const FMaterialHandle Handle)
	{
		if (!IsAlive(Handle))
		{
			return false;
		}

		FSlot& Slot = Slots[Handle.Index];
		Slot.Value.reset();
		++Slot.Generation;
		if (Slot.Generation == 0)
		{
			Slot.Generation = 1;
		}
		FreeSlots.push_back(Handle.Index);
		return true;
	}

	[[nodiscard]] bool IsAlive(const FMaterialHandle Handle) const noexcept
	{
		return Handle.Index < Slots.size() && Slots[Handle.Index].Generation == Handle.Generation &&
			   Slots[Handle.Index].Value.has_value();
	}

private:
	// Внутренний слот generation pool с optional value и текущим поколением.
	struct FSlot
	{
		xr_optional<T> Value;
		u32 Generation = 1;
	};

	xr_vector<FSlot> Slots;
	xr_vector<u32> FreeSlots;
};

enum class EMaterialUpdateError : u8
{
	None,
	UnknownParameter,
	TypeMismatch,
	StaticParameterIsImmutable
};

// Runtime instance, разрешающий менять только нестатические параметры без смены pipeline.
class TiramisuMaterialInstanceDynamic
{
public:
	TiramisuMaterialInstanceDynamic() = default;
	TiramisuMaterialInstanceDynamic(FMaterialAssetId InMaster, FMaterialParameterMap InRuntimeParameters, FMaterialStaticParameterSet InStaticParameters);

	[[nodiscard]] EMaterialUpdateError SetParameter(
		const FMaterialParameterDefinition& Definition, FMaterialValue Value
	);
	[[nodiscard]] EMaterialUpdateError SetStaticParameter(
		const FMaterialParameterDefinition& Definition, FMaterialValue Value
	);

	[[nodiscard]] const FMaterialAssetId& GetMaster() const noexcept { return Master; }
	[[nodiscard]] const FMaterialParameterMap& GetRuntimeParameters() const noexcept { return RuntimeParameters; }
	[[nodiscard]] const FMaterialStaticParameterSet& GetStaticParameters() const noexcept { return StaticParameters; }

private:
	FMaterialAssetId Master;
	FMaterialParameterMap RuntimeParameters;
	FMaterialStaticParameterSet StaticParameters;
};

// Общие операции над GUID, типами, permutations и строковыми enum-представлениями.
[[nodiscard]] bool IsValidStableId(xr_string_view Value) noexcept;
[[nodiscard]] xr_string GenerateMaterialGuid();
[[nodiscard]] xr_string GenerateDeterministicMaterialGuid(
	xr_string_view Namespace, xr_string_view Name
);
[[nodiscard]] bool ValueMatchesParameterType(const FMaterialValue& Value, EMaterialParameterType Type) noexcept;
[[nodiscard]] EMaterialValueType ToValueType(EMaterialParameterType Type) noexcept;
[[nodiscard]] FMaterialPermutationStatistics CalculateMaterialPermutationStatistics(
	xr_span<const FMaterialParameterDefinition> Parameters
) noexcept;
[[nodiscard]] xr_string_view ToString(EMaterialDomain Value) noexcept;
[[nodiscard]] xr_string_view ToString(EMaterialBlendMode Value) noexcept;
[[nodiscard]] xr_string_view ToString(EMaterialShadingModel Value) noexcept;
[[nodiscard]] xr_string_view ToString(EMaterialParameterType Value) noexcept;
[[nodiscard]] xr_string_view ToString(EMaterialValueType Value) noexcept;
[[nodiscard]] xr_optional<EMaterialDomain> ParseMaterialDomain(xr_string_view Value) noexcept;
[[nodiscard]] xr_optional<EMaterialBlendMode> ParseMaterialBlendMode(xr_string_view Value) noexcept;
[[nodiscard]] xr_optional<EMaterialShadingModel> ParseMaterialShadingModel(xr_string_view Value) noexcept;
[[nodiscard]] xr_optional<EMaterialParameterType> ParseMaterialParameterType(xr_string_view Value) noexcept;
[[nodiscard]] xr_optional<EMaterialValueType> ParseMaterialValueType(xr_string_view Value) noexcept;
