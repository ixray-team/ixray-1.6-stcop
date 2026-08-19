#pragma once

#include "MaterialGraph.h"

#include <optional>
#include <span>
#include <string_view>
#include <vector>

// Schema одного допустимого node type, его pins и semantic rules.
struct FMaterialNodeDefinition
{
	xr_string_view Type;
	xr_string_view DisplayName;
	xr_string_view Category;
	u32 TypeVersion = 1;
	EMaterialValueType DefaultValueType = EMaterialValueType::Float1;
	bool ValueTypeConfigurable = false;
};

enum class EMaterialNodePropertyKind : u8
{
	Value,
	ParameterId,
	HlslExpression,
	String
};

// Schema редактируемого свойства material node.
struct FMaterialNodePropertyDefinition
{
	xr_string_view Name;
	xr_string_view DisplayName;
	EMaterialNodePropertyKind Kind = EMaterialNodePropertyKind::Value;
	bool Multiline = false;
};

// Результат проверки направления и совместимости типов graph link.
struct FMaterialGraphLinkValidationResult
{
	xr_vector<FMaterialDiagnostic> Diagnostics;

	[[nodiscard]] bool Succeeded() const noexcept;
};

// Результат проверки значения node property по schema.
struct FMaterialGraphNodePropertyValidationResult
{
	xr_vector<FMaterialDiagnostic> Diagnostics;

	[[nodiscard]] bool Succeeded() const noexcept;
};

// Именованный вход Custom HLSL expression с явно заданным material value type.
struct FMaterialCustomHlslInputDefinition
{
	xr_string Name;
	EMaterialValueType Type = EMaterialValueType::Float1;
};

// Результат проверки и перестроения типизированной сигнатуры Custom HLSL node.
struct FMaterialCustomHlslSignatureResult
{
	xr_vector<FMaterialDiagnostic> Diagnostics;

	[[nodiscard]] bool Succeeded() const noexcept;
};

// Единый каталог compiler, cooker tests и editor palette описывает только material
// expressions; stages и bindings принадлежат engine template.
// Возвращает canonical node schema и проверяет создаваемые nodes, properties и links.
[[nodiscard]] xr_span<const FMaterialNodeDefinition> GetMaterialNodeDefinitions() noexcept;
[[nodiscard]] const FMaterialNodeDefinition* FindMaterialNodeDefinition(xr_string_view Type) noexcept;
[[nodiscard]] xr_span<const FMaterialNodePropertyDefinition> GetMaterialNodePropertyDefinitions(
	xr_string_view NodeType
) noexcept;

// Создаёт versioned node со стабильными pin ids, производными от NodeId.
[[nodiscard]] xr_optional<FMaterialGraphNode> CreateMaterialGraphNode(
	xr_string_view Type, FMaterialNodeId NodeId, FFloat2 EditorPosition = {0.0f, 0.0f}, EMaterialValueType ValueType = EMaterialValueType::Invalid
);

[[nodiscard]] const FMaterialGraphNode* FindMaterialGraphNode(
	const FMaterialGraph& Graph, const FMaterialNodeId& NodeId
) noexcept;
[[nodiscard]] const FMaterialGraphPin* FindMaterialGraphPin(
	const FMaterialGraph& Graph, const FMaterialPinId& PinId
) noexcept;
[[nodiscard]] bool AreMaterialValueTypesCompatible(
	EMaterialValueType Source, EMaterialValueType Destination
) noexcept;

// Checks a candidate editor link before it is committed. Cycle diagnostics
// remain a compiler responsibility.
[[nodiscard]] FMaterialGraphLinkValidationResult ValidateMaterialGraphLink(
	const FMaterialGraph& Graph, const FMaterialPinId& FromPin, const FMaterialPinId& ToPin
);
[[nodiscard]] FMaterialGraphNodePropertyValidationResult ValidateMaterialGraphNodeProperty(
	const FMaterialGraphNode& Node, xr_string_view PropertyName, const FMaterialValue& Value
);

// Перестраивает входные pins и единственный Result pin Custom HLSL node.
// Стабильные имена сохраняют deterministic pin GUID.
[[nodiscard]] FMaterialCustomHlslSignatureResult ConfigureMaterialCustomHlslNode(
	FMaterialGraphNode& Node,
	xr_span<const FMaterialCustomHlslInputDefinition> Inputs,
	EMaterialValueType OutputType
);
