#pragma once

#include "MaterialTypes.h"

#include <map>
#include <string>
#include <string_view>
#include <vector>

enum class EMaterialPinDirection : u8
{
	Input,
	Output
};

// Типизированный входной или выходной pin material expression node.
struct FMaterialGraphPin
{
	FMaterialPinId Id;
	xr_string Name;
	EMaterialPinDirection Direction = EMaterialPinDirection::Input;
	EMaterialValueType Type = EMaterialValueType::Invalid;
};

// Versioned node material graph со стабильным GUID и editor position.
struct FMaterialGraphNode
{
	FMaterialNodeId Id;
	xr_string Type;
	u32 TypeVersion = 1;
	FFloat2 EditorPosition = {0.0f, 0.0f};
	xr_vector<FMaterialGraphPin> Pins;
	xr_map<xr_string, FMaterialValue> Properties;
};

// Направленная связь между совместимыми pins двух graph nodes.
struct FMaterialGraphLink
{
	xr_string Id;
	FMaterialPinId FromPin;
	FMaterialPinId ToPin;
};

// Сериализуемая семантическая модель material expression graph.
struct FMaterialGraph
{
	u32 Version = MaterialGraphVersion;
	xr_vector<FMaterialGraphNode> Nodes;
	xr_vector<FMaterialGraphLink> Links;
};

// Результат чтения и миграции graph JSON.
struct FMaterialGraphParseResult
{
	FMaterialGraph Graph;
	xr_vector<FMaterialDiagnostic> Diagnostics;

	[[nodiscard]] bool Succeeded() const noexcept;
};

// Параметры типизации, оптимизации и генерации HLSL graph compiler.
struct FMaterialGraphCompileOptions
{
	xr_vector<FMaterialParameterDefinition> Parameters;
	FMaterialStaticParameterSet StaticParameters;
	bool EmitNodeLineDirectives = true;
};

// Сгенерированный HLSL и node-addressable diagnostics graph compiler.
struct FMaterialGraphCompileResult
{
	xr_string GeneratedHlsl;
	xr_vector<FMaterialDiagnostic> Diagnostics;
	xr_vector<FMaterialParameterId> UsedParameters;

	[[nodiscard]] bool Succeeded() const noexcept;
};

// Читает versioned graph JSON и компилирует expressions в общий material HLSL contract.
[[nodiscard]] FMaterialGraphParseResult ParseMaterialGraphJson(xr_string_view JsonText);
[[nodiscard]] xr_string SerializeMaterialGraphJson(const FMaterialGraph& Graph);
[[nodiscard]] FMaterialGraphCompileResult CompileMaterialGraph(const FMaterialGraph& Graph, const FMaterialGraphCompileOptions& Options);
