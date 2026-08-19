#include "MaterialGraph.h"
#include "MaterialJsonHelpers.h"

#include <nlohmann/json.hpp>

#include <algorithm>
#include <cctype>
#include <charconv>
#include <cmath>
#include <iomanip>
#include <ranges>
#include <set>
#include <sstream>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>

namespace
{
using Json = nlohmann::json;

bool HasErrors(const xr_vector<FMaterialDiagnostic>& Diagnostics)
{
	return std::ranges::any_of(Diagnostics, [](const FMaterialDiagnostic& Diagnostic)
							   { return Diagnostic.Severity == EMaterialDiagnosticSeverity::Error; });
}

void AddDiagnostic(xr_vector<FMaterialDiagnostic>& Diagnostics, const xr_string_view Code, const xr_string& Message, const FMaterialNodeId Node = {}, const FMaterialPinId Pin = {}, const EMaterialDiagnosticSeverity Severity = EMaterialDiagnosticSeverity::Error)
{
	Diagnostics.push_back({Severity, xr_string(Code), Message, Node, Pin});
}

xr_optional<FMaterialValue> ParseValue(const Json& Value)
{
	bool Boolean = false;
	if (MaterialJsonDetail::TryGetBoolean(Value, Boolean))
	{
		return Boolean;
	}
	s32 Integer = 0;
	if (MaterialJsonDetail::TryGetInt32(Value, Integer))
	{
		return Integer;
	}
	float Scalar = 0.0f;
	if (MaterialJsonDetail::TryGetFloat(Value, Scalar))
	{
		return Scalar;
	}
	xr_string Text;
	if (MaterialJsonDetail::TryGetString(Value, Text))
	{
		return Text;
	}
	if (!Value.is_array())
	{
		return std::nullopt;
	}

	float Components[4]{};
	if (Value.size() < 2 || Value.size() > 4)
	{
		return std::nullopt;
	}
	for (size_t Index = 0; Index < Value.size(); ++Index)
	{
		if (!MaterialJsonDetail::TryGetFloat(Value[Index], Components[Index]))
		{
			return std::nullopt;
		}
	}
	if (Value.size() == 2)
	{
		return FFloat2{Components[0], Components[1]};
	}
	if (Value.size() == 3)
	{
		return FFloat3{Components[0], Components[1], Components[2]};
	}
	if (Value.size() == 4)
	{
		return FFloat4{Components[0], Components[1], Components[2], Components[3]};
	}
	return std::nullopt;
}

xr_string ReadGraphStringField(const Json& Object, const xr_string_view Name, xr_vector<FMaterialDiagnostic>& Diagnostics, const FMaterialNodeId Node = {})
{
	const Json* Field = MaterialJsonDetail::Find(Object, Name);
	if (!Field)
	{
		return {};
	}

	xr_string Result;
	if (!MaterialJsonDetail::TryGetString(*Field, Result))
	{
		AddDiagnostic(Diagnostics, "graph.invalid_field_type", "Field '" + xr_string(Name) + "' must be a string.", Node);
	}
	return Result;
}

u32 ReadGraphUIntField(const Json& Object, const xr_string_view Name, const u32 Default, xr_vector<FMaterialDiagnostic>& Diagnostics, const FMaterialNodeId Node = {})
{
	const Json* Field = MaterialJsonDetail::Find(Object, Name);
	if (!Field)
	{
		return Default;
	}

	u32 Result = Default;
	if (!MaterialJsonDetail::TryGetUInt32(*Field, Result))
	{
		AddDiagnostic(Diagnostics, "graph.invalid_field_type", "Field '" + xr_string(Name) + "' must be an unsigned integer.", Node);
	}
	return Result;
}

const Json& ReadGraphArrayField(const Json& Object, const xr_string_view Name, const Json& Empty, xr_vector<FMaterialDiagnostic>& Diagnostics, const FMaterialNodeId Node = {})
{
	const Json* Field = MaterialJsonDetail::Find(Object, Name);
	if (!Field)
	{
		return Empty;
	}
	if (!Field->is_array())
	{
		AddDiagnostic(Diagnostics, "graph.invalid_field_type", "Field '" + xr_string(Name) + "' must be an array.", Node);
		return Empty;
	}
	return *Field;
}

Json SerializeValue(const FMaterialValue& Value)
{
	return std::visit(
		[](const auto& Item) -> Json
		{
			using TValue = std::decay_t<decltype(Item)>;
			if constexpr (std::is_same_v<TValue, std::monostate>)
			{
				return nullptr;
			}
			else
			{
				return Item;
			}
		},
		Value
	);
}

const FMaterialGraphPin* FindPin(const FMaterialGraphNode& Node, const xr_string_view Name, const EMaterialPinDirection Direction)
{
	const auto Iterator = std::ranges::find_if(Node.Pins, [Name, Direction](const FMaterialGraphPin& Pin)
											   { return Pin.Name == Name && Pin.Direction == Direction; });
	return Iterator == Node.Pins.end() ? nullptr : &*Iterator;
}

bool IsNumeric(const EMaterialValueType Type)
{
	return Type >= EMaterialValueType::Float1 && Type <= EMaterialValueType::Float4;
}

u32 ComponentCount(const EMaterialValueType Type)
{
	if (!IsNumeric(Type))
	{
		return 0;
	}
	return static_cast<u32>(Type) - static_cast<u32>(EMaterialValueType::Float1) + 1;
}

bool IsValidSwizzlePattern(
	const xr_string_view Pattern,
	const EMaterialValueType Type
)
{
	const u32 Count = ComponentCount(Type);
	if (Count < 2 || Pattern.size() != Count)
	{
		return false;
	}

	constexpr xr_string_view PositionComponents = "xyzw";
	constexpr xr_string_view ColorComponents = "rgba";
	const bool UsesPositionSet =
		PositionComponents.find(Pattern.front()) != xr_string_view::npos;
	const xr_string_view Components = UsesPositionSet
		? PositionComponents
		: ColorComponents;
	for (const char Component : Pattern)
	{
		const size_t Index = Components.find(Component);
		if (Index == xr_string_view::npos || Index >= Count)
		{
			return false;
		}
	}
	return true;
}

bool TypesCompatible(const EMaterialValueType Source, const EMaterialValueType Destination)
{
	if (Source == Destination)
	{
		return true;
	}
	return Source == EMaterialValueType::Float1 && IsNumeric(Destination);
}

xr_string FloatLiteral(const float Value)
{
	std::ostringstream Stream;
	Stream << std::setprecision(9) << Value;
	xr_string Result = Stream.str();
	if (Result.find_first_of(".eE") == xr_string::npos)
	{
		Result += ".0";
	}
	return Result + "f";
}

xr_string ValueExpression(const FMaterialValue& Value)
{
	if (const auto* Scalar = std::get_if<float>(&Value))
	{
		return FloatLiteral(*Scalar);
	}
	if (const auto* Vector = std::get_if<FFloat2>(&Value))
	{
		return "float2(" + FloatLiteral((*Vector)[0]) + ", " + FloatLiteral((*Vector)[1]) + ")";
	}
	if (const auto* Vector = std::get_if<FFloat3>(&Value))
	{
		return "float3(" + FloatLiteral((*Vector)[0]) + ", " + FloatLiteral((*Vector)[1]) + ", " +
			   FloatLiteral((*Vector)[2]) + ")";
	}
	if (const auto* Vector = std::get_if<FFloat4>(&Value))
	{
		return "float4(" + FloatLiteral((*Vector)[0]) + ", " + FloatLiteral((*Vector)[1]) + ", " +
			   FloatLiteral((*Vector)[2]) + ", " + FloatLiteral((*Vector)[3]) + ")";
	}
	if (const auto* Boolean = std::get_if<bool>(&Value))
	{
		return *Boolean ? "true" : "false";
	}
	if (const auto* Integer = std::get_if<s32>(&Value))
	{
		return std::to_string(*Integer);
	}
	return {};
}

xr_string ParameterFieldName(const FMaterialParameterId& Id)
{
	xr_string Result = "P_";
	for (const char Character : Id.Value)
	{
		Result += std::isalnum(static_cast<unsigned char>(Character)) ? Character : '_';
	}
	return Result;
}

struct FPinOwner
{
	const FMaterialGraphNode* Node = nullptr;
	const FMaterialGraphPin* Pin = nullptr;
};

struct FExpression
{
	EMaterialValueType Type = EMaterialValueType::Invalid;
	xr_string Code;
	xr_optional<FMaterialValue> Constant;
};

class FGraphCompiler
{
public:
	FGraphCompiler(const FMaterialGraph& InGraph, const FMaterialGraphCompileOptions& InOptions)
		: Graph(InGraph), Options(InOptions)
	{
		for (const FMaterialParameterDefinition& Parameter : Options.Parameters)
		{
			Parameters.emplace(Parameter.Id.Value, &Parameter);
		}
	}

	FMaterialGraphCompileResult Run()
	{
		ValidateAndIndex();
		if (HasErrors(Result.Diagnostics))
		{
			return std::move(Result);
		}

		const FMaterialGraphNode* Output = nullptr;
		for (const FMaterialGraphNode& Node : Graph.Nodes)
		{
			if (Node.Type != "material_output")
			{
				continue;
			}
			if (Output)
			{
				AddDiagnostic(Result.Diagnostics, "graph.multiple_outputs", "Material graph contains more than one material_output node.", Node.Id);
			}
			Output = &Node;
		}
		if (!Output)
		{
			AddDiagnostic(Result.Diagnostics, "graph.missing_output", "Material graph requires one material_output node.");
			return std::move(Result);
		}

		std::ostringstream Hlsl;
		Hlsl << "// Generated by xrTiramisuMaterialCore. Do not edit.\n";
		Hlsl << "void EvaluateMaterial(\n";
		Hlsl << "    in MaterialContext Context,\n";
		Hlsl << "    in MaterialParameters Parameters,\n";
		Hlsl << "    out MaterialInputs Result)\n{\n";
		Hlsl << "    Result.BaseColor = float3(1.0f, 1.0f, 1.0f);\n";
		Hlsl << "    Result.Normal = Context.WorldNormal;\n";
		Hlsl << "    Result.Roughness = 0.5f;\n";
		Hlsl << "    Result.Metallic = 0.0f;\n";
		Hlsl << "    Result.AmbientOcclusion = 1.0f;\n";
		Hlsl << "    Result.Emissive = float3(0.0f, 0.0f, 0.0f);\n";
		Hlsl << "    Result.Opacity = 1.0f;\n";
		Hlsl << "    Result.OpacityMask = 1.0f;\n";
		Hlsl << "    Result.WorldPositionOffset = float3(0.0f, 0.0f, 0.0f);\n";

		struct FOutputField
		{
			xr_string_view Pin;
			xr_string_view Field;
			EMaterialValueType Type;
		};
		constexpr FOutputField Fields[] = {
			{"BaseColor", "BaseColor", EMaterialValueType::Float3},
			{"Normal", "Normal", EMaterialValueType::Float3},
			{"Roughness", "Roughness", EMaterialValueType::Float1},
			{"Metallic", "Metallic", EMaterialValueType::Float1},
			{"AmbientOcclusion", "AmbientOcclusion", EMaterialValueType::Float1},
			{"Emissive", "Emissive", EMaterialValueType::Float3},
			{"Opacity", "Opacity", EMaterialValueType::Float1},
			{"OpacityMask", "OpacityMask", EMaterialValueType::Float1},
			{"WorldPositionOffset", "WorldPositionOffset", EMaterialValueType::Float3},
		};

		for (const FOutputField& Field : Fields)
		{
			const FMaterialGraphPin* Input = FindPin(*Output, Field.Pin, EMaterialPinDirection::Input);
			if (!Input)
			{
				continue;
			}
			const auto LinkIterator = InputLinks.find(Input->Id.Value);
			if (LinkIterator == InputLinks.end())
			{
				continue;
			}

			const xr_optional<FExpression> Expression = CompileOutput(LinkIterator->second);
			if (!Expression)
			{
				continue;
			}
			if (!TypesCompatible(Expression->Type, Field.Type))
			{
				AddDiagnostic(Result.Diagnostics, "graph.output_type_mismatch", "Material output '" + xr_string(Field.Pin) + "' expects " + xr_string(ToString(Field.Type)) + ", got " + xr_string(ToString(Expression->Type)) + ".", Output->Id, Input->Id);
				continue;
			}
			if (Options.EmitNodeLineDirectives)
			{
				Hlsl << "#line 1 \"material-node/" << Output->Id.Value << "\"\n";
			}
			Hlsl << "    Result." << Field.Field << " = " << Convert(Expression->Code, Expression->Type, Field.Type) << ";\n";
		}
		Hlsl << "}\n";

		std::ranges::sort(Result.UsedParameters, [](const FMaterialParameterId& Left, const FMaterialParameterId& Right)
						  { return Left.Value < Right.Value; });
		Result.UsedParameters.erase(std::ranges::unique(Result.UsedParameters).begin(), Result.UsedParameters.end());
		if (!HasErrors(Result.Diagnostics))
		{
			Result.GeneratedHlsl = Hlsl.str();
		}
		return std::move(Result);
	}

private:
	void ValidateAndIndex()
	{
		if (Graph.Version != MaterialGraphVersion)
		{
			AddDiagnostic(Result.Diagnostics, "graph.unsupported_version", "Unsupported material graph version " + std::to_string(Graph.Version) + ".");
		}

		xr_hash_set<xr_string> NodeIds;
		for (const FMaterialGraphNode& Node : Graph.Nodes)
		{
			if (!IsValidStableId(Node.Id.Value) || !NodeIds.emplace(Node.Id.Value).second)
			{
				AddDiagnostic(Result.Diagnostics, "graph.invalid_node_id", "Node GUID is missing or duplicated.", Node.Id);
			}
			for (const FMaterialGraphPin& Pin : Node.Pins)
			{
				if (!IsValidStableId(Pin.Id.Value) || !Pins.emplace(Pin.Id.Value, FPinOwner{&Node, &Pin}).second)
				{
					AddDiagnostic(Result.Diagnostics, "graph.invalid_pin_id", "Pin GUID is missing or duplicated.", Node.Id, Pin.Id);
				}
				if (Pin.Type == EMaterialValueType::Invalid)
				{
					AddDiagnostic(Result.Diagnostics, "graph.invalid_pin_type", "Pin has an invalid value type.", Node.Id, Pin.Id);
				}
			}
		}

		for (const FMaterialGraphLink& Link : Graph.Links)
		{
			const auto From = Pins.find(Link.FromPin.Value);
			const auto To = Pins.find(Link.ToPin.Value);
			if (From == Pins.end() || To == Pins.end())
			{
				AddDiagnostic(Result.Diagnostics, "graph.dangling_link", "Link references a missing pin.", {}, Link.ToPin);
				continue;
			}
			if (From->second.Pin->Direction != EMaterialPinDirection::Output ||
				To->second.Pin->Direction != EMaterialPinDirection::Input)
			{
				AddDiagnostic(Result.Diagnostics, "graph.invalid_link_direction", "Links must connect output pins to input pins.", To->second.Node->Id, To->second.Pin->Id);
				continue;
			}
			if (!TypesCompatible(From->second.Pin->Type, To->second.Pin->Type))
			{
				AddDiagnostic(Result.Diagnostics, "graph.link_type_mismatch", "Cannot connect " + xr_string(ToString(From->second.Pin->Type)) + " to " + xr_string(ToString(To->second.Pin->Type)) + ".", To->second.Node->Id, To->second.Pin->Id);
				continue;
			}
			if (!InputLinks.emplace(Link.ToPin.Value, Link.FromPin.Value).second)
			{
				AddDiagnostic(Result.Diagnostics, "graph.multiple_input_links", "An input pin accepts only one link.", To->second.Node->Id, To->second.Pin->Id);
			}
		}
	}

	xr_optional<FExpression> CompileInput(const FMaterialGraphNode& Node, const xr_string_view Name)
	{
		const FMaterialGraphPin* Pin = FindPin(Node, Name, EMaterialPinDirection::Input);
		if (!Pin)
		{
			AddDiagnostic(Result.Diagnostics, "graph.missing_pin", "Node '" + Node.Type + "' is missing input pin '" + xr_string(Name) + "'.", Node.Id);
			return std::nullopt;
		}
		const auto Link = InputLinks.find(Pin->Id.Value);
		if (Link == InputLinks.end())
		{
			AddDiagnostic(Result.Diagnostics, "graph.unconnected_input", "Required input pin '" + xr_string(Name) + "' is not connected.", Node.Id, Pin->Id);
			return std::nullopt;
		}
		return CompileOutput(Link->second);
	}

	xr_optional<FExpression> CompileOutput(const xr_string& PinId)
	{
		if (const auto Cached = ExpressionCache.find(PinId); Cached != ExpressionCache.end())
		{
			return Cached->second;
		}
		if (!ActivePins.emplace(PinId).second)
		{
			const FPinOwner& Owner = Pins.at(PinId);
			AddDiagnostic(Result.Diagnostics, "graph.cycle", "Material expression graph contains a cycle.", Owner.Node->Id, Owner.Pin->Id);
			return std::nullopt;
		}

		const FPinOwner Owner = Pins.at(PinId);
		xr_optional<FExpression> Expression = CompileNodeOutput(*Owner.Node, *Owner.Pin);
		ActivePins.erase(PinId);
		if (Expression)
		{
			ExpressionCache.emplace(PinId, *Expression);
		}
		return Expression;
	}

	xr_optional<FExpression> CompileNodeOutput(const FMaterialGraphNode& Node, const FMaterialGraphPin& Output)
	{
		if (Node.Type == "constant")
		{
			const auto Value = Node.Properties.find("value");
			if (Value == Node.Properties.end())
			{
				AddDiagnostic(Result.Diagnostics, "graph.constant_missing_value", "Constant node has no value.", Node.Id, Output.Id);
				return std::nullopt;
			}
			const xr_string Code = ValueExpression(Value->second);
			if (Code.empty())
			{
				AddDiagnostic(Result.Diagnostics, "graph.constant_invalid_value", "Constant value is not an HLSL scalar/vector.", Node.Id, Output.Id);
				return std::nullopt;
			}
			return FExpression{Output.Type, Code, Value->second};
		}

		if (Node.Type == "parameter")
		{
			const auto IdValue = Node.Properties.find("parameter_id");
			if (IdValue == Node.Properties.end() || !std::holds_alternative<xr_string>(IdValue->second))
			{
				AddDiagnostic(Result.Diagnostics, "graph.parameter_missing_id", "Parameter node has no parameter_id.", Node.Id, Output.Id);
				return std::nullopt;
			}
			const FMaterialParameterId Id{std::get<xr_string>(IdValue->second)};
			const auto Parameter = Parameters.find(Id.Value);
			if (Parameter == Parameters.end())
			{
				AddDiagnostic(Result.Diagnostics, "graph.unknown_parameter", "Parameter '" + Id.Value + "' is not declared by the master material.", Node.Id, Output.Id);
				return std::nullopt;
			}
			if (ToValueType(Parameter->second->Type) != Output.Type)
			{
				AddDiagnostic(Result.Diagnostics, "graph.parameter_type_mismatch", "Parameter pin type does not match its declaration.", Node.Id, Output.Id);
				return std::nullopt;
			}
			Result.UsedParameters.push_back(Id);
			return FExpression{Output.Type, "Parameters." + ParameterFieldName(Id), std::nullopt};
		}

		if (Node.Type == "texcoord0")
		{
			return FExpression{Output.Type, "Context.TexCoord0", std::nullopt};
		}
		if (Node.Type == "vertex_color")
		{
			return FExpression{Output.Type, "Context.VertexColor", std::nullopt};
		}
		if (Node.Type == "vertex_normal")
		{
			return FExpression{Output.Type, "Context.WorldNormal", std::nullopt};
		}
		if (Node.Type == "world_position")
		{
			return FExpression{Output.Type, "Context.WorldPosition", std::nullopt};
		}
		if (Node.Type == "camera_position")
		{
			return FExpression{Output.Type, "Context.CameraPosition", std::nullopt};
		}
		if (Node.Type == "time")
		{
			return FExpression{Output.Type, "Context.Time", std::nullopt};
		}

		if (Node.Type == "add" || Node.Type == "subtract" || Node.Type == "multiply" || Node.Type == "divide")
		{
			const xr_optional<FExpression> A = CompileInput(Node, "A");
			const xr_optional<FExpression> B = CompileInput(Node, "B");
			if (!A || !B)
			{
				return std::nullopt;
			}
			const char* Operator = Node.Type == "add" ? "+" : Node.Type == "subtract" ? "-"
														  : Node.Type == "multiply"	  ? "*"
																					  : "/";
			if (const auto* Left = A->Constant ? std::get_if<float>(&*A->Constant) : nullptr)
			{
				if (const auto* Right = B->Constant ? std::get_if<float>(&*B->Constant) : nullptr)
				{
					if (Node.Type == "divide" && std::abs(*Right) <= 1e-8f)
					{
						AddDiagnostic(Result.Diagnostics, "graph.divide_by_zero", "Constant division by zero.", Node.Id, Output.Id);
						return std::nullopt;
					}
					const float Folded = Node.Type == "add" ? *Left + *Right : Node.Type == "subtract" ? *Left - *Right
																		   : Node.Type == "multiply"   ? *Left * *Right
																									   : *Left / *Right;
					return FExpression{Output.Type, FloatLiteral(Folded), FMaterialValue{Folded}};
				}
			}
			return FExpression{Output.Type, "(" + A->Code + " " + Operator + " " + B->Code + ")", std::nullopt};
		}

		if (Node.Type == "lerp")
		{
			const auto A = CompileInput(Node, "A");
			const auto B = CompileInput(Node, "B");
			const auto Alpha = CompileInput(Node, "Alpha");
			if (!A || !B || !Alpha)
			{
				return std::nullopt;
			}
			return FExpression{Output.Type, "lerp(" + A->Code + ", " + B->Code + ", " + Alpha->Code + ")", std::nullopt};
		}
		if (Node.Type == "clamp")
		{
			const auto Value = CompileInput(Node, "Value");
			const auto Minimum = CompileInput(Node, "Min");
			const auto Maximum = CompileInput(Node, "Max");
			if (!Value || !Minimum || !Maximum)
			{
				return std::nullopt;
			}
			return FExpression{Output.Type, "clamp(" + Value->Code + ", " + Minimum->Code + ", " + Maximum->Code + ")", std::nullopt};
		}
		if (Node.Type == "normalize")
		{
			const auto Value = CompileInput(Node, "Value");
			if (!Value)
			{
				return std::nullopt;
			}
			return FExpression{Output.Type, "normalize(" + Value->Code + ")", std::nullopt};
		}
		if (Node.Type == "dot")
		{
			const auto A = CompileInput(Node, "A");
			const auto B = CompileInput(Node, "B");
			if (!A || !B)
			{
				return std::nullopt;
			}
			return FExpression{Output.Type, "dot(" + A->Code + ", " + B->Code + ")", std::nullopt};
		}
		if (Node.Type == "make_vector")
		{
			constexpr xr_array ComponentNames = {"X", "Y", "Z", "W"};
			xr_vector<FExpression> Components;
			Components.reserve(ComponentCount(Output.Type));
			for (u32 Index = 0; Index < ComponentCount(Output.Type); ++Index)
			{
				const xr_optional<FExpression> Component =
					CompileInput(Node, ComponentNames[Index]);
				if (!Component)
				{
					return std::nullopt;
				}
				Components.push_back(*Component);
			}

			xr_string Code = xr_string(ToString(Output.Type)) + "(";
			for (size_t Index = 0; Index < Components.size(); ++Index)
			{
				if (Index != 0)
				{
					Code += ", ";
				}
				Code += Components[Index].Code;
			}
			Code += ")";
			return FExpression{Output.Type, std::move(Code), std::nullopt};
		}
		if (Node.Type == "break_vector")
		{
			if (Output.Name.size() != 1 ||
				xr_string_view{"XYZW"}.find(Output.Name.front()) == xr_string_view::npos)
			{
				AddDiagnostic(
					Result.Diagnostics,
					"graph.invalid_break_vector_output",
					"Break Float output must be X, Y, Z or W.",
					Node.Id,
					Output.Id
				);
				return std::nullopt;
			}
			const xr_optional<FExpression> Value = CompileInput(Node, "Value");
			if (!Value)
			{
				return std::nullopt;
			}
			const FMaterialGraphPin* ValuePin = FindPin(
				Node,
				"Value",
				EMaterialPinDirection::Input
			);
			if (!ValuePin)
			{
				AddDiagnostic(
					Result.Diagnostics,
					"graph.missing_pin",
					"Break Float node is missing input pin 'Value'.",
					Node.Id,
					Output.Id
				);
				return std::nullopt;
			}
			const char Component = static_cast<char>(
				std::tolower(static_cast<unsigned char>(Output.Name.front()))
			);
			xr_string Code = "(" + Convert(
				Value->Code,
				Value->Type,
				ValuePin->Type
			) + ").";
			Code += Component;
			return FExpression{
				Output.Type,
				std::move(Code),
				std::nullopt
			};
		}
		if (Node.Type == "swizzle")
		{
			const auto PatternProperty = Node.Properties.find("pattern");
			const xr_string* Pattern = PatternProperty == Node.Properties.end()
				? nullptr
				: std::get_if<xr_string>(&PatternProperty->second);
			if (!Pattern || !IsValidSwizzlePattern(*Pattern, Output.Type))
			{
				AddDiagnostic(
					Result.Diagnostics,
					"graph.invalid_swizzle_pattern",
					"Swizzle pattern must use valid xyzw or rgba components and preserve vector width.",
					Node.Id,
					Output.Id
				);
				return std::nullopt;
			}
			const xr_optional<FExpression> Value = CompileInput(Node, "Value");
			if (!Value)
			{
				return std::nullopt;
			}
			const FMaterialGraphPin* ValuePin = FindPin(
				Node,
				"Value",
				EMaterialPinDirection::Input
			);
			if (!ValuePin)
			{
				AddDiagnostic(
					Result.Diagnostics,
					"graph.missing_pin",
					"Swizzle node is missing input pin 'Value'.",
					Node.Id,
					Output.Id
				);
				return std::nullopt;
			}
			return FExpression{
				Output.Type,
				"(" + Convert(
					Value->Code,
					Value->Type,
					ValuePin->Type
				) + ")." + *Pattern,
				std::nullopt
			};
		}
		if (Node.Type == "fresnel")
		{
			const auto Normal = CompileInput(Node, "Normal");
			const auto Exponent = CompileInput(Node, "Exponent");
			if (!Normal || !Exponent)
			{
				return std::nullopt;
			}
			return FExpression{Output.Type, "pow(saturate(1.0f - dot(normalize(" + Normal->Code + "), normalize(Context.CameraVector))), " + Exponent->Code + ")", std::nullopt};
		}
		if (Node.Type == "static_switch")
		{
			const auto IdValue = Node.Properties.find("parameter_id");
			if (IdValue == Node.Properties.end() || !std::holds_alternative<xr_string>(IdValue->second))
			{
				AddDiagnostic(Result.Diagnostics, "graph.static_switch_missing_id", "Static switch has no parameter_id.", Node.Id, Output.Id);
				return std::nullopt;
			}
			const FMaterialParameterId Id{std::get<xr_string>(IdValue->second)};
			const auto StaticValue = Options.StaticParameters.find(Id);
			if (StaticValue == Options.StaticParameters.end() || !std::holds_alternative<bool>(StaticValue->second))
			{
				AddDiagnostic(Result.Diagnostics, "graph.static_switch_missing_value", "Static switch value is not present in the permutation.", Node.Id, Output.Id);
				return std::nullopt;
			}
			Result.UsedParameters.push_back(Id);
			return CompileInput(Node, std::get<bool>(StaticValue->second) ? "True" : "False");
		}
		if (Node.Type == "texture_sample")
		{
			xr_optional<FExpression> Texture;
			const FMaterialGraphPin* TexturePin = FindPin(
				Node, "Texture", EMaterialPinDirection::Input
			);
			if (TexturePin && InputLinks.contains(TexturePin->Id.Value))
			{
				Texture = CompileInput(Node, "Texture");
			}
			else
			{
				const auto IdValue = Node.Properties.find("texture_parameter_id");
				const xr_string* Id = IdValue == Node.Properties.end()
										  ? nullptr
										  : std::get_if<xr_string>(&IdValue->second);
				if (!Id || Id->empty())
				{
					Texture = CompileInput(Node, "Texture");
				}
				else
				{
					const auto Parameter = Parameters.find(*Id);
					if (Parameter == Parameters.end())
					{
						AddDiagnostic(Result.Diagnostics, "graph.unknown_texture_parameter", "Texture parameter '" + *Id + "' is not declared by the master material.", Node.Id, TexturePin ? TexturePin->Id : FMaterialPinId{});
						return std::nullopt;
					}
					if (Parameter->second->Type != EMaterialParameterType::Texture2D)
					{
						AddDiagnostic(Result.Diagnostics, "graph.texture_parameter_type", "texture_sample requires a texture2d parameter.", Node.Id, TexturePin ? TexturePin->Id : FMaterialPinId{});
						return std::nullopt;
					}
					const FMaterialParameterId ParameterId{*Id};
					Result.UsedParameters.push_back(ParameterId);
					Texture = FExpression{EMaterialValueType::Texture2D, "Parameters." + ParameterFieldName(ParameterId), std::nullopt};
				}
			}
			const auto Coordinates = CompileInput(Node, "UV");
			if (!Texture || !Coordinates)
			{
				return std::nullopt;
			}
			if (Texture->Type != EMaterialValueType::Texture2D)
			{
				AddDiagnostic(Result.Diagnostics, "graph.texture_type", "texture_sample currently requires texture2d.", Node.Id, Output.Id);
				return std::nullopt;
			}
			xr_string Code = "SampleMaterialTexture2D(" + Texture->Code + ", Parameters.MaterialSamplerIndex, " + Coordinates->Code + ")";
			if (Output.Name == "R")
			{
				Code += ".r";
			}
			else if (Output.Name == "G")
			{
				Code += ".g";
			}
			else if (Output.Name == "B")
			{
				Code += ".b";
			}
			else if (Output.Name == "A")
			{
				Code += ".a";
			}
			else if (Output.Name == "RGB")
			{
				Code += ".rgb";
			}
			return FExpression{Output.Type, std::move(Code), std::nullopt};
		}
		if (Node.Type == "custom_hlsl")
		{
			const auto CodeProperty = Node.Properties.find("code");
			if (CodeProperty == Node.Properties.end() || !std::holds_alternative<xr_string>(CodeProperty->second))
			{
				AddDiagnostic(Result.Diagnostics, "graph.custom_hlsl_missing_code", "Custom HLSL node has no code expression.", Node.Id, Output.Id);
				return std::nullopt;
			}
			xr_string Code = std::get<xr_string>(CodeProperty->second);
			constexpr xr_string_view Forbidden[] = {"register", "cbuffer", "Texture2D", "TextureCube", "RWTexture", "#include", "#define", "void "};
			for (const xr_string_view Token : Forbidden)
			{
				if (Code.find(Token) != xr_string::npos)
				{
					AddDiagnostic(Result.Diagnostics, "graph.custom_hlsl_forbidden_token", "Custom HLSL expression contains forbidden token '" + xr_string(Token) + "'.", Node.Id, Output.Id);
					return std::nullopt;
				}
			}
			for (const FMaterialGraphPin& Pin : Node.Pins)
			{
				if (Pin.Direction != EMaterialPinDirection::Input)
				{
					continue;
				}
				const auto Input = CompileInput(Node, Pin.Name);
				if (!Input)
				{
					return std::nullopt;
				}
				const xr_string Marker = "{" + Pin.Name + "}";
				const xr_string InputCode = Convert(
					Input->Code,
					Input->Type,
					Pin.Type
				);
				size_t Position = 0;
				while ((Position = Code.find(Marker, Position)) != xr_string::npos)
				{
					Code.replace(Position, Marker.size(), "(" + InputCode + ")");
					Position += InputCode.size() + 2;
				}
			}
			if (Code.find_first_of("{}") != xr_string::npos)
			{
				AddDiagnostic(
					Result.Diagnostics,
					"graph.custom_hlsl_unknown_input",
					"Custom HLSL expression references an undeclared input marker.",
					Node.Id,
					Output.Id
				);
				return std::nullopt;
			}
			return FExpression{Output.Type, "(" + Code + ")", std::nullopt};
		}

		AddDiagnostic(Result.Diagnostics, "graph.unknown_node_type", "Unknown material node type '" + Node.Type + "'.", Node.Id, Output.Id);
		return std::nullopt;
	}

	static xr_string Convert(const xr_string& Code, const EMaterialValueType Source, const EMaterialValueType Destination)
	{
		if (Source == Destination)
		{
			return Code;
		}
		if (Source == EMaterialValueType::Float1 && IsNumeric(Destination))
		{
			return xr_string(ToString(Destination)) + "(" + Code + ")";
		}
		return Code;
	}

	const FMaterialGraph& Graph;
	const FMaterialGraphCompileOptions& Options;
	FMaterialGraphCompileResult Result;
	xr_hash_map<xr_string, FPinOwner> Pins;
	xr_hash_map<xr_string, xr_string> InputLinks;
	xr_hash_map<xr_string, const FMaterialParameterDefinition*> Parameters;
	xr_hash_map<xr_string, FExpression> ExpressionCache;
	xr_hash_set<xr_string> ActivePins;
};
} // namespace

bool FMaterialGraphParseResult::Succeeded() const noexcept
{
	return !HasErrors(Diagnostics);
}

bool FMaterialGraphCompileResult::Succeeded() const noexcept
{
	return !GeneratedHlsl.empty() && !HasErrors(Diagnostics);
}

FMaterialGraphParseResult ParseMaterialGraphJson(const xr_string_view JsonText)
{
	FMaterialGraphParseResult Result;
	try
	{
		const Json Root = Json::parse(JsonText, nullptr, false);
		if (Root.is_discarded())
		{
			AddDiagnostic(Result.Diagnostics, "graph.invalid_json", "Material graph contains invalid JSON.");
			return Result;
		}
		if (!Root.is_object())
		{
			AddDiagnostic(Result.Diagnostics, "graph.invalid_root", "Material graph root must be an object.");
			return Result;
		}
		Result.Graph.Version = ReadGraphUIntField(Root, "version", 0u, Result.Diagnostics);
		const Json* Nodes = MaterialJsonDetail::Find(Root, "nodes");
		if (!Nodes || !Nodes->is_array())
		{
			AddDiagnostic(Result.Diagnostics, "graph.invalid_nodes", "graph.nodes must be an array.");
			return Result;
		}
		const Json EmptyArray = Json::array();

		for (const Json& NodeJson : *Nodes)
		{
			if (!NodeJson.is_object())
			{
				AddDiagnostic(Result.Diagnostics, "graph.invalid_node", "Every graph node must be an object.");
				continue;
			}
			FMaterialGraphNode Node;
			Node.Id.Value = ReadGraphStringField(NodeJson, "guid", Result.Diagnostics);
			Node.Type = ReadGraphStringField(NodeJson, "type", Result.Diagnostics, Node.Id);
			Node.TypeVersion = ReadGraphUIntField(NodeJson, "type_version", 1u, Result.Diagnostics, Node.Id);
			if (const Json* Position = MaterialJsonDetail::Find(NodeJson, "position"))
			{
				float X = 0.0f;
				float Y = 0.0f;
				if (!Position->is_array() || Position->size() != 2 ||
					!MaterialJsonDetail::TryGetFloat((*Position)[0], X) || !MaterialJsonDetail::TryGetFloat((*Position)[1], Y))
				{
					AddDiagnostic(Result.Diagnostics, "graph.invalid_position", "Node position must be an array of two numeric values.", Node.Id);
				}
				else
				{
					Node.EditorPosition = {X, Y};
				}
			}
			for (const Json& PinJson : ReadGraphArrayField(NodeJson, "pins", EmptyArray, Result.Diagnostics, Node.Id))
			{
				if (!PinJson.is_object())
				{
					AddDiagnostic(Result.Diagnostics, "graph.invalid_pin", "Every graph pin must be an object.", Node.Id);
					continue;
				}
				FMaterialGraphPin Pin;
				Pin.Id.Value = ReadGraphStringField(PinJson, "guid", Result.Diagnostics, Node.Id);
				Pin.Name = ReadGraphStringField(PinJson, "name", Result.Diagnostics, Node.Id);
				const xr_string Direction = ReadGraphStringField(PinJson, "direction", Result.Diagnostics, Node.Id);
				if (Direction == "output")
				{
					Pin.Direction = EMaterialPinDirection::Output;
				}
				else
				{
					Pin.Direction = EMaterialPinDirection::Input;
					if (!Direction.empty() && Direction != "input")
					{
						AddDiagnostic(Result.Diagnostics, "graph.invalid_pin_direction", "Pin direction must be 'input' or 'output'.", Node.Id, Pin.Id);
					}
				}
				const auto Type = ParseMaterialValueType(ReadGraphStringField(PinJson, "type", Result.Diagnostics, Node.Id));
				Pin.Type = Type.value_or(EMaterialValueType::Invalid);
				Node.Pins.push_back(std::move(Pin));
			}
			if (const Json* Properties = MaterialJsonDetail::Find(NodeJson, "properties"))
			{
				if (!Properties->is_object())
				{
					AddDiagnostic(Result.Diagnostics, "graph.invalid_field_type", "Field 'properties' must be an object.", Node.Id);
				}
				else
				{
					for (const auto& [Name, ValueJson] : Properties->items())
					{
						if (const auto Value = ParseValue(ValueJson))
						{
							Node.Properties.emplace(Name, *Value);
						}
						else
						{
							AddDiagnostic(Result.Diagnostics, "graph.invalid_property", "Unsupported property value '" + Name + "'.", Node.Id);
						}
					}
				}
			}
			Result.Graph.Nodes.push_back(std::move(Node));
		}

		for (const Json& LinkJson : ReadGraphArrayField(Root, "links", EmptyArray, Result.Diagnostics))
		{
			if (!LinkJson.is_object())
			{
				AddDiagnostic(Result.Diagnostics, "graph.invalid_link", "Every graph link must be an object.");
				continue;
			}
			FMaterialGraphLink Link;
			Link.Id = ReadGraphStringField(LinkJson, "guid", Result.Diagnostics);
			Link.FromPin.Value = ReadGraphStringField(LinkJson, "from_pin", Result.Diagnostics);
			Link.ToPin.Value = ReadGraphStringField(LinkJson, "to_pin", Result.Diagnostics);
			Result.Graph.Links.push_back(std::move(Link));
		}
	}
	catch (const std::exception& Error)
	{
		AddDiagnostic(Result.Diagnostics, "graph.invalid_json", Error.what());
	}
	return Result;
}

xr_string SerializeMaterialGraphJson(const FMaterialGraph& Graph)
{
	Json Root;
	Root["version"] = Graph.Version;
	Root["nodes"] = Json::array();
	for (const FMaterialGraphNode& Node : Graph.Nodes)
	{
		Json NodeJson;
		NodeJson["guid"] = Node.Id.Value;
		NodeJson["type"] = Node.Type;
		NodeJson["type_version"] = Node.TypeVersion;
		NodeJson["position"] = Node.EditorPosition;
		NodeJson["pins"] = Json::array();
		for (const FMaterialGraphPin& Pin : Node.Pins)
		{
			NodeJson["pins"].push_back({{"guid", Pin.Id.Value}, {"name", Pin.Name}, {"direction", Pin.Direction == EMaterialPinDirection::Output ? "output" : "input"}, {"type", ToString(Pin.Type)}});
		}
		NodeJson["properties"] = Json::object();
		for (const auto& [Name, Value] : Node.Properties)
		{
			NodeJson["properties"][Name.c_str()] = SerializeValue(Value);
		}
		Root["nodes"].push_back(std::move(NodeJson));
	}
	Root["links"] = Json::array();
	for (const FMaterialGraphLink& Link : Graph.Links)
	{
		Root["links"].push_back({{"guid", Link.Id}, {"from_pin", Link.FromPin.Value}, {"to_pin", Link.ToPin.Value}});
	}
	return Root.dump(2);
}

FMaterialGraphCompileResult CompileMaterialGraph(const FMaterialGraph& Graph, const FMaterialGraphCompileOptions& Options)
{
	return FGraphCompiler(Graph, Options).Run();
}
