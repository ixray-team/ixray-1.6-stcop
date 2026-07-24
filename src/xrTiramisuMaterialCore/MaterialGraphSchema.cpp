#include "MaterialGraphSchema.h"

#include <algorithm>
#include <array>
#include <cctype>
#include <ranges>
#include <string>

namespace
{
constexpr xr_array Definitions = {
    FMaterialNodeDefinition{"material_output", "Material Output", "Output", 1, EMaterialValueType::Invalid, false},
    FMaterialNodeDefinition{"constant", "Constant", "Constants", 1, EMaterialValueType::Float1, true},
    FMaterialNodeDefinition{"parameter", "Parameter", "Parameters", 1, EMaterialValueType::Float1, true},
    FMaterialNodeDefinition{"add", "Add", "Math", 1, EMaterialValueType::Float1, true},
    FMaterialNodeDefinition{"subtract", "Subtract", "Math", 1, EMaterialValueType::Float1, true},
    FMaterialNodeDefinition{"multiply", "Multiply", "Math", 1, EMaterialValueType::Float1, true},
    FMaterialNodeDefinition{"divide", "Divide", "Math", 1, EMaterialValueType::Float1, true},
    FMaterialNodeDefinition{"lerp", "Lerp", "Math", 1, EMaterialValueType::Float1, true},
    FMaterialNodeDefinition{"clamp", "Clamp", "Math", 1, EMaterialValueType::Float1, true},
    FMaterialNodeDefinition{"normalize", "Normalize", "Vector", 1, EMaterialValueType::Float3, true},
    FMaterialNodeDefinition{"dot", "Dot Product", "Vector", 1, EMaterialValueType::Float3, true},
    FMaterialNodeDefinition{"fresnel", "Fresnel", "Vector", 1, EMaterialValueType::Float1, false},
    FMaterialNodeDefinition{"texture_sample", "Texture Sample", "Texture", 1, EMaterialValueType::Float4, false},
    FMaterialNodeDefinition{"texcoord0", "Texture Coordinate 0", "Coordinates", 1, EMaterialValueType::Float2, false},
    FMaterialNodeDefinition{"vertex_color", "Vertex Color", "Coordinates", 1, EMaterialValueType::Float4, false},
    FMaterialNodeDefinition{"vertex_normal", "Vertex Normal", "Coordinates", 1, EMaterialValueType::Float3, false},
    FMaterialNodeDefinition{"world_position", "World Position", "Coordinates", 1, EMaterialValueType::Float3, false},
    FMaterialNodeDefinition{"camera_position", "Camera Position", "Coordinates", 1, EMaterialValueType::Float3, false},
    FMaterialNodeDefinition{"time", "Time", "Coordinates", 1, EMaterialValueType::Float1, false},
    FMaterialNodeDefinition{"static_switch", "Static Switch", "Parameters", 1, EMaterialValueType::Float1, true},
    FMaterialNodeDefinition{"custom_hlsl", "Custom HLSL", "Custom", 1, EMaterialValueType::Float1, true},
};

constexpr xr_array ConstantProperties = {
    FMaterialNodePropertyDefinition{"value", "Value", EMaterialNodePropertyKind::Value, false},
};

constexpr xr_array ParameterProperties = {
    FMaterialNodePropertyDefinition{
        "parameter_id", "Parameter", EMaterialNodePropertyKind::ParameterId, false},
};

constexpr xr_array TextureSampleProperties = {
    FMaterialNodePropertyDefinition{
        "texture_parameter_id", "Texture Parameter", EMaterialNodePropertyKind::ParameterId, false},
};

constexpr xr_array CustomHlslProperties = {
    FMaterialNodePropertyDefinition{
        "code", "HLSL Expression", EMaterialNodePropertyKind::HlslExpression, true},
};

bool IsNumeric(const EMaterialValueType Type) noexcept
{
    return Type >= EMaterialValueType::Float1 && Type <= EMaterialValueType::Float4;
}

bool IsVector(const EMaterialValueType Type) noexcept
{
    return Type >= EMaterialValueType::Float2 && Type <= EMaterialValueType::Float4;
}

bool ValueMatchesGraphType(const FMaterialValue& Value, const EMaterialValueType Type) noexcept
{
    switch (Type)
    {
    case EMaterialValueType::Float1: return std::holds_alternative<float>(Value);
    case EMaterialValueType::Float2: return std::holds_alternative<FFloat2>(Value);
    case EMaterialValueType::Float3: return std::holds_alternative<FFloat3>(Value);
    case EMaterialValueType::Float4: return std::holds_alternative<FFloat4>(Value);
    case EMaterialValueType::Bool: return std::holds_alternative<bool>(Value);
    case EMaterialValueType::Integer: return std::holds_alternative<s32>(Value);
    default: return false;
    }
}

xr_string PinId(const FMaterialNodeId& NodeId, const xr_string_view Name,
    const EMaterialPinDirection Direction)
{
    xr_string Semantic = Direction == EMaterialPinDirection::Input ? "input:" : "output:";
    for (const char Character : Name)
    {
        const unsigned char Value = static_cast<unsigned char>(Character);
        Semantic += std::isalnum(Value) ? static_cast<char>(std::tolower(Value)) : '_';
    }
    return GenerateDeterministicMaterialGuid(NodeId.Value, Semantic);
}

void AddPin(FMaterialGraphNode& Node, const xr_string_view Name,
    const EMaterialPinDirection Direction, const EMaterialValueType Type)
{
    Node.Pins.push_back({{PinId(Node.Id, Name, Direction)}, xr_string(Name), Direction, Type});
}

FMaterialValue DefaultValue(const EMaterialValueType Type)
{
    switch (Type)
    {
    case EMaterialValueType::Float2: return FFloat2{0.0f, 0.0f};
    case EMaterialValueType::Float3: return FFloat3{0.0f, 0.0f, 0.0f};
    case EMaterialValueType::Float4: return FFloat4{0.0f, 0.0f, 0.0f, 0.0f};
    case EMaterialValueType::Bool: return false;
    case EMaterialValueType::Integer: return s32{0};
    default: return 0.0f;
    }
}

void AddBinaryPins(FMaterialGraphNode& Node, const EMaterialValueType Type)
{
    AddPin(Node, "A", EMaterialPinDirection::Input, Type);
    AddPin(Node, "B", EMaterialPinDirection::Input, Type);
    AddPin(Node, "Result", EMaterialPinDirection::Output, Type);
}

void AddDiagnostic(FMaterialGraphLinkValidationResult& Result, const xr_string_view Code,
    xr_string Message, const FMaterialNodeId Node = {}, const FMaterialPinId Pin = {})
{
    Result.Diagnostics.push_back(
        {EMaterialDiagnosticSeverity::Error, xr_string(Code), std::move(Message), Node, Pin});
}

void AddDiagnostic(FMaterialGraphNodePropertyValidationResult& Result,
    const xr_string_view Code, xr_string Message, const FMaterialNodeId Node = {})
{
    Result.Diagnostics.push_back(
        {EMaterialDiagnosticSeverity::Error, xr_string(Code), std::move(Message), Node, {}});
}

struct FPinOwner
{
    const FMaterialGraphNode* Node = nullptr;
    const FMaterialGraphPin* Pin = nullptr;
};

FPinOwner FindPinOwner(const FMaterialGraph& Graph, const FMaterialPinId& Id) noexcept
{
    for (const FMaterialGraphNode& Node : Graph.Nodes)
    {
        const auto Pin = std::ranges::find_if(Node.Pins,
            [&Id](const FMaterialGraphPin& Candidate) { return Candidate.Id == Id; });
        if (Pin != Node.Pins.end())
            return {&Node, &*Pin};
    }
    return {};
}
} // namespace

bool FMaterialGraphLinkValidationResult::Succeeded() const noexcept
{
    return std::ranges::none_of(Diagnostics,
        [](const FMaterialDiagnostic& Diagnostic)
        { return Diagnostic.Severity == EMaterialDiagnosticSeverity::Error; });
}

bool FMaterialGraphNodePropertyValidationResult::Succeeded() const noexcept
{
    return std::ranges::none_of(Diagnostics,
        [](const FMaterialDiagnostic& Diagnostic)
        { return Diagnostic.Severity == EMaterialDiagnosticSeverity::Error; });
}

xr_span<const FMaterialNodeDefinition> GetMaterialNodeDefinitions() noexcept
{
    return Definitions;
}

const FMaterialNodeDefinition* FindMaterialNodeDefinition(const xr_string_view Type) noexcept
{
    const auto Definition = std::ranges::find(Definitions, Type, &FMaterialNodeDefinition::Type);
    return Definition == Definitions.end() ? nullptr : &*Definition;
}

xr_span<const FMaterialNodePropertyDefinition> GetMaterialNodePropertyDefinitions(
    const xr_string_view NodeType) noexcept
{
    if (NodeType == "constant")
        return ConstantProperties;
    if (NodeType == "parameter" || NodeType == "static_switch")
        return ParameterProperties;
    if (NodeType == "texture_sample")
        return TextureSampleProperties;
    if (NodeType == "custom_hlsl")
        return CustomHlslProperties;
    return {};
}

xr_optional<FMaterialGraphNode> CreateMaterialGraphNode(const xr_string_view Type,
    FMaterialNodeId NodeId, const FFloat2 EditorPosition, EMaterialValueType ValueType)
{
    const FMaterialNodeDefinition* Definition = FindMaterialNodeDefinition(Type);
    if (!Definition || !NodeId.IsValid())
        return std::nullopt;
    if (ValueType == EMaterialValueType::Invalid)
        ValueType = Definition->DefaultValueType;

    FMaterialGraphNode Node;
    Node.Id = std::move(NodeId);
    Node.Type = xr_string(Type);
    Node.TypeVersion = Definition->TypeVersion;
    Node.EditorPosition = EditorPosition;

    if (Type == "material_output")
    {
        AddPin(Node, "BaseColor", EMaterialPinDirection::Input, EMaterialValueType::Float3);
        AddPin(Node, "Normal", EMaterialPinDirection::Input, EMaterialValueType::Float3);
        AddPin(Node, "Roughness", EMaterialPinDirection::Input, EMaterialValueType::Float1);
        AddPin(Node, "Metallic", EMaterialPinDirection::Input, EMaterialValueType::Float1);
        AddPin(Node, "AmbientOcclusion", EMaterialPinDirection::Input, EMaterialValueType::Float1);
        AddPin(Node, "Emissive", EMaterialPinDirection::Input, EMaterialValueType::Float3);
        AddPin(Node, "Opacity", EMaterialPinDirection::Input, EMaterialValueType::Float1);
        AddPin(Node, "OpacityMask", EMaterialPinDirection::Input, EMaterialValueType::Float1);
        AddPin(Node, "WorldPositionOffset", EMaterialPinDirection::Input, EMaterialValueType::Float3);
    }
    else if (Type == "constant")
    {
        if (!IsNumeric(ValueType) && ValueType != EMaterialValueType::Bool &&
            ValueType != EMaterialValueType::Integer)
            ValueType = EMaterialValueType::Float1;
        AddPin(Node, "Value", EMaterialPinDirection::Output, ValueType);
        Node.Properties["value"] = DefaultValue(ValueType);
    }
    else if (Type == "parameter")
    {
        AddPin(Node, "Value", EMaterialPinDirection::Output, ValueType);
        Node.Properties["parameter_id"] = xr_string{};
    }
    else if (Type == "add" || Type == "subtract" || Type == "multiply" || Type == "divide")
    {
        if (!IsNumeric(ValueType))
            ValueType = EMaterialValueType::Float1;
        AddBinaryPins(Node, ValueType);
    }
    else if (Type == "lerp")
    {
        if (!IsNumeric(ValueType)) ValueType = EMaterialValueType::Float1;
        AddPin(Node, "A", EMaterialPinDirection::Input, ValueType);
        AddPin(Node, "B", EMaterialPinDirection::Input, ValueType);
        AddPin(Node, "Alpha", EMaterialPinDirection::Input, EMaterialValueType::Float1);
        AddPin(Node, "Result", EMaterialPinDirection::Output, ValueType);
    }
    else if (Type == "clamp")
    {
        if (!IsNumeric(ValueType)) ValueType = EMaterialValueType::Float1;
        AddPin(Node, "Value", EMaterialPinDirection::Input, ValueType);
        AddPin(Node, "Min", EMaterialPinDirection::Input, ValueType);
        AddPin(Node, "Max", EMaterialPinDirection::Input, ValueType);
        AddPin(Node, "Result", EMaterialPinDirection::Output, ValueType);
    }
    else if (Type == "normalize")
    {
        if (!IsVector(ValueType)) ValueType = EMaterialValueType::Float3;
        AddPin(Node, "Value", EMaterialPinDirection::Input, ValueType);
        AddPin(Node, "Result", EMaterialPinDirection::Output, ValueType);
    }
    else if (Type == "dot")
    {
        if (!IsVector(ValueType)) ValueType = EMaterialValueType::Float3;
        AddPin(Node, "A", EMaterialPinDirection::Input, ValueType);
        AddPin(Node, "B", EMaterialPinDirection::Input, ValueType);
        AddPin(Node, "Result", EMaterialPinDirection::Output, EMaterialValueType::Float1);
    }
    else if (Type == "fresnel")
    {
        AddPin(Node, "Normal", EMaterialPinDirection::Input, EMaterialValueType::Float3);
        AddPin(Node, "Exponent", EMaterialPinDirection::Input, EMaterialValueType::Float1);
        AddPin(Node, "Result", EMaterialPinDirection::Output, EMaterialValueType::Float1);
    }
    else if (Type == "texture_sample")
    {
        AddPin(Node, "Texture", EMaterialPinDirection::Input, EMaterialValueType::Texture2D);
        AddPin(Node, "UV", EMaterialPinDirection::Input, EMaterialValueType::Float2);
        AddPin(Node, "RGBA", EMaterialPinDirection::Output, EMaterialValueType::Float4);
        AddPin(Node, "RGB", EMaterialPinDirection::Output, EMaterialValueType::Float3);
        AddPin(Node, "R", EMaterialPinDirection::Output, EMaterialValueType::Float1);
        AddPin(Node, "G", EMaterialPinDirection::Output, EMaterialValueType::Float1);
        AddPin(Node, "B", EMaterialPinDirection::Output, EMaterialValueType::Float1);
        AddPin(Node, "A", EMaterialPinDirection::Output, EMaterialValueType::Float1);
        Node.Properties["texture_parameter_id"] = xr_string{};
    }
    else if (Type == "texcoord0") AddPin(Node, "UV", EMaterialPinDirection::Output, EMaterialValueType::Float2);
    else if (Type == "vertex_color") AddPin(Node, "Color", EMaterialPinDirection::Output, EMaterialValueType::Float4);
    else if (Type == "vertex_normal") AddPin(Node, "Normal", EMaterialPinDirection::Output, EMaterialValueType::Float3);
    else if (Type == "world_position" || Type == "camera_position")
        AddPin(Node, "Position", EMaterialPinDirection::Output, EMaterialValueType::Float3);
    else if (Type == "time") AddPin(Node, "Time", EMaterialPinDirection::Output, EMaterialValueType::Float1);
    else if (Type == "static_switch")
    {
        if (!IsNumeric(ValueType)) ValueType = EMaterialValueType::Float1;
        AddPin(Node, "True", EMaterialPinDirection::Input, ValueType);
        AddPin(Node, "False", EMaterialPinDirection::Input, ValueType);
        AddPin(Node, "Result", EMaterialPinDirection::Output, ValueType);
        Node.Properties["parameter_id"] = xr_string{};
    }
    else if (Type == "custom_hlsl")
    {
        if (!IsNumeric(ValueType)) ValueType = EMaterialValueType::Float1;
        AddPin(Node, "In", EMaterialPinDirection::Input, ValueType);
        AddPin(Node, "Result", EMaterialPinDirection::Output, ValueType);
        Node.Properties["code"] = xr_string{"{In}"};
    }
    return Node;
}

const FMaterialGraphNode* FindMaterialGraphNode(
    const FMaterialGraph& Graph, const FMaterialNodeId& NodeId) noexcept
{
    const auto Node = std::ranges::find(Graph.Nodes, NodeId, &FMaterialGraphNode::Id);
    return Node == Graph.Nodes.end() ? nullptr : &*Node;
}

const FMaterialGraphPin* FindMaterialGraphPin(
    const FMaterialGraph& Graph, const FMaterialPinId& PinId) noexcept
{
    return FindPinOwner(Graph, PinId).Pin;
}

bool AreMaterialValueTypesCompatible(
    const EMaterialValueType Source, const EMaterialValueType Destination) noexcept
{
    return Source == Destination || (Source == EMaterialValueType::Float1 && IsNumeric(Destination));
}

FMaterialGraphLinkValidationResult ValidateMaterialGraphLink(
    const FMaterialGraph& Graph, const FMaterialPinId& FromPin, const FMaterialPinId& ToPin)
{
    FMaterialGraphLinkValidationResult Result;
    const FPinOwner From = FindPinOwner(Graph, FromPin);
    const FPinOwner To = FindPinOwner(Graph, ToPin);
    if (!From.Pin || !To.Pin)
    {
        AddDiagnostic(Result, "graph.dangling_link", "Link references a missing pin.", {},
            To.Pin ? FromPin : ToPin);
        return Result;
    }
    if (From.Pin->Direction != EMaterialPinDirection::Output ||
        To.Pin->Direction != EMaterialPinDirection::Input)
    {
        AddDiagnostic(Result, "graph.invalid_link_direction",
            "Links must connect an output pin to an input pin.", To.Node->Id, To.Pin->Id);
        return Result;
    }
    if (!AreMaterialValueTypesCompatible(From.Pin->Type, To.Pin->Type))
    {
        AddDiagnostic(Result, "graph.link_type_mismatch",
            "Cannot connect " + xr_string(ToString(From.Pin->Type)) + " to " +
                xr_string(ToString(To.Pin->Type)) + ".", To.Node->Id, To.Pin->Id);
    }
    if (std::ranges::any_of(Graph.Links,
            [&ToPin](const FMaterialGraphLink& Link) { return Link.ToPin == ToPin; }))
    {
        AddDiagnostic(Result, "graph.multiple_input_links",
            "An input pin accepts only one link.", To.Node->Id, To.Pin->Id);
    }
    return Result;
}

FMaterialGraphNodePropertyValidationResult ValidateMaterialGraphNodeProperty(
    const FMaterialGraphNode& Node, const xr_string_view PropertyName,
    const FMaterialValue& Value)
{
    FMaterialGraphNodePropertyValidationResult Result;
    const xr_span<const FMaterialNodePropertyDefinition> Definitions =
        GetMaterialNodePropertyDefinitions(Node.Type);
    const auto Definition = std::ranges::find(Definitions, PropertyName,
        &FMaterialNodePropertyDefinition::Name);
    if (Definition == Definitions.end())
    {
        AddDiagnostic(Result, "graph.unknown_node_property",
            "Node type '" + Node.Type + "' has no editable property '" +
                xr_string(PropertyName) + "'.", Node.Id);
        return Result;
    }

    bool TypeMatches = false;
    if (Definition->Kind == EMaterialNodePropertyKind::Value)
    {
        const auto Output = std::ranges::find_if(Node.Pins,
            [](const FMaterialGraphPin& Pin)
            { return Pin.Direction == EMaterialPinDirection::Output; });
        TypeMatches = Output != Node.Pins.end() && ValueMatchesGraphType(Value, Output->Type);
    }
    else
    {
        TypeMatches = std::holds_alternative<xr_string>(Value);
    }

    if (!TypeMatches)
    {
        AddDiagnostic(Result, "graph.node_property_type_mismatch",
            "Property '" + xr_string(PropertyName) + "' has an incompatible value type.",
            Node.Id);
        return Result;
    }

    if (Definition->Kind == EMaterialNodePropertyKind::ParameterId)
    {
        const xr_string& Id = std::get<xr_string>(Value);
        if (!Id.empty() && !IsValidStableId(Id))
        {
            AddDiagnostic(Result, "graph.invalid_parameter_id_property",
                "Parameter id contains characters that are not valid in a stable id.", Node.Id);
        }
    }
    return Result;
}
