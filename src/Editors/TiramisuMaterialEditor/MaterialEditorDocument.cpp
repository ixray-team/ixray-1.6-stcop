#include "MaterialEditorDocument.h"
#include "MaterialEditorFileIO.h"

#include <algorithm>
#include <fstream>
#include <iterator>
#include <ranges>
#include <unordered_map>
#include <unordered_set>
#include <utility>

namespace Tiramisu::Editor
{

bool FMaterialEditorOperationResult::Succeeded() const noexcept
{
	return std::ranges::none_of(Diagnostics, [](const FMaterialDiagnostic& Diagnostic)
								{ return Diagnostic.Severity == EMaterialDiagnosticSeverity::Error; });
}

TiramisuMaterialEditorDocument::TiramisuMaterialEditorDocument()
{
	NewMaterial();
}

void TiramisuMaterialEditorDocument::NewMaterial()
{
	MaterialAsset = {};
	MaterialAsset.Id.Value = GenerateMaterialGuid();
	MaterialAsset.Name = "New Material";
	MaterialAsset.HlslTemplate = "materials/MaterialTemplate.hlsl";
	MaterialAsset.Implementation.Type = EMaterialImplementationType::Graph;
	MaterialAsset.Dependencies.push_back(MaterialAsset.HlslTemplate);
	if (auto Output = CreateMaterialGraphNode(
			"material_output", {GenerateMaterialGuid()}, {600.0f, 200.0f}
		))
	{
		MaterialAsset.Implementation.Graph.Nodes.push_back(std::move(*Output));
	}
	UndoHistory.clear();
	RedoHistory.clear();
	MarkSaved();
}

void TiramisuMaterialEditorDocument::NewGraph()
{
	NewMaterial();
}

void TiramisuMaterialEditorDocument::OpenGraph(FMaterialGraph Graph)
{
	FMaterialAsset Asset;
	Asset.Id.Value = GenerateMaterialGuid();
	Asset.Name = "Graph Material";
	Asset.HlslTemplate = "materials/MaterialTemplate.hlsl";
	Asset.Implementation.Type = EMaterialImplementationType::Graph;
	Asset.Implementation.Graph = std::move(Graph);
	Asset.Dependencies.push_back(Asset.HlslTemplate);
	OpenMaterial(std::move(Asset));
}

void TiramisuMaterialEditorDocument::OpenMaterial(FMaterialAsset Asset)
{
	MaterialAsset = std::move(Asset);
	UndoHistory.clear();
	RedoHistory.clear();
	MarkSaved();
}

FMaterialEditorOperationResult TiramisuMaterialEditorDocument::OpenMaterialJson(
	const xr_string_view JsonText, const xr_string_view SourcePath
)
{
	FMaterialEditorOperationResult Result;
	FMaterialAssetParseResult Parsed = ParseMaterialAssetJson(JsonText, SourcePath);
	Result.Diagnostics = Parsed.Diagnostics;
	if (Parsed.Succeeded())
	{
		const bool Migrated = std::ranges::any_of(Result.Diagnostics, [](const FMaterialDiagnostic& Diagnostic)
												  { return Diagnostic.Code.starts_with("asset.migrated_"); });
		OpenMaterial(std::move(Parsed.Value));
		if (Migrated)
		{
			SavedMaterial.clear();
		}
	}
	return Result;
}

FMaterialEditorOperationResult TiramisuMaterialEditorDocument::OpenMaterialFile(
	const std::filesystem::path& Path
)
{
	FMaterialEditorOperationResult Result;
	std::ifstream Input(Path, std::ios::binary);
	if (!Input)
	{
		AddDiagnostic(Result, "editor.material_open_failed", "Cannot open material asset '" + ToXrString(Path.string()) + "'.");
		return Result;
	}

	const std::string Text{std::istreambuf_iterator<char>(Input), std::istreambuf_iterator<char>()};
	const xr_string JsonText = ToXrString(Text);
	return OpenMaterialJson(JsonText, Path.generic_string());
}

FMaterialEditorOperationResult TiramisuMaterialEditorDocument::SaveMaterialFile(
	const std::filesystem::path& Path
)
{
	FMaterialEditorOperationResult Result;
	const xr_string JsonText = SerializeMaterial();
	FMaterialAssetParseResult Validation =
		ParseMaterialAssetJson(JsonText, Path.generic_string());
	Result.Diagnostics = Validation.Diagnostics;
	if (!Validation.Succeeded())
	{
		return Result;
	}

	const FAtomicTextFileWriteResult WriteResult = WriteTextFileAtomically(Path, JsonText);
	if (!WriteResult.Success)
	{
		AddDiagnostic(Result, "editor.material_save_failed", "Cannot save material asset '" + ToXrString(Path.string()) + "': " + WriteResult.Error);
		return Result;
	}

	MaterialAsset.SourcePath = Path.generic_string();
	MarkSaved();
	return Result;
}

FMaterialEditorOperationResult TiramisuMaterialEditorDocument::SaveRecoveryFile(
	const std::filesystem::path& RecoveryPath
) const
{
	FMaterialEditorOperationResult Result;
	const xr_string JsonText = SerializeMaterial();
	FMaterialAssetParseResult Validation = ParseMaterialAssetJson(
		JsonText, MaterialAsset.SourcePath
	);
	Result.Diagnostics = std::move(Validation.Diagnostics);
	if (!Validation.Succeeded())
	{
		return Result;
	}

	const FAtomicTextFileWriteResult WriteResult =
		WriteTextFileAtomically(RecoveryPath, JsonText);
	if (!WriteResult.Success)
	{
		AddDiagnostic(Result, "editor.material_autosave_failed", "Cannot write material recovery file '" + ToXrString(RecoveryPath.string()) + "': " + WriteResult.Error);
	}
	return Result;
}

FMaterialEditorOperationResult TiramisuMaterialEditorDocument::OpenRecoveryFile(
	const std::filesystem::path& RecoveryPath,
	const std::filesystem::path& OriginalPath
)
{
	FMaterialEditorOperationResult Result;
	std::ifstream Input(RecoveryPath, std::ios::binary);
	if (!Input)
	{
		AddDiagnostic(Result, "editor.material_recovery_open_failed", "Cannot open material recovery file '" + ToXrString(RecoveryPath.string()) + "'.");
		return Result;
	}
	const std::string Text{std::istreambuf_iterator<char>(Input), std::istreambuf_iterator<char>()};
	const xr_string JsonText = ToXrString(Text);
	Result = OpenMaterialJson(JsonText, OriginalPath.generic_string());
	if (Result.Succeeded())
	{
		SavedMaterial.clear();
	}
	return Result;
}

bool TiramisuMaterialEditorDocument::SetMaterialName(xr_string Name)
{
	if (MaterialAsset.Name == Name)
	{
		return false;
	}
	RecordMutation();
	MaterialAsset.Name = std::move(Name);
	return true;
}

bool TiramisuMaterialEditorDocument::SetMaterialDomain(const EMaterialDomain Domain)
{
	if (MaterialAsset.Domain == Domain)
	{
		return false;
	}
	RecordMutation();
	MaterialAsset.Domain = Domain;
	return true;
}

bool TiramisuMaterialEditorDocument::SetMaterialBlendMode(const EMaterialBlendMode BlendMode)
{
	if (MaterialAsset.BlendMode == BlendMode)
	{
		return false;
	}
	RecordMutation();
	MaterialAsset.BlendMode = BlendMode;
	return true;
}

bool TiramisuMaterialEditorDocument::SetMaterialShadingModel(
	const EMaterialShadingModel ShadingModel
)
{
	if (MaterialAsset.ShadingModel == ShadingModel)
	{
		return false;
	}
	RecordMutation();
	MaterialAsset.ShadingModel = ShadingModel;
	return true;
}

bool TiramisuMaterialEditorDocument::SetMaterialTwoSided(const bool TwoSided)
{
	if (MaterialAsset.TwoSided == TwoSided)
	{
		return false;
	}
	RecordMutation();
	MaterialAsset.TwoSided = TwoSided;
	return true;
}

bool TiramisuMaterialEditorDocument::SetMaterialHlslTemplate(xr_string HlslTemplate)
{
	if (MaterialAsset.HlslTemplate == HlslTemplate)
	{
		return false;
	}

	RecordMutation();
	const xr_string PreviousTemplate = MaterialAsset.HlslTemplate;
	MaterialAsset.HlslTemplate = std::move(HlslTemplate);
	const auto Dependency = std::ranges::find(MaterialAsset.Dependencies, PreviousTemplate);
	if (Dependency != MaterialAsset.Dependencies.end())
	{
		if (MaterialAsset.HlslTemplate.empty())
		{
			MaterialAsset.Dependencies.erase(Dependency);
		}
		else
		{
			*Dependency = MaterialAsset.HlslTemplate;
		}
	}
	else if (!MaterialAsset.HlslTemplate.empty() &&
			 !std::ranges::contains(MaterialAsset.Dependencies, MaterialAsset.HlslTemplate))
	{
		MaterialAsset.Dependencies.push_back(MaterialAsset.HlslTemplate);
	}
	return true;
}

FMaterialEditorOperationResult TiramisuMaterialEditorDocument::AddParameter(
	FMaterialParameterDefinition Definition
)
{
	FMaterialEditorOperationResult Result;
	if (!ValidateParameterDefinition(Definition, Result))
	{
		return Result;
	}
	if (MaterialAsset.FindParameter(Definition.Id))
	{
		AddDiagnostic(Result, "editor.duplicate_parameter_id", "A material parameter with this GUID already exists.");
		return Result;
	}

	RecordMutation();
	MaterialAsset.Parameters.push_back(std::move(Definition));
	return Result;
}

FMaterialEditorOperationResult TiramisuMaterialEditorDocument::UpdateParameter(
	const FMaterialParameterId& ParameterId, FMaterialParameterDefinition Definition
)
{
	FMaterialEditorOperationResult Result;
	const auto Existing = std::ranges::find_if(MaterialAsset.Parameters, [&ParameterId](const FMaterialParameterDefinition& Parameter)
											   { return Parameter.Id == ParameterId; });
	if (Existing == MaterialAsset.Parameters.end())
	{
		AddDiagnostic(Result, "editor.missing_parameter", "The material parameter no longer exists.");
		return Result;
	}
	if (Definition.Id != ParameterId)
	{
		AddDiagnostic(Result, "editor.parameter_guid_is_stable", "A material parameter GUID cannot be changed after creation.");
		return Result;
	}
	if (!ValidateParameterDefinition(Definition, Result))
	{
		return Result;
	}
	if (Existing->Type != Definition.Type && IsParameterReferenced(ParameterId))
	{
		AddDiagnostic(Result, "editor.parameter_type_in_use", "Disconnect parameter nodes before changing the parameter type.");
		return Result;
	}
	if (*Existing == Definition)
	{
		return Result;
	}

	RecordMutation();
	*Existing = std::move(Definition);
	return Result;
}

FMaterialEditorOperationResult TiramisuMaterialEditorDocument::RemoveParameter(
	const FMaterialParameterId& ParameterId
)
{
	FMaterialEditorOperationResult Result;
	const auto Existing = std::ranges::find_if(MaterialAsset.Parameters, [&ParameterId](const FMaterialParameterDefinition& Parameter)
											   { return Parameter.Id == ParameterId; });
	if (Existing == MaterialAsset.Parameters.end())
	{
		AddDiagnostic(Result, "editor.missing_parameter", "The material parameter no longer exists.");
		return Result;
	}
	if (IsParameterReferenced(ParameterId))
	{
		AddDiagnostic(Result, "editor.parameter_in_use", "Remove or reassign graph nodes that reference this parameter first.");
		return Result;
	}

	RecordMutation();
	MaterialAsset.Parameters.erase(Existing);
	return Result;
}

FMaterialEditorOperationResult TiramisuMaterialEditorDocument::AddNode(const xr_string_view Type, FMaterialNodeId NodeId, const FFloat2 Position, const EMaterialValueType ValueType)
{
	FMaterialEditorOperationResult Result;
	if (!RequireGraph(Result))
	{
		return Result;
	}
	FMaterialGraph& Graph = MaterialAsset.Implementation.Graph;
	if (FindMaterialGraphNode(Graph, NodeId))
	{
		AddDiagnostic(Result, "editor.duplicate_node_id", "A node with this GUID already exists.", NodeId);
		return Result;
	}
	if (Type == "material_output" && std::ranges::any_of(Graph.Nodes, [](const FMaterialGraphNode& Node)
														 { return Node.Type == "material_output"; }))
	{
		AddDiagnostic(Result, "editor.duplicate_material_output", "A material graph can contain only one material output node.", NodeId);
		return Result;
	}

	xr_optional<FMaterialGraphNode> Node =
		CreateMaterialGraphNode(Type, NodeId, Position, ValueType);
	if (!Node)
	{
		AddDiagnostic(Result, "editor.unknown_node_type", "Unknown or invalid material node type '" + xr_string(Type) + "'.", NodeId);
		return Result;
	}

	RecordMutation();
	Graph.Nodes.push_back(std::move(*Node));
	return Result;
}

FMaterialEditorOperationResult TiramisuMaterialEditorDocument::RemoveNode(
	const FMaterialNodeId& NodeId
)
{
	FMaterialEditorOperationResult Result;
	if (!RequireGraph(Result))
	{
		return Result;
	}
	FMaterialGraph& Graph = MaterialAsset.Implementation.Graph;
	const auto Node = std::ranges::find(Graph.Nodes, NodeId, &FMaterialGraphNode::Id);
	if (Node == Graph.Nodes.end())
	{
		AddDiagnostic(Result, "editor.missing_node", "The node no longer exists.", NodeId);
		return Result;
	}
	if (Node->Type == "material_output")
	{
		AddDiagnostic(Result, "editor.cannot_remove_material_output", "The material output node cannot be removed.", NodeId);
		return Result;
	}

	RecordMutation();
	xr_hash_set<xr_string> RemovedPins;
	for (const FMaterialGraphPin& Pin : Node->Pins)
	{
		RemovedPins.emplace(Pin.Id.Value);
	}
	std::erase_if(Graph.Links, [&RemovedPins](const FMaterialGraphLink& Link)
				  { return RemovedPins.contains(Link.FromPin.Value) ||
						   RemovedPins.contains(Link.ToPin.Value); });
	Graph.Nodes.erase(Node);
	return Result;
}

FMaterialEditorOperationResult TiramisuMaterialEditorDocument::Connect(xr_string LinkId, const FMaterialPinId& FromPin, const FMaterialPinId& ToPin)
{
	FMaterialEditorOperationResult Result;
	if (!RequireGraph(Result))
	{
		return Result;
	}
	FMaterialGraph& Graph = MaterialAsset.Implementation.Graph;
	if (LinkId.empty() || std::ranges::any_of(Graph.Links, [&LinkId](const FMaterialGraphLink& Link)
											  { return Link.Id == LinkId; }))
	{
		AddDiagnostic(Result, "editor.invalid_link_id", "Link GUID is missing or duplicated.", {}, ToPin);
		return Result;
	}

	FMaterialGraphLinkValidationResult Validation =
		ValidateMaterialGraphLink(Graph, FromPin, ToPin);
	Result.Diagnostics = std::move(Validation.Diagnostics);
	if (!Result.Succeeded())
	{
		return Result;
	}

	RecordMutation();
	Graph.Links.push_back({std::move(LinkId), FromPin, ToPin});
	return Result;
}

FMaterialEditorOperationResult TiramisuMaterialEditorDocument::Disconnect(
	const xr_string_view LinkId
)
{
	FMaterialEditorOperationResult Result;
	if (!RequireGraph(Result))
	{
		return Result;
	}
	FMaterialGraph& Graph = MaterialAsset.Implementation.Graph;
	const auto Link = std::ranges::find_if(Graph.Links, [LinkId](const FMaterialGraphLink& Candidate)
										   { return xr_string_view(Candidate.Id.data(), Candidate.Id.size()) ==
													LinkId; });
	if (Link == Graph.Links.end())
	{
		AddDiagnostic(Result, "editor.missing_link", "The link no longer exists.");
		return Result;
	}
	RecordMutation();
	Graph.Links.erase(Link);
	return Result;
}

FMaterialEditorOperationResult TiramisuMaterialEditorDocument::SetNodeProperty(
	const FMaterialNodeId& NodeId, const xr_string_view PropertyName, FMaterialValue Value
)
{
	FMaterialEditorOperationResult Result;
	if (!RequireGraph(Result))
	{
		return Result;
	}
	FMaterialGraph& Graph = MaterialAsset.Implementation.Graph;
	const auto Node = std::ranges::find(Graph.Nodes, NodeId, &FMaterialGraphNode::Id);
	if (Node == Graph.Nodes.end())
	{
		AddDiagnostic(Result, "editor.missing_node", "The node no longer exists.", NodeId);
		return Result;
	}

	FMaterialGraphNodePropertyValidationResult Validation =
		ValidateMaterialGraphNodeProperty(*Node, PropertyName, Value);
	Result.Diagnostics = std::move(Validation.Diagnostics);
	if (!Result.Succeeded())
	{
		return Result;
	}

	const xr_span<const FMaterialNodePropertyDefinition> PropertyDefinitions =
		GetMaterialNodePropertyDefinitions(Node->Type);
	const auto PropertyDefinition = std::ranges::find(
		PropertyDefinitions, PropertyName, &FMaterialNodePropertyDefinition::Name
	);
	if (PropertyDefinition != PropertyDefinitions.end() &&
		PropertyDefinition->Kind == EMaterialNodePropertyKind::ParameterId)
	{
		const xr_string& ParameterId = std::get<xr_string>(Value);
		if (!ParameterId.empty())
		{
			const FMaterialParameterDefinition* Parameter =
				MaterialAsset.FindParameter({ParameterId});
			if (!Parameter)
			{
				AddDiagnostic(Result, "editor.unknown_node_parameter", "The selected material parameter does not exist.", NodeId);
				return Result;
			}

			bool Compatible = true;
			if (Node->Type == "parameter")
			{
				const auto Output = std::ranges::find_if(Node->Pins, [](const FMaterialGraphPin& Pin)
														 { return Pin.Direction == EMaterialPinDirection::Output; });
				Compatible = !Parameter->IsStatic() && Output != Node->Pins.end() &&
							 ToValueType(Parameter->Type) == Output->Type;
			}
			else if (Node->Type == "static_switch")
			{
				Compatible = Parameter->Type == EMaterialParameterType::StaticBool;
			}
			else if (Node->Type == "texture_sample")
			{
				Compatible = Parameter->Type == EMaterialParameterType::Texture2D;
			}

			if (!Compatible)
			{
				AddDiagnostic(Result, "editor.node_parameter_type_mismatch", "The selected material parameter is incompatible with this node.", NodeId);
				return Result;
			}
		}
	}

	const auto Existing = Node->Properties.find(xr_string(PropertyName));
	if (Existing != Node->Properties.end() && Existing->second == Value)
	{
		return Result;
	}

	RecordMutation();
	Node->Properties[xr_string(PropertyName)] = std::move(Value);
	return Result;
}

FMaterialEditorOperationResult TiramisuMaterialEditorDocument::SetCustomHlslSignature(
	const FMaterialNodeId& NodeId,
	const xr_span<const FMaterialCustomHlslInputDefinition> Inputs,
	const EMaterialValueType OutputType
)
{
	FMaterialEditorOperationResult Result;
	if (!RequireGraph(Result))
	{
		return Result;
	}

	FMaterialGraph& Graph = MaterialAsset.Implementation.Graph;
	const auto Node = std::ranges::find(Graph.Nodes, NodeId, &FMaterialGraphNode::Id);
	if (Node == Graph.Nodes.end())
	{
		AddDiagnostic(
			Result,
			"editor.missing_node",
			"The node no longer exists.",
			NodeId
		);
		return Result;
	}

	FMaterialGraphNode Updated = *Node;
	FMaterialCustomHlslSignatureResult SignatureResult =
		ConfigureMaterialCustomHlslNode(Updated, Inputs, OutputType);
	Result.Diagnostics = std::move(SignatureResult.Diagnostics);
	if (!Result.Succeeded())
	{
		return Result;
	}

	const bool Unchanged = Node->Pins.size() == Updated.Pins.size() &&
		std::ranges::equal(
			Node->Pins,
			Updated.Pins,
			[](const FMaterialGraphPin& Left, const FMaterialGraphPin& Right)
			{
				return Left.Id == Right.Id &&
					Left.Name == Right.Name &&
					Left.Direction == Right.Direction &&
					Left.Type == Right.Type;
			}
		);
	if (Unchanged)
	{
		return Result;
	}

	xr_hash_set<xr_string> InvalidatedPins;
	for (const FMaterialGraphPin& OldPin : Node->Pins)
	{
		const auto NewPin = std::ranges::find(
			Updated.Pins,
			OldPin.Id,
			&FMaterialGraphPin::Id
		);
		if (NewPin == Updated.Pins.end() || NewPin->Type != OldPin.Type)
		{
			InvalidatedPins.emplace(OldPin.Id.Value);
		}
	}

	RecordMutation();
	*Node = std::move(Updated);
	std::erase_if(
		Graph.Links,
		[&InvalidatedPins](const FMaterialGraphLink& Link)
		{
			return InvalidatedPins.contains(Link.FromPin.Value) ||
				InvalidatedPins.contains(Link.ToPin.Value);
		}
	);
	return Result;
}

FMaterialEditorOperationResult TiramisuMaterialEditorDocument::CopyNodes(
	const xr_span<const FMaterialNodeId> NodeIds, xr_string& ClipboardJson
) const
{
	FMaterialEditorOperationResult Result;
	ClipboardJson.clear();
	if (!RequireGraph(Result))
	{
		return Result;
	}

	xr_hash_set<xr_string> SelectedIds;
	for (const FMaterialNodeId& NodeId : NodeIds)
	{
		SelectedIds.emplace(NodeId.Value);
	}

	FMaterialGraph ClipboardGraph;
	ClipboardGraph.Version = MaterialAsset.Implementation.Graph.Version;
	xr_hash_set<xr_string> SelectedPins;
	for (const FMaterialGraphNode& Node : MaterialAsset.Implementation.Graph.Nodes)
	{
		if (!SelectedIds.contains(Node.Id.Value) || Node.Type == "material_output")
		{
			continue;
		}
		ClipboardGraph.Nodes.push_back(Node);
		for (const FMaterialGraphPin& Pin : Node.Pins)
		{
			SelectedPins.emplace(Pin.Id.Value);
		}
	}

	if (ClipboardGraph.Nodes.empty())
	{
		AddDiagnostic(Result, "editor.clipboard_empty_selection", "Select at least one material expression node to copy.");
		return Result;
	}

	for (const FMaterialGraphLink& Link : MaterialAsset.Implementation.Graph.Links)
	{
		if (SelectedPins.contains(Link.FromPin.Value) &&
			SelectedPins.contains(Link.ToPin.Value))
		{
			ClipboardGraph.Links.push_back(Link);
		}
	}
	ClipboardJson = SerializeMaterialGraphJson(ClipboardGraph);
	return Result;
}

FMaterialEditorOperationResult TiramisuMaterialEditorDocument::PasteNodes(
	const xr_string_view ClipboardJson, const FFloat2 PositionOffset, xr_vector<FMaterialNodeId>& PastedNodeIds
)
{
	FMaterialEditorOperationResult Result;
	PastedNodeIds.clear();
	if (!RequireGraph(Result))
	{
		return Result;
	}

	FMaterialGraphParseResult Parsed = ParseMaterialGraphJson(ClipboardJson);
	Result.Diagnostics = std::move(Parsed.Diagnostics);
	if (!Result.Succeeded())
	{
		return Result;
	}
	if (Parsed.Graph.Version != MaterialGraphVersion)
	{
		AddDiagnostic(Result, "editor.clipboard_graph_version", "Clipboard material graph version is not supported.");
		return Result;
	}
	if (Parsed.Graph.Nodes.empty())
	{
		AddDiagnostic(Result, "editor.clipboard_empty_graph", "The material clipboard contains no nodes.");
		return Result;
	}

	FMaterialGraph Prospective = MaterialAsset.Implementation.Graph;
	xr_hash_set<xr_string> SourceNodeIds;
	xr_hash_set<xr_string> SourcePinIds;
	xr_hash_map<xr_string, FMaterialPinId> PinRemap;
	xr_vector<FMaterialGraphNode> NewNodes;
	NewNodes.reserve(Parsed.Graph.Nodes.size());

	for (const FMaterialGraphNode& SourceNode : Parsed.Graph.Nodes)
	{
		if (SourceNode.Type == "material_output")
		{
			AddDiagnostic(Result, "editor.clipboard_material_output", "Material Output cannot be pasted.", SourceNode.Id);
			continue;
		}
		if (SourceNode.Id.Value.empty() ||
			!SourceNodeIds.emplace(SourceNode.Id.Value).second)
		{
			AddDiagnostic(Result, "editor.clipboard_duplicate_node_id", "Clipboard nodes must have unique source GUIDs.", SourceNode.Id);
			continue;
		}

		const FMaterialNodeDefinition* Definition =
			FindMaterialNodeDefinition(SourceNode.Type);
		if (!Definition)
		{
			AddDiagnostic(Result, "editor.clipboard_unknown_node_type", "Clipboard contains unknown node type '" + SourceNode.Type + "'.", SourceNode.Id);
			continue;
		}
		if (SourceNode.TypeVersion > Definition->TypeVersion)
		{
			AddDiagnostic(Result, "editor.clipboard_newer_node_version", "Clipboard node type '" + SourceNode.Type + "' is newer than this editor.", SourceNode.Id);
			continue;
		}

		EMaterialValueType ValueType = Definition->DefaultValueType;
		if (Definition->ValueTypeConfigurable)
		{
			const auto VectorPin = std::ranges::find_if(
				SourceNode.Pins,
				[](const FMaterialGraphPin& Pin)
				{
					return Pin.Type >= EMaterialValueType::Float2 &&
						Pin.Type <= EMaterialValueType::Float4;
				}
			);
			const auto Output = std::ranges::find_if(
				SourceNode.Pins,
				[](const FMaterialGraphPin& Pin)
				{
					return Pin.Direction == EMaterialPinDirection::Output;
				}
			);
			if (VectorPin != SourceNode.Pins.end())
			{
				ValueType = VectorPin->Type;
			}
			else if (Output != SourceNode.Pins.end())
			{
				ValueType = Output->Type;
			}
		}

		const FMaterialNodeId NewNodeId{GenerateMaterialGuid()};
		xr_optional<FMaterialGraphNode> NewNode = CreateMaterialGraphNode(
			SourceNode.Type, NewNodeId, {SourceNode.EditorPosition[0] + PositionOffset[0], SourceNode.EditorPosition[1] + PositionOffset[1]}, ValueType
		);
		if (!NewNode)
		{
			AddDiagnostic(Result, "editor.clipboard_node_migration_failed", "Clipboard node could not be recreated with the current schema.", SourceNode.Id);
			continue;
		}
		if (SourceNode.Type == "custom_hlsl")
		{
			xr_vector<FMaterialCustomHlslInputDefinition> Inputs;
			EMaterialValueType OutputType = EMaterialValueType::Invalid;
			for (const FMaterialGraphPin& Pin : SourceNode.Pins)
			{
				if (Pin.Direction == EMaterialPinDirection::Input)
				{
					Inputs.push_back({Pin.Name, Pin.Type});
				}
				else if (Pin.Name == "Result")
				{
					OutputType = Pin.Type;
				}
			}
			FMaterialCustomHlslSignatureResult Signature =
				ConfigureMaterialCustomHlslNode(
					*NewNode,
					Inputs,
					OutputType
				);
			Result.Diagnostics.insert(
				Result.Diagnostics.end(),
				Signature.Diagnostics.begin(),
				Signature.Diagnostics.end()
			);
			if (!Signature.Succeeded())
			{
				continue;
			}
		}

		for (const auto& [Name, Value] : SourceNode.Properties)
		{
			FMaterialGraphNodePropertyValidationResult Validation =
				ValidateMaterialGraphNodeProperty(*NewNode, Name, Value);
			Result.Diagnostics.insert(Result.Diagnostics.end(), Validation.Diagnostics.begin(), Validation.Diagnostics.end());
			if (Validation.Succeeded())
			{
				NewNode->Properties[Name] = Value;
			}
		}

		for (const FMaterialGraphPin& SourcePin : SourceNode.Pins)
		{
			if (SourcePin.Id.Value.empty() ||
				!SourcePinIds.emplace(SourcePin.Id.Value).second)
			{
				AddDiagnostic(Result, "editor.clipboard_duplicate_pin_id", "Clipboard pins must have unique source GUIDs.", SourceNode.Id, SourcePin.Id);
				continue;
			}
			const auto NewPin = std::ranges::find_if(NewNode->Pins, [&SourcePin](const FMaterialGraphPin& Candidate)
													 { return Candidate.Name == SourcePin.Name &&
															  Candidate.Direction == SourcePin.Direction; });
			if (NewPin == NewNode->Pins.end() || NewPin->Type != SourcePin.Type)
			{
				AddDiagnostic(Result, "editor.clipboard_pin_migration_failed", "Clipboard pin is incompatible with the current node schema.", SourceNode.Id, SourcePin.Id);
				continue;
			}
			PinRemap.emplace(SourcePin.Id.Value, NewPin->Id);
		}
		PastedNodeIds.push_back(NewNodeId);
		NewNodes.push_back(std::move(*NewNode));
	}

	if (!Result.Succeeded())
	{
		PastedNodeIds.clear();
		return Result;
	}

	Prospective.Nodes.insert(Prospective.Nodes.end(), NewNodes.begin(), NewNodes.end());
	for (const FMaterialGraphLink& SourceLink : Parsed.Graph.Links)
	{
		const auto From = PinRemap.find(SourceLink.FromPin.Value);
		const auto To = PinRemap.find(SourceLink.ToPin.Value);
		if (From == PinRemap.end() || To == PinRemap.end())
		{
			AddDiagnostic(Result, "editor.clipboard_dangling_link", "Clipboard link references a pin outside the copied selection.");
			continue;
		}
		FMaterialGraphLinkValidationResult Validation =
			ValidateMaterialGraphLink(Prospective, From->second, To->second);
		Result.Diagnostics.insert(Result.Diagnostics.end(), Validation.Diagnostics.begin(), Validation.Diagnostics.end());
		if (Validation.Succeeded())
		{
			Prospective.Links.push_back({GenerateMaterialGuid(), From->second, To->second});
		}
	}

	if (!Result.Succeeded())
	{
		PastedNodeIds.clear();
		return Result;
	}

	RecordMutation();
	MaterialAsset.Implementation.Graph = std::move(Prospective);
	return Result;
}

bool TiramisuMaterialEditorDocument::SetNodePosition(const FMaterialNodeId& NodeId, const FFloat2 Position, const bool RecordUndo)
{
	if (!IsGraphImplementation())
	{
		return false;
	}
	FMaterialGraph& Graph = MaterialAsset.Implementation.Graph;
	const auto Node = std::ranges::find(Graph.Nodes, NodeId, &FMaterialGraphNode::Id);
	if (Node == Graph.Nodes.end() || Node->EditorPosition == Position)
	{
		return false;
	}
	if (RecordUndo)
	{
		RecordMutation();
	}
	Node->EditorPosition = Position;
	return true;
}

FMaterialGraphCompileResult TiramisuMaterialEditorDocument::Compile(
	const FMaterialGraphCompileOptions& Options
) const
{
	if (!IsGraphImplementation())
	{
		FMaterialGraphCompileResult Result;
		Result.Diagnostics.push_back({EMaterialDiagnosticSeverity::Error, "editor.graph_implementation_required", "The graph editor is available only for graph material implementations.", {}, {}});
		return Result;
	}

	FMaterialGraphCompileOptions EffectiveOptions = Options;
	EffectiveOptions.Parameters = MaterialAsset.Parameters;
	for (const FMaterialParameterDefinition& Parameter : MaterialAsset.Parameters)
	{
		if (Parameter.IsStatic() &&
			!EffectiveOptions.StaticParameters.contains(Parameter.Id))
		{
			EffectiveOptions.StaticParameters.emplace(Parameter.Id, Parameter.DefaultValue);
		}
	}
	return CompileMaterialGraph(MaterialAsset.Implementation.Graph, EffectiveOptions);
}

xr_string TiramisuMaterialEditorDocument::Serialize() const
{
	return SerializeMaterialGraphJson(MaterialAsset.Implementation.Graph);
}

xr_string TiramisuMaterialEditorDocument::SerializeMaterial() const
{
	return SerializeMaterialAssetJson(MaterialAsset);
}

bool TiramisuMaterialEditorDocument::Undo()
{
	if (UndoHistory.empty())
	{
		return false;
	}
	RedoHistory.push_back(std::move(MaterialAsset));
	MaterialAsset = std::move(UndoHistory.back());
	UndoHistory.pop_back();
	return true;
}

bool TiramisuMaterialEditorDocument::Redo()
{
	if (RedoHistory.empty())
	{
		return false;
	}
	UndoHistory.push_back(std::move(MaterialAsset));
	MaterialAsset = std::move(RedoHistory.back());
	RedoHistory.pop_back();
	return true;
}

bool TiramisuMaterialEditorDocument::IsDirty() const
{
	return SerializeMaterial() != SavedMaterial;
}

void TiramisuMaterialEditorDocument::MarkSaved()
{
	SavedMaterial = SerializeMaterial();
}

void TiramisuMaterialEditorDocument::RecordMutation()
{
	if (UndoHistory.size() == MaxHistoryEntries)
	{
		UndoHistory.erase(UndoHistory.begin());
	}
	UndoHistory.push_back(MaterialAsset);
	RedoHistory.clear();
}

bool TiramisuMaterialEditorDocument::IsGraphImplementation() const noexcept
{
	return MaterialAsset.Implementation.Type == EMaterialImplementationType::Graph;
}

bool TiramisuMaterialEditorDocument::RequireGraph(FMaterialEditorOperationResult& Result) const
{
	if (IsGraphImplementation())
	{
		return true;
	}
	AddDiagnostic(Result, "editor.graph_implementation_required", "The graph editor is available only for graph material implementations.");
	return false;
}

bool TiramisuMaterialEditorDocument::IsParameterReferenced(
	const FMaterialParameterId& ParameterId
) const
{
	if (!IsGraphImplementation())
	{
		return false;
	}
	return std::ranges::any_of(MaterialAsset.Implementation.Graph.Nodes, [&ParameterId](const FMaterialGraphNode& Node)
							   {
            for (const FMaterialNodePropertyDefinition& Definition :
                GetMaterialNodePropertyDefinitions(Node.Type))
            {
                if (Definition.Kind != EMaterialNodePropertyKind::ParameterId){
                    continue;
}
                const auto Property = Node.Properties.find(xr_string(Definition.Name));
                const xr_string* Id = Property == Node.Properties.end()
                    ? nullptr : std::get_if<xr_string>(&Property->second);
                if (Id && *Id == ParameterId.Value){
                    return true;
}
            }
            return false; });
}

bool TiramisuMaterialEditorDocument::ValidateParameterDefinition(
	const FMaterialParameterDefinition& Definition,
	FMaterialEditorOperationResult& Result
) const
{
	if (!IsValidStableId(Definition.Id.Value))
	{
		AddDiagnostic(Result, "editor.invalid_parameter_id", "Material parameter GUID is missing or invalid.");
	}
	if (Definition.Name.empty())
	{
		AddDiagnostic(Result, "editor.missing_parameter_name", "Material parameter name cannot be empty.");
	}
	if (!ValueMatchesParameterType(Definition.DefaultValue, Definition.Type))
	{
		AddDiagnostic(Result, "editor.parameter_default_type_mismatch", "Material parameter default value does not match its type.");
	}
	if (Definition.Minimum && Definition.Maximum &&
		*Definition.Minimum > *Definition.Maximum)
	{
		AddDiagnostic(Result, "editor.parameter_range_invalid", "Material parameter minimum cannot be greater than maximum.");
	}
	return Result.Succeeded();
}

void TiramisuMaterialEditorDocument::AddDiagnostic(FMaterialEditorOperationResult& Result, const xr_string_view Code, xr_string Message, const FMaterialNodeId Node, const FMaterialPinId Pin)
{
	Result.Diagnostics.push_back(
		{EMaterialDiagnosticSeverity::Error, xr_string(Code), std::move(Message), Node, Pin}
	);
}
} // namespace Tiramisu::Editor
