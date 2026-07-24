#include "MaterialAsset.h"
#include "MaterialJsonHelpers.h"

#include <nlohmann/json.hpp>

#include <algorithm>
#include <ranges>
#include <set>
#include <type_traits>
#include <utility>

namespace
{
using Json = nlohmann::json;

void AddDiagnostic(xr_vector<FMaterialDiagnostic>& Diagnostics, const xr_string_view Code, const xr_string& Message, const EMaterialDiagnosticSeverity Severity = EMaterialDiagnosticSeverity::Error)
{
	Diagnostics.push_back({Severity, xr_string(Code), Message, {}, {}});
}

bool HasErrors(const xr_vector<FMaterialDiagnostic>& Diagnostics)
{
	return std::ranges::any_of(Diagnostics, [](const FMaterialDiagnostic& Diagnostic)
							   { return Diagnostic.Severity == EMaterialDiagnosticSeverity::Error; });
}

xr_optional<FMaterialValue> ParseTypedValue(const Json& Value, const EMaterialParameterType Type)
{
	float Components[4]{};
	const auto ReadVector = [&Value, &Components](const size_t Size)
	{
		if (!Value.is_array() || Value.size() != Size)
		{
			return false;
		}
		for (size_t Index = 0; Index < Size; ++Index)
		{
			if (!MaterialJsonDetail::TryGetFloat(Value[Index], Components[Index]))
			{
				return false;
			}
		}
		return true;
	};

	switch (Type)
	{
		case EMaterialParameterType::Scalar:
			if (MaterialJsonDetail::TryGetFloat(Value, Components[0]))
			{
				return Components[0];
			}
			break;
		case EMaterialParameterType::Float2:
			if (ReadVector(2))
			{
				return FFloat2{Components[0], Components[1]};
			}
			break;
		case EMaterialParameterType::Float3:
			if (ReadVector(3))
			{
				return FFloat3{Components[0], Components[1], Components[2]};
			}
			break;
		case EMaterialParameterType::Float4:
		case EMaterialParameterType::Color:
			if (ReadVector(4))
			{
				return FFloat4{Components[0], Components[1], Components[2], Components[3]};
			}
			break;
		case EMaterialParameterType::Texture2D:
		case EMaterialParameterType::TextureCube:
		case EMaterialParameterType::SamplerPreset:
		{
			xr_string Text;
			if (MaterialJsonDetail::TryGetString(Value, Text))
			{
				return Text;
			}
			break;
		}
		case EMaterialParameterType::StaticBool:
		{
			bool Boolean = false;
			if (MaterialJsonDetail::TryGetBoolean(Value, Boolean))
			{
				return Boolean;
			}
			break;
		}
		case EMaterialParameterType::StaticEnum:
		{
			s32 Integer = 0;
			if (MaterialJsonDetail::TryGetInt32(Value, Integer))
			{
				return Integer;
			}
			break;
		}
	}
	return {};
}

xr_string NormalizeAssetReference(const xr_string_view Reference)
{
	xr_string Result(Reference);
	std::ranges::replace(Result, '\\', '/');
	while (Result.starts_with("./"))
	{
		Result.erase(0, 2);
	}
	return Result;
}

xr_optional<FMaterialValue> ParseOverrideValue(const Json& Value, const bool Static)
{
	bool Boolean = false;
	if (MaterialJsonDetail::TryGetBoolean(Value, Boolean))
	{
		return Boolean;
	}

	if (Static)
	{
		s32 Integer = 0;
		if (MaterialJsonDetail::TryGetInt32(Value, Integer))
		{
			return Integer;
		}
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

	if (!Value.is_array() || Value.size() < 2 || Value.size() > 4)
	{
		return std::nullopt;
	}
	float Components[4]{};
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
	return FFloat4{Components[0], Components[1], Components[2], Components[3]};
}

xr_string ReadStringField(const Json& Object, const xr_string_view Name, xr_vector<FMaterialDiagnostic>& Diagnostics)
{
	const Json* Field = MaterialJsonDetail::Find(Object, Name);
	if (!Field)
	{
		return {};
	}

	xr_string Result;
	if (!MaterialJsonDetail::TryGetString(*Field, Result))
	{
		AddDiagnostic(Diagnostics, "asset.invalid_field_type", "Field '" + xr_string(Name) + "' must be a string.");
	}
	return Result;
}

bool ReadBooleanField(const Json& Object, const xr_string_view Name, const bool Default, xr_vector<FMaterialDiagnostic>& Diagnostics)
{
	const Json* Field = MaterialJsonDetail::Find(Object, Name);
	if (!Field)
	{
		return Default;
	}

	bool Result = Default;
	if (!MaterialJsonDetail::TryGetBoolean(*Field, Result))
	{
		AddDiagnostic(Diagnostics, "asset.invalid_field_type", "Field '" + xr_string(Name) + "' must be a boolean.");
	}
	return Result;
}

const Json& ReadCollectionField(const Json& Object, const xr_string_view Name, const Json& Empty, const bool Array, xr_vector<FMaterialDiagnostic>& Diagnostics)
{
	const Json* Field = MaterialJsonDetail::Find(Object, Name);
	if (!Field)
	{
		return Empty;
	}
	if ((Array && !Field->is_array()) || (!Array && !Field->is_object()))
	{
		AddDiagnostic(Diagnostics, "asset.invalid_field_type", "Field '" + xr_string(Name) + "' must be " + (Array ? "an array." : "an object."));
		return Empty;
	}
	return *Field;
}

u32 ReadVersionValue(const Json& Value, const xr_string_view Name, xr_vector<FMaterialDiagnostic>& Diagnostics)
{
	u32 Version = 0;
	if (!MaterialJsonDetail::TryGetUInt32(Value, Version))
	{
		AddDiagnostic(Diagnostics, "asset.invalid_field_type", "Field '" + xr_string(Name) + "' must be an unsigned integer.");
	}
	return Version;
}

xr_optional<float> ReadOptionalFloat(const Json& Object, const xr_string_view Name, xr_vector<FMaterialDiagnostic>& Diagnostics)
{
	const Json* Field = MaterialJsonDetail::Find(Object, Name);
	if (!Field)
	{
		return std::nullopt;
	}
	float Result = 0.0f;
	if (!MaterialJsonDetail::TryGetFloat(*Field, Result))
	{
		AddDiagnostic(Diagnostics, "asset.invalid_field_type", "Field 'ui." + xr_string(Name) + "' must be numeric.");
		return std::nullopt;
	}
	return Result;
}

u32 ReadVersion(const Json& Root, xr_vector<FMaterialDiagnostic>& Diagnostics)
{
	if (const Json* Version = MaterialJsonDetail::Find(Root, "asset_version"))
	{
		return ReadVersionValue(*Version, "asset_version", Diagnostics);
	}
	if (const Json* Version = MaterialJsonDetail::Find(Root, "version"))
	{
		AddDiagnostic(Diagnostics, "asset.migrated_version_field", "Migrated legacy 'version' field to 'asset_version'.", EMaterialDiagnosticSeverity::Warning);
		return ReadVersionValue(*Version, "version", Diagnostics);
	}
	AddDiagnostic(Diagnostics, "asset.missing_version", "Asset has no asset_version.");
	return 0;
}

xr_string ReadGuid(const Json& Root, xr_vector<FMaterialDiagnostic>& Diagnostics)
{
	if (MaterialJsonDetail::Find(Root, "guid"))
	{
		return ReadStringField(Root, "guid", Diagnostics);
	}
	if (MaterialJsonDetail::Find(Root, "id"))
	{
		AddDiagnostic(Diagnostics, "asset.migrated_id_field", "Migrated legacy 'id' field to 'guid'.", EMaterialDiagnosticSeverity::Warning);
		return ReadStringField(Root, "id", Diagnostics);
	}
	return {};
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

bool ParseParameterArray(const Json& Values, const bool Static, xr_vector<FMaterialParameterDefinition>& Parameters, xr_vector<FMaterialDiagnostic>& Diagnostics)
{
	if (!Values.is_array())
	{
		AddDiagnostic(Diagnostics, "asset.parameters_not_array", Static ? "static_parameters must be an array." : "parameters must be an array.");
		return false;
	}

	xr_set<xr_string> Existing;
	for (const FMaterialParameterDefinition& Parameter : Parameters)
	{
		Existing.emplace(Parameter.Id.Value);
	}

	for (const Json& ParameterJson : Values)
	{
		if (!ParameterJson.is_object())
		{
			AddDiagnostic(Diagnostics, "asset.invalid_parameter", "Every parameter entry must be an object.");
			continue;
		}
		FMaterialParameterDefinition Parameter;
		Parameter.Id.Value = ReadStringField(ParameterJson, "guid", Diagnostics);
		Parameter.Name = ReadStringField(ParameterJson, "name", Diagnostics);
		const auto Type = ParseMaterialParameterType(ReadStringField(ParameterJson, "type", Diagnostics));
		if (!IsValidStableId(Parameter.Id.Value) || !Existing.emplace(Parameter.Id.Value).second)
		{
			AddDiagnostic(Diagnostics, "asset.invalid_parameter_id", "Parameter GUID is missing or duplicated: '" + Parameter.Id.Value + "'.");
			continue;
		}
		if (!Type || Static != (*Type == EMaterialParameterType::StaticBool ||
								*Type == EMaterialParameterType::StaticEnum))
		{
			AddDiagnostic(Diagnostics, "asset.invalid_parameter_type", "Parameter '" + Parameter.Id.Value + "' has an invalid runtime/static type.");
			continue;
		}
		Parameter.Type = *Type;
		if (!ParameterJson.contains("default"))
		{
			AddDiagnostic(Diagnostics, "asset.missing_parameter_default", "Parameter '" + Parameter.Id.Value + "' has no default value.");
			continue;
		}
		const auto Default = ParseTypedValue(ParameterJson["default"], Parameter.Type);
		if (!Default)
		{
			AddDiagnostic(Diagnostics, "asset.parameter_default_type", "Default value for parameter '" + Parameter.Id.Value + "' does not match its type.");
			continue;
		}
		Parameter.DefaultValue = *Default;
		if (const Json* Ui = MaterialJsonDetail::Find(ParameterJson, "ui"))
		{
			if (!Ui->is_object())
			{
				AddDiagnostic(Diagnostics, "asset.invalid_field_type", "Field 'ui' must be an object.");
			}
			else
			{
				Parameter.DisplayName = ReadStringField(*Ui, "display_name", Diagnostics);
				Parameter.Category = ReadStringField(*Ui, "category", Diagnostics);
				Parameter.Description = ReadStringField(*Ui, "description", Diagnostics);
				Parameter.Minimum = ReadOptionalFloat(*Ui, "min", Diagnostics);
				Parameter.Maximum = ReadOptionalFloat(*Ui, "max", Diagnostics);
			}
		}
		Parameters.push_back(std::move(Parameter));
	}
	return true;
}

Json SerializeParameter(const FMaterialParameterDefinition& Parameter)
{
	Json Result{{"guid", Parameter.Id.Value}, {"name", Parameter.Name}, {"type", ToString(Parameter.Type)}, {"default", SerializeValue(Parameter.DefaultValue)}};
	Json Ui = Json::object();
	if (!Parameter.DisplayName.empty())
	{
		Ui["display_name"] = Parameter.DisplayName;
	}
	if (!Parameter.Category.empty())
	{
		Ui["category"] = Parameter.Category;
	}
	if (!Parameter.Description.empty())
	{
		Ui["description"] = Parameter.Description;
	}
	if (Parameter.Minimum)
	{
		Ui["min"] = *Parameter.Minimum;
	}
	if (Parameter.Maximum)
	{
		Ui["max"] = *Parameter.Maximum;
	}
	if (!Ui.empty())
	{
		Result["ui"] = std::move(Ui);
	}
	return Result;
}
} // namespace

const FMaterialParameterDefinition* FMaterialAsset::FindParameter(const FMaterialParameterId& Parameter) const noexcept
{
	const auto Iterator = std::ranges::find_if(Parameters, [&Parameter](const FMaterialParameterDefinition& Definition)
											   { return Definition.Id == Parameter; });
	return Iterator == Parameters.end() ? nullptr : &*Iterator;
}

FMaterialAssetParseResult ParseMaterialAssetJson(const xr_string_view JsonText, const xr_string_view SourcePath)
{
	FMaterialAssetParseResult Result;
	Result.Value.SourcePath = SourcePath;
	try
	{
		const Json Root = Json::parse(JsonText, nullptr, false);
		if (Root.is_discarded())
		{
			AddDiagnostic(Result.Diagnostics, "asset.invalid_json", "Material asset contains invalid JSON.");
			return Result;
		}
		if (!Root.is_object())
		{
			AddDiagnostic(Result.Diagnostics, "asset.invalid_root", "Material asset root must be an object.");
			return Result;
		}
		Result.Value.Version = ReadVersion(Root, Result.Diagnostics);
		Result.Value.Id.Value = ReadGuid(Root, Result.Diagnostics);
		Result.Value.Name = ReadStringField(Root, "name", Result.Diagnostics);
		if (Result.Value.Version != MaterialAssetVersion)
		{
			AddDiagnostic(Result.Diagnostics, "asset.unsupported_version", "Unsupported material asset version " + std::to_string(Result.Value.Version) + ".");
		}
		if (!IsValidStableId(Result.Value.Id.Value))
		{
			AddDiagnostic(Result.Diagnostics, "asset.invalid_guid", "Master material GUID is missing or invalid.");
		}

		const auto Domain = ParseMaterialDomain(ReadStringField(Root, "domain", Result.Diagnostics));
		const auto Blend = ParseMaterialBlendMode(ReadStringField(Root, "blend_mode", Result.Diagnostics));
		const auto Shading = ParseMaterialShadingModel(ReadStringField(Root, "shading_model", Result.Diagnostics));
		if (!Domain)
		{
			AddDiagnostic(Result.Diagnostics, "asset.invalid_domain", "Unknown material domain.");
		}
		else
		{
			Result.Value.Domain = *Domain;
		}
		if (!Blend)
		{
			AddDiagnostic(Result.Diagnostics, "asset.invalid_blend", "Unknown material blend mode.");
		}
		else
		{
			Result.Value.BlendMode = *Blend;
		}
		if (!Shading)
		{
			AddDiagnostic(Result.Diagnostics, "asset.invalid_shading_model", "Unknown material shading model.");
		}
		else
		{
			Result.Value.ShadingModel = *Shading;
		}
		Result.Value.TwoSided = ReadBooleanField(Root, "two_sided", false, Result.Diagnostics);
		Result.Value.HlslTemplate = ReadStringField(Root, "template", Result.Diagnostics);
		if (Result.Value.HlslTemplate.empty())
		{
			AddDiagnostic(Result.Diagnostics, "asset.missing_template", "Master material has no HLSL template.");
		}

		if (!Root.contains("implementation") || !Root["implementation"].is_object())
		{
			AddDiagnostic(Result.Diagnostics, "asset.missing_implementation", "Master material has no implementation object.");
		}
		else
		{
			const Json& Implementation = Root["implementation"];
			const xr_string Type = ReadStringField(Implementation, "type", Result.Diagnostics);
			if (Type == "hlsl")
			{
				Result.Value.Implementation.Type = EMaterialImplementationType::Hlsl;
				Result.Value.Implementation.Source = ReadStringField(Implementation, "source", Result.Diagnostics);
				if (Result.Value.Implementation.Source.empty())
				{
					AddDiagnostic(Result.Diagnostics, "asset.missing_hlsl_source", "HLSL implementation has no source path.");
				}
			}
			else if (Type == "graph")
			{
				Result.Value.Implementation.Type = EMaterialImplementationType::Graph;
				if (!Implementation.contains("graph"))
				{
					AddDiagnostic(Result.Diagnostics, "asset.missing_graph", "Graph implementation has no graph object.");
				}
				else
				{
					FMaterialGraphParseResult Graph = ParseMaterialGraphJson(Implementation["graph"].dump());
					Result.Value.Implementation.Graph = std::move(Graph.Graph);
					Result.Diagnostics.insert(Result.Diagnostics.end(), Graph.Diagnostics.begin(), Graph.Diagnostics.end());
				}
			}
			else
			{
				AddDiagnostic(Result.Diagnostics, "asset.invalid_implementation", "Implementation type must be 'hlsl' or 'graph'.");
			}
		}

		const Json EmptyArray = Json::array();
		ParseParameterArray(ReadCollectionField(Root, "parameters", EmptyArray, true, Result.Diagnostics), false, Result.Value.Parameters, Result.Diagnostics);
		ParseParameterArray(ReadCollectionField(Root, "static_parameters", EmptyArray, true, Result.Diagnostics), true, Result.Value.Parameters, Result.Diagnostics);
		for (const Json& Dependency : ReadCollectionField(Root, "dependencies", EmptyArray, true, Result.Diagnostics))
		{
			xr_string Path;
			if (!MaterialJsonDetail::TryGetString(Dependency, Path))
			{
				AddDiagnostic(Result.Diagnostics, "asset.invalid_dependency", "Every dependency must be a string.");
			}
			else
			{
				Result.Value.Dependencies.push_back(std::move(Path));
			}
		}
	}
	catch (const std::exception& Error)
	{
		AddDiagnostic(Result.Diagnostics, "asset.invalid_json", Error.what());
	}
	return Result;
}

FMaterialInstanceParseResult ParseMaterialInstanceJson(const xr_string_view JsonText, const xr_string_view SourcePath)
{
	FMaterialInstanceParseResult Result;
	Result.Value.SourcePath = SourcePath;
	try
	{
		const Json Root = Json::parse(JsonText, nullptr, false);
		if (Root.is_discarded())
		{
			AddDiagnostic(Result.Diagnostics, "asset.invalid_json", "Material instance contains invalid JSON.");
			return Result;
		}
		if (!Root.is_object())
		{
			AddDiagnostic(Result.Diagnostics, "asset.invalid_root", "Material instance root must be an object.");
			return Result;
		}
		Result.Value.Version = ReadVersion(Root, Result.Diagnostics);
		Result.Value.Id.Value = ReadGuid(Root, Result.Diagnostics);
		Result.Value.Name = ReadStringField(Root, "name", Result.Diagnostics);
		Result.Value.Parent = ReadStringField(Root, "parent", Result.Diagnostics);
		if (Result.Value.Version != MaterialAssetVersion)
		{
			AddDiagnostic(Result.Diagnostics, "asset.unsupported_version", "Unsupported material instance version " + std::to_string(Result.Value.Version) + ".");
		}
		if (!IsValidStableId(Result.Value.Id.Value))
		{
			AddDiagnostic(Result.Diagnostics, "asset.invalid_guid", "Material instance GUID is missing or invalid.");
		}
		if (Result.Value.Parent.empty())
		{
			AddDiagnostic(Result.Diagnostics, "instance.missing_parent", "Material instance has no parent GUID/path.");
		}
		for (const xr_string_view Forbidden : {"domain", "blend_mode", "shading_model"})
		{
			if (Root.contains(Forbidden))
			{
				AddDiagnostic(Result.Diagnostics, "instance.forbidden_master_property", "Instance cannot override '" + xr_string(Forbidden) + "'.");
			}
		}

		const Json EmptyObject = Json::object();
		for (const auto& [Id, ValueJson] :
			 ReadCollectionField(Root, "overrides", EmptyObject, false, Result.Diagnostics).items())
		{
			const auto Value = ParseOverrideValue(ValueJson, false);
			if (!Value)
			{
				AddDiagnostic(Result.Diagnostics, "instance.invalid_override", "Invalid runtime override '" + Id + "'.");
			}
			else
			{
				Result.Value.Overrides.emplace(FMaterialParameterId{Id}, *Value);
			}
		}
		for (const auto& [Id, ValueJson] :
			 ReadCollectionField(Root, "static_overrides", EmptyObject, false, Result.Diagnostics).items())
		{
			const auto Value = ParseOverrideValue(ValueJson, true);
			if (!Value)
			{
				AddDiagnostic(Result.Diagnostics, "instance.invalid_static_override", "Invalid static override '" + Id + "'.");
			}
			else
			{
				Result.Value.StaticOverrides.emplace(FMaterialParameterId{Id}, *Value);
			}
		}
	}
	catch (const std::exception& Error)
	{
		AddDiagnostic(Result.Diagnostics, "asset.invalid_json", Error.what());
	}
	return Result;
}

xr_string SerializeMaterialAssetJson(const FMaterialAsset& Asset)
{
	Json Root{{"asset_version", Asset.Version}, {"guid", Asset.Id.Value}, {"name", Asset.Name}, {"domain", ToString(Asset.Domain)}, {"blend_mode", ToString(Asset.BlendMode)}, {"shading_model", ToString(Asset.ShadingModel)}, {"two_sided", Asset.TwoSided}, {"template", Asset.HlslTemplate}};
	if (Asset.Implementation.Type == EMaterialImplementationType::Hlsl)
	{
		Root["implementation"] = {{"type", "hlsl"}, {"source", Asset.Implementation.Source}};
	}
	else
	{
		Root["implementation"] = {{"type", "graph"}, {"graph", Json::parse(SerializeMaterialGraphJson(Asset.Implementation.Graph), nullptr, false)}};
	}
	Root["parameters"] = Json::array();
	Root["static_parameters"] = Json::array();
	for (const FMaterialParameterDefinition& Parameter : Asset.Parameters)
	{
		Root[Parameter.IsStatic() ? "static_parameters" : "parameters"].push_back(SerializeParameter(Parameter));
	}
	Root["dependencies"] = Asset.Dependencies;
	return Root.dump(2);
}

xr_string SerializeMaterialInstanceJson(const FMaterialInstanceAsset& Instance)
{
	Json Root{{"asset_version", Instance.Version}, {"guid", Instance.Id.Value}, {"name", Instance.Name}, {"parent", Instance.Parent}};
	Root["overrides"] = Json::object();
	for (const auto& [Id, Value] : Instance.Overrides)
	{
		Root["overrides"][Id.Value.c_str()] = SerializeValue(Value);
	}
	Root["static_overrides"] = Json::object();
	for (const auto& [Id, Value] : Instance.StaticOverrides)
	{
		Root["static_overrides"][Id.Value.c_str()] = SerializeValue(Value);
	}
	return Root.dump(2);
}

bool FMaterialResolveResult::Succeeded() const noexcept
{
	return !HasErrors(Diagnostics) && Value.MasterHandle.IsValid();
}

bool FMaterialRegistrationResult::Succeeded() const noexcept
{
	return Handle.IsValid() && !HasErrors(Diagnostics);
}

FMaterialRegistrationResult TiramisuMaterialLibrary::RegisterMaster(FMaterialAsset Asset)
{
	FMaterialRegistrationResult Result;
	if (!IsValidStableId(Asset.Id.Value))
	{
		AddDiagnostic(Result.Diagnostics, "library.invalid_master_id", "Cannot register a master with an invalid GUID.");
		return Result;
	}
	const xr_string SourceReference = NormalizeAssetReference(Asset.SourcePath);
	if (MasterReferences.contains(Asset.Id.Value) ||
		InstanceReferences.contains(Asset.Id.Value) ||
		(!SourceReference.empty() &&
		 (MasterReferences.contains(SourceReference) ||
		  InstanceReferences.contains(SourceReference))))
	{
		AddDiagnostic(Result.Diagnostics, "library.duplicate_reference", "Material reference is already registered: '" + Asset.Id.Value + "'.");
		return Result;
	}
	Result.Handle = Masters.Add(std::move(Asset));
	IndexMaster(*Masters.Get(Result.Handle), Result.Handle);
	return Result;
}

FMaterialRegistrationResult TiramisuMaterialLibrary::RegisterInstance(FMaterialInstanceAsset Instance)
{
	FMaterialRegistrationResult Result;
	if (!IsValidStableId(Instance.Id.Value))
	{
		AddDiagnostic(Result.Diagnostics, "library.invalid_instance_id", "Cannot register an instance with an invalid GUID.");
		return Result;
	}
	const xr_string SourceReference = NormalizeAssetReference(Instance.SourcePath);
	if (MasterReferences.contains(Instance.Id.Value) ||
		InstanceReferences.contains(Instance.Id.Value) ||
		(!SourceReference.empty() &&
		 (MasterReferences.contains(SourceReference) ||
		  InstanceReferences.contains(SourceReference))))
	{
		AddDiagnostic(Result.Diagnostics, "library.duplicate_reference", "Material reference is already registered: '" + Instance.Id.Value + "'.");
		return Result;
	}
	Result.Handle = Instances.Add(std::move(Instance));
	IndexInstance(*Instances.Get(Result.Handle), Result.Handle);
	return Result;
}

bool TiramisuMaterialLibrary::ReloadMaster(const FMaterialHandle Handle, FMaterialAsset Asset)
{
	const FMaterialAsset* Existing = Masters.Get(Handle);
	if (!Existing || Existing->Id != Asset.Id)
	{
		return false;
	}
	const FMaterialAsset Old = *Existing;
	RemoveMasterIndices(Old);
	if (!Masters.Replace(Handle, std::move(Asset)))
	{
		return false;
	}
	IndexMaster(*Masters.Get(Handle), Handle);
	return true;
}

bool TiramisuMaterialLibrary::ReloadInstance(const FMaterialHandle Handle, FMaterialInstanceAsset Instance)
{
	const FMaterialInstanceAsset* Existing = Instances.Get(Handle);
	if (!Existing || Existing->Id != Instance.Id)
	{
		return false;
	}
	const FMaterialInstanceAsset Old = *Existing;
	RemoveInstanceIndices(Old);
	if (!Instances.Replace(Handle, std::move(Instance)))
	{
		return false;
	}
	IndexInstance(*Instances.Get(Handle), Handle);
	return true;
}

bool TiramisuMaterialLibrary::RemoveMaster(const FMaterialHandle Handle)
{
	const FMaterialAsset* Existing = Masters.Get(Handle);
	if (!Existing)
	{
		return false;
	}
	RemoveMasterIndices(*Existing);
	return Masters.Remove(Handle);
}

bool TiramisuMaterialLibrary::RemoveInstance(const FMaterialHandle Handle)
{
	const FMaterialInstanceAsset* Existing = Instances.Get(Handle);
	if (!Existing)
	{
		return false;
	}
	RemoveInstanceIndices(*Existing);
	return Instances.Remove(Handle);
}

const FMaterialAsset* TiramisuMaterialLibrary::GetMaster(const FMaterialHandle Handle) const noexcept
{
	return Masters.Get(Handle);
}

const FMaterialInstanceAsset* TiramisuMaterialLibrary::GetInstance(const FMaterialHandle Handle) const noexcept
{
	return Instances.Get(Handle);
}

xr_optional<FMaterialHandle> TiramisuMaterialLibrary::FindMaster(const xr_string_view IdOrPath) const
{
	const auto Iterator = MasterReferences.find(NormalizeAssetReference(IdOrPath));
	return Iterator == MasterReferences.end() ? std::nullopt : xr_optional{Iterator->second};
}

xr_optional<FMaterialHandle> TiramisuMaterialLibrary::FindInstance(const xr_string_view IdOrPath) const
{
	const auto Iterator = InstanceReferences.find(NormalizeAssetReference(IdOrPath));
	return Iterator == InstanceReferences.end() ? std::nullopt : xr_optional{Iterator->second};
}

FMaterialResolveResult TiramisuMaterialLibrary::Resolve(const xr_string_view MasterOrInstance) const
{
	FMaterialResolveResult Result;
	xr_vector<xr_string> Active;
	ResolveRecursive(MasterOrInstance, Active, Result.Value, Result.Diagnostics);
	return Result;
}

TiramisuMaterialInstanceDynamic TiramisuMaterialLibrary::CreateDynamic(const FResolvedMaterialInstance& Resolved) const
{
	return {Resolved.MasterId, Resolved.Parameters, Resolved.StaticParameters};
}

bool TiramisuMaterialLibrary::ResolveRecursive(const xr_string_view Reference, xr_vector<xr_string>& ActiveReferences, FResolvedMaterialInstance& Resolved, xr_vector<FMaterialDiagnostic>& Diagnostics) const
{
	const xr_string OwnedReference(Reference);
	if (std::ranges::find(ActiveReferences, OwnedReference) != ActiveReferences.end())
	{
		AddDiagnostic(Diagnostics, "instance.parent_cycle", "Material instance parent cycle detected at '" + OwnedReference + "'.");
		return false;
	}

	if (const auto MasterHandle = FindMaster(Reference))
	{
		if (Resolved.MasterHandle.IsValid())
		{
			AddDiagnostic(Diagnostics, "instance.multiple_masters", "Instance chain resolved more than one master material.");
			return false;
		}
		const FMaterialAsset* Master = Masters.Get(*MasterHandle);
		Resolved.MasterHandle = *MasterHandle;
		Resolved.MasterId = Master->Id;
		Resolved.Domain = Master->Domain;
		Resolved.BlendMode = Master->BlendMode;
		Resolved.ShadingModel = Master->ShadingModel;
		Resolved.TwoSided = Master->TwoSided;
		for (const FMaterialParameterDefinition& Parameter : Master->Parameters)
		{
			if (Parameter.IsStatic())
			{
				Resolved.StaticParameters.emplace(Parameter.Id, Parameter.DefaultValue);
			}
			else
			{
				Resolved.Parameters.emplace(Parameter.Id, Parameter.DefaultValue);
			}
		}
		return true;
	}

	const auto InstanceHandle = FindInstance(Reference);
	if (!InstanceHandle)
	{
		AddDiagnostic(Diagnostics, "instance.parent_not_found", "Material parent was not found: '" + OwnedReference + "'.");
		return false;
	}

	const FMaterialInstanceAsset* Instance = Instances.Get(*InstanceHandle);
	ActiveReferences.push_back(OwnedReference);
	const bool ParentResolved = ResolveRecursive(Instance->Parent, ActiveReferences, Resolved, Diagnostics);
	ActiveReferences.pop_back();
	if (!ParentResolved || !Resolved.MasterHandle.IsValid())
	{
		return false;
	}

	const FMaterialAsset* Master = Masters.Get(Resolved.MasterHandle);
	for (const auto& [Id, Value] : Instance->Overrides)
	{
		const FMaterialParameterDefinition* Definition = Master->FindParameter(Id);
		if (!Definition)
		{
			AddDiagnostic(Diagnostics, "instance.unknown_parameter", "Unknown material parameter override '" + Id.Value + "'.");
		}
		else if (Definition->IsStatic())
		{
			AddDiagnostic(Diagnostics, "instance.static_in_runtime_overrides", "Static parameter '" + Id.Value + "' is in runtime overrides.");
		}
		else if (!ValueMatchesParameterType(Value, Definition->Type))
		{
			AddDiagnostic(Diagnostics, "instance.override_type_mismatch", "Override type does not match parameter '" + Id.Value + "'.");
		}
		else
		{
			Resolved.Parameters[Id] = Value;
		}
	}
	for (const auto& [Id, Value] : Instance->StaticOverrides)
	{
		const FMaterialParameterDefinition* Definition = Master->FindParameter(Id);
		if (!Definition)
		{
			AddDiagnostic(Diagnostics, "instance.unknown_static_parameter", "Unknown static parameter override '" + Id.Value + "'.");
		}
		else if (!Definition->IsStatic())
		{
			AddDiagnostic(Diagnostics, "instance.runtime_in_static_overrides", "Runtime parameter '" + Id.Value + "' is in static overrides.");
		}
		else if (!ValueMatchesParameterType(Value, Definition->Type))
		{
			AddDiagnostic(Diagnostics, "instance.static_override_type_mismatch", "Static override type does not match parameter '" + Id.Value + "'.");
		}
		else
		{
			Resolved.StaticParameters[Id] = Value;
		}
	}
	Resolved.ParentChain.push_back(Instance->Id);
	return !HasErrors(Diagnostics);
}

void TiramisuMaterialLibrary::IndexMaster(const FMaterialAsset& Asset, const FMaterialHandle Handle)
{
	MasterReferences[Asset.Id.Value] = Handle;
	if (!Asset.SourcePath.empty())
	{
		MasterReferences[NormalizeAssetReference(Asset.SourcePath)] = Handle;
	}
}

void TiramisuMaterialLibrary::IndexInstance(const FMaterialInstanceAsset& Instance, const FMaterialHandle Handle)
{
	InstanceReferences[Instance.Id.Value] = Handle;
	if (!Instance.SourcePath.empty())
	{
		InstanceReferences[NormalizeAssetReference(Instance.SourcePath)] = Handle;
	}
}

void TiramisuMaterialLibrary::RemoveMasterIndices(const FMaterialAsset& Asset)
{
	MasterReferences.erase(Asset.Id.Value);
	if (!Asset.SourcePath.empty())
	{
		MasterReferences.erase(NormalizeAssetReference(Asset.SourcePath));
	}
}

void TiramisuMaterialLibrary::RemoveInstanceIndices(const FMaterialInstanceAsset& Instance)
{
	InstanceReferences.erase(Instance.Id.Value);
	if (!Instance.SourcePath.empty())
	{
		InstanceReferences.erase(NormalizeAssetReference(Instance.SourcePath));
	}
}
