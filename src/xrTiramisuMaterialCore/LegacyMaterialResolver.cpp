#include "LegacyMaterialResolver.h"
#include "MaterialJsonHelpers.h"

#include <nlohmann/json.hpp>

#include <algorithm>
#include <cctype>
#include <ranges>
#include <sstream>

namespace
{
using Json = nlohmann::json;

bool HasErrors(const xr_vector<FMaterialDiagnostic>& Diagnostics)
{
	return std::ranges::any_of(Diagnostics, [](const FMaterialDiagnostic& Diagnostic)
							   { return Diagnostic.Severity == EMaterialDiagnosticSeverity::Error; });
}

void AddDiagnostic(xr_vector<FMaterialDiagnostic>& Diagnostics, const xr_string_view Code, const xr_string& Message, const EMaterialDiagnosticSeverity Severity)
{
	Diagnostics.push_back({Severity, xr_string(Code), Message, {}, {}});
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
		AddDiagnostic(Diagnostics, "legacy_map.invalid_field_type", "Field '" + xr_string(Name) + "' must be a string.", EMaterialDiagnosticSeverity::Error);
	}
	return Result;
}
} // namespace

bool FLegacyMaterialMapParseResult::Succeeded() const noexcept
{
	return !HasErrors(Diagnostics);
}

xr_string NormalizeLegacyShaderName(const xr_string_view ShaderName)
{
	size_t Begin = 0;
	size_t End = ShaderName.size();
	while (Begin < End && std::isspace(static_cast<unsigned char>(ShaderName[Begin])))
	{
		++Begin;
	}
	while (End > Begin && std::isspace(static_cast<unsigned char>(ShaderName[End - 1])))
	{
		--End;
	}

	xr_string Result;
	Result.reserve(End - Begin);
	bool PreviousSeparator = false;
	for (size_t Index = Begin; Index < End; ++Index)
	{
		const unsigned char Character = static_cast<unsigned char>(ShaderName[Index]);
		const bool Separator = Character == '/' || Character == '\\';
		if (Separator)
		{
			if (!PreviousSeparator)
			{
				Result.push_back('\\');
			}
		}
		else
		{
			Result.push_back(static_cast<char>(std::tolower(Character)));
		}
		PreviousSeparator = Separator;
	}
	return Result;
}

xr_string MakeLegacyMaterialInstanceCacheKey(const FResolvedLegacyMaterial& Material)
{
	std::ostringstream Key;
	const auto Append = [&Key](const xr_string_view Value)
	{
		Key << Value.size() << ':' << Value << '|';
	};
	Key << "$legacy-material-instance$|";
	Append(Material.Material);
	Append(NormalizeLegacyShaderName(Material.LegacyShaderName));
	for (const xr_string& Texture : Material.Textures)
	{
		Append(Texture);
	}
	return Key.str();
}

FMaterialParameterMap MakeLegacyMaterialRuntimeOverrides(
	const FResolvedLegacyMaterial& Material
)
{
	FMaterialParameterMap Result;
	if (!Material.Textures.empty())
	{
		Result.emplace(FMaterialParameterId{xr_string(LegacyBaseTextureParameterId)}, Material.Textures[0]);
	}
	if (Material.Textures.size() > 1)
	{
		Result.emplace(FMaterialParameterId{xr_string(LegacyLightmapTextureParameterId)}, Material.Textures[1]);
	}
	return Result;
}

FLegacyMaterialMapParseResult ParseLegacyMaterialMapJson(const xr_string_view JsonText)
{
	FLegacyMaterialMapParseResult Result;
	try
	{
		const Json Root = Json::parse(JsonText, nullptr, false);
		if (Root.is_discarded())
		{
			AddDiagnostic(Result.Diagnostics, "legacy_map.invalid_json", "Legacy material map contains invalid JSON.", EMaterialDiagnosticSeverity::Error);
			return Result;
		}
		if (!Root.is_object())
		{
			AddDiagnostic(Result.Diagnostics, "legacy_map.invalid_root", "Legacy material map root must be an object.", EMaterialDiagnosticSeverity::Error);
			return Result;
		}
		if (const Json* Version = MaterialJsonDetail::Find(Root, "asset_version"))
		{
			if (!MaterialJsonDetail::TryGetUInt32(*Version, Result.Value.Version))
			{
				AddDiagnostic(Result.Diagnostics, "legacy_map.invalid_field_type", "Field 'asset_version' must be an unsigned integer.", EMaterialDiagnosticSeverity::Error);
			}
		}
		Result.Value.StandardMaterial = ReadStringField(Root, "standard_material", Result.Diagnostics);
		Result.Value.ErrorMaterial = ReadStringField(Root, "error_material", Result.Diagnostics);
		if (Result.Value.Version != LegacyMaterialMapVersion)
		{
			AddDiagnostic(Result.Diagnostics, "legacy_map.unsupported_version", "Unsupported legacy material map version " + std::to_string(Result.Value.Version) + ".", EMaterialDiagnosticSeverity::Error);
		}
		if (Result.Value.StandardMaterial.empty())
		{
			AddDiagnostic(Result.Diagnostics, "legacy_map.missing_standard", "legacy-map.json has no standard_material.", EMaterialDiagnosticSeverity::Error);
		}
		if (Result.Value.ErrorMaterial.empty())
		{
			AddDiagnostic(Result.Diagnostics, "legacy_map.missing_error", "legacy-map.json has no error_material.", EMaterialDiagnosticSeverity::Error);
		}
		if (const Json* Mappings = MaterialJsonDetail::Find(Root, "mappings"))
		{
			if (!Mappings->is_object())
			{
				AddDiagnostic(Result.Diagnostics, "legacy_map.invalid_field_type", "Field 'mappings' must be an object.", EMaterialDiagnosticSeverity::Error);
			}
			else
			{
				for (const auto& [Shader, Material] : Mappings->items())
				{
					xr_string MaterialReference;
					if (!MaterialJsonDetail::TryGetString(Material, MaterialReference))
					{
						AddDiagnostic(Result.Diagnostics, "legacy_map.invalid_mapping", "Mapping for legacy shader '" + Shader + "' must be a string.", EMaterialDiagnosticSeverity::Error);
					}
					else
					{
						Result.Value.ShaderMappings.emplace(
							NormalizeLegacyShaderName(Shader), std::move(MaterialReference)
						);
					}
				}
			}
		}
	}
	catch (const std::exception& Error)
	{
		AddDiagnostic(Result.Diagnostics, "legacy_map.invalid_json", Error.what(), EMaterialDiagnosticSeverity::Error);
	}
	return Result;
}

xr_string SerializeLegacyMaterialMapJson(const FLegacyMaterialMap& Map)
{
	Json Root{{"asset_version", Map.Version}, {"standard_material", Map.StandardMaterial}, {"error_material", Map.ErrorMaterial}, {"mappings", Json::object()}};
	xr_vector<xr_pair<xr_string, xr_string>> Sorted(Map.ShaderMappings.begin(), Map.ShaderMappings.end());
	std::ranges::sort(Sorted, {}, &xr_pair<xr_string, xr_string>::first);
	for (const auto& [Shader, Material] : Sorted)
	{
		Root["mappings"][Shader.c_str()] = Material;
	}
	return Root.dump(2);
}

FResolvedLegacyMaterial ResolveLegacyMaterial(const FLegacyMaterialMap& Map, const FLegacyMaterialRequest& Request)
{
	FResolvedLegacyMaterial Result;
	Result.LegacyShaderName = NormalizeLegacyShaderName(Request.ShaderName);
	Result.Textures = Request.Textures;
	if (!Request.Textures.empty())
	{
		Result.BaseTexture = Request.Textures.front();
	}

	if (Request.ExplicitMaterial && !Request.ExplicitMaterial->empty())
	{
		Result.Resolution = ELegacyMaterialResolution::ExplicitMaterial;
		Result.Material = *Request.ExplicitMaterial;
		return Result;
	}

	if (const auto Mapping = Map.ShaderMappings.find(Result.LegacyShaderName); Mapping != Map.ShaderMappings.end())
	{
		Result.Resolution = ELegacyMaterialResolution::LegacyMap;
		Result.Material = Mapping->second;
		return Result;
	}

	if (!Map.StandardMaterial.empty() && !Request.ShaderName.empty() && !Result.BaseTexture.empty())
	{
		Result.Resolution = ELegacyMaterialResolution::AutomaticStandard;
		Result.Material = Map.StandardMaterial;
		AddDiagnostic(Result.Diagnostics, "legacy.automatic_standard", "Legacy shader '" + Result.LegacyShaderName + "' used automatic standard material fallback.", EMaterialDiagnosticSeverity::Warning);
		return Result;
	}

	Result.Resolution = ELegacyMaterialResolution::ErrorMaterial;
	Result.Material = Map.ErrorMaterial;
	AddDiagnostic(Result.Diagnostics, "legacy.error_material", "Legacy material could not be mapped and uses the diagnostic error material.", EMaterialDiagnosticSeverity::Error);
	return Result;
}
