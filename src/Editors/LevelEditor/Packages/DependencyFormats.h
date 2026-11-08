#pragma once

#include "PackageStore.h"

namespace SdkPackages::DependencyFormats
{
enum class Kind
{
	Object,
	Texture,
	SharedMaterial
};

struct Reference
{
	Kind Type;
	xr_string Name;
	xr_string Shader;
};

struct Scene
{
	xr_vector<Reference> References;
	bool HasObjectData = false;
};

// Read references only: no editor scene, renderer, converters or resource writes.
bool ReadScene(const xr_path& Path, Scene& Result, xr_string& Error, std::stop_token Stop = {});
bool ReadObject(const xr_path& Path, xr_vector<Reference>& Result, xr_string& Error, std::stop_token Stop = {});
bool ReadMaterials(const xr_path& Path, xr_hash_map<xr_string, Reference>& Result, xr_string& Error, std::stop_token Stop = {});
bool ReadTerrainShaders(const xr_path& Path, xr_hash_set<xr_string>& Result, xr_string& Error, std::stop_token Stop = {});
} // namespace SdkPackages::DependencyFormats
