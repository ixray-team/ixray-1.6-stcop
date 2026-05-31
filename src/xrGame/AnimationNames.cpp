#include "StdAfx.h"
#include "AnimationNames.h"

#include <nlohmann/json.hpp>

CStalkerAnimationNames GAnimationNames;

static void LoadCollection(const char* FileName, xr_vector<shared_str>& Result)
{
	Result.clear();

	string_path Path;
	string_path ExFileName = {};
	xr_strconcat(ExFileName, "animations\\collections\\", FileName);

	FS.update_path(Path, "$game_config$", ExFileName);

	IReader* Reader = FS.r_open(Path);

	if (!Reader)
	{
		Msg("! Failed to open animation collection: %s", Path);
		return;
	}

	xr_string JsonString((const char*)Reader->pointer(), Reader->length());
	nlohmann::json Json = nlohmann::json::parse(JsonString.c_str());;
	
	if (!Json.contains("Items"))
	{
		Msg("! Animation collection '%s' has no Items array", Path);
		FS.r_close(Reader);
		return;
	}

	for (const auto& Item : Json["Items"])
	{
		Result.emplace_back(Item.get<std::string>().c_str());
	}

	FS.r_close(Reader);
}

CStalkerAnimationNames::CStalkerAnimationNames()
{
	Load();
}

void CStalkerAnimationNames::Load()
{
	LoadCollection("state_names.json", StateNames);
	LoadCollection("weapon_names.json", WeaponNames);
	LoadCollection("weapon_action_names.json", WeaponActionNames);
	LoadCollection("movement_names.json", MovementNames);
	LoadCollection("movement_action_names.json", MovementActionNames);
	LoadCollection("in_place_names.json", InPlaceNames);
	LoadCollection("global_names.json", GlobalNames);
	LoadCollection("head_names.json", HeadNames);
	LoadCollection("torso_names.json", TorsoNames);

	Msg("* Loaded animation name collections");
}

const xr_vector<shared_str>& CStalkerAnimationNames::GetCollection(ECollectionType Type) const &
{
	switch (Type)
	{
		case ECollectionType::State:			return StateNames;
		case ECollectionType::Weapon:			return WeaponNames;
		case ECollectionType::WeaponAction:		return WeaponActionNames;
		case ECollectionType::Movement:			return MovementNames;
		case ECollectionType::MovementAction:	return MovementActionNames;
		case ECollectionType::InPlace:			return InPlaceNames;
		case ECollectionType::Global:			return GlobalNames;
		case ECollectionType::Head:				return HeadNames;
		case ECollectionType::Torso:			return TorsoNames;
		default:								NODEFAULT; return StateNames;
	}
}
