#pragma once

class CStalkerAnimationNames
{
public:
	CStalkerAnimationNames();

	enum class ECollectionType
	{
		State,
		Weapon,
		WeaponAction,
		Movement,
		MovementAction,
		InPlace,
		Global,
		Head,
		Torso
	};

private:
	void Load();

	xr_vector<shared_str> StateNames;
	xr_vector<shared_str> WeaponNames;
	xr_vector<shared_str> WeaponActionNames;
	xr_vector<shared_str> MovementNames;
	xr_vector<shared_str> MovementActionNames;
	xr_vector<shared_str> InPlaceNames;
	xr_vector<shared_str> GlobalNames;
	xr_vector<shared_str> HeadNames;
	xr_vector<shared_str> TorsoNames;

public:
	const xr_vector<shared_str>& GetCollection(ECollectionType Type) const &;
};

extern CStalkerAnimationNames GAnimationNames;