#pragma once

struct BoosterParams
{
	u8 EffectIsBooster; // if its 0, then HitImmunitySect is uninitialized and HitImmunitySect = "null"
	float AddWeight; //Additional actor weight
	CHitImmunity BoosterHitImmunities;
	shared_str HitImmunitySect; // for save/load process, so that we dont have to save the whole hit immunity class object into savefile

	BoosterParams()
	{
		EffectIsBooster = false;
		AddWeight = 0.f;
		HitImmunitySect = "nullnull";
	};
};