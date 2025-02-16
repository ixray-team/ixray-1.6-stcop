#include "stdafx.h"
#include "actorcondition.h"
#include "../xrCore/object_broker.h"

bool debug_effects = false;
extern BOOL GodMode();

void CActorCondition::UpdateEatablesEffects()
{
	for (u16 i = 0; i < Eat_Effects.size(); i++){
		if (debug_effects) Msg("Eat_Effect %i EngineTime() %f DurationExpiration %f, AffectedStat = %u, UseTimeExpiration = %f", i, Device.fTimeGlobal, Eat_Effects[i].DurationExpiration, Eat_Effects[i].AffectedStat, Eat_Effects[i].UseTimeExpiration);

		if (Eat_Effects[i].DurationExpiration >= Device.fTimeGlobal)
		{
			if (Eat_Effects[i].UseTimeExpiration < Device.fTimeGlobal)
			{
				if (GodMode())
				{
					if (debug_effects) Msg("Eatable Effects are skipped: Developer?!");
					continue;
				}

				if (Eat_Effects[i].AffectedStat == 1)
				{
					if (debug_effects) Msg("Health %f", Eat_Effects[i].Rate * Device.fTimeDelta);
					ChangeHealth(Eat_Effects[i].Rate *  Device.fTimeDelta);
				}
				else if (Eat_Effects[i].AffectedStat == 2)
				{
					if (debug_effects) Msg("Bleed %f", Eat_Effects[i].Rate *  Device.fTimeDelta);
					ChangeBleeding(Eat_Effects[i].Rate *  Device.fTimeDelta);
				}
				else if (Eat_Effects[i].AffectedStat == 3)
				{
					if (debug_effects) Msg("Rad %f", Eat_Effects[i].Rate *   Device.fTimeDelta);
					ChangeRadiation(Eat_Effects[i].Rate *  Device.fTimeDelta);
				}
				else if (Eat_Effects[i].AffectedStat == 4)
				{
					if (debug_effects) Msg("Psy %f", Eat_Effects[i].Rate *  Device.fTimeDelta);
					ChangePsyHealth(Eat_Effects[i].Rate *  Device.fTimeDelta);
				}
				else if (Eat_Effects[i].AffectedStat == 5)
				{
					if (debug_effects) Msg("Food %f", Eat_Effects[i].Rate *  Device.fTimeDelta);
					ChangeSatiety(Eat_Effects[i].Rate *  Device.fTimeDelta);
				}
				else if (Eat_Effects[i].AffectedStat == 6)
				{
					if (debug_effects) Msg("Thirst %f", Eat_Effects[i].Rate *  Device.fTimeDelta);
					ChangeThirsty(Eat_Effects[i].Rate *  Device.fTimeDelta);
				}
				else if (Eat_Effects[i].AffectedStat == 7)
				{
					if (debug_effects) Msg("Energy %f", Eat_Effects[i].Rate *  Device.fTimeDelta);
					ChangePower(Eat_Effects[i].Rate *  Device.fTimeDelta);
				}
				else if (Eat_Effects[i].AffectedStat == 8)
				{
					if (debug_effects) Msg("Boris %f", Eat_Effects[i].Rate *  Device.fTimeDelta);
					ChangeAlcohol(Eat_Effects[i].Rate *  Device.fTimeDelta);
				}
			}
		}
		else
		{
			if (debug_effects) Msg("Effect Expired, remove it");
			if (Eat_Effects[i].BoosterParam.EffectIsBooster)
			{
				m_fBoostersAddWeight -= Eat_Effects[i].BoosterParam.AddWeight;
			}
			Eat_Effects.erase(Eat_Effects.begin() + i);
		}
	}

	//if (debug_effects) Msg("Eat_Effects size2 == %i", Eat_Effects.size());
}


void CActorCondition::save_effects(NET_Packet &output_packet)
{
#ifdef DEBUG
	Msg("# Saving Actor Eatable Items Effects");
#endif

	save_data(m_fBoostersAddWeight, output_packet);

	effects_size = (u16)Eat_Effects.size();
	save_data(effects_size, output_packet);

	if (effects_size > 0) // save data only if array is not empty
	{
		xr_vector<float>		Saved_Durs;
		xr_vector<float>		Saved_UseDurs;
		xr_vector<float>		Saved_Rates;
		xr_vector<u8>			Saved_AffStats;
		xr_vector<u8>			Saved_Groups;
		xr_vector<u8>			Saved_IsBosters;
		xr_vector<float>		Saved_AddWeights;
		xr_vector<shared_str>	Saved_ImmunSections;

		for (u32 i = 0; i < Eat_Effects.size(); i++)
		{
			Saved_Durs.push_back			(Eat_Effects[i].DurationExpiration - Device.fTimeGlobal);
			Saved_UseDurs.push_back			(Eat_Effects[i].UseTimeExpiration - Device.fTimeGlobal);
			Saved_Rates.push_back			(Eat_Effects[i].Rate);
			Saved_AffStats.push_back		(Eat_Effects[i].AffectedStat);
			Saved_Groups.push_back			(Eat_Effects[i].BlockingGroup);
			Saved_IsBosters.push_back		(Eat_Effects[i].BoosterParam.EffectIsBooster);
			Saved_AddWeights.push_back		(Eat_Effects[i].BoosterParam.AddWeight);
			Saved_ImmunSections.push_back	(Eat_Effects[i].BoosterParam.HitImmunitySect);

			if (debug_effects)
			{
				Msg("Save Eat_Effect: [%i]", i);
				Msg("EngineTime() %f, DurationExpiration %f, UseTimeExpiration = %f, Rate = %f, AffectedStat = %u, BlockingGroup = %u, EffectIsBooster = %u, AddWeight = %f",
					Device.fTimeGlobal, Saved_Durs[i], Saved_UseDurs[i], Saved_Rates[i], Saved_AffStats[i], Saved_Groups[i], Saved_IsBosters[i], Saved_AddWeights[i]);
				Msg("HitImmunitySect = %s", Saved_ImmunSections[i]);

			}
		}

		save_data(Saved_Durs, output_packet);
		save_data(Saved_UseDurs, output_packet);
		save_data(Saved_Rates, output_packet);
		save_data(Saved_AffStats, output_packet);
		save_data(Saved_Groups, output_packet);
		save_data(Saved_IsBosters, output_packet);
		save_data(Saved_AddWeights, output_packet);
		save_data(Saved_ImmunSections, output_packet);
	}

}


void CActorCondition::load_effects(IReader &input_packet)
{
#ifdef DEBUG
	Msg("# Loading Actor Eatable Items Effects");
#endif

	load_data(m_fBoostersAddWeight, input_packet);

	Eat_Effects.clear();
	load_data(effects_size, input_packet);
	if (effects_size > 0) // load data only if array is not empty
	{
		xr_vector<float>		Saved_Durs;
		xr_vector<float>		Saved_UseDurs;
		xr_vector<float>		Saved_Rates;
		xr_vector<u8>			Saved_AffStats;
		xr_vector<u8>			Saved_Groups;
		xr_vector<u8>			Saved_IsBosters;
		xr_vector<float>		Saved_AddWeights;
		xr_vector<shared_str>	Saved_ImmunSections;

		load_data(Saved_Durs, input_packet);
		load_data(Saved_UseDurs, input_packet);
		load_data(Saved_Rates, input_packet);
		load_data(Saved_AffStats, input_packet);
		load_data(Saved_Groups, input_packet);
		load_data(Saved_IsBosters, input_packet);
		load_data(Saved_AddWeights, input_packet);
		load_data(Saved_ImmunSections, input_packet);

		for (u32 i = 0; i < Saved_Durs.size(); i++)
		{
			if (debug_effects) Msg("Loading Eat_Effect [%u]", i);

			Eat_Effect loaded_effect;
			loaded_effect.DurationExpiration	= Saved_Durs[i] + Device.fTimeGlobal;
			loaded_effect.UseTimeExpiration		= Saved_UseDurs[i] + Device.fTimeGlobal;
			loaded_effect.Rate					= Saved_Rates[i];

			loaded_effect.AffectedStat			= Saved_AffStats[i];
			loaded_effect.BlockingGroup			= Saved_Groups[i];

			BoosterParams loaded_booster;
			loaded_booster.EffectIsBooster		= Saved_IsBosters[i];
			if (loaded_booster.EffectIsBooster)
			{
				loaded_booster.AddWeight			= Saved_AddWeights[i];

				LPCSTR immun_section = Saved_ImmunSections[i].c_str();

				if (!immun_section || immun_section && !pSettings->section_exist(immun_section)) // to protect save file from immunities sections editions/changes
				{
					Msg("!Can't find immunity section [%s] for booster effect, it was renamed or removed from configs. Trying to use booster_protection_base section", immun_section ? immun_section : "null");

					if (!pSettings->section_exist("booster_protection_base")) // for complete savefile protection
					{
						Msg("!Can't find neither [%s], nor booster_protection_base; booster is disabled. Return booster_protection_base or rename backup section in engine", immun_section ? immun_section : "null");
						loaded_booster.EffectIsBooster = false;
					}
					else
						immun_section = "booster_protection_base";
				}

				if (loaded_booster.EffectIsBooster) // check again as it could become false if no section found
				{
					CHitImmunity immunity;
					immunity.LoadImmunities(immun_section, pSettings);
					loaded_booster.BoosterHitImmunities = immunity;

					loaded_booster.HitImmunitySect = immun_section;
				}
			}

			loaded_effect.BoosterParam = loaded_booster;

			Eat_Effects.push_back(loaded_effect);

			if (debug_effects)
			{
				Msg("Loaded Eat_Effect: [%i], EngineTime() %f, DurationExpiration %f, UseTimeExpiration = %f, Rate = %f, AffectedStat = %u, BlockingGroup = %u, EffectIsBooster = %d, AddWeight = %f",
					i, Device.fTimeGlobal, loaded_effect.DurationExpiration, loaded_effect.UseTimeExpiration, loaded_effect.Rate, loaded_effect.AffectedStat, loaded_effect.BlockingGroup, loaded_effect.BoosterParam.EffectIsBooster, loaded_effect.BoosterParam.AddWeight);
				Msg("HitImmunitySect = %s", loaded_booster.HitImmunitySect.c_str());
			}
		}
	}
}
