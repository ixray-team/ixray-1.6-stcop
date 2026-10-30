#include "FFxExtendedEntityesLogic/stdafx.h"
#include <algorithm>
#include <cmath>
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedPerceptionProfiles.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"

namespace ExtendedEntityesProfiles
{
const char* const NoiseNames[] = {"shot", "explosion", "actor_steps", "monster_steps", "jump", "melee", "attack", "injury", "death", "bullet_hit", "breaking", "collision", "talking", "eating", "idle", "ambient", "pickup", "drop", "hide", "take", "use", "empty_click", "reload", "other", "light"};

// Читает числовой параметр, заменяет недопустимое значение и ограничивает диапазон.
float Number(const CInifile* SystemSettings, const char* Section, const char* LocalKey, const char* SharedKey, float DefaultValue, float Minimum, float Maximum)
{
	float Value = SystemSettings && *SharedKey ? (SystemSettings)->read_if_exists<float>(CELSharedSettings::SectionName(), SharedKey, DefaultValue) : DefaultValue;
	if (!_valid(Value))
	{
		Value = DefaultValue;
	}
	if (*LocalKey)
	{
		Value = (pSettings)->read_if_exists<float>(Section, LocalKey, Value);
	}
	if (!_valid(Value))
	{
		Value = DefaultValue;
	}
	clamp(Value, Minimum, Maximum);
	return Value;
}

// Читает настройки отдельного типа звуковой реакции.
SELNoiseProfile Noise(const CInifile* SystemSettings, const char* Section, const char* Prefix, const char* SharedPrefix)
{
	SELNoiseProfile Result;
	string128 Key;
	string128 SharedKey;
	xr_sprintf(Key, "%s_enabled", Prefix);
	Result.IsEnabled = (pSettings)->read_if_exists<bool>(Section, Key, false);
	xr_sprintf(Key, "%s_radius", Prefix);
	Result.Radius = Number(SystemSettings, Section, Key, "", 0.f, 0.f, 500.f);
	xr_sprintf(Key, "%s_panic_chance", Prefix);
	Result.PanicChance = Number(SystemSettings, Section, Key, "", 0.f, 0.f, 1.f);
	xr_sprintf(Key, "%s_attract", Prefix);
	Result.CanAttract = (pSettings)->read_if_exists<bool>(Section, Key, false);
	xr_sprintf(Key, "%s_interest_enabled", Prefix);
	Result.CanAttract = (pSettings)->read_if_exists<bool>(Section, Key, Result.CanAttract);
	xr_sprintf(Key, "%s_fear_enabled", Prefix);
	Result.CanFrighten = (pSettings)->read_if_exists<bool>(Section, Key, Result.PanicChance > 0.f);
	xr_sprintf(Key, "%s_interest_step", Prefix);
	xr_sprintf(SharedKey, "%sInterestStep", SharedPrefix);
	Result.InterestStep = Number(SystemSettings, Section, Key, SharedKey, .25f, 0.f, 1.f);
	xr_sprintf(Key, "%s_interest_threshold", Prefix);
	xr_sprintf(SharedKey, "%sInterestThreshold", SharedPrefix);
	Result.InterestThreshold = Number(SystemSettings, Section, Key, SharedKey, .65f, .01f, 1.f);
	xr_sprintf(Key, "%s_fear_step", Prefix);
	xr_sprintf(SharedKey, "%sFearStep", SharedPrefix);
	Result.FearStep = Number(SystemSettings, Section, Key, SharedKey, .25f, 0.f, 1.f);
	xr_sprintf(Key, "%s_fear_threshold", Prefix);
	xr_sprintf(SharedKey, "%sFearThreshold", SharedPrefix);
	Result.FearThreshold = Number(SystemSettings, Section, Key, SharedKey, .65f, .01f, 1.f);
	if (Result.CanFrighten)
	{
		Result.CanAttract = false;
	}
	return Result;
}
} // namespace ExtendedEntityesProfiles

namespace ExtendedEntityesProfiles
{
// Разбирает список типов шума через запятую в маску разрешённых реакций.
u32 ReadNoiseTypes(const CInifile* SystemSettings, const char* Section, const char* LocalKey, const char* SharedKey, u32 DefaultTypes)
{
	const CInifile* SelectedSettings = nullptr;
	const char* SelectedSection = Section;
	const char* SelectedKey = LocalKey;
	if (pSettings->line_exist(Section, LocalKey))
	{
		SelectedSettings = pSettings;
	}
	else if (SystemSettings && SystemSettings->line_exist(CELSharedSettings::SectionName(), SharedKey))
	{
		SelectedSettings = SystemSettings;
		SelectedSection = CELSharedSettings::SectionName();
		SelectedKey = SharedKey;
	}
	if (!SelectedSettings)
	{
		return DefaultTypes;
	}
	const char* ConfiguredTypes = SelectedSettings->r_string(SelectedSection, SelectedKey);
	// CInifile represents an explicitly empty value as nullptr, not an absent key.
	if (!ConfiguredTypes || !*ConfiguredTypes)
	{
		return 0;
	}
	static_assert(sizeof(ExtendedEntityesProfiles::NoiseNames) / sizeof(ExtendedEntityesProfiles::NoiseNames[0]) == u32(EELNoiseType::Count));
	u32 Result = 0;
	u32 TypeIndex;
	int TokenIndex;
	const int TokenCount = _GetItemCount(ConfiguredTypes);
	xr_string TypeName;
	bool WasFound;
	for (TokenIndex = 0; TokenIndex < TokenCount; ++TokenIndex)
	{
		_GetItem(ConfiguredTypes, TokenIndex, TypeName);
		if (TypeName.empty())
		{
			continue;
		}
		WasFound = false;
		for (TypeIndex = 0; TypeIndex < u32(EELNoiseType::Count); ++TypeIndex)
		{
			if (TypeName == NoiseNames[TypeIndex])
			{
				Result |= 1u << TypeIndex;
				WasFound = true;
				break;
			}
		}
		if (!WasFound)
		{
			Msg("! ExtendedEntityesLogic: [%s] %s: неизвестный тип шума '%s'", Section, LocalKey, TypeName.c_str());
		}
	}
	return Result;
}

// Читает списки интереса и испуга и параметры накопления реакции.
void ReadNoiseReactions(SELPerceptionProfile& Result, const CInifile* SystemSettings, const char* Section)
{
	const u32 ShotMask = ELNoiseTypeMask(EELNoiseType::Shot);
	const u32 ExplosionMask = ELNoiseTypeMask(EELNoiseType::Explosion);
	u32 GeneralMask = (1u << u32(EELNoiseType::Count)) - 1u;
	GeneralMask &= ~(ShotMask | ExplosionMask);
	if (!Result.CanHearActorSteps)
	{
		GeneralMask &= ~(ELNoiseTypeMask(EELNoiseType::ActorSteps) | ELNoiseTypeMask(EELNoiseType::Jump));
	}
	if (!Result.CanHearMonsterSteps)
	{
		GeneralMask &= ~ELNoiseTypeMask(EELNoiseType::MonsterSteps);
	}
	const u32 CombatMask = ELNoiseTypeMask(EELNoiseType::Melee) | ELNoiseTypeMask(EELNoiseType::Attack) | ELNoiseTypeMask(EELNoiseType::Injury) | ELNoiseTypeMask(EELNoiseType::Death) | ELNoiseTypeMask(EELNoiseType::BulletHit);
	const u32 StepMask = ELNoiseTypeMask(EELNoiseType::ActorSteps) | ELNoiseTypeMask(EELNoiseType::MonsterSteps) | ELNoiseTypeMask(EELNoiseType::Jump);
	if (!Result.CanHearCombat)
	{
		GeneralMask &= ~CombatMask;
	}
	if (!Result.CanHearWorld)
	{
		GeneralMask &= CombatMask | StepMask;
	}
	const SELNoiseProfile& GeneralReaction = Result.Shot.CanAttract ? Result.Shot : Result.Explosion;
	u32 InterestTypes = GeneralReaction.CanAttract ? GeneralMask : 0;
	u32 FearTypes = 0;
	if (Result.Shot.IsEnabled)
	{
		if (Result.Shot.CanAttract)
		{
			InterestTypes |= ShotMask;
		}
		if (Result.Shot.CanFrighten)
		{
			FearTypes |= ShotMask;
		}
	}
	if (Result.Explosion.IsEnabled)
	{
		if (Result.Explosion.CanAttract)
		{
			InterestTypes |= ExplosionMask;
		}
		if (Result.Explosion.CanFrighten)
		{
			FearTypes |= ExplosionMask;
		}
	}
	Result.FearNoiseTypes = ReadNoiseTypes(SystemSettings, Section, "monster_noise_fear_types", "MonsterNoiseFearTypes", FearTypes);
	Result.InterestNoiseTypes = ReadNoiseTypes(SystemSettings, Section, "monster_noise_interest_types", "MonsterNoiseInterestTypes", InterestTypes) & ~Result.FearNoiseTypes;
	Result.NoiseFearStep = Number(SystemSettings, Section, "monster_noise_fear_step", "MonsterNoiseFearStep", .25f, 0.f, 1.f);
	Result.NoiseFearThreshold = Number(SystemSettings, Section, "monster_noise_fear_threshold", "MonsterNoiseFearThreshold", .65f, .01f, 1.f);
	Result.NoisePanicChance = Number(SystemSettings, Section, "monster_noise_panic_chance", "MonsterNoisePanicChance", 0.f, 0.f, 1.f);
}

// Читает дальности и множители слуха для разных источников звука.
void ReadHearing(SELPerceptionProfile& Result, const CInifile* SystemSettings, const char* Section)
{
	const char* RatioNames[] = {"shot", "explosion", "step", "combat", "injury", "death", "breaking", "collision", "bullet_hit"};
	string128 Key;
	u32 RatioIndex;
	string128 SharedRatioKey;
	const char* SharedRatioNames[] = {"Shot", "Explosion", "Step", "Combat", "Injury", "Death", "Breaking", "Collision", "BulletHit"};
	for (RatioIndex = 0; RatioIndex < 9; ++RatioIndex)
	{
		xr_sprintf(Key, "monster_%s_hearing_ratio", RatioNames[RatioIndex]);
		xr_sprintf(SharedRatioKey, "Monster%sHearingRatio", SharedRatioNames[RatioIndex]);
		Result.HearingRatios[RatioIndex] = Number(SystemSettings, Section, Key, SharedRatioKey, 1.f, 0.f, 5.f);
	}
	Result.HearingFactor = Number(SystemSettings, Section, "monster_hearing_factor", "", 1.f, 0.f, 5.f);
	Result.SuppressedRatio = Number(SystemSettings, Section, "monster_suppressed_hearing_ratio", "MonsterSuppressedHearingRatio", .3f, 0.f, 1.f);
	Result.MeleeRadius = Number(SystemSettings, Section, "monster_melee_sound_radius", "MonsterMeleeSoundRadius", 5.f, 0.f, 500.f);
	Result.MeleeRatio = Number(SystemSettings, Section, "monster_melee_hearing_ratio", "MonsterMeleeHearingRatio", 1.f, 0.f, 1.f);
	Result.ActorJumpRadius = Number(SystemSettings, Section, "monster_actor_jump_sound_radius", "MonsterActorJumpSoundRadius", 20.f, 0.f, 500.f);
	Result.NoiseInterestInterval = u32(Number(SystemSettings, Section, "monster_noise_interest_interval", "MonsterNoiseInterestInterval", .1f, .05f, 5.f) * 1000.f);
	const char* MovementNames[] = {"walk", "run", "sprint", "high_crouch", "low_crouch"};
	const char* MovementSharedNames[] = {"Walk", "Run", "Sprint", "HighCrouch", "LowCrouch"};
	const float MovementDefaults[] = {7.f, 10.f, 15.f, 5.f, 2.f};
	const char* WeaponNames[] = {"automatic", "sniper", "shotgun", "pistol", "grenade_launcher"};
	const char* WeaponSharedNames[] = {"Automatic", "Sniper", "Shotgun", "Pistol", "GrenadeLauncher"};
	const float WeaponDefaults[] = {.6f, 1.f, .75f, .4f, .25f};
	string128 SharedKey;
	for (RatioIndex = 0; RatioIndex < 5; ++RatioIndex)
	{
		xr_sprintf(Key, "monster_actor_%s_sound_radius", MovementNames[RatioIndex]);
		xr_sprintf(SharedKey, "MonsterActor%sSoundRadius", MovementSharedNames[RatioIndex]);
		Result.ActorMovementRadii[RatioIndex] = Number(SystemSettings, Section, Key, SharedKey, MovementDefaults[RatioIndex], 0.f, 500.f);
		xr_sprintf(Key, "monster_%s_shot_ratio", WeaponNames[RatioIndex]);
		xr_sprintf(SharedKey, "Monster%sShotRatio", WeaponSharedNames[RatioIndex]);
		Result.WeaponRatios[RatioIndex] = Number(SystemSettings, Section, Key, SharedKey, WeaponDefaults[RatioIndex], 0.f, 1.f);
	}
}

// Читает правила обнаружения тел и учёта опасных областей.
void ReadDeaths(SELPerceptionProfile& Result, const CInifile* SystemSettings, const char* Section)
{
	Result.CanInspectBodies = (pSettings)->read_if_exists<bool>(Section, "monster_corpse_investigation_enabled", SystemSettings ? (SystemSettings)->read_if_exists<bool>(CELSharedSettings::SectionName(), "MonsterCorpseInvestigationEnabled", true) : true);
	Result.CanAvoidDeaths = (pSettings)->read_if_exists<bool>(Section, "monster_death_hazard_enabled", SystemSettings ? (SystemSettings)->read_if_exists<bool>(CELSharedSettings::SectionName(), "MonsterDeathHazardEnabled", true) : true);
	Result.CanHearDeaths = (pSettings)->read_if_exists<bool>(Section, "monster_death_alert_enabled", SystemSettings ? (SystemSettings)->read_if_exists<bool>(CELSharedSettings::SectionName(), "MonsterDeathAlertEnabled", true) : true);
	Result.DeathHearingRadius = Number(SystemSettings, Section, "monster_death_hearing_radius", "MonsterDeathHearingRadius", 20.f, 0.f, 100.f);
	Result.BodyDetectionRadius = Number(SystemSettings, Section, "monster_corpse_detection_radius", "MonsterCorpseDetectionRadius", 25.f, 0.f, 100.f);
	Result.BodyInterestTime = u32(Number(SystemSettings, Section, "monster_corpse_interest_time", "MonsterCorpseInterestTime", 90.f, 0.f, 600.f) * 1000.f);
	Result.DeathHazardRadius = Number(SystemSettings, Section, "monster_death_hazard_radius", "MonsterDeathHazardRadius", 12.f, 1.f, 50.f);
	Result.DeathHazardTime = u32(Number(SystemSettings, Section, "monster_death_hazard_time", "MonsterDeathHazardTime", 120.f, 5.f, 600.f) * 1000.f);
	Result.DeathHazardThreshold = u32(Number(SystemSettings, Section, "monster_death_hazard_threshold", "MonsterDeathHazardThreshold", 3.f, 1.f, 20.f));
	Result.BodyInvestigatorLimit = u32(Number(SystemSettings, Section, "monster_corpse_investigators", "MonsterCorpseInvestigators", 2.f, 1.f, 8.f));
	Result.DeathFightQuietTime = u32(Number(SystemSettings, Section, "monster_death_hazard_fight_quiet_time", "MonsterDeathHazardFightQuietTime", 15.f, 1.f, 120.f) * 1000.f);
}

// Разбирает разрешённые стратегии наземного подхода из конфигурации.
void ReadStrategies(SELPerceptionProfile& Result, const char* Section)
{
	const char* StrategyNames[] = {"direct", "flank", "wide", "lunge", "charge", "encircle", "pressure", "retreat"};
	static_assert(std::size(StrategyNames) == u32(EELApproachStrategy::Count));
	const char* ConfiguredStrategies = (pSettings)->read_if_exists<str_c>(Section, "monster_behavior_strategies", "direct,flank,pressure");
	string64 StrategyName;
	u32 StrategyIndex;
	u32 TokenIndex;
	bool HasStrategy;
	Result.Strategies = 0;
	for (TokenIndex = 0; ConfiguredStrategies && TokenIndex < _GetItemCount(ConfiguredStrategies); ++TokenIndex)
	{
		_GetItem(ConfiguredStrategies, TokenIndex, StrategyName);
		HasStrategy = false;
		for (StrategyIndex = 0; StrategyIndex < u32(EELApproachStrategy::Count); ++StrategyIndex)
		{
			if (!xr_strcmp(StrategyName, StrategyNames[StrategyIndex]))
			{
				Result.Strategies |= 1u << StrategyIndex;
				HasStrategy = true;
			}
		}
		if (!HasStrategy)
		{
			Msg("! [ExtendedEntityesLogic] Неизвестная стратегия подхода '%s' в '%s'", StrategyName, Section);
		}
	}
	if (!Result.Strategies)
	{
		Result.Strategies = 1;
	}
}
} // namespace ExtendedEntityesProfiles

// Возвращает кэшированный профиль секции, загружая его при первом обращении.
SELPerceptionProfile CELSharedPerceptionProfiles::Read(const char* Section, const CInifile* SystemSettings)
{
	using namespace ExtendedEntityesProfiles;
	SELPerceptionProfile Result;
	Result.CanUseBehaviors = (pSettings)->read_if_exists<bool>(Section, "monster_behavior_enabled", (SystemSettings ? (SystemSettings)->read_if_exists<bool>(CELSharedSettings::SectionName(), "MonsterBehaviorEnabled", true) : true));
	Result.IsPassive = (pSettings)->read_if_exists<bool>(Section, "monster_peaceful", false);
	Result.CanShare = (pSettings)->read_if_exists<bool>(Section, "monster_enemy_sharing_enabled", true);
	Result.IsWireless = (pSettings)->read_if_exists<bool>(Section, "monster_enemy_sharing_wireless", false);
	Result.CanTrackLive = (pSettings)->read_if_exists<bool>(Section, "monster_enemy_tracking_live", false);
	Result.CanCall = (pSettings)->read_if_exists<bool>(Section, "monster_enemy_call_enabled", true);
	Result.CanAutoFocus = (pSettings)->read_if_exists<bool>(Section, "autofocus_attacker", false);
	Result.CanPanicOnDamage = (pSettings)->read_if_exists<bool>(Section, "autopanic_on_invisible_damage", false);
	Result.CanInspect = (pSettings)->read_if_exists<bool>(Section, "monster_search_sniff", false);
	Result.SharingRadius = Number(SystemSettings, Section, "monster_enemy_sharing_radius", "", 40.0f, 0.0f, 500.0f);
	Result.CloseSharingRatio = Number(SystemSettings, Section, "monster_enemy_sharing_close_ratio", "MonsterEnemySharingCloseRatio", 0.25f, 0.0f, 1.0f);
	Result.CallSharingRatio = Number(SystemSettings, Section, "monster_enemy_sharing_call_ratio", "MonsterEnemySharingCallRatio", 0.6f, 0.0f, 1.0f);
	Result.CloseDetectionRadius = Number(SystemSettings, Section, "monster_enemy_close_detection_radius", "", 0.0f, 0.0f, 100.0f);
	Result.AutoFocusRadius = Number(SystemSettings, Section, "autofocus_max_dist", "", 200.0f, 0.0f, 500.0f);
	Result.CanRecordTrail = (pSettings)->read_if_exists<bool>(Section, "monster_trail_enabled", SystemSettings ? (SystemSettings)->read_if_exists<bool>(CELSharedSettings::SectionName(), "MonsterTrailEnabled", true) : true);
	Result.TargetCrowdDistance = Number(SystemSettings, Section, "monster_target_crowd_distance", "MonsterTargetCrowdDistance", 20.f, 1.f, 100.f);
	Result.TargetSwitchDistanceRatio = Number(SystemSettings, Section, "monster_target_switch_distance_ratio", "", .6f, .1f, 1.f);
	Result.TargetCloseThreatDistance = Number(SystemSettings, Section, "monster_target_close_threat_distance", "", 12.f, 1.f, 100.f);
	Result.TargetPursuitGrace = u32(Number(SystemSettings, Section, "monster_target_pursuit_grace", "", 5.f, 0.f, 60.f) * 1000.f);
	Result.TargetDamageMemory = u32(Number(SystemSettings, Section, "monster_target_damage_memory", "", 3.f, 0.f, 60.f) * 1000.f);
	Result.TargetMemoryCost = Number(SystemSettings, Section, "monster_target_memory_cost", "", 4.f, 1.f, 20.f);
	Result.TargetDamageCost = Number(SystemSettings, Section, "monster_target_damage_cost", "", .25f, .01f, 1.f);
	Result.TargetCloseThreatCost = Number(SystemSettings, Section, "monster_target_close_threat_cost", "", .5f, .01f, 1.f);
	Result.TargetInterestGain = Number(SystemSettings, Section, "monster_target_interest_gain", "", .025f, 0.f, 1.f);
	Result.TargetInterestDecay = Number(SystemSettings, Section, "monster_target_interest_decay", "", .02f, 0.f, 1.f);
	Result.TargetInterestHitGain = Number(SystemSettings, Section, "monster_target_interest_hit_gain", "", .35f, 0.f, 1.f);
	Result.TargetInterestDamageGain = Number(SystemSettings, Section, "monster_target_interest_damage_gain", "", .5f, 0.f, 1.f);
	Result.TargetInterestWeight = Number(SystemSettings, Section, "monster_target_interest_weight", "", 3.f, 0.f, 20.f);
	Result.ApproachSpeed = Number(SystemSettings, Section, "monster_approach_speed_estimate", "MonsterApproachSpeedEstimate", 2.f, .1f, 20.f);
	Result.TrailLength = Number(SystemSettings, Section, "monster_trail_length", "MonsterTrailLength", 5.0f, 1.0f, 50.0f);
	Result.RetreatHealthThreshold = Number(SystemSettings, Section, "monster_retreat_health_threshold", "", .35f, 0.f, 1.f);
	Result.ChargeMinDistance = Number(SystemSettings, Section, "monster_charge_min_distance", "", 15.f, 0.f, 200.f);
	Result.LungeMaxDistance = Number(SystemSettings, Section, "monster_lunge_max_distance", "", 7.f, 0.f, 200.f);
	Result.DirectRushMinMembers = u32(Number(SystemSettings, Section, "monster_direct_rush_min_members", "", 4.f, 1.f, 64.f));
	Result.SearchRunDistance = Number(SystemSettings, Section, "monster_search_run_distance", "", 3.f, 0.f, 30.f);
	Result.ApproachWidth = Number(SystemSettings, Section, "monster_approach_width", "MonsterApproachWidth", 5.0f, 0.0f, 20.0f);
	Result.SearchRadius = Number(SystemSettings, Section, "monster_search_radius", "MonsterSearchRadius", 12.0f, 1.0f, 50.0f);
	Result.SearchAngle = Number(SystemSettings, Section, "monster_search_angle", "MonsterSearchAngle", 60.0f, 10.0f, 180.0f);
	Result.CanHearActorSteps = (pSettings)->read_if_exists<bool>(Section, "monster_noise_actor_steps_enabled", true);
	Result.CanHearMonsterSteps = (pSettings)->read_if_exists<bool>(Section, "monster_noise_monster_steps_enabled", true);
	Result.CanHearCombat = (pSettings)->read_if_exists<bool>(Section, "monster_noise_combat_enabled", true);
	Result.CanHearWorld = (pSettings)->read_if_exists<bool>(Section, "monster_noise_world_enabled", true);
	Result.NoiseInterestStep = Number(SystemSettings, Section, "monster_noise_interest_step", "MonsterNoiseInterestStep", .25f, 0.f, 1.f);
	Result.NoiseInterestThreshold = Number(SystemSettings, Section, "monster_noise_interest_threshold", "MonsterNoiseInterestThreshold", .65f, .01f, 1.f);
	Result.NoiseSearchTime = u32(Number(SystemSettings, Section, "monster_noise_search_time", "MonsterNoiseSearchTime", 60.f, 1.f, 3600.f) * 1000.f);
	Result.NoiseMinPower = Number(SystemSettings, Section, "monster_noise_min_power", "", 0.01f, 0.0f, 1.0f);
	Result.NoiseShotPriority = u32(Number(SystemSettings, Section, "monster_noise_shot_priority", "", 80.f, 0.f, 1000.f));
	Result.NoiseDangerPriority = u32(Number(SystemSettings, Section, "monster_noise_danger_priority", "", 100.f, 0.f, 1000.f));
	Result.NoiseOtherPriority = u32(Number(SystemSettings, Section, "monster_noise_other_priority", "", 10.f, 0.f, 1000.f));
	Result.GroundLocalRetry = u32(Number(SystemSettings, Section, "monster_ground_local_retry", "", .2f, .05f, 5.f) * 1000.f);
	Result.GroundStallTime = u32(Number(SystemSettings, Section, "monster_ground_stall_time", "", .5f, .1f, 5.f) * 1000.f);
	Result.SearchPointTime = u32(Number(SystemSettings, Section, "monster_search_point_time", "", 3.f, .1f, 60.f) * 1000.f);
	Result.BodyInspectMin = u32(Number(SystemSettings, Section, "monster_body_inspect_time_min", "", 3.f, .1f, 60.f) * 1000.f);
	Result.BodyInspectMax = std::max(Result.BodyInspectMin, u32(Number(SystemSettings, Section, "monster_body_inspect_time_max", "", 6.f, .1f, 60.f) * 1000.f));
	Result.CanWitnessAggression = (pSettings)->read_if_exists<bool>(Section, "monster_witness_aggression", true);
	Result.WitnessWindow = u32(Number(SystemSettings, Section, "monster_witness_window", "", .75f, .1f, 3.f) * 1000.f);
	Result.CanExpandNoiseSearch = (pSettings)->read_if_exists<bool>(Section, "monster_noise_expand_search", true);
	Result.NoiseImmediateChance = Number(SystemSettings, Section, "monster_noise_immediate_interest_chance", "MonsterNoiseImmediateInterestChance", 0.35f, 0.0f, 1.0f);
	Result.NoiseDecay = Number(SystemSettings, Section, "monster_noise_interest_decay", "MonsterNoiseInterestDecay", 0.1f, 0.0f, 1.0f);
	Result.GeneralNoiseRadius = Number(SystemSettings, Section, "monster_sound_reaction_radius", "MonsterSoundReactionRadius", 25.0f, 0.0f, 500.0f);
	Result.SharingIntervalMin = u32(Number(SystemSettings, Section, "", "MonsterEnemySharingIntervalMin", 3.0f, .1f, 3600.f) * 1000.f);
	Result.SharingIntervalMax = u32(Number(SystemSettings, Section, "", "MonsterEnemySharingIntervalMax", 5.0f, .1f, 3600.f) * 1000.f);
	Result.MemoryMin = u32(Number(SystemSettings, Section, "monster_enemy_memory_min", "MonsterEnemyMemoryMin", 30.0f, .1f, 3600.f) * 1000.f);
	Result.MemoryMax = u32(Number(SystemSettings, Section, "monster_enemy_memory_max", "MonsterEnemyMemoryMax", 60.0f, .1f, 3600.f) * 1000.f);
	Result.SearchMin = u32(Number(SystemSettings, Section, "monster_search_time_min", "MonsterSearchTimeMin", 15.0f, .1f, 3600.f) * 1000.f);
	Result.SearchMax = u32(Number(SystemSettings, Section, "monster_search_time_max", "MonsterSearchTimeMax", 25.0f, .1f, 3600.f) * 1000.f);
	Result.ApproachMin = u32(Number(SystemSettings, Section, "monster_approach_time_min", "MonsterApproachTimeMin", 15.f, .1f, 3600.f) * 1000.f);
	Result.ApproachMax = u32(Number(SystemSettings, Section, "monster_approach_time_max", "MonsterApproachTimeMax", 90.0f, .1f, 3600.f) * 1000.f);
	Result.CallCooldown = u32(Number(SystemSettings, Section, "monster_enemy_call_cooldown", "", 30.0f, .1f, 3600.f) * 1000.f);
	Result.NoiseCooldown = u32(Number(SystemSettings, Section, "monster_noise_reaction_cooldown", "", 3.0f, .1f, 3600.f) * 1000.f);
	const float BodyMass = Number(SystemSettings, Section, "ph_mass", "", 60.f, 1.f, 10000.f);
	const float Fearfulness = clampr(logf(400.f / BodyMass) / logf(40.f), 0.f, 1.f);
	Result.NoisePanicDuration = u32(Number(SystemSettings, Section, "monster_noise_panic_duration", "", 15.f + 25.f * Fearfulness, 15.f, 40.f) * 1000.f);
	Result.ShotPanicDuration = u32(Number(SystemSettings, Section, "monster_shot_panic_duration", "", 10.f + 20.f * Fearfulness, 10.f, 30.f) * 1000.f);
	Result.ExplosionPanicDuration = u32(Number(SystemSettings, Section, "monster_explosion_panic_duration", "", 20.f + 40.f * Fearfulness, 20.f, 60.f) * 1000.f);
	Result.MemoryMax = std::max(Result.MemoryMin, Result.MemoryMax);
	Result.SharingIntervalMax = std::max(Result.SharingIntervalMin, Result.SharingIntervalMax);
	Result.SearchMax = std::max(Result.SearchMin, Result.SearchMax);
	Result.ApproachMin = std::min(Result.ApproachMin, Result.ApproachMax);
	Result.VisualAcquireTime = u32(Number(SystemSettings, Section, "monster_player_visual_acquire_time", "MonsterPlayerVisualAcquireTime", 400.f, 0.f, 10000.f));
	Result.CanIgnoreFoliage = (pSettings)->read_if_exists<bool>(Section, "monster_vision_ignore_foliage", false) && (!SystemSettings || (SystemSettings)->read_if_exists<bool>(CELSharedSettings::SectionName(), "EnableMonsterVisionIgnoreFoliage", true));
	Result.CloseVisualRadius = Number(SystemSettings, Section, "monster_close_visual_radius", "MonsterCloseVisualRadius", 10.f, 0.f, 100.f);
	const char* Schemes = (pSettings)->read_if_exists<str_c>(Section, "monster_behavior_schemes", "");
	bool AllowsCloseScent = false;
	for (int Index = 0; Index < _GetItemCount(Schemes); ++Index)
	{
		xr_string Scheme;
		_GetItem(Schemes, Index, Scheme);
		AllowsCloseScent = AllowsCloseScent || Scheme == "close_scent";
	}
	Result.CanUseCloseScent = AllowsCloseScent;
	Result.CloseScentRadius = Number(SystemSettings, Section, "monster_close_scent_radius", "", std::min(7.f, Result.CloseVisualRadius), 0.f, 100.f);
	Result.CloseScentTime = u32(Number(SystemSettings, Section, "monster_close_scent_time", "", 5.f, 0.f, 60.f) * 1000.f);
	Result.CloseVisualTime = u32(Number(SystemSettings, Section, "monster_close_visual_time", "MonsterCloseVisualTime", 5.f, 0.f, 60.f) * 1000.f);
	Result.NativeAttackDistance = Number(SystemSettings, Section, "monster_enemy_close_attack_distance", "MonsterEnemyCloseAttackDistance", 5.f, .1f, 30.f);
	Result.CanUsePeriphery = (pSettings)->read_if_exists<bool>(Section, "monster_peripheral_enabled", SystemSettings ? (SystemSettings)->read_if_exists<bool>(CELSharedSettings::SectionName(), "MonsterPeripheralEnabled", true) : true);
	Result.PeripheralAngle = Number(SystemSettings, Section, "monster_peripheral_angle", "MonsterPeripheralAngle", 150.f, 0.f, 360.f);
	Result.PeripheralRadius = Number(SystemSettings, Section, "monster_peripheral_radius", "MonsterPeripheralRadius", 25.f, 0.f, 100.f);
	Result.PeripheralInterval = u32(Number(SystemSettings, Section, "monster_peripheral_interval", "MonsterPeripheralInterval", 1.f, .1f, 10.f) * 1000.f);
	Result.CanAvoidAnomalies = (pSettings)->read_if_exists<bool>(Section, "monster_anomaly_avoidance_enabled", false);
	Result.AnomalyScanRadius = Number(SystemSettings, Section, "monster_anomaly_scan_radius", "MonsterAnomalyScanRadius", 60.f, 5.f, 150.f);
	Result.AnomalyMargin = Number(SystemSettings, Section, "monster_anomaly_margin", "MonsterAnomalyMargin", 1.f, 0.f, 5.f);
	Result.DayVision = Number(SystemSettings, Section, "monster_vision_day_factor", "", 1.f, .1f, 2.f);
	Result.NightVision = Number(SystemSettings, Section, "monster_vision_night_factor", "", 1.f, .1f, 2.f);
	Result.DayAcquire = Number(SystemSettings, Section, "monster_acquire_day_factor", "", 1.f, .1f, 5.f);
	Result.NightAcquire = Number(SystemSettings, Section, "monster_acquire_night_factor", "", 1.f, .1f, 5.f);
	Result.RainVisionLoss = Number(SystemSettings, Section, "monster_rain_vision_loss", "", .15f, 0.f, .15f);
	Result.FogVisionLoss = Number(SystemSettings, Section, "monster_fog_vision_loss", "", .4f, 0.f, .4f);
	const bool LegacyDodge = pSettings->read_if_exists<bool>(Section, "monster_dodge_enabled", true);
	Result.CanDodgeFire = pSettings->read_if_exists<bool>(Section, "monster_dodge_fire_enabled", true);
	Result.CanDodgeStep = pSettings->read_if_exists<bool>(Section, "monster_dodge_step_enabled", LegacyDodge);
	Result.CanDodgeJump = pSettings->read_if_exists<bool>(Section, "monster_dodge_jump_enabled", false);
	Result.DodgeJumpOnAim = pSettings->read_if_exists<bool>(Section, "monster_dodge_jump_on_aim", false);
	Result.DodgeTrackTarget = pSettings->read_if_exists<bool>(Section, "monster_dodge_track_target", true);
	Result.DodgeTrackInterval = u32(Number(SystemSettings, Section, "monster_dodge_track_interval", "", .1f, .05f, 1.f) * 1000.f);
	Result.CanDodge = Result.CanDodgeStep || Result.CanDodgeJump;
	Result.CanAttackJump = pSettings->read_if_exists<bool>(Section, "monster_attack_jump_enabled", true);
	Result.CanEvadeDive = pSettings->read_if_exists<bool>(Section, "monster_evade_dive_enabled", true);
	Result.CanCounterAttack = pSettings->read_if_exists<bool>(Section, "monster_counterattack_enabled", true);
	Result.CounterAttackDuration = u32(Number(SystemSettings, Section, "monster_counterattack_duration", "", 1.5f, .1f, 10.f) * 1000.f);
	Result.CounterOnReload = pSettings->read_if_exists<bool>(Section, "monster_counterattack_on_reload", true);
	Result.CounterOnEmptyClick = pSettings->read_if_exists<bool>(Section, "monster_counterattack_on_empty_click", true);
	Result.CounterOnNearMiss = pSettings->read_if_exists<bool>(Section, "monster_counterattack_on_near_miss", true);
	Result.CounterNearMissRadius = Number(SystemSettings, Section, "monster_counterattack_near_miss_radius", "", 3.f, .1f, 20.f);
	Result.CounterOpportunityTime = u32(Number(SystemSettings, Section, "monster_counterattack_opportunity_time", "", 2.f, .1f, 10.f) * 1000.f);
	Result.CounterOpportunityCooldown = u32(Number(SystemSettings, Section, "monster_counterattack_opportunity_cooldown", "", 2.f, .1f, 30.f) * 1000.f);
	Result.DodgeStrength = Number(SystemSettings, Section, "monster_dodge_strength", "", .45f, 0.f, 1.f);
	Result.DodgeAimAngle = Number(SystemSettings, Section, "monster_dodge_aim_angle", "", 10.f, 0.1f, 90.f);
	Result.DodgeReleaseAngle = Number(SystemSettings, Section, "monster_dodge_release_angle", "", 12.f, 0.1f, 100.f);
	Result.DodgeDurationMin = u32(Number(SystemSettings, Section, "monster_dodge_duration_min", "", 1.5f, 0.1f, 5.f) * 1000.f);
	Result.DodgeDurationMax = u32(Number(SystemSettings, Section, "monster_dodge_duration_max", "", 2.2f, 0.1f, 5.f) * 1000.f);
	Result.DodgePauseMin = u32(Number(SystemSettings, Section, "monster_dodge_pause_min", "", 0.2f, 0.05f, 5.f) * 1000.f);
	Result.DodgeFocusTime = u32(Number(SystemSettings, Section, "monster_dodge_focus_time", "", 0.3f, 0.f, 2.f) * 1000.f);
	Result.DodgeVisibilityGrace = u32(Number(SystemSettings, Section, "monster_dodge_visibility_grace", "", 0.25f, 0.f, 1.f) * 1000.f);
	Result.DodgeCounterTime = u32(Number(SystemSettings, Section, "monster_dodge_counter_time", "", 0.5f, 0.f, 5.f) * 1000.f);
	Result.DodgeJumpCooldown = u32(Number(SystemSettings, Section, "monster_dodge_jump_cooldown", "", 8.f, 1.f, 60.f) * 1000.f);
	Result.DodgeJumpRetry = u32(Number(SystemSettings, Section, "monster_dodge_jump_retry", "", 3.f, 1.f, 60.f) * 1000.f);
	Result.DodgeAttackCommitTime = Number(SystemSettings, Section, "monster_dodge_attack_commit_time", "", 0.35f, 0.f, 2.f);
	Result.SoundLookTime = u32(Number(SystemSettings, Section, "monster_sound_look_time", "", 1.2f, 0.1f, 10.f) * 1000.f);
	Result.SoundDangerLookTime = u32(Number(SystemSettings, Section, "monster_sound_danger_look_time", "", 2.5f, 0.1f, 10.f) * 1000.f);
	Result.SoundLookInterval = u32(Number(SystemSettings, Section, "monster_sound_look_interval", "", 0.1f, 0.05f, 1.f) * 1000.f);
	Result.DodgeReleaseAngle = std::max(Result.DodgeAimAngle, Result.DodgeReleaseAngle);
	Result.DodgeDurationMax = std::max(Result.DodgeDurationMin, Result.DodgeDurationMax);
	Result.SoundLookTurnSpeed = deg2rad(Number(SystemSettings, Section, "monster_sound_look_turn_speed", "", 180.f, 10.f, 720.f));
	Result.DodgeWidth = Number(SystemSettings, Section, "monster_dodge_width", "", 2.5f, .1f, 5.f);
	Result.DodgeMaxWidth = Number(SystemSettings, Section, "monster_dodge_max_width", "", 6.f, .1f, 20.f);
	Result.DodgeConeMargin = Number(SystemSettings, Section, "monster_dodge_cone_margin", "", 1.f, 0.f, 5.f);
	Result.DodgeJumpMaxAngle = Number(SystemSettings, Section, "monster_dodge_jump_max_angle", "", 40.f, 15.f, 40.f);
	Result.DodgePredictionTime = Number(SystemSettings, Section, "monster_dodge_prediction_time", "", .35f, 0.f, 1.5f);
	Result.DodgeDirectionInterval = u32(Number(SystemSettings, Section, "monster_dodge_direction_interval", "", .45f, .1f, 3.f) * 1000.f);
	const float LegacyStepCooldown = Number(SystemSettings, Section, "monster_dodge_cooldown", "", .6f, .05f, 30.f);
	Result.DodgeStepCooldown = u32(Number(SystemSettings, Section, "monster_dodge_step_cooldown", "", LegacyStepCooldown, .05f, 30.f) * 1000.f);
	Result.DodgeJumpChance = Number(SystemSettings, Section, "monster_dodge_jump_chance", "", .35f, 0.f, 1.f);
	Result.DodgeJumpDistanceMax = Number(SystemSettings, Section, "monster_dodge_jump_distance_max", "", 5.f, .5f, 12.f);
	Result.DodgeJumpDistanceMin = std::min(Result.DodgeJumpDistanceMax, Number(SystemSettings, Section, "monster_dodge_jump_distance_min", "", .5f, .1f, 12.f));
	Result.DodgeJumpWidthMin = Number(SystemSettings, Section, "monster_dodge_jump_width_min", "", 1.8f, .1f, 20.f);
	Result.DodgeJumpWidthMax = std::max(Result.DodgeJumpWidthMin, Number(SystemSettings, Section, "monster_dodge_jump_width_max", "", 3.2f, .1f, 20.f));
	Result.DodgeApproachAngle = Number(SystemSettings, Section, "monster_dodge_approach_angle", "", 15.f, 15.f, 40.f);
	Result.DodgeStepVariation = Number(SystemSettings, Section, "monster_dodge_step_variation", "", 0.f, 0.f, .5f);
	Result.DodgeStepMaxAngle = Number(SystemSettings, Section, "monster_dodge_step_max_angle", "", 40.f, 5.f, 40.f);
	Result.DodgeStepDistance = Number(SystemSettings, Section, "monster_dodge_step_distance", "", 4.f, .5f, 12.f);
	Result.DodgeStepArrivalDistance = Number(SystemSettings, Section, "monster_dodge_step_arrival_distance", "", 1.f, .1f, 3.f);
	Result.DodgeJumpMaxAngle = std::max(Result.DodgeApproachAngle, Result.DodgeJumpMaxAngle);
	Result.CanFeint = pSettings->read_if_exists<bool>(Section, "monster_feint_enabled", false);
	Result.FeintChance = Number(SystemSettings, Section, "monster_feint_chance", "", .25f, 0.f, 1.f);
	Result.PairedAttackDelay = u32(Number(SystemSettings, Section, "monster_paired_attack_delay", "", .22f, 0.f, 1.f) * 1000.f);
	Result.CanSprint = pSettings->read_if_exists<bool>(Section, "monster_sprint_enabled", false);
	Result.SprintBonus = Number(SystemSettings, Section, "monster_sprint_bonus", "", .3f, 0.f, 1.f);
	Result.SprintDuration = u32(Number(SystemSettings, Section, "monster_sprint_duration", "", 3.f, .1f, 30.f) * 1000.f);
	Result.SprintCooldown = u32(Number(SystemSettings, Section, "monster_sprint_cooldown", "", 8.f, .5f, 120.f) * 1000.f);
	Result.SprintDistance = Number(SystemSettings, Section, "monster_sprint_distance", "", 10.f, 1.f, 100.f);
	Result.CanPursuitJump = pSettings->read_if_exists<bool>(Section, "monster_pursuit_jump_enabled", false);
	Result.CanInterceptFlying = pSettings->read_if_exists<bool>(Section, "monster_flying_intercept_enabled", false);
	Result.InterceptChance = Number(SystemSettings, Section, "monster_flying_intercept_chance", "", .35f, 0.f, 1.f);
	Result.TacticalJumpCooldown = u32(Number(SystemSettings, Section, "monster_tactical_jump_cooldown", "", 4.f, .5f, 60.f) * 1000.f);
	Result.CanAmbush = pSettings->read_if_exists<bool>(Section, "monster_ambush_enabled", false);
	Result.GroupInterceptDistance = Number(SystemSettings, Section, "monster_group_intercept_distance", "", 15.f, 1.f, 100.f);
	Result.CanGrabFlying = pSettings->read_if_exists<bool>(Section, "monster_flying_grab_enabled", false);
	Result.GrabHealthThreshold = Number(SystemSettings, Section, "monster_flying_grab_health", "", .3f, .01f, 1.f);
	Result.GrabMassRatio = Number(SystemSettings, Section, "monster_flying_grab_mass_ratio", "", .5f, .01f, 1.f);
	Result.GrabDuration = u32(Number(SystemSettings, Section, "monster_flying_grab_duration", "", .65f, .2f, 2.f) * 1000.f);
	Result.GrabThrowSpeed = Number(SystemSettings, Section, "monster_flying_grab_throw_speed", "", 6.f, 0.f, 15.f);
	Result.GrabMouthOffset = Number(SystemSettings, Section, "monster_flying_grab_mouth_offset", "", .15f, -.5f, 1.f);
	Result.GrabBone = pSettings->read_if_exists<str_c>(Section, "monster_flying_grab_bone", pSettings->read_if_exists<str_c>(Section, "bone_head", "bip01_head"));
	Result.CallSound = (pSettings)->read_if_exists<str_c>(Section, "monster_enemy_call_sound", "");
	Result.Shot = Noise(SystemSettings, Section, "monster_shot_reaction", "MonsterShotReaction");
	Result.Explosion = Noise(SystemSettings, Section, "monster_explosion_reaction", "MonsterExplosionReaction");
	ExtendedEntityesProfiles::ReadHearing(Result, SystemSettings, Section);
	ExtendedEntityesProfiles::ReadNoiseReactions(Result, SystemSettings, Section);
	ExtendedEntityesProfiles::ReadDeaths(Result, SystemSettings, Section);
	ExtendedEntityesProfiles::ReadStrategies(Result, Section);
	if (!Result.CanAttackJump)
	{
		Result.Strategies &= ~ELApproachStrategyMask(EELApproachStrategy::Lunge);
		if (!Result.Strategies)
		{
			Result.Strategies = ELApproachStrategyMask(EELApproachStrategy::Direct);
		}
	}
	return Result;
}

// Возвращает русское название типа шума для отладки; ключи LTX не изменяются.
const char* CELSharedPerceptionProfiles::NoiseName(EELNoiseType Type)
{
	static const char* Names[] = {"Выстрел", "Взрыв", "Шаги", "Шаги существа", "Прыжок", "Удар вблизи", "Атака", "Ранение", "Смерть", "Попадание пули", "Разрушение", "Столкновение", "Голос / вокализация", "Кормление", "Покой", "Окружение", "Подбор", "Бросок предмета", "Убирание", "Доставание", "Использование", "Пустой щелчок", "Перезарядка", "Прочий шум", "Свет"};
	static_assert(sizeof(Names) / sizeof(Names[0]) == u32(EELNoiseType::Count));
	return u32(Type) < u32(EELNoiseType::Count) ? Names[u32(Type)] : "Прочий шум";
}
