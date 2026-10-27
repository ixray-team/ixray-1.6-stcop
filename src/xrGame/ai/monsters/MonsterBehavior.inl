// Included once by base_monster_think.cpp to keep the existing project source lists compatible.
namespace MonsterBehaviorRuntime
{
struct SParticipant
{
	u32 ID = u32(-1), Target = u32(-1), Time = 0, Pack = 0;
	Fvector Position = {0.f, 0.f, 0.f};
};
struct SBody
{
	u32 ID = u32(-1), DeathTime = 0;
	Fvector Position = {0.f, 0.f, 0.f};
	u32 Witnesses[32]{}, WitnessCount = 0;
	bool Environmental = true;
};
struct SInvestigation
{
	u32 Body = u32(-1), Pack = 0, DeathTime = 0, Until = 0, Investigators = 0;
	Fvector Position = {0.f, 0.f, 0.f};
	bool Done = false, Environmental = true;
};
struct SArea
{
	u32 Pack = 0, Until = 0, EvidenceTime = 0;
	Fvector Position = {0.f, 0.f, 0.f};
	float Radius = 0.f;
	u32 FightUntil = 0;
};
struct SFocus
{
	u32 ID = u32(-1), Until = 0;
};
struct SWorld
{
	const void* Level = nullptr;
	u32 Frame = u32(-1), Updates = 0, Queries = 0, Rays = 0, Nodes = 0;
	u32 MaxUpdates = 12, MaxQueries = 2, MaxRays = 16, MaxNodes = 72;
	u32 LastTime = 0;
	bool Configured = false;
	u32 BodyCursor = 0, InvestigationCursor = 0, AreaCursor = 0;
	SParticipant Participants[256]{};
	SBody Bodies[256]{};
	SInvestigation Investigations[256]{};
	SArea Areas[64]{};
	SArea Fights[32]{};
	u32 FightCursor = 0;
	SFocus Focus[16]{};
};
SWorld& World()
{
	static SWorld Value;
	if (Value.Level != g_pGameLevel || Device.dwTimeGlobal < Value.LastTime)
	{
		Value = {};
		Value.Level = g_pGameLevel;
	}
	Value.LastTime = Device.dwTimeGlobal;
	if (!Value.Configured)
	{
		CInifile* Options = EngineExternal().GetMonsterLogicIniFile();
		Value.MaxUpdates = std::clamp(Options->read_if_exists<u32>("monsters_logic", "MonsterBehaviorUpdatesPerFrame", 12u), 1u, 64u);
		Value.MaxQueries = std::min(Options->read_if_exists<u32>("monsters_logic", "MonsterBehaviorQueriesPerFrame", 2u), 8u);
		Value.MaxRays = std::min(Options->read_if_exists<u32>("monsters_logic", "MonsterBehaviorRaysPerFrame", 16u), 64u);
		Value.MaxNodes = std::min(Options->read_if_exists<u32>("monsters_logic", "MonsterBehaviorNodesPerFrame", 72u), 256u);
		Value.Configured = true;
	}
	if (Value.Frame != Device.dwFrame)
	{
		Value.Frame = Device.dwFrame;
		Value.Updates = Value.MaxUpdates;
		Value.Queries = Value.MaxQueries;
		Value.Rays = Value.MaxRays;
		Value.Nodes = Value.MaxNodes;
	}
	return Value;
}
u32 Pack(const CBaseMonster& Monster)
{
	return (u32(Monster.g_Team()) << 16) | (u32(Monster.g_Squad()) << 8) | u32(Monster.g_Group());
}
bool Expired(u32 Until)
{
	return !Until || s32(Until - Device.dwTimeGlobal) <= 0;
}
bool Friend(const CBaseMonster& Monster, const CBaseMonster& Other)
{
	return !Monster.is_relation_enemy(&Other) && !Other.is_relation_enemy(&Monster) &&
		   (Pack(Monster) == Pack(Other) ||
			(EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFriendlyEnemySharing] &&
			 Monster.tfGetRelationType(&Other) == ALife::eRelationTypeFriend && Other.tfGetRelationType(&Monster) == ALife::eRelationTypeFriend));
}
bool Visible(CBaseMonster& Monster, CObject& Object, float Angle, float Radius)
{
	Fvector From = Monster.eye_matrix.c, To = Object.Center(), Direction;
	Direction.sub(To, From);
	const float Distance = Direction.magnitude();
	if (Distance > Radius || Distance < EPS_L || !World().Rays)
	{
		return false;
	}
	Direction.div(Distance);
	if (Angle < 360.f && Monster.Direction().dotproduct(Direction) < cosf(deg2rad(Angle * .5f)))
	{
		return false;
	}
	--World().Rays;
	if (Monster.VisionIgnoreFoliage && EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterVisionIgnoreFoliage])
	{
		struct SVisibilityQuery
		{
			CBaseMonster* Monster;
			CObject* Target;
			bool Visible = true;
		} Query{&Monster, &Object};
		collide::rq_results Results;
		collide::ray_defs Ray(From, Direction, Distance, CDB::OPT_FULL_TEST, collide::rqtBoth);
		Level().ObjectSpace.RayQuery(Results, Ray, [](const collide::rq_result& Hit, LPVOID Context) -> bool
		{
			auto& Query = *static_cast<SVisibilityQuery*>(Context);
			if (Hit.IsStatic() && Query.Monster->feel_vision_mtl_transp(Hit) >= 1.f)
			{
				return true;
			}
			Query.Visible = Hit.GetDynamic() == Query.Target;
			return false;
		}, &Query, nullptr, &Monster);
		return Query.Visible;
	}
	collide::rq_result Hit;
	return !Level().ObjectSpace.RayPick(From, Direction, Distance, collide::rqtBoth, Hit, &Monster) || Hit.GetDynamic() == &Object;
}
void AddArea(u32 Group, const Fvector& Position, float Radius, u32 Evidence, u32 Duration)
{
	SWorld& State = World();
	SArea* Selected = nullptr;
	for (SArea& Area : State.Areas)
	{
		if (!Expired(Area.Until) && Area.Pack == Group && Area.Position.distance_to_sqr(Position) < Radius * Radius)
		{
			if (s32(Evidence - Area.EvidenceTime) <= 0)
			{
				return;
			}
			Selected = &Area;
			break;
		}
	}
	if (!Selected)
	{
		Selected = &State.Areas[State.AreaCursor++ % 64];
	}
	const u32 FightUntil = Selected->Pack == Group ? Selected->FightUntil : 0;
	*Selected = {Group, Device.dwTimeGlobal + Duration, Evidence, Position, Radius, FightUntil};
}
SInvestigation* KnowBody(CBaseMonster& Monster, u32 ID, const Fvector& Position)
{
	SWorld& State = World();
	const SBody* Body = nullptr;
	for (const SBody& Candidate : State.Bodies)
	{
		if (Candidate.ID == ID && Device.dwTimeGlobal - Candidate.DeathTime < Monster.Behavior.BodyFreshTime)
		{
			Body = &Candidate;
			break;
		}
	}
	if (!Body)
	{
		return nullptr;
	}
	SInvestigation* Selected = nullptr;
	for (SInvestigation& Investigation : State.Investigations)
	{
		if (Investigation.Body == ID && Investigation.Pack == Pack(Monster))
		{
			Selected = &Investigation;
			break;
		}
	}
	if (Selected && Selected->DeathTime != Body->DeathTime)
	{
		*Selected = {ID, Pack(Monster), Body->DeathTime, 0, 0, Position, false, Body->Environmental};
	}
	if (!Selected)
	{
		Selected = &State.Investigations[State.InvestigationCursor++ % 256];
		*Selected = {ID, Pack(Monster), Body->DeathTime, 0, 0, Position, false, Body->Environmental};
	}
	// Known combat deaths must not turn the enemy's position into a forbidden area.
	if (!Body->Environmental)
	{
		return Selected;
	}
	u32 Count = 0, Latest = 0;
	for (const SInvestigation& Investigation : State.Investigations)
	{
		if (Investigation.Body != u32(-1) && Investigation.Pack == Pack(Monster) &&
			Device.dwTimeGlobal - Investigation.DeathTime < Monster.Behavior.BodyFreshTime &&
			Investigation.Position.distance_to_sqr(Position) <= _sqr(Monster.Behavior.HazardRadius))
		{
			if (!Investigation.Environmental)
			{
				continue;
			}
			++Count;
			Latest = std::max(Latest, Investigation.DeathTime);
		}
	}
	if (Monster.Behavior.HazardsEnabled && Count >= Monster.Behavior.HazardThreshold)
	{
		AddArea(Pack(Monster), Position, Monster.Behavior.HazardRadius, Latest, Monster.Behavior.HazardTime);
	}
	return Selected;
}
// HACK: renamed from "Point" - a global Carbon type on macOS.
bool ValidGoalPosition(CBaseMonster& Monster, Fvector& Position, u32& Vertex)
{
	if (!World().Nodes || !Monster.Home->at_home(Position) || Monster.Behavior.CrossesDanger(Monster, Position) ||
		!ai().level_graph().valid_vertex_position(Position))
	{
		return false;
	}
	--World().Nodes;
	Vertex = ai().level_graph().vertex_id(Position);
	if (!ai().level_graph().valid_vertex_id(Vertex) || !Monster.control().path_builder().accessible(Vertex))
	{
		return false;
	}
	Position.y = ai().level_graph().vertex_plane_y(Vertex, Position.x, Position.z);
	return Monster.control().path_builder().accessible(Position);
}
float ReadFloat(const char* Section, const char* Local, const char* Common, float Default, float Min, float Max)
{
	const float Value = pSettings->read_if_exists<float>(Section, Local, EngineExternal().GetMonsterLogicIniFile()->read_if_exists<float>("monsters_logic", Common, Default));
	return _valid(Value) ? std::clamp(Value, Min, Max) : Default;
}
} // namespace MonsterBehaviorRuntime

void SMonsterBehavior::Load(CBaseMonster& Monster, const char* Section)
{
	*this = {};
	Monster.SetAttentionVisionObject(nullptr, 0);
	Enabled = pSettings->read_if_exists<bool>(Section, "monster_behavior_enabled", EngineExternal().GetMonsterLogicIniFile()->read_if_exists<bool>("monsters_logic", "MonsterBehaviorEnabled", true));
	const char* List = pSettings->read_if_exists<str_c>(Section, "monster_behavior_strategies", "direct, flank, pressure");
	Strategies = 0;
	const char* Names[] = {"direct", "flank", "wide", "cautious", "lunge", "charge", "encircle", "pressure", "retreat"};
	for (u32 Index = 0; Index < _GetItemCount(List); ++Index)
	{
		string64 Name;
		_GetItem(List, Index, Name);
		for (u32 StrategyIndex = 0; StrategyIndex < 9; ++StrategyIndex)
		{
			if (!xr_strcmp(Name, Names[StrategyIndex]))
			{
				Strategies |= 1u << StrategyIndex;
			}
		}
	}
	if (!Strategies)
	{
		Strategies = Direct;
	}
	SearchMin = u32(MonsterBehaviorRuntime::ReadFloat(Section, "monster_search_time_min", "MonsterSearchTimeMin", 15.f, 1.f, 300.f) * 1000.f);
	SearchMax = u32(MonsterBehaviorRuntime::ReadFloat(Section, "monster_search_time_max", "MonsterSearchTimeMax", 25.f, 1.f, 300.f) * 1000.f);
	if (SearchMax < SearchMin)
	{
		std::swap(SearchMin, SearchMax);
	}
	ApproachMax = u32(MonsterBehaviorRuntime::ReadFloat(Section, "monster_approach_time_max", "MonsterApproachTimeMax", 90.f, 5.f, 300.f) * 1000.f);
	SearchRadius = MonsterBehaviorRuntime::ReadFloat(Section, "monster_search_radius", "MonsterSearchRadius", 12.f, 1.f, 50.f);
	SearchAngle = MonsterBehaviorRuntime::ReadFloat(Section, "monster_search_angle", "MonsterSearchAngle", 60.f, 10.f, 180.f);
	TrailLength = MonsterBehaviorRuntime::ReadFloat(Section, "monster_trail_length", "MonsterTrailLength", 5.f, 1.f, 10.f);
	ApproachWidth = MonsterBehaviorRuntime::ReadFloat(Section, "monster_approach_width", "MonsterApproachWidth", 5.f, 0.f, 20.f);
	PeripheralAngle = std::max(Monster.eye_fov, MonsterBehaviorRuntime::ReadFloat(Section, "monster_peripheral_angle", "MonsterPeripheralAngle", 150.f, 0.f, 360.f));
	PeripheralRadius = MonsterBehaviorRuntime::ReadFloat(Section, "monster_peripheral_radius", "MonsterPeripheralRadius", 25.f, 0.f, 100.f);
	PeripheralInterval = u32(MonsterBehaviorRuntime::ReadFloat(Section, "monster_peripheral_interval", "MonsterPeripheralInterval", 1.f, .25f, 10.f) * 1000.f);
	DeathRadius = MonsterBehaviorRuntime::ReadFloat(Section, "monster_death_hearing_radius", "MonsterDeathHearingRadius", 20.f, 0.f, 100.f);
	BodyRadius = MonsterBehaviorRuntime::ReadFloat(Section, "monster_corpse_detection_radius", "MonsterCorpseDetectionRadius", 25.f, 0.f, 100.f);
	BodyFreshTime = u32(MonsterBehaviorRuntime::ReadFloat(Section, "monster_corpse_interest_time", "MonsterCorpseInterestTime", 90.f, 0.f, 600.f) * 1000.f);
	HazardRadius = MonsterBehaviorRuntime::ReadFloat(Section, "monster_death_hazard_radius", "MonsterDeathHazardRadius", 12.f, 1.f, 50.f);
	HazardThreshold = u32(MonsterBehaviorRuntime::ReadFloat(Section, "monster_death_hazard_threshold", "MonsterDeathHazardThreshold", 3.f, 1.f, 20.f));
	HazardTime = u32(MonsterBehaviorRuntime::ReadFloat(Section, "monster_death_hazard_time", "MonsterDeathHazardTime", 120.f, 5.f, 600.f) * 1000.f);
	InvestigatorLimit = u32(MonsterBehaviorRuntime::ReadFloat(Section, "monster_corpse_investigators", "MonsterCorpseInvestigators", 2.f, 1.f, 8.f));
	Sniff = pSettings->read_if_exists<bool>(Section, "monster_search_sniff", false);
	TrailEnabled = pSettings->read_if_exists<bool>(Section, "monster_trail_enabled", EngineExternal().GetMonsterLogicIniFile()->read_if_exists<bool>("monsters_logic", "MonsterTrailEnabled", true));
	PeripheralEnabled = pSettings->read_if_exists<bool>(Section, "monster_peripheral_enabled", EngineExternal().GetMonsterLogicIniFile()->read_if_exists<bool>("monsters_logic", "MonsterPeripheralEnabled", true));
	DeathAlertEnabled = pSettings->read_if_exists<bool>(Section, "monster_death_alert_enabled", EngineExternal().GetMonsterLogicIniFile()->read_if_exists<bool>("monsters_logic", "MonsterDeathAlertEnabled", true));
	BodiesEnabled = pSettings->read_if_exists<bool>(Section, "monster_corpse_investigation_enabled", EngineExternal().GetMonsterLogicIniFile()->read_if_exists<bool>("monsters_logic", "MonsterCorpseInvestigationEnabled", true));
	HazardsEnabled = pSettings->read_if_exists<bool>(Section, "monster_death_hazard_enabled", EngineExternal().GetMonsterLogicIniFile()->read_if_exists<bool>("monsters_logic", "MonsterDeathHazardEnabled", true));
	ApproachSpeed = MonsterBehaviorRuntime::ReadFloat(Section, "monster_approach_speed_estimate", "MonsterApproachSpeedEstimate", 2.f, .1f, 20.f);
	CloseVisualRadius = MonsterBehaviorRuntime::ReadFloat(Section, "monster_close_visual_radius", "MonsterCloseVisualRadius", 10.f, 0.f, 30.f);
	CloseVisualTime = u32(MonsterBehaviorRuntime::ReadFloat(Section, "monster_close_visual_time", "MonsterCloseVisualTime", 5.f, 0.f, 15.f) * 1000.f);
	NoiseSearchTime = u32(MonsterBehaviorRuntime::ReadFloat(Section, "monster_noise_search_time", "MonsterNoiseSearchTime", 10.f, 1.f, 60.f) * 1000.f);
	CrowdDistance = MonsterBehaviorRuntime::ReadFloat(Section, "monster_target_crowd_distance", "MonsterTargetCrowdDistance", 20.f, 1.f, 100.f);
	NextUpdate = Device.dwTimeGlobal + (u32(Monster.ID()) * 37u) % 500u;
	NextPeripheral = NextUpdate;
	NoiseInterestDecay = MonsterBehaviorRuntime::ReadFloat(Section, "monster_noise_interest_decay", "MonsterNoiseInterestDecay", .1f, 0.f, 1.f);
	NoiseImmediateInterestChance = MonsterBehaviorRuntime::ReadFloat(Section, "monster_noise_immediate_interest_chance", "MonsterNoiseImmediateInterestChance", .35f, 0.f, 1.f);
	SoundReactionRadius = MonsterBehaviorRuntime::ReadFloat(Section, "monster_sound_reaction_radius", "MonsterSoundReactionRadius", 25.f, 0.f, 500.f);
	ShotHearingRatio = MonsterBehaviorRuntime::ReadFloat(Section, "monster_shot_hearing_ratio", "MonsterShotHearingRatio", 1.f, 0.f, 1.f);
	ExplosionHearingRatio = MonsterBehaviorRuntime::ReadFloat(Section, "monster_explosion_hearing_ratio", "MonsterExplosionHearingRatio", 1.f, 0.f, 1.f);
	StepHearingRatio = MonsterBehaviorRuntime::ReadFloat(Section, "monster_step_hearing_ratio", "MonsterStepHearingRatio", 1.f, 0.f, 1.f);
	CombatHearingRatio = MonsterBehaviorRuntime::ReadFloat(Section, "monster_combat_hearing_ratio", "MonsterCombatHearingRatio", 1.f, 0.f, 1.f);
	InjuryHearingRatio = MonsterBehaviorRuntime::ReadFloat(Section, "monster_injury_hearing_ratio", "MonsterInjuryHearingRatio", 1.f, 0.f, 1.f);
	DeathHearingRatio = MonsterBehaviorRuntime::ReadFloat(Section, "monster_death_hearing_ratio", "MonsterDeathHearingRatio", 1.f, 0.f, 1.f);
	BreakingHearingRatio = MonsterBehaviorRuntime::ReadFloat(Section, "monster_breaking_hearing_ratio", "MonsterBreakingHearingRatio", 1.f, 0.f, 1.f);
	CollisionHearingRatio = MonsterBehaviorRuntime::ReadFloat(Section, "monster_collision_hearing_ratio", "MonsterCollisionHearingRatio", 1.f, 0.f, 1.f);
	BulletHitHearingRatio = MonsterBehaviorRuntime::ReadFloat(Section, "monster_bullet_hit_hearing_ratio", "MonsterBulletHitHearingRatio", 1.f, 0.f, 1.f);
	SuppressedHearingRatio = MonsterBehaviorRuntime::ReadFloat(Section, "monster_suppressed_hearing_ratio", "MonsterSuppressedHearingRatio", 0.3f, 0.f, 1.f);
	MeleeHearingRatio = MonsterBehaviorRuntime::ReadFloat(Section, "monster_melee_hearing_ratio", "MonsterMeleeHearingRatio", 0.05f, 0.f, 1.f);
	MeleeSoundRadius = MonsterBehaviorRuntime::ReadFloat(Section, "monster_melee_sound_radius", "MonsterMeleeSoundRadius", 5.0f, 0.f, 500.f);
	ActorJumpSoundRadius = MonsterBehaviorRuntime::ReadFloat(Section, "monster_actor_jump_sound_radius", "MonsterActorJumpSoundRadius", 20.0f, 0.f, 500.f);
	ActorSprintSoundRadius = MonsterBehaviorRuntime::ReadFloat(Section, "monster_actor_sprint_sound_radius", "MonsterActorSprintSoundRadius", 15.0f, 0.f, 500.f);
	ActorRunSoundRadius = MonsterBehaviorRuntime::ReadFloat(Section, "monster_actor_run_sound_radius", "MonsterActorRunSoundRadius", 10.0f, 0.f, 500.f);
	ActorWalkSoundRadius = MonsterBehaviorRuntime::ReadFloat(Section, "monster_actor_walk_sound_radius", "MonsterActorWalkSoundRadius", 7.0f, 0.f, 500.f);
	ActorHighCrouchSoundRadius = MonsterBehaviorRuntime::ReadFloat(Section, "monster_actor_high_crouch_sound_radius", "MonsterActorHighCrouchSoundRadius", 5.0f, 0.f, 500.f);
	ActorLowCrouchSoundRadius = MonsterBehaviorRuntime::ReadFloat(Section, "monster_actor_low_crouch_sound_radius", "MonsterActorLowCrouchSoundRadius", 2.0f, 0.f, 500.f);
	SniperShotRatio = MonsterBehaviorRuntime::ReadFloat(Section, "monster_sniper_shot_ratio", "MonsterSniperShotRatio", 1.0f, 0.f, 1.f);
	ShotgunShotRatio = MonsterBehaviorRuntime::ReadFloat(Section, "monster_shotgun_shot_ratio", "MonsterShotgunShotRatio", 0.75f, 0.f, 1.f);
	AutomaticShotRatio = MonsterBehaviorRuntime::ReadFloat(Section, "monster_automatic_shot_ratio", "MonsterAutomaticShotRatio", 0.6f, 0.f, 1.f);
	PistolShotRatio = MonsterBehaviorRuntime::ReadFloat(Section, "monster_pistol_shot_ratio", "MonsterPistolShotRatio", 0.4f, 0.f, 1.f);
	GrenadeLauncherShotRatio = MonsterBehaviorRuntime::ReadFloat(Section, "monster_grenade_launcher_shot_ratio", "MonsterGrenadeLauncherShotRatio", 0.25f, 0.f, 1.f);
	NoiseInterestInterval = u32(MonsterBehaviorRuntime::ReadFloat(Section, "monster_noise_interest_interval", "MonsterNoiseInterestInterval", .1f, .05f, 5.f) * 1000.f);
	FightQuietTime = u32(MonsterBehaviorRuntime::ReadFloat(Section, "monster_death_hazard_fight_quiet_time", "MonsterDeathHazardFightQuietTime", 15.f, 1.f, 120.f) * 1000.f);
	AnomalyAvoidance = pSettings->read_if_exists<bool>(Section, "monster_anomaly_avoidance_enabled", false);
	AnomalyScanRadius = MonsterBehaviorRuntime::ReadFloat(Section, "monster_anomaly_scan_radius", "MonsterAnomalyScanRadius", 60.f, 5.f, 150.f);
	AnomalyMargin = MonsterBehaviorRuntime::ReadFloat(Section, "monster_anomaly_margin", "MonsterAnomalyMargin", 1.f, 0.f, 5.f);
}

bool SMonsterBehavior::Avoids(CBaseMonster& Monster, const Fvector& Point) const
{
	if (!Enabled || !EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation]) return false;
	// Anomalies remain hazardous even during combat, unlike temporary corpse areas.
	if (AnomalyAvoidance)
	{
		for (u32 Index = 0; Index < AnomalyCount; ++Index)
		{
			if (Anomalies[Index].Position.distance_to_sqr(Point) < _sqr(Anomalies[Index].Radius)) return true;
		}
	}
	if (!HazardsEnabled || Monster.EnemyMan.see_enemy_now()) return false;
	for (const auto& Fight : MonsterBehaviorRuntime::World().Fights)
	{
		if (!MonsterBehaviorRuntime::Expired(Fight.Until) && Fight.Pack == MonsterBehaviorRuntime::Pack(Monster) &&
			Fight.Position.distance_to_sqr(Point) < _sqr(Fight.Radius)) return false;
	}
	for (const auto& Area : MonsterBehaviorRuntime::World().Areas)
	{
		if (!MonsterBehaviorRuntime::Expired(Area.Until) && MonsterBehaviorRuntime::Expired(Area.FightUntil) &&
			Area.Pack == MonsterBehaviorRuntime::Pack(Monster) && Area.Position.distance_to_sqr(Point) < _sqr(Area.Radius)) return true;
	}
	return false;
}

bool SMonsterBehavior::CrossesDanger(CBaseMonster& Monster, const Fvector& Point) const
{
	if (!Enabled || !EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation]) return false;
	const Fvector& From = Monster.Position();
	Fvector Delta; Delta.sub(Point, From);
	const float LengthSquared = Delta.square_magnitude();
	auto Crosses = [&](const Fvector& Center, float Radius)
	{
		// Permit an animal caught inside a field to move outward, but never enter a new field.
		if (From.distance_to_sqr(Center) < _sqr(Radius) && Point.distance_to_sqr(Center) > From.distance_to_sqr(Center)) return false;
		Fvector ToCenter; ToCenter.sub(Center, From);
		const float Fraction = LengthSquared > EPS_S ? std::clamp(ToCenter.dotproduct(Delta) / LengthSquared, 0.f, 1.f) : 0.f;
		Fvector Nearest; Nearest.mad(From, Delta, Fraction);
		return Nearest.distance_to_sqr(Center) < _sqr(Radius);
	};
	if (AnomalyAvoidance)
	{
		for (u32 Index = 0; Index < AnomalyCount; ++Index)
		{
			if (Crosses(Anomalies[Index].Position, Anomalies[Index].Radius)) return true;
		}
	}
	if (HazardsEnabled)
	{
		for (const auto& Area : MonsterBehaviorRuntime::World().Areas)
		{
			if (Area.Pack == MonsterBehaviorRuntime::Pack(Monster) && !MonsterBehaviorRuntime::Expired(Area.Until) &&
				Avoids(Monster, Area.Position) && Crosses(Area.Position, Area.Radius)) return true;
		}
	}
	return false;
}

u32 SMonsterBehavior::HearNoise(CBaseMonster& Monster, CObject* Source, const Fvector& Position, float Power, bool Explosion, bool General, u32 SoundType)
{
	const auto& Reaction = Explosion || (General && !Monster.ShotReaction.Attract) ? Monster.ExplosionReaction : Monster.ShotReaction;
	const u32 ID = Source ? u32(Source->ID()) : u32(-1);
	SNoiseAttention* Slot = nullptr;
	for (auto& Entry : NoiseAttention)
	{
		if (!MonsterBehaviorRuntime::Expired(Entry.Until) && Entry.Source == ID && Entry.SoundType == SoundType &&
			Entry.Position.distance_to_sqr(Position) <= .01f)
		{
			// Repeated mixer pulses from the same point are the same acoustic observation.
			LastNoiseInterest = Entry.Interest;
			Entry.Until = Device.dwTimeGlobal + 10000;
			return 0;
		}
	}
	if (!Slot)
	{
		Slot = &NoiseAttention[NoiseCursor++ % 8];
		*Slot = {};
	}
	Slot->Source = ID;
	Slot->Position = Position;
	Slot->Power = Power; Slot->Explosion = Explosion; Slot->General = General;
	Slot->SoundType = SoundType; Slot->ActorSource = Source && Source->cast_actor();
	const u32 Now = Device.dwTimeGlobal;
	if (!Slot->LastInterest || Now - Slot->LastInterest >= NoiseInterestInterval)
	{
		const float Decay = Slot->LastInterest ? float(Now - Slot->LastInterest) * .001f * NoiseInterestDecay : 0.f;
		Slot->Interest = Reaction.Attract ? std::min(1.f, std::max(0.f, Slot->Interest - Decay) + Reaction.InterestStep) : 0.f;
		if (Reaction.Attract && Random.randF() < NoiseImmediateInterestChance) Slot->Interest = 1.f;
		Slot->Fear = !General && Reaction.FearEnabled ? std::min(1.f, std::max(0.f, Slot->Fear - Decay) + Reaction.FearStep) : 0.f;
		Slot->LastInterest = Now;
	}
	LastNoiseInterest = Slot->Interest;
	Slot->Until = Now + 10000;
	// Updating a burst never postpones the already scheduled visual check.
	return (Reaction.Attract && Slot->Interest >= Reaction.InterestThreshold ? 1u : 0u) |
		(!General && Reaction.FearEnabled && Slot->Fear >= Reaction.FearThreshold ? 2u : 0u);
}

void SMonsterBehavior::ShareAreas(CBaseMonster& Receiver, const CBaseMonster& Source)
{
	if (!Enabled || !HazardsEnabled || !Source.Behavior.Enabled || !Source.Behavior.HazardsEnabled)
	{
		return;
	}
	auto& State = MonsterBehaviorRuntime::World();
	// Copy first: inserting into the bounded ring must not change the source iteration.
	MonsterBehaviorRuntime::SArea Snapshot[64];
	std::copy(std::begin(State.Areas), std::end(State.Areas), Snapshot);
	for (const auto& Area : Snapshot)
	{
		if (!MonsterBehaviorRuntime::Expired(Area.Until) && Area.Pack == MonsterBehaviorRuntime::Pack(Source))
		{
			MonsterBehaviorRuntime::AddArea(MonsterBehaviorRuntime::Pack(Receiver), Area.Position, Area.Radius, Area.EvidenceTime, Area.Until - Device.dwTimeGlobal);
		}
	}
}

void SMonsterBehavior::OnDeath(CBaseMonster& Monster)
{
	if (!Enabled || !EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation])
	{
		return;
	}
	DeathTime = Device.dwTimeGlobal ? Device.dwTimeGlobal : 1;
	auto& State = MonsterBehaviorRuntime::World();
	MonsterBehaviorRuntime::SBody* Existing = nullptr;
	for (auto& Body : State.Bodies)
	{
		if (Body.ID == u32(Monster.ID()))
		{
			Existing = &Body;
			break;
		}
	}
	if (!Existing)
	{
		Existing = &State.Bodies[State.BodyCursor++ % 256];
	}
	*Existing = {u32(Monster.ID()), DeathTime, Monster.Position()};
	CObject* Killer = Level().Objects.net_Find(Monster.killer_id());
	Existing->Environmental = !Killer || !Killer->cast_entity_alive();
	// Capture witnesses now, not when delayed death audio arrives.
	for (const auto& Participant : State.Participants)
	{
		if (Existing->WitnessCount == 32)
		{
			break;
		}
		if (Participant.ID == u32(-1) || Participant.ID == u32(Monster.ID()) || Device.dwTimeGlobal - Participant.Time > 3000)
		{
			continue;
		}
		CObject* Object = Level().Objects.net_Find(ALife::_OBJECT_ID(Participant.ID));
		CBaseMonster* Observer = Object ? Object->cast_base_monster() : nullptr;
		if (!Observer || !Observer->g_Alive() || !Observer->Behavior.DeathAlertEnabled || !MonsterBehaviorRuntime::Friend(*Observer, Monster) ||
			Observer->Position().distance_to_sqr(Monster.Position()) > _sqr(Observer->Behavior.DeathRadius))
		{
			continue;
		}
		Fvector SeenPosition;
		u32 SeenTime = 0;
		const bool CachedSight = Observer->GetOwnVisionObservation(&Monster, SeenPosition, SeenTime) && Device.dwTimeGlobal - SeenTime <= 100;
		if (CachedSight || MonsterBehaviorRuntime::Visible(*Observer, Monster, Observer->eye_fov, Observer->Behavior.DeathRadius))
		{
			Existing->Witnesses[Existing->WitnessCount++] = Observer->ID();
		}
	}

	Remove(Monster);
}

void SMonsterBehavior::Remove(CBaseMonster& Monster)
{
	Monster.SetAttentionVisionObject(nullptr, 0);
	if (!Enabled)
	{
		return;
	}
	for (auto& Participant : MonsterBehaviorRuntime::World().Participants)
	{
		if (Participant.ID == u32(Monster.ID()))
		{
			Participant = {};
		}
	}
}

void SMonsterBehavior::HearDeath(CBaseMonster& Monster, CBaseMonster* Source, const Fvector& Position, float Power)
{
	if (!Enabled || !DeathAlertEnabled || !EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] || !Source || Source->g_Alive() || !MonsterBehaviorRuntime::Friend(Monster, *Source) || Power < Monster.NoiseMinPower ||
		Monster.Position().distance_to_sqr(Position) > DeathRadius * DeathRadius || Monster.EnemyMan.see_enemy_now())
	{
		return;
	}
	const u32 Now = Device.dwTimeGlobal;
	if (LastDeathSource == u32(Source->ID()) && Now - LastDeathTime < 5000)
	{
		return;
	}
	LastDeathSource = Source->ID();
	LastDeathTime = Now;
	if (Monster.EnemyMan.get_enemy() && Monster.Position().distance_to_sqr(Position) < 100.f)
	{
		return;
	}
	bool SeesBody = false;
	Fvector DeathPosition = Position;
	for (const auto& Body : MonsterBehaviorRuntime::World().Bodies)
	{
		if (Body.ID != u32(Source->ID()) || Device.dwTimeGlobal - Body.DeathTime > 5000)
		{
			continue;
		}
		SeesBody = std::find(Body.Witnesses, Body.Witnesses + Body.WitnessCount, u32(Monster.ID())) != Body.Witnesses + Body.WitnessCount;
		DeathPosition = Body.Position;
		break;
	}
	Fvector Direction;
	Direction.sub(Position, Monster.Position());
	const float Distance = Direction.magnitude();
	if (Distance < EPS_L)
	{
		return;
	}
	Direction.div(Distance);
	Fvector AlertPosition;
	AlertPosition.mad(Monster.Position(), Direction, std::min(Distance, DeathRadius * .5f));
	if (SeesBody)
	{
		AlertPosition = DeathPosition;
	}
	Attention = AlertPosition;
	AttentionUntil = Now + 1000;
	if (!MonsterBehaviorRuntime::Expired(ActionUntil) && Anchor.distance_to_sqr(AlertPosition) < _sqr(SearchRadius))
	{
		return;
	}
	Anchor = AlertPosition;
	ActionUntil = Device.dwTimeGlobal + u32(Random.randI(int(SearchMin), int(SearchMax) + 1));
	BodyID = u32(-1);
	if (SeesBody && BodiesEnabled)
	{
		MonsterBehaviorRuntime::KnowBody(Monster, Source->ID(), DeathPosition);
	}
	ActionMode = "DEATH ALERT SEARCH";
	Mode = "DEATH ALERT";
	NextPoint = 0;
}

void SMonsterBehavior::Update(CBaseMonster& Monster)
{
	using namespace MonsterBehaviorRuntime;
	if (!Enabled || Monster.MonsterPeaceful || !EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ||
		Monster.GetScriptControl() || Monster.EnemyMan.get_script_enemy() || Monster.control().is_captured_pure() ||
		(Monster.m_controlled && Monster.m_controlled->is_under_control()))
	{
		Moving = false;
		Monster.SetAttentionVisionObject(nullptr, 0);
		return;
	}
	const u32 Now = Device.dwTimeGlobal;
	if (Searching && Monster.EnemyMan.see_enemy_now())
	{
		Searching = false; Moving = false; SniffUntil = 0; SearchUntil = 0; NextUpdate = 0;
	}
	if (Monster.HasDamagePanic() || is_state(Monster.StateMan->get_state_type(), eStatePanic) || Monster.anim().m_tAction == ACT_ATTACK)
	{
		Moving = false;
		Mode = Monster.anim().m_tAction == ACT_ATTACK ? "ATTACK" : "RETREAT / PANIC";
		return;
	}
	if (s32(Now - NextUpdate) < 0 || !World().Updates)
	{
		Apply(Monster);
		return;
	}
	Moving = false;
	--World().Updates;
	CMonsterLogicTimerScope Cost(EMonsterLogicTimer::NewLogicTotal);
	NextUpdate = Now + 400 + u32(Monster.ID()) % 101;
	const CEntityAlive* Target = Monster.EnemyMan.get_enemy();
	const bool Sees = Target && Monster.EnemyMan.see_enemy_now(Target);
	if (Sees)
	{
		SArea* Fight = nullptr;
		const Fvector& Position = Monster.EnemyMan.GetTrackingPosition();
		for (auto& Entry : World().Fights)
		{
			if (!Expired(Entry.Until) && Entry.Pack == Pack(Monster) && Entry.Position.distance_to_sqr(Position) < _sqr(HazardRadius))
			{ Fight = &Entry; break; }
		}
		if (!Fight) Fight = &World().Fights[World().FightCursor++ % 32];
		*Fight = {Pack(Monster), Now + FightQuietTime, Now, Position, HazardRadius, Now + FightQuietTime};
	}
	for (auto& Area : World().Areas)
	{
		if (Sees && Area.Pack == Pack(Monster) && !Expired(Area.Until) &&
			Area.Position.distance_to_sqr(Monster.EnemyMan.GetTrackingPosition()) < _sqr(Area.Radius + 5.f))
		{
			Area.FightUntil = Now + FightQuietTime;
		}
	}
	if (AnomalyAvoidance && s32(Now - NextAnomalyScan) >= 0 && World().Queries)
	{
		--World().Queries;
		NextAnomalyScan = Now + 1000 + Monster.ID() % 201;
		AnomalyCount = 0;
		xr_vector<ISpatialShared> Fields;
		g_SpatialSpace->q_sphere(Fields, 0, ESPATIAL_TYPE::ANOMALY_ZONE, Monster.Position(), AnomalyScanRadius);
		for (const auto& Spatial : Fields)
		{
			CObject* Object = Spatial ? Spatial->dcast_CObject() : nullptr;
			CGameObject* GameObject = Object ? Object->cast_game_object() : nullptr;
			CAnomalyZone* Zone = GameObject ? GameObject->cast_anomaly_zone() : nullptr;
			if (!Zone || Zone->getDestroy() || !Zone->IsEnabled()) continue;
			if (AnomalyCount == 32) break;
			auto& Field = Anomalies[AnomalyCount++];
			Zone->Center(Field.Position);
			Field.Radius = Zone->Radius() + AnomalyMargin;
		}
	}
	if (!Target && Expired(AttentionUntil) &&
		(Expired(ActionUntil) || xr_strcmp(ActionMode, "NOISE SEARCH") != 0) &&
		(!Monster.NoiseInvestigationUntil || !Expired(Monster.NoiseInvestigationLookUntil)) &&
		!Moving && !Monster.HasDamagePanic() &&
		!is_state(Monster.StateMan->get_state_type(), eStatePanic))
	{
		for (u32 Attempt = 0; Attempt < 8; ++Attempt)
		{
			auto& Noise = NoiseAttention[NoiseCheckCursor++ % 8];
			if (Expired(Noise.Until) || !Expired(Noise.NextCheck) || Noise.LastCheckedEvent == Noise.LastInterest) continue;
			CObject* Source = Noise.Source != u32(-1) ? Level().Objects.net_Find(ALife::_OBJECT_ID(Noise.Source)) : nullptr;
			if (Monster.IsFriendlyNoiseSource(Source))
			{
				Noise = {};
				continue;
			}
			if (!(Noise.Explosion || (Noise.General && !Monster.ShotReaction.Attract) ? Monster.ExplosionReaction : Monster.ShotReaction).Attract) continue;
			Noise.NextCheck = Now + PeripheralInterval;
			Noise.LastCheckedEvent = Noise.LastInterest;
			// Hearing can turn an idle animal toward a sound behind it; this is not visual acquisition.
			Attention = Noise.Position; AttentionUntil = Now + std::max(1500u, PeripheralInterval + 500u);
			Monster.dir().face_target(Attention, 0);
			if (MonsterEnemySharingDebug().IsEnabled()) MonsterEnemySharingDebug().RecordNoiseReaction(Monster, Noise.Position, Noise.Power, false, Noise.Explosion, Monster.NoiseInvestigationUntil &&
				(Monster.NoiseInvestigationSource != u32(-1) ? Noise.Source == Monster.NoiseInvestigationSource : Noise.Position.distance_to_xz(Monster.NoiseInvestigationPosition) < 2.f), Noise.General, Noise.SoundType, Noise.ActorSource, Noise.Interest);

			CEntityAlive* Candidate = Source ? Source->cast_entity_alive() : nullptr;
			// Explosions and detached sound objects need a small source-centered candidate query.
			if (PeripheralEnabled && (!Candidate || !Candidate->g_Alive()) && World().Queries)
			{
				--World().Queries;
				xr_vector<ISpatialShared> Nearby;
				g_SpatialSpace->q_sphere(Nearby, 0, ESPATIAL_TYPE::COLLIDEABLE, Noise.Position, std::min(PeripheralRadius, 10.f));
				u32 Examined = 0;
				for (const auto& Spatial : Nearby)
				{
					if (++Examined > 16) break;
					CObject* Object = Spatial ? Spatial->dcast_CObject() : nullptr;
					CEntityAlive* Entity = Object ? Object->cast_entity_alive() : nullptr;
					if (Entity && Entity->g_Alive() && !Entity->getDestroy() && Monster.EnemyMan.is_enemy(Entity) && !Entity->m_bEntityIgnoredByMonsters) { Candidate = Entity; break; }
				}
			}
			if (PeripheralEnabled && Candidate && Candidate->g_Alive() && !Candidate->getDestroy() &&
				!Candidate->m_bEntityIgnoredByMonsters && Monster.EnemyMan.is_enemy(Candidate) &&
				Monster.Position().distance_to_sqr(Candidate->Position()) <= _sqr(Monster.eye_range))
			{
				for (auto& Focus : World().Focus)
				{
					if (Focus.ID == u32(Monster.ID()) || Expired(Focus.Until))
					{
						Attention = Candidate->Center(); AttentionUntil = Now + std::max(1500u, PeripheralInterval + 500u);
						Focus = {u32(Monster.ID()), AttentionUntil};
						Monster.SetAttentionVisionObject(Candidate, AttentionUntil, PeripheralAngle, Monster.eye_range);
						Monster.dir().face_target(Attention, 0);
						break;
					}
				}
			}
			NoiseLookUntil = AttentionUntil;
			break;
		}
	}
	if (!Target && Monster.NoiseInvestigationUntil)
	{
		// A committed sound approach preempts corpse and ambient search destinations.
		TargetID = u32(-1);
		Moving = false;
		Searching = false;
		Mode = "NOISE APPROACH";
		return;
	}
	if (!Target && Monster.HasPriorityNoise() && (Expired(ActionUntil) || xr_strcmp(ActionMode, "NOISE SEARCH") != 0))
	{
		// Potential threats preempt feeding and corpse investigations; confirmed prey still wins.
		Moving = false;
		Searching = false;
		Mode = "SOUND CHECK";
		// The direction controller owns this brief turn; look-around animation would fight it.
		Monster.set_action(ACT_STAND_IDLE);
		Apply(Monster);
		return;
	}
	if (AnomalyAvoidance && !Target)
	{
		const auto& NativeTravel = Monster.control().path_builder().detail().path();
		const u32 Start = Monster.control().path_builder().detail().curr_travel_point_index();
		for (u32 Index = Start; Index < NativeTravel.size() && Index < Start + 8; ++Index)
		{
			if (Avoids(Monster, NativeTravel[Index].position))
			{
				Anchor = Monster.Position(); ActionUntil = Now + 4000; NextPoint = 0;
				break;
			}
		}
	}
	// Native feeding and dragging own their movement and animation when there is no threat.
	if (!Target && is_state(Monster.StateMan->get_state_type(), eStateEat) && !Avoids(Monster, Monster.EatedCorpse ? Monster.EatedCorpse->Position() : Monster.CorpseMan.get_corpse() ? Monster.CorpseMan.get_corpse()->Position() : Monster.Position()))
	{
		BodyID = u32(-1); ActionUntil = 0; Moving = false;
		Mode = Monster.anim().m_tAction == ACT_DRAG ? "DRAG CORPSE" : "FEED / APPROACH CORPSE";
		return;
	}

	if (!Expired(HazardMoveUntil) && Monster.Position().distance_to_sqr(Goal) > 1.f && !CrossesDanger(Monster, Goal))
	{
		Moving = true; Searching = false; Running = Sees;
		TargetID = Target ? u32(Target->ID()) : u32(-1);
		Mode = "HAZARD / SAFE ARC";
		Apply(Monster); return;
	}
	HazardMoveUntil = 0;
	SParticipant* Own = nullptr;
	for (auto& Participant : World().Participants)
	{
		if (Participant.ID == u32(Monster.ID()))
		{
			Own = &Participant;
			break;
		}
		if (!Own && (Participant.ID == u32(-1) || Now - Participant.Time > 3000))
		{
			Own = &Participant;
		}
	}
	if (Own)
	{
		*Own = {u32(Monster.ID()), Target ? u32(Target->ID()) : u32(-1), Now, Pack(Monster), Monster.Position()};
	}
	if ((PeripheralEnabled || BodiesEnabled || HazardsEnabled) && !Sees && s32(Now - NextPeripheral) >= 0 && World().Queries && std::max(PeripheralRadius, BodyRadius) > 0.f)
	{
		--World().Queries;
		NextPeripheral = Now + PeripheralInterval + u32(Monster.ID()) % 201;
		xr_vector<ISpatialShared> Candidates;
		g_SpatialSpace->q_sphere(Candidates, 0, ESPATIAL_TYPE::COLLIDEABLE, Monster.Position(), std::max(PeripheralRadius, BodyRadius));
		u32 Examined = 0;
		const u32 CandidateCount = u32(Candidates.size());
		const u32 CandidateStart = CandidateCount ? PeripheralCursor % CandidateCount : 0;
		for (u32 CandidateIndex = 0; CandidateIndex < CandidateCount; ++CandidateIndex)
		{
			const auto& Spatial = Candidates[(CandidateStart + CandidateIndex) % CandidateCount];
			PeripheralCursor = CandidateStart + CandidateIndex + 1;
			if (++Examined > 64 || !World().Rays)
			{
				break;
			}
			CObject* Object = Spatial ? Spatial->dcast_CObject() : nullptr;
			CEntityAlive* Candidate = Object ? Object->cast_entity_alive() : nullptr;
			if (!Candidate || Candidate == &Monster || Candidate->getDestroy())
			{
				continue;
			}
			if (Candidate->g_Alive())
			{
				if (!PeripheralEnabled || !Monster.EnemyMan.is_enemy(Candidate) || Candidate->m_bEntityIgnoredByMonsters ||
					!Visible(Monster, *Candidate, PeripheralAngle, std::min(PeripheralRadius, Monster.eye_range)))
				{
					continue;
				}
				Attention = Candidate->Center();
				SFocus* Focus = nullptr;
				for (auto& Slot : World().Focus)
				{
					if (Slot.ID == u32(Monster.ID()) || Expired(Slot.Until))
					{
						Focus = &Slot;
						break;
					}
				}
				if (Focus)
				{
					AttentionUntil = Now + 800;
					*Focus = {u32(Monster.ID()), AttentionUntil};
					Monster.SetAttentionVisionObject(Candidate, AttentionUntil, PeripheralAngle, std::min(PeripheralRadius, Monster.eye_range));
				}
				break;
			}
			if (!Target && !Avoids(Monster, Candidate->Position()) && Visible(Monster, *Candidate, Monster.eye_fov, BodyRadius))
			{
				Monster.CorpseMemory.add_corpse(Candidate);
			}
			CBaseMonster* Body = Candidate->cast_base_monster();
			if ((!BodiesEnabled && !HazardsEnabled) || !Body || !Friend(Monster, *Body) || !Visible(Monster, *Body, Monster.eye_fov, BodyRadius))
			{
				continue;
			}
			SInvestigation* Investigation = KnowBody(Monster, Body->ID(), Body->Position());
			if (!BodiesEnabled || Target || BodyID != u32(-1) || !Expired(ActionUntil) || !Investigation || Investigation->Done || Avoids(Monster, Body->Position()))
			{
				continue;
			}
			if (Investigation->Until && Expired(Investigation->Until))
			{
				Investigation->Done = true;
				continue;
			}
			if (Investigation->Investigators >= InvestigatorLimit)
			{
				continue;
			}
			++Investigation->Investigators;
			if (!Investigation->Until)
			{
				Investigation->Until = Now + SearchMax;
			}
			Anchor = Body->Position();
			BodyID = Body->ID();
			ActionUntil = Investigation->Until;
			NextPoint = 0;
			Mode = "BODY INVESTIGATION";
			break;
		}
	}
	if (Monster.HasDamagePanic() || is_state(Monster.StateMan->get_state_type(), eStatePanic))
	{
		Mode = "RETREAT / PANIC";
		return;
	}
	if (Monster.anim().m_tAction == ACT_ATTACK)
	{
		Mode = "ATTACK";
		return;
	}
	Fvector Desired = Monster.Position();
	bool Move = false, Run = false, Search = false;
	if (Target)
	{
		const auto& Memory = Monster.EnemyMemory.get_memory();
		const auto Entry = Memory.find(Target);
		if (Entry == Memory.end() || !Monster.EnemyMemory.IsActual(Entry->second))
		{
			Mode = "WAIT";
			return;
		}
		if (TargetID != u32(Target->ID()) || (Sees && s32(Entry->second.time - ObservationTime) > 0))
		{
			if (TargetID != u32(Target->ID()) || Sees)
			{
				float RouteLength = Monster.Position().distance_to(Entry->second.position);
				const auto& Route = Monster.control().path_builder().detail().path();
				const u32 Start = Monster.control().path_builder().detail().curr_travel_point_index();
				if (Start < Route.size())
				{
					float Measured = 0.f;
					Fvector Previous = Monster.Position();
					for (u32 Index = Start; Index < Route.size() && Index < Start + 64; ++Index)
					{
						Measured += Previous.distance_to(Route[Index].position);
						Previous = Route[Index].position;
					}
					RouteLength = std::max(RouteLength, Measured);
				}
				ApproachUntil = Now + std::min(ApproachMax, CMonsterBehaviorPolicy::ApproachDuration(RouteLength, ApproachSpeed, ApproachMax));
				Phase = 0;
				NextPoint = 0;
				ProgressTime = Now;
				ProgressPosition = Monster.Position();
				GoalVertex = u32(-1);
			}
			TargetID = Target->ID();
			ObservationTime = Entry->second.time;
			SearchUntil = 0;
		}
		const Fvector Known = Monster.EnemyMan.GetNavigationPosition();
		Anchor = Entry->second.position;
		const float Distance = Monster.Position().distance_to(Known);
		if (ProgressTime == 0 || ProgressPosition.distance_to_sqr(Monster.Position()) > .5625f)
		{
			ProgressPosition = Monster.Position();
			ProgressTime = Now;
		}
		if (!Sees && !Monster.EnemyTrackingLive && !Entry->second.UnreachableSearchUntil && !Entry->second.ReachedPointTime &&
			!Expired(ApproachUntil) && Now - ProgressTime < Monster.EnemyStuckTime)
		{
			Monster.EnemyMemory.KeepApproach(Target, ApproachUntil);
		}
		Search = !Sees && !Monster.EnemyTrackingLive && !Monster.HasCloseSightCombat(Target) &&
				 (Entry->second.ReachedPointTime || (!Monster.PursuitSearchingAround && Monster.HasReachedEnemyPoint(Anchor, Entry->second.vertex)));
		if (Search)
		{
			if (!SearchUntil)
			{
				Phase = 0;
				NextPoint = 0;
				GoalVertex = u32(-1);
			}
			if (!SearchUntil)
			{
				SearchUntil = Entry->second.ForgetTime;
			}
			Mode = Sniff && Monster.anim().SearchInspectionAvailable ? "TRAIL SEARCH / INSPECT" : "SECTOR SEARCH";
			Run = false;
		}
		else if (Monster.PursuitSearchingAround)
		{
			Mode = Sees ? "PRESSURE / BLOCKED" : "LOCAL SEARCH";
			Moving = ai().level_graph().valid_vertex_id(Monster.UnreachableSearchVertex);
			if (Moving)
			{
				Goal = Monster.UnreachableSearchPoint;
				GoalVertex = Monster.UnreachableSearchVertex;
			}
			Running = Sees;
			Searching = !Sees;
			return;
		}
		else
		{
			Run = Sees || Monster.EnemyTrackingLive || !Monster.ShouldApproachHiddenEnemy();
			Mode = Monster.EnemyTrackingLive ? "PSI PURSUIT" : Sees ? "PURSUIT"
																	: "LAST KNOWN APPROACH";
			if (Monster.HasReachedEnemyPoint(Known, Entry->second.vertex) && !Avoids(Monster, Known))
			{
				return;
			}
			u32 Nearby = 0, AttackRank = 0;
			Fvector Separation = {0.f, 0.f, 0.f};
			u32 SeparationCount = 0;
			for (const auto& Peer : World().Participants)
			{
				if (Peer.ID == u32(Monster.ID()) || Peer.Pack != Pack(Monster) || Now - Peer.Time > 3000)
				{
					continue;
				}
				if (Peer.Target == TargetID && Peer.Position.distance_to_sqr(Known) < 100.f)
				{
					++Nearby;
					const float PeerDistance = Peer.Position.distance_to_sqr(Known);
					if (PeerDistance < Distance * Distance || (std::abs(PeerDistance - Distance * Distance) < .1f && Peer.ID < u32(Monster.ID())))
					{
						++AttackRank;
					}
				}
				const float Gap = Peer.Position.distance_to_sqr(Monster.Position());
				if (Gap > .01f && Gap < 9.f && SeparationCount < 6)
				{
					Fvector Away;
					Away.sub(Monster.Position(), Peer.Position);
					Away.div(std::max(1.f, Gap));
					Separation.add(Away);
					++SeparationCount;
				}
			}
			if (Expired(NextPoint))
			{
				const u32 Choices[] = {Direct, Flank, Wide, Cautious, Lunge, Charge, Encircle, Pressure, Retreat};
				u32 Allowed[9], Count = 0;
				for (u32 Choice : Choices)
				{
					if (Strategies & Choice)
					{
						Allowed[Count++] = Choice;
					}
				}
				Strategy = Allowed[(u32(Monster.ID()) + Phase++) % Count];
				if (Sees && Distance > 15.f && (Strategies & Charge))
				{
					Strategy = Charge;
				}
				if (Sees && Distance < 7.f && (Strategies & Lunge))
				{
					Strategy = Lunge;
				}
				if (Monster.GetfHealth() < .35f && Nearby == 0 && (Strategies & Retreat))
				{
					Strategy = Retreat;
				}
				if (Nearby >= 3 && (Strategies & Encircle))
				{
					Strategy = AttackRank < 3 ? Direct : Encircle;
				}
				if (!Sees && !Monster.EnemyTrackingLive && (Strategies & Direct))
				{
					Strategy = Direct;
				}
				if (!Sees && Monster.ShouldApproachHiddenEnemy() && (Strategies & Cautious))
				{
					Strategy = Cautious;
				}
				NextPoint = Now + (Strategy == Charge ? 2200 : 1400 + u32(Monster.ID()) % 1200);
				Fvector Forward;
				Forward.sub(Monster.Position(), Known);
				Forward.y = 0.f;
				if (Forward.magnitude() > EPS_L)
				{
					Forward.normalize();
				}
				else
				{
					Forward.set(1.f, 0.f, 0.f);
				}
				const float Side = (Monster.ID() & 1) ? 1.f : -1.f;
				Fvector Lateral = {Forward.z * Side, 0.f, -Forward.x * Side};
				const float Width = std::min(ApproachWidth, Distance * .45f);
				Desired = Known;
				if (Strategy == Flank || Strategy == Wide || Strategy == Encircle || Strategy == Pressure)
				{
					Desired.mad(Lateral, Width * (Strategy == Wide ? 1.6f : 1.f));
					if (Nearby >= 3)
					{
						Desired.mad(Forward, 3.f + float(Monster.ID() % 4));
					}
					Mode = Strategy == Wide ? "WIDE ARC" : Strategy == Encircle ? "ENCIRCLE"
													   : Strategy == Pressure	? "PRESSURE / ARC"
																				: "FLANK";
				}
				else if (Strategy == Lunge && Distance < 7.f && (Phase % 3) == 0)
				{
					Desired.mad(Forward, 3.f);
					Desired.mad(Lateral, 2.f);
					Mode = "LUNGE / REPOSITION";
				}
				else if (Strategy == Retreat && Nearby == 0 && Monster.GetfHealth() < .35f)
				{
					Desired.mad(Forward, 8.f);
					Mode = "RETREAT / REGROUP";
					float Nearest = 625.f;
					for (const auto& Peer : World().Participants)
					{
						const float Gap = Peer.Position.distance_to_sqr(Monster.Position());
						if (Peer.ID != u32(Monster.ID()) && Peer.Pack == Pack(Monster) && Now - Peer.Time < 3000 && Gap < Nearest && Peer.Position.distance_to_sqr(Known) > 16.f)
						{
							Desired = Peer.Position;
							Nearest = Gap;
						}
					}
				}
				else
				{
					Mode = Strategy == Charge ? "CHARGE" : Strategy == Cautious ? "CAUTIOUS"
																				: "DIRECT APPROACH";
				}
				if (!Sees && !Monster.EnemyTrackingLive)
				{
					Desired = Known;
					Mode = "LAST KNOWN APPROACH";
				}
				DecisionMode = Mode;
				if (Sees || Monster.EnemyTrackingLive) Desired.add(Separation);
				// Charge holds a line; other approaches keep a small individual lateral preference.
				if (Sees && Strategy == Direct && Nearby > 0)
				{
					Desired.mad(Lateral, std::min(Width, 1.5f));
				}
				if (World().Nodes)
				{
					GoalVertex = u32(-1);
				}
				for (u32 Attempt = 0; Attempt < ((!Sees && !Monster.EnemyTrackingLive) ? 1u : 6u); ++Attempt)
				{
					Fvector Candidate = Desired;
					if (Attempt)
					{
						Candidate.x += cosf(float(Attempt) * 1.3f) * float(Attempt);
						Candidate.z += sinf(float(Attempt) * 1.3f) * float(Attempt);
					}
					if (ValidGoalPosition(Monster, Candidate, GoalVertex))
					{
						Goal = Candidate;
						Move = true;
						break;
					}
				}
			}
			else
			{
				Move = ai().level_graph().valid_vertex_id(GoalVertex);
				Mode = DecisionMode;
			}
			if (Strategy == Cautious && !Sees && !Monster.EnemyTrackingLive)
			{
				Run = false;
			}
		}
	}
	else
	{
		TargetID = u32(-1);
		SearchUntil = 0;
		if (!Expired(ActionUntil) && !Avoids(Monster, Anchor))
		{
			Search = true;
			Mode = BodyID != u32(-1) ? "BODY INVESTIGATION" : ActionMode;
		}
		else
		{
			BodyID = u32(-1);
			Mode = Monster.NoiseInvestigationUntil ? "NOISE APPROACH" : !Expired(AttentionUntil) ? "SOUND CHECK" : "WANDER / NATIVE";
			return;
		}
	}
	if (Search)
	{
		if (s32(Now - NextPoint) >= 0 || Monster.Position().distance_to_sqr(Goal) < 1.f)
		{
			Fvector Heading = {0.f, 0.f, 0.f};
			bool HasHeading = false;
			if (Target && TrailEnabled)
			{
				const auto Entry = Monster.EnemyMemory.get_memory().find(Target);
				if (Entry != Monster.EnemyMemory.get_memory().end() && Entry->second.HasTrailHeading)
				{
					Heading = Entry->second.TrailHeading;
					HasHeading = true;
				}
			}
			const float BaseAngle = atan2f(Heading.z, Heading.x);
			const float Sector = deg2rad(SearchAngle);
			for (u32 Attempt = 0; Attempt < 6; ++Attempt)
			{
				const u32 Step = Phase++ + u32(Monster.ID()) % 7;
				const float Angle = BaseAngle + (HasHeading && Step < 7 ? (float(Step % 7) / 6.f - .5f) * Sector : float(Step) * 2.399963f);
				const float Radius = SearchRadius * (.35f + .65f * float((Step * 3) % 7) / 6.f);
				Fvector Candidate = Anchor;
				Candidate.x += cosf(Angle) * Radius;
				Candidate.z += sinf(Angle) * Radius;
				bool Visited = false;
				for (u32 Index = 0; Index < 8; ++Index)
				{
					if (RecentPointTimes[Index] && Now - RecentPointTimes[Index] < SearchMax && RecentPoints[Index].distance_to_sqr(Candidate) < 2.25f)
					{
						Visited = true;
						break;
					}
				}
				if (!Visited && ValidGoalPosition(Monster, Candidate, GoalVertex))
				{
					Goal = Candidate;
					Move = true;
					const u32 Index = RecentPointCursor++ % 8;
					RecentPoints[Index] = Goal;
					RecentPointTimes[Index] = Now;
					break;
				}
			}
			NextPoint = Now + 2000 + u32(Monster.ID()) % 1000;
			if (Expired(AttentionUntil) && Move)
			{
				Attention = Goal;
				AttentionUntil = Now + 700;
				// Deliberate search turns use the native direction controller; running maneuvers keep their heading.
				Monster.dir().face_target(Attention, 0);
			}
		}
		else
		{
			Move = ai().level_graph().valid_vertex_id(GoalVertex);
		}
	}
	if (Move || AnomalyAvoidance)
	{
		const auto& Travel = Monster.control().path_builder().detail().path();
		const u32 Start = Monster.control().path_builder().detail().curr_travel_point_index();
		for (u32 Index = Start; Index < Travel.size() && Index < Start + 8; ++Index)
		{
			if (CrossesDanger(Monster, Travel[Index].position))
			{
				Move = false;
				Anchor = Travel[Index].position;
				break;
			}
		}
	}
	if (Move && !CrossesDanger(Monster, Goal))
	{
		Moving = true;
		Running = Run;
		Searching = Search;
		Monster.path().set_target_point(Goal, GoalVertex);
		Monster.path().set_rebuild_time(1000);
		Monster.path().set_distance_to_end(.7f);
		Monster.path().set_use_covers(false);
		Monster.path().extrapolate_path(false);
		Monster.set_action(Run ? ACT_RUN : ACT_WALK_FWD);
		if (Run && Strategy == Charge)
		{
			Monster.anim().accel_activate(eAT_Aggressive);
		}
		if (Search && Sniff && Monster.anim().SearchInspectionAvailable && Expired(NextSniff))
		{
			SniffUntil = Now + Monster.anim().SearchInspectionDuration;
			NextSniff = SniffUntil + 3500 + Monster.ID() % 1500;
		}
		if (Search && Sniff && Monster.anim().SearchInspectionAvailable && !Expired(SniffUntil))
		{
			Monster.set_action(Monster.anim().SearchInspectionAction);
		}
		Monster.set_state_sound(Search ? MonsterSound::eMonsterSoundIdle : MonsterSound::eMonsterSoundAggressive);
	}
	else if (Avoids(Monster, Anchor) || (Target && CrossesDanger(Monster, Monster.EnemyMan.GetTrackingPosition())))
	{
		Mode = AnomalyAvoidance ? "HAZARD / SAFE ARC" : "AVOID DEATH ZONE";
		// A temporary hazardous area does not make the enemy unreachable on the AI graph.
		Fvector Away;
		Away.sub(Monster.Position(), Anchor);
		Away.y = 0.f;
		if (Away.magnitude() > EPS_L)
		{
			Away.normalize();
		}
		else
		{
			Away.set(1.f, 0.f, 0.f);
		}
		bool Escape = false;
		Fvector Candidate = Monster.Position();
		for (u32 Attempt = 0; Attempt < 6; ++Attempt)
		{
			Candidate = Monster.Position();
			// Alternate side steps with outward movement instead of repeatedly facing away.
			Fvector Step = Away;
			if (Attempt < 4)
			{
				const float Side = ((Monster.ID() + Attempt) % 2) ? 1.f : -1.f;
				Step.set(Away.x * .4f - Away.z * Side, 0.f, Away.z * .4f + Away.x * Side);
				Step.normalize();
			}
			Candidate.mad(Step, 4.f + float(Attempt) * std::max(2.f, HazardRadius * .4f));
			if (ValidGoalPosition(Monster, Candidate, GoalVertex))
			{
				Escape = true;
				break;
			}
		}
		if (Escape)
		{
			Goal = Candidate;
			Moving = true;
			Running = Sees;
			Searching = false;
			Monster.path().set_target_point(Goal, GoalVertex);
			Monster.set_action(Sees ? ACT_RUN : ACT_WALK_FWD);
			NextPoint = Now + 1500;
			HazardMoveUntil = Now + 1500;
			if (!Target) ActionUntil = HazardMoveUntil;
		}
		else
		{
			Moving = false;
			Monster.set_action(ACT_LOOK_AROUND);
		}
	}
}

void SMonsterBehavior::Apply(CBaseMonster& Monster)
{
	const CEntityAlive* Target = Monster.EnemyMan.get_enemy();
	// Hold an idle sound turn between budgeted decisions; never redirect combat or external control.
	if (!Target && !Moving &&
		(!Monster.NoiseInvestigationUntil || !MonsterBehaviorRuntime::Expired(Monster.NoiseInvestigationLookUntil)) &&
		!MonsterBehaviorRuntime::Expired(AttentionUntil) && !Monster.HasDamagePanic() &&
		!Monster.GetScriptControl() && !Monster.EnemyMan.get_script_enemy() && !Monster.control().is_captured_pure() &&
		!(Monster.m_controlled && Monster.m_controlled->is_under_control()) &&
		!is_state(Monster.StateMan->get_state_type(), eStatePanic) && !is_state(Monster.StateMan->get_state_type(), eStateEat))
	{
		if (!MonsterBehaviorRuntime::Expired(NoiseLookUntil)) Monster.set_action(ACT_STAND_IDLE);
		Monster.dir().face_target(Attention, 0);
	}
	if ((!Monster.EnemyMan.get_enemy() && is_state(Monster.StateMan->get_state_type(), eStateEat)) || !Moving || !ai().level_graph().valid_vertex_id(GoalVertex) || Monster.anim().m_tAction == ACT_ATTACK ||
		Monster.HasDamagePanic() || is_state(Monster.StateMan->get_state_type(), eStatePanic))
	{
		return;
	}
	if (TargetID != u32(-1) && (!Target || u32(Target->ID()) != TargetID || !Target->g_Alive()))
	{
		return;
	}
	if (TargetID == u32(-1) && (Target || MonsterBehaviorRuntime::Expired(ActionUntil)))
	{
		return;
	}
	if (Searching && Target && Monster.EnemyMan.see_enemy_now(Target))
	{
		Moving = false;
		SniffUntil = 0;
		NextUpdate = 0;
		return;
	}
	if (AnomalyAvoidance)
	{
		const auto& Travel = Monster.control().path_builder().detail().path();
		const u32 Start = Monster.control().path_builder().detail().curr_travel_point_index();
		for (u32 Index = Start; Index < Travel.size() && Index < Start + 4; ++Index)
		{
			if (CrossesDanger(Monster, Travel[Index].position))
			{
				Moving = false; HazardMoveUntil = 0; NextUpdate = 0;
				Monster.set_action(ACT_LOOK_AROUND);
				return;
			}
		}
	}
	Monster.path().set_target_point(Goal, GoalVertex);
	Monster.path().set_rebuild_time(1000);
	Monster.path().set_distance_to_end(.7f);
	Monster.path().set_use_covers(false);
	Monster.path().set_try_min_time(false);
	Monster.path().extrapolate_path(false);
	if (Running && Strategy == Charge)
	{
		Monster.anim().accel_activate(eAT_Aggressive);
	}
	Monster.set_action(Running ? ACT_RUN : Searching && Sniff && Monster.anim().SearchInspectionAvailable && !MonsterBehaviorRuntime::Expired(SniffUntil) ? Monster.anim().SearchInspectionAction
																											  : ACT_WALK_FWD);
}

void SMonsterBehavior::RecordDebug(CBaseMonster& Monster)
{
	if (!Enabled || !MonsterEnemySharingDebug().IsEnabled())
	{
		return;
	}
	auto& Debug = MonsterEnemySharingDebug();
	Debug.RecordBehavior(Monster);
	const u32 Pack = MonsterBehaviorRuntime::Pack(Monster);
	const auto& State = MonsterBehaviorRuntime::World();
	for (const auto& Area : State.Areas)
	{
		if (Area.Pack == Pack && !MonsterBehaviorRuntime::Expired(Area.Until))
		{
			Debug.RecordZone(Pack, Area.EvidenceTime | 0x80000000u, Area.Position, Area.Radius, Area.Until, MonsterBehaviorRuntime::Expired(Area.FightUntil) ? "DANGER" : "FIGHT");
		}
	}
	for (u32 Index = 0; Index < 32; ++Index)
	{
		const auto& Fight = State.Fights[Index];
		if (Fight.Pack == Pack && !MonsterBehaviorRuntime::Expired(Fight.Until))
		{
			Debug.RecordZone(Pack, 0x20000000u | Index, Fight.Position, Fight.Radius, Fight.Until, "FIGHT");
		}
	}
	if (AnomalyAvoidance)
	{
		for (u32 Index = 0; Index < AnomalyCount; ++Index)
		{
			Debug.RecordZone(Pack, 0x10000000u | (u32(Monster.ID()) << 5) | Index, Anomalies[Index].Position, Anomalies[Index].Radius, Device.dwTimeGlobal + 1500, "ANOMALY");
		}
	}
	for (const auto& Body : State.Investigations)
	{
		if (Body.Pack == Pack && Body.Body != u32(-1) && Device.dwTimeGlobal - Body.DeathTime < BodyFreshTime)
		{
			Debug.RecordZone(Pack, Body.Body | 0x40000000u, Body.Position, SearchRadius, Body.DeathTime + BodyFreshTime, Body.Done ? "BODY DONE" : "BODY KNOWN");
		}
	}
	if (!MonsterBehaviorRuntime::Expired(ActionUntil))
	{
		Debug.RecordZone(Pack, Monster.ID(), Anchor, SearchRadius, ActionUntil, BodyID == u32(-1) ? ActionMode : "INVESTIGATION");
	}
}

bool SMonsterBehavior::TakeNavigationSample()
{
	auto& State = MonsterBehaviorRuntime::World();
	if (!State.Nodes)
	{
		return false;
	}
	--State.Nodes;
	return true;
}

bool SMonsterBehavior::HasAreas(const CBaseMonster& Monster) const
{
	if (!Enabled || !HazardsEnabled)
	{
		return false;
	}
	for (const auto& Area : MonsterBehaviorRuntime::World().Areas)
	{
		if (Area.Pack == MonsterBehaviorRuntime::Pack(Monster) && !MonsterBehaviorRuntime::Expired(Area.Until))
		{
			return true;
		}
	}
	return false;
}

bool SMonsterBehavior::TakeSpatialQuery()
{
	auto& State = MonsterBehaviorRuntime::World();
	if (!State.Queries)
	{
		return false;
	}
	--State.Queries;
	return true;
}
