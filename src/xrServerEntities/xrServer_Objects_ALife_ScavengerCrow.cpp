#include "StdAfx.h"
#include "xrServer_Objects_ALife_ScavengerCrow.h"
#include "../xrCore/Save/SaveInterface.h"

void CSE_ALifeScavengerCrow::STATE_Serialize(ISaveObject& Object)
{
	CSE_ALifeMonsterBase::STATE_Serialize(Object);
	// An optional named chunk preserves old .scop saves and does not change
	// STATE_Read/Write or the client net_save/net_load layout.
	if (!Object.IsSave() && !Object.HasChunk("ScavengerCrowOffline")) return;
	BEGIN_CHUNK(Object, "ScavengerCrowOffline")
	{
		u8 Version=2;
		Object << Version;
		R_ASSERT2(Version==1 || Version==2, "Unsupported crow offline save version");
		u8 Rest=u8(State.Rest);
		Object << State.Valid << State.UpdatedOffline << State.Enabled;
		Object << State.Satiety << State.Fear << State.Remaining << State.Altitude;
		Object << Rest;
		if (!Object.IsSave() && Version == 1)
		{
			u16 LegacyCorpse = u16(-1);
			Object << LegacyCorpse;
			State.Corpse = LegacyCorpse == u16(-1) ? u32(-1) : u32(LegacyCorpse);
		}
		else Object << State.Corpse;
		Object << State.Time;
		Object << State.Danger.x << State.Danger.y << State.Danger.z;
		Object << State.Rain.Observed << State.Rain.Active << State.Rain.Remaining;
		Object << State.Night.Observed << State.Night.Active << State.Night.Remaining;
		if (!Object.IsSave())
		{
			R_ASSERT2(Rest<=u8(ERest::Night) && _valid(State.Satiety) && State.Satiety>=0.f && State.Satiety<=1.f &&
				_valid(State.Fear) && State.Fear>=0.f && State.Fear<=1.f && _valid(State.Remaining) &&
				_valid(State.Altitude) && State.Altitude>0.f && _valid(State.Danger) &&
				_valid(State.Rain.Remaining) && State.Rain.Remaining>=0.f &&
				_valid(State.Night.Remaining) && State.Night.Remaining>=0.f, "Invalid crow offline save state");
			State.Rest=ERest(Rest);
			State.NeedsPlacement=State.Valid;
			ConfigLoaded=false;
			WeatherRain=State.Rain.Observed;
		}
	}
}

#ifdef XRGAME_EXPORTS
#include "alife_monster_brain.h"
#include "../xrGame/ai_space.h"
#include "../xrGame/game_graph.h"
#include "../xrGame/level_graph.h"
#include "../xrGame/alife_simulator.h"
#include "../xrGame/alife_time_manager.h"
#include "../xrGame/alife_graph_registry.h"
#include "../xrGame/alife_monster_movement_manager.h"
#include "../xrGame/alife_monster_detail_path_manager.h"
#include "../xrGame/GamePersistent.h"
#include "../xrEngine/Environment.h"
#include "clsid_game.h"

void CSE_ALifeScavengerCrow::add_offline(const xr_vector<ALife::_OBJECT_ID>& SavedChildren, const bool& UpdateRegistries)
{
	CSE_ALifeMonsterBase::add_offline(SavedChildren, UpdateRegistries);
	State.NeedsPlacement=true;
}

void CSE_ALifeScavengerCrow::LoadOfflineConfig()
{
	const char* Section = s_name.c_str();
	Sky.load(Section,"crow_sky_time",20.f,50.f);
	Ground.load(Section,"crow_ground_time",6.f,14.f);
	Roof.load(Section,"crow_roof_time",10.f,30.f);
	Corpse.load(Section,"crow_corpse_time",60.f,180.f);
	Escape.load(Section,"crow_escape_time",12.f,25.f);
	NightRest.load(Section,"crow_night_rest_time",30.f,90.f);
	NightDelay.load(Section,"crow_night_reaction_delay",5.f,120.f);
	WakeDelay.load(Section,"crow_day_reaction_delay",5.f,120.f);
	RainDelay.load(Section,"crow_rain_reaction_delay",5.f,90.f);
	DryDelay.load(Section,"crow_shelter_dry_delay",5.f,120.f);
	Tick.load(Section,"crow_offline_update_interval",1.f,3.f);
	R_ASSERT2(Tick.low > 0.f, "Invalid crow offline update interval");
#define CROW_OFFLINE_FLOAT(Field, Key, Default) Field = READ_IF_EXISTS(pSettings,r_float,Section,Key,Default)
	CROW_OFFLINE_FLOAT(HungerTime,"crow_hunger_time",900.f);
	CROW_OFFLINE_FLOAT(Hungry,"crow_hungry_threshold",.35f);
	CROW_OFFLINE_FLOAT(FearRadius,"crow_fear_radius",10.f);
	CROW_OFFLINE_FLOAT(FearDecay,"crow_fear_decay_speed",.2f);
	CROW_OFFLINE_FLOAT(FearCalm,"crow_fear_calm_threshold",.1f);
	CROW_OFFLINE_FLOAT(NightStart,"crow_night_start_hour",20.f);
	CROW_OFFLINE_FLOAT(NightEnd,"crow_night_end_hour",6.f);
	CROW_OFFLINE_FLOAT(RainStart,"crow_rain_start_threshold",.1f);
	CROW_OFFLINE_FLOAT(RainStop,"crow_rain_stop_threshold",.03f);
	CROW_OFFLINE_FLOAT(AltitudeMin,"crow_sky_height_min",12.f);
	CROW_OFFLINE_FLOAT(AltitudeMax,"crow_sky_height_max",25.f);
	CROW_OFFLINE_FLOAT(SeedGain,"crow_seed_gain_per_peck",.002f);
	CROW_OFFLINE_FLOAT(MeatGain,"crow_meat_gain_per_peck",.07f);
	CROW_OFFLINE_FLOAT(FlightSpeed,"flight_speed",6.f);
	CROW_OFFLINE_FLOAT(FoodRadius,"crow_corpse_search_radius",70.f);
#undef CROW_OFFLINE_FLOAT
	NightEnabled=READ_IF_EXISTS(pSettings,r_bool,Section,"crow_night_enabled",true);
	RainEnabled=READ_IF_EXISTS(pSettings,r_bool,Section,"crow_rain_shelter_enabled",true);
	CrowBehaviorTiming::SCrowRange SeedPeck, MeatPeck;
	SeedPeck.load(Section,"crow_peck_interval",.5f,1.1f);
	MeatPeck.load(Section,"crow_corpse_peck_interval",.8f,1.6f);
	SeedGain/=std::max(.01f,(SeedPeck.low+SeedPeck.high)*.5f);
	MeatGain/=std::max(.01f,(MeatPeck.low+MeatPeck.high)*.5f);
	R_ASSERT2(_valid(HungerTime) && HungerTime>0.f && _valid(FlightSpeed) && FlightSpeed>0.f &&
		_valid(AltitudeMin) && _valid(AltitudeMax) && AltitudeMin>0.f && AltitudeMax>=AltitudeMin,
		"Invalid crow offline configuration");
	if (!State.Valid)
	{
		State.Enabled=READ_IF_EXISTS(pSettings,r_bool,Section,"crow_ai_enabled",false);
		CrowBehaviorTiming::SCrowRange Initial;
		Initial.load(Section,"crow_initial_satiety",.25f,.9f);
		State.Satiety=Initial.sample();
		State.Remaining=Sky.sample();
		State.Altitude=Random.randF(AltitudeMin,AltitudeMax);
		State.Valid=true;
	}
	WeatherRain=State.Rain.Observed;
	m_fCurrentLevelGoingSpeed=FlightSpeed;
	m_fGoingSpeed=FlightSpeed;
	// Ground-monster terrain masks do not restrict symbolic offline air travel.
	m_tpaTerrain.clear();
	Interval=Tick.sample();
	ConfigLoaded=true;
}

void CSE_ALifeScavengerCrow::ChooseOfflineGoal()
{
	const auto& Graph=ai().game_graph();
	if (!Graph.valid_vertex_id(m_tGraphID)) return;
	const auto LevelID=Graph.vertex(m_tGraphID)->level_id();
	GameGraph::_GRAPH_ID Goal=m_tGraphID;
	// A bounded random walk explores this level without scanning the whole map.
	for (u32 Step=0; Step<4; ++Step)
	{
		IGameGraph::const_iterator Begin, End;
		Graph.begin(Goal,Begin,End);
		u32 Count=0;
		GameGraph::_GRAPH_ID Next=Goal;
		float BestDistance=Graph.vertex(Goal)->level_point().distance_to_sqr(State.Danger);
		for (auto Edge=Begin; Edge!=End; ++Edge)
		{
			const auto Vertex=Edge->vertex_id();
			if (!Graph.accessible(Vertex) || Graph.vertex(Vertex)->level_id()!=LevelID) continue;
			++Count;
			if (State.Fear>FearCalm)
			{
				const float Distance=Graph.vertex(Vertex)->level_point().distance_to_sqr(State.Danger);
				if (Distance>BestDistance) { BestDistance=Distance; Next=Vertex; }
			}
			else if (Random.randI(0,int(Count))==0) Next=Vertex;
		}
		Goal=Next;
	}
	auto& Movement=brain().movement();
	Movement.path_type(MovementManager::ePathTypeGamePath);
	Movement.detail().target(Goal);
}

void CSE_ALifeScavengerCrow::update()
{
	if (m_bOnline || !bfActive()) return;
	if (!ConfigLoaded) LoadOfflineConfig();
	const auto Now=ai().alife().time_manager().game_time();
	if (!State.Time || Now<State.Time) { State.Time=Now; return; }
	const float Dt=float(Now-State.Time)/1000.f/
		std::max(EPS_L,ai().alife().time_manager().normal_time_factor());
	if (Dt<Interval) return;
	State.Time=Now;
	Interval=Tick.sample();
	if (!State.Enabled) return;
	const bool FirstOfflineTick=!State.UpdatedOffline;
	State.UpdatedOffline=true;
	State.Satiety=std::max(0.f,State.Satiety-Dt/HungerTime);
	const bool WasAfraid=State.Fear>FearCalm;
	State.Fear=std::max(0.f,State.Fear-FearDecay*Dt);
	State.Remaining-=Dt;
	float Hour=float((Now/1000u)%86400u)/3600.f;
	const auto& Graph=ai().game_graph();
	if (!Graph.valid_vertex_id(m_tGraphID)) return;
	if (FirstOfflineTick)
	{
		if (State.Rest==ERest::None) o_Position=Graph.vertex(m_tGraphID)->level_point();
		// The native detail manager was idle while online; reset its elapsed-time
		// baseline at the current vertex before assigning a new offline route.
		auto& Detail=brain().movement().detail();
		Detail.target(m_tGraphID);
		Detail.make_inactual();
		Detail.update();
		brain().movement().path_type(MovementManager::ePathTypeNoPath);
	}
	const bool CurrentLevel=ai().get_level_graph() && Graph.vertex(m_tGraphID)->level_id()==ai().level_graph().level_id();
	// Geometry/weather on unloaded levels is not available. Rain is only sampled
	// for this level; night is derived from ALife time on every level.
	if (CurrentLevel && g_pGamePersistent && g_pGamePersistent->Environment().CurrentEnv)
	{
		Hour=g_pGamePersistent->Environment().GetGameTime()/3600.f;
		const float Density=g_pGamePersistent->Environment().CurrentEnv->rain_density;
		WeatherRain=RainEnabled && (WeatherRain ? Density>RainStop : Density>RainStart);
	}
	const bool IsNight=NightEnabled && (NightStart>NightEnd ?
		(Hour>=NightStart || Hour<NightEnd) : (Hour>=NightStart && Hour<NightEnd));
	const ERest PreviousRest=State.Rest;
	State.Rain.Update(WeatherRain,Dt,RainDelay,DryDelay);
	State.Night.Update(IsNight,Dt,NightDelay,WakeDelay);
	CSE_ALifeCreatureAbstract* Food=nullptr;
	float FoodDistance=_sqr(FoodRadius);
	bool Threatened=false;
	const auto& Points=ai().alife().graph().objects();
	if (m_tGraphID<Points.size())
	{
		u32 Checked=0;
		for (const auto& Entry : Points[m_tGraphID].objects().objects())
		{
			if (++Checked>64u) break;
			auto* Creature=Entry.second->cast_creature_abstract();
			if (!Creature || Creature==this || Creature->m_tClassID==CLSID_AI_SCAVENGER_CROW ||
				Creature->m_tClassID==CLSID_AI_CROW) continue;
			Fvector BirdPosition=o_Position;
			if (State.Rest==ERest::None) BirdPosition.y+=State.Altitude;
			const float Distance=Creature->o_Position.distance_to_sqr(BirdPosition);
			if (Creature->g_Alive() && Distance<_sqr(FearRadius))
			{
				Threatened=true;
				State.Danger=Creature->o_Position;
			}
			if (!Creature->g_Alive() && Distance<FoodDistance)
			{
				Food=Creature;
				FoodDistance=Distance;
			}
		}
	}
	if (Threatened)
	{
		State.Fear=1.f;
		State.Remaining=Escape.sample();
	}
	const ERest Required=State.Fear>FearCalm ? ERest::None : State.Rain.Active ? ERest::Shelter :
		State.Night.Active ? ERest::Night : ERest::None;
	if (Required!=ERest::None)
	{
		State.Rest=Required;
	}
	else if (State.Fear>FearCalm || State.Rest==ERest::Night || State.Rest==ERest::Shelter)
	{
		if (State.Rest!=ERest::None || !WasAfraid)
		{
			State.Rest=ERest::None;
			State.Remaining=State.Fear>FearCalm ? Escape.sample() : Sky.sample();
			ChooseOfflineGoal();
		}
	}
	else if (State.Remaining<=0.f)
	{
		if (State.Rest!=ERest::None)
		{
			State.Rest=ERest::None;
			State.Remaining=Sky.sample();
			ChooseOfflineGoal();
		}
		else if (State.Satiety<=Hungry && Food)
		{
			State.Rest=ERest::Corpse; State.Corpse=Food->ID;
			State.Remaining=Corpse.sample();
		}
		else if (State.Satiety>Hungry)
		{
			State.Rest=Random.randF()<.6f ? ERest::Ground : ERest::Roof;
			State.Remaining=State.Rest==ERest::Ground ? Ground.sample() : Roof.sample();
		}
		else { State.Remaining=Sky.sample(); ChooseOfflineGoal(); }
	}
	if (PreviousRest==State.Rest && (State.Rest==ERest::Ground ||
		(State.Rest==ERest::Corpse && Food && Food->ID==State.Corpse)))
		State.Satiety=std::min(1.f,State.Satiety+Dt*(State.Rest==ERest::Corpse ? MeatGain : SeedGain));
	if (State.Rest==ERest::None)
	{
		auto& Movement=brain().movement();
		if (Movement.path_type()!=MovementManager::ePathTypeGamePath || Movement.detail().completed()) ChooseOfflineGoal();
		Movement.update();
		o_Position=Movement.detail().draw_level_position();
		State.Altitude=Random.randF(AltitudeMin,AltitudeMax);
		// Keep graph positions on their native anchor plane. Online spawn alone
		// projects the symbolic flight height through real collision geometry.
	}
	else
	{
		// A rest period must not accumulate as travel time in the native manager.
		auto& Movement=brain().movement();
		Movement.detail().target(m_tGraphID);
		Movement.detail().make_inactual();
		Movement.detail().update();
		Movement.path_type(MovementManager::ePathTypeNoPath);
	}
}
#endif
