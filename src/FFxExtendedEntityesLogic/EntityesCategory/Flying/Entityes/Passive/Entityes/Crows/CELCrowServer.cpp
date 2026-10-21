#include "FFxExtendedEntityesLogic/stdafx.h"
#include <algorithm>
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowServer.h"
#ifdef XRGAME_EXPORTS
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowSettings.h"
#include "FFxExtendedEntityesLogic/CoreComponent/CELNativeEntityes.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "xrServerEntities/alife_monster_brain.h"
#include "xrGame/alife_monster_movement_manager.h"
#include "xrGame/alife_monster_detail_path_manager.h"
#include "xrGame/ai_space.h"
#include "xrGame/alife_simulator.h"
#include "xrGame/alife_time_manager.h"
#include "xrGame/alife_graph_registry.h"
#include "xrGame/game_graph.h"
#include "xrGame/level_graph.h"
#include "xrGame/GamePersistent.h"
#include "xrEngine/Environment.h"
#include "xrServerEntities/clsid_game.h"

// Согласует состояние вороны при переходе в offline.
void CELCrowServer::add_offline(const xr_vector<ALife::_OBJECT_ID>& SavedChildren, const bool& UpdateRegistries)
{
	CSE_ALifeMonsterBase::add_offline(SavedChildren, UpdateRegistries);
	State.NeedsPlacement = true;
}
// Загружает настройки и подготавливает состояние перед использованием.
void CELCrowServer::Initialize()
{
	const SELCrowSettings& Settings = CELCrowSettings::Read(s_name.c_str());
	if (!State.Valid)
	{
		State.Satiety = Settings.InitialSatiety.Sample();
		State.Remaining = Settings.Sky.Sample();
		State.Altitude = Settings.Altitude.Sample();
		State.Enabled = Settings.Enabled;
		State.Valid = true;
	}
	IsRaining = State.Rain.Observed;
	m_fCurrentLevelGoingSpeed = (pSettings)->read_if_exists<float>(s_name.c_str(), "flight_speed", 6.f);
	if (!_valid(m_fCurrentLevelGoingSpeed))
	{
		m_fCurrentLevelGoingSpeed = 6.f;
	}
	clamp(m_fCurrentLevelGoingSpeed, 1.f, 30.f);
	m_fGoingSpeed = m_fCurrentLevelGoingSpeed;
	m_tpaTerrain.clear();
	NextInterval = Settings.OfflineTick.Sample();
	HasConfiguration = true;
}
// Выбирает следующую точку маршрута с учётом текущей фазы поведения.
void CELCrowServer::ChooseGoal()
{
	const IGameGraph& Graph = ai().game_graph();
	if (!Graph.valid_vertex_id(m_tGraphID))
	{
		return;
	}
	const u32 LevelIdentifier = Graph.vertex(m_tGraphID)->level_id();
	const SELCrowSettings& Settings = CELCrowSettings::Read(s_name.c_str());
	GameGraph::_GRAPH_ID Goal = m_tGraphID;
	GameGraph::_GRAPH_ID Next;
	GameGraph::_GRAPH_ID Candidate;
	IGameGraph::const_iterator Begin;
	IGameGraph::const_iterator End;
	IGameGraph::const_iterator Edge;
	u32 Step;
	u32 Count;
	float BestDistance;
	float Distance;
	for (Step = 0; Step < 4; ++Step)
	{
		Graph.begin(Goal, Begin, End);
		Next = Goal;
		Count = 0;
		BestDistance = Graph.vertex(Goal)->level_point().distance_to_sqr(State.Danger);
		for (Edge = Begin; Edge != End; ++Edge)
		{
			Candidate = Edge->vertex_id();
			if (!Graph.accessible(Candidate) || Graph.vertex(Candidate)->level_id() != LevelIdentifier)
			{
				continue;
			}
			++Count;
			if (State.Fear > Settings.FearCalm)
			{
				Distance = Graph.vertex(Candidate)->level_point().distance_to_sqr(State.Danger);
				if (Distance > BestDistance)
				{
					BestDistance = Distance;
					Next = Candidate;
				}
			}
			else if (Random.randI(0, int(Count)) == 0)
			{
				Next = Candidate;
			}
		}
		Goal = Next;
	}
	brain().movement().path_type(MovementManager::ePathTypeGamePath);
	brain().movement().detail().target(Goal);
}
// Обновляет offline-реакции вороны на время суток и осадки.
void CELCrowServer::UpdateWeather(float DeltaTime, ALife::_TIME_ID CurrentTime)
{
	const SELCrowSettings& Settings = CELCrowSettings::Read(s_name.c_str());
	float Hour = float((CurrentTime / 1000u) % 86400u) / 3600.f;
	const bool CurrentLevel = ai().get_level_graph() && ai().game_graph().vertex(m_tGraphID)->level_id() == ai().level_graph().level_id();
	if (CurrentLevel && g_pGamePersistent && g_pGamePersistent->Environment().CurrentEnv)
	{
		Hour = g_pGamePersistent->Environment().GetGameTime() / 3600.f;
		const float Density = g_pGamePersistent->Environment().CurrentEnv->rain_density;
		IsRaining = Settings.RainEnabled && (IsRaining ? Density > Settings.RainStop : Density > Settings.RainStart);
	}
	const bool IsNight = Settings.NightEnabled && (Settings.NightStart > Settings.NightEnd ? (Hour >= Settings.NightStart || Hour < Settings.NightEnd) : (Hour >= Settings.NightStart && Hour < Settings.NightEnd));
	State.Rain.Update(IsRaining, DeltaTime, Settings.RainDelay, Settings.DryDelay);
	State.Night.Update(IsNight, DeltaTime, Settings.NightDelay, Settings.WakeDelay);
}
// Учитывает доступные offline-сведения об окружающих сущностях.
CSE_ALifeCreatureAbstract* CELCrowServer::ObserveNeighbors(bool& Threatened)
{
	const SELCrowSettings& Settings = CELCrowSettings::Read(s_name.c_str());
	const CALifeGraphRegistry::GRAPH_REGISTRY& Points = ai().alife().graph().objects();
	if (m_tGraphID >= Points.size())
	{
		return nullptr;
	}
	const CALifeGraphRegistry::OBJECT_REGISTRY::_REGISTRY& Objects = Points[m_tGraphID].objects().objects();
	CALifeGraphRegistry::OBJECT_REGISTRY::_const_iterator ObjectIterator;
	CSE_ALifeCreatureAbstract* Candidate;
	CSE_ALifeCreatureAbstract* Food = nullptr;
	Fvector BirdPosition = o_Position;
	if (State.Rest == EELCrowRest::None)
	{
		BirdPosition.y += State.Altitude;
	}
	float FoodDistance = Settings.FoodRadius * Settings.FoodRadius;
	float Distance;
	u32 Checked = 0;
	for (ObjectIterator = Objects.begin(); ObjectIterator != Objects.end() && Checked < 64; ++ObjectIterator, ++Checked)
	{
		Candidate = ObjectIterator->second->cast_creature_abstract();
		if (!Candidate || Candidate == this || Candidate->m_tClassID == ExtendedEntityesClasses::Crow || Candidate->m_tClassID == CLSID_AI_CROW)
		{
			continue;
		}
		Distance = Candidate->o_Position.distance_to_sqr(BirdPosition);
		if (Candidate->g_Alive() && Distance < Settings.FearRadius * Settings.FearRadius)
		{
			Threatened = true;
			State.Danger = Candidate->o_Position;
		}
		if (!Candidate->g_Alive() && Distance < FoodDistance)
		{
			Food = Candidate;
			FoodDistance = Distance;
		}
	}
	return Food;
}
// Выбирает вид отдыха и его продолжительность.
void CELCrowServer::SelectRest(CSE_ALifeCreatureAbstract* Food, bool WasAfraid)
{
	const SELCrowSettings& Settings = CELCrowSettings::Read(s_name.c_str());
	const EELCrowRest Required = State.Fear > Settings.FearCalm ? EELCrowRest::None : State.Rain.Active ? EELCrowRest::Shelter
																				  : State.Night.Active	? EELCrowRest::Night
																										: EELCrowRest::None;
	if (Required != EELCrowRest::None)
	{
		State.Rest = Required;
		return;
	}
	if (State.Fear > Settings.FearCalm || State.Rest == EELCrowRest::Night || State.Rest == EELCrowRest::Shelter)
	{
		if (State.Rest != EELCrowRest::None || !WasAfraid)
		{
			State.Rest = EELCrowRest::None;
			State.Remaining = State.Fear > Settings.FearCalm ? Settings.Escape.Sample() : Settings.Sky.Sample();
			ChooseGoal();
		}
		return;
	}
	if (State.Remaining > 0.f)
	{
		return;
	}
	if (State.Rest != EELCrowRest::None)
	{
		State.Rest = EELCrowRest::None;
		State.Remaining = Settings.Sky.Sample();
		ChooseGoal();
	}
	else if (State.Satiety <= Settings.Hungry && Food)
	{
		State.Rest = EELCrowRest::Corpse;
		State.Corpse = Food->ID;
		State.Remaining = Settings.Corpse.Sample();
	}
	else if (State.Satiety > Settings.Hungry)
	{
		State.Rest = Random.randF() < Settings.GroundChance ? EELCrowRest::Ground : EELCrowRest::Roof;
		State.Remaining = State.Rest == EELCrowRest::Ground ? Settings.Ground.Sample() : Settings.Roof.Sample();
	}
	else
	{
		State.Remaining = Settings.Sky.Sample();
		ChooseGoal();
	}
}
// Продвигает offline-полёт к выбранной точке.
void CELCrowServer::UpdateMovement()
{
	CALifeMonsterMovementManager& Movement = brain().movement();
	if (State.Rest == EELCrowRest::None)
	{
		if (Movement.path_type() != MovementManager::ePathTypeGamePath || Movement.detail().completed())
		{
			ChooseGoal();
		}
		Movement.update();
		o_Position = Movement.detail().draw_level_position();
	}
	else
	{
		// Native graph anchors remain on their plane; online placement alone adds flight height.
		Movement.detail().target(m_tGraphID);
		Movement.detail().make_inactual();
		Movement.detail().update();
		Movement.path_type(MovementManager::ePathTypeNoPath);
	}
}
// Продвигает offline-поведение, голод, погоду и движение вороны.
void CELCrowServer::update()
{
	if (m_bOnline || !bfActive())
	{
		return;
	}
	if (!HasConfiguration)
	{
		Initialize();
	}
	const ALife::_TIME_ID CurrentTime = ai().alife().time_manager().game_time();
	if (!State.Time || CurrentTime < State.Time)
	{
		State.Time = CurrentTime;
		return;
	}
	const float DeltaTime = float(CurrentTime - State.Time) / 1000.f / std::max(EPS_L, ai().alife().time_manager().normal_time_factor());
	if (DeltaTime < NextInterval)
	{
		return;
	}
	const SELCrowSettings& Settings = CELCrowSettings::Read(s_name.c_str());
	State.Time = CurrentTime;
	NextInterval = Settings.OfflineTick.Sample();
	if (!CELSharedSettings::IsEnabled() || !Settings.Enabled || !State.Enabled)
	{
		return;
	}
	const bool FirstTick = !State.UpdatedOffline;
	const bool WasAfraid = State.Fear > Settings.FearCalm;
	if (!ai().game_graph().valid_vertex_id(m_tGraphID))
	{
		return;
	}
	if (FirstTick)
	{
		if (State.Rest == EELCrowRest::None)
		{
			o_Position = ai().game_graph().vertex(m_tGraphID)->level_point();
		}
		brain().movement().detail().target(m_tGraphID);
		brain().movement().detail().make_inactual();
		brain().movement().detail().update();
		brain().movement().path_type(MovementManager::ePathTypeNoPath);
	}
	State.UpdatedOffline = true;
	State.Satiety = std::max(0.f, State.Satiety - DeltaTime / Settings.HungerTime);
	State.Fear = std::max(0.f, State.Fear - Settings.FearDecay * DeltaTime);
	State.Remaining -= DeltaTime;
	UpdateWeather(DeltaTime, CurrentTime);
	bool Threatened = false;
	CSE_ALifeCreatureAbstract* Food = ObserveNeighbors(Threatened);
	if (Threatened)
	{
		State.Fear = 1.f;
		State.Remaining = Settings.Escape.Sample();
	}
	const EELCrowRest PreviousRest = State.Rest;
	SelectRest(Food, WasAfraid);
	if (PreviousRest == State.Rest && (State.Rest == EELCrowRest::Ground || (State.Rest == EELCrowRest::Corpse && Food && Food->ID == State.Corpse)))
	{
		const float Gain = State.Rest == EELCrowRest::Corpse ? Settings.MeatGain / std::max(.01f, (Settings.MeatPeck.Minimum + Settings.MeatPeck.Maximum) * .5f) : Settings.SeedGain / std::max(.01f, (Settings.Peck.Minimum + Settings.Peck.Maximum) * .5f);
		State.Satiety = std::min(1.f, State.Satiety + DeltaTime * Gain);
	}
	UpdateMovement();
}
#endif
