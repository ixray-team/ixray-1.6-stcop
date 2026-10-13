#pragma once

#include "../xrCore/Kernel/xrSyncronize.h"
#include "../xrCore/Kernel/EngineExternal.h"
#include "../xrEngine/MonsterLogicTelemetry.h"
#include "../xrSound/ai_sounds.h"
#include "HUDManager.h"
#include "Level.h"
#include "Actor.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "ai/monsters/control_path_builder_base.h"
#include "ai/monsters/monster_home.h"
#include "ai/monsters/state_manager.h"
#include "ai_space.h"
#include "level_graph.h"

class CMonsterEnemySharingDebug
{
	static constexpr u32 MaxLinks = 256;
	static constexpr u32 MaxMarkers = 256;
	static constexpr u32 MaxGoals = 256;
	static constexpr u32 LinkDisplayTime = 5000;
	static constexpr u32 CallDisplayTime = 5000;
	struct SLink
	{
		ALife::_OBJECT_ID Source = ALife::INVALID_OBJECT_ID;
		ALife::_OBJECT_ID Receiver = ALife::INVALID_OBJECT_ID;
		u32 Until = 0;
		bool Wireless = false;
		bool Active = false;
	};
	struct SGoal
	{
		ALife::_OBJECT_ID Receiver = ALife::INVALID_OBJECT_ID;
		ALife::_OBJECT_ID Target = ALife::INVALID_OBJECT_ID;
		Fvector SharedPosition = {0.f, 0.f, 0.f};
		u32 ObservationTime = 0;
		bool Active = false;
	};
	struct SMarker
	{
		ALife::_OBJECT_ID Monster = ALife::INVALID_OBJECT_ID;
		bool Sight = false;
		bool CloseDetection = false;
		bool Call = false;
		u32 CallUntil = 0;
		bool Active = false;
	};
	struct SNoiseReactionMarker
	{
		ALife::_OBJECT_ID Monster = ALife::INVALID_OBJECT_ID;
		u32 Until = 0;
		Fvector Position = {0.f, 0.f, 0.f};
		float Power = 0.f;
		bool Panic = false, Explosion = false, Investigating = false, General = false;
		u32 SoundType = 0;
		bool ActorSource = false;
		float Interest = 0.f;
	};
	static const char* NoiseTypeName(u32 SoundType, bool ActorSource)
	{
		if (SoundType & SOUND_TYPE_OBJECT_EXPLODING)
		{
			return "Взрыв";
		}
		if (SoundType & SOUND_TYPE_MELEE)
		{
			return "Холодное оружие";
		}
		if (SoundType & SOUND_TYPE_SHOOTING)
		{
			return SoundType & SOUND_TYPE_SUPPRESSED ? "Выстрел с глушителем" : "Выстрел";
		}
		if (SoundType & SOUND_TYPE_ACTOR_JUMP)
		{
			return "Прыжок ГГ";
		}
		if (SoundType & SOUND_TYPE_STEP)
		{
			if (ActorSource)
			{
				switch (SoundType & SOUND_TYPE_ACTOR_STEP_MASK)
				{
				case SOUND_TYPE_ACTOR_SPRINT: return "Спринт ГГ";
				case SOUND_TYPE_ACTOR_RUN: return "Бег ГГ";
				case SOUND_TYPE_ACTOR_WALK: return "Шаг ГГ";
				case SOUND_TYPE_ACTOR_HIGH_CROUCH: return "Высокий присед ГГ";
				case SOUND_TYPE_ACTOR_LOW_CROUCH: return "Низкий присед ГГ";
				default: return "Шаги ГГ";
				}
			}
			return SoundType & SOUND_TYPE_MONSTER ? "Шаги мутанта" : "Шаги";
		}
		if (SoundType & SOUND_TYPE_BULLET_HIT)
		{
			return "Попадание пули";
		}
		if (SoundType & SOUND_TYPE_ATTACKING)
		{
			return "Атака / бой";
		}
		if (SoundType & SOUND_TYPE_INJURING)
		{
			return "Звук ранения";
		}
		if (SoundType & SOUND_TYPE_DYING)
		{
			return "Предсмертный звук";
		}
		if (SoundType & SOUND_TYPE_OBJECT_BREAKING)
		{
			return "Разрушение предмета";
		}
		if (SoundType & SOUND_TYPE_OBJECT_COLLIDING)
		{
			return "Столкновение предметов";
		}
		return "Другой шум";
	}
	SNoiseReactionMarker NoiseReactions[MaxMarkers]{};
	u32 NoiseReactionCursor = 0;
	struct SBehavior
	{
		ALife::_OBJECT_ID Monster = ALife::INVALID_OBJECT_ID, Target = ALife::INVALID_OBJECT_ID;
		u32 Time = 0, Memory = 0, Approach = 0, Search = 0;
		const char* Mode = "NATIVE";
		const char* Evidence = "NONE";
		Fvector Anchor = {0.f, 0.f, 0.f}, Goal = {0.f, 0.f, 0.f}, Heading = {1.f, 0.f, 0.f};
		Fvector Trail[5]{};
		u32 TrailTimes[5]{}, TrailCount = 0;
		float Radius = 0.f, Angle = 0.f;
		bool Moving = false, Searching = false, Psi = false, Committed = false;
	};
	struct SZone
	{
		u32 Group = 0, Key = u32(-1), Until = 0;
		Fvector Position = {0.f, 0.f, 0.f};
		float Radius = 0.f;
		const char* Kind = "AREA";
	};
	SBehavior Behaviors[MaxMarkers]{};
	SZone Zones[128]{};
	u32 BehaviorCursor = 0, ZoneCursor = 0;
	std::atomic<u32> Epoch{0};
	xrCriticalSection Mutex;
	SLink Links[MaxLinks]{};
	SMarker Markers[MaxMarkers]{};
	SGoal Goals[MaxGoals]{};
	u32 GoalCursor = 0;
	u32 MarkerCursor = 0;
	u32 Generation = 0;
	const void* OwnerLevel = nullptr;

	void ResetLocked()
	{
		Epoch.fetch_add(1, std::memory_order_relaxed);
		for (SLink& Link : Links)
		{
			Link.Active = false;
		}
		for (SMarker& Marker : Markers)
		{
			Marker.Active = false;
		}
		for (SGoal& Goal : Goals)
		{
			Goal.Active = false;
		}
		for (auto& Reaction : NoiseReactions) Reaction = {};
		NoiseReactionCursor = 0;
		BehaviorCursor = 0; ZoneCursor = 0;
		for (auto& Behavior : Behaviors) Behavior = {};
		for (auto& Zone : Zones) Zone = {};
		GoalCursor = 0;
		MarkerCursor = 0;
		Generation = g_MonsterLogicTelemetry.Generation.load(std::memory_order_relaxed);
		OwnerLevel = g_pGameLevel;
	}
public:
	bool IsEnabled() const { return g_MonsterLogicTelemetry.DrawEnabled.load(std::memory_order_relaxed); }
	u32 GetEpoch() const { return Epoch.load(std::memory_order_relaxed); }
	void SetEnabled(bool Value)
	{
		g_MonsterLogicTelemetry.DrawEnabled.store(Value, std::memory_order_relaxed);
		xrCriticalSectionGuard Guard(Mutex);
		ResetLocked();
	}
	void Record(const CBaseMonster& Source, const CBaseMonster& Receiver, const CEntityAlive& Target, const Fvector& SharedPosition, u32 ObservationTime)
	{
		if (!IsEnabled())
		{
			return;
		}
		xrCriticalSectionGuard Guard(Mutex);
		if (!IsEnabled())
		{
			return;
		}
		if (OwnerLevel != g_pGameLevel || Generation != g_MonsterLogicTelemetry.Generation.load(std::memory_order_relaxed))
		{
			ResetLocked();
		}
		SGoal* SelectedGoal = nullptr;
		for (SGoal& Goal : Goals)
		{
			if (Goal.Active && Goal.Receiver == Receiver.ID() && Goal.Target == Target.ID())
			{
				SelectedGoal = &Goal;
				break;
			}
		}
		if (!SelectedGoal)
		{
			SelectedGoal = &Goals[GoalCursor];
			GoalCursor = (GoalCursor + 1) % MaxGoals;
		}
		*SelectedGoal = {Receiver.ID(), Target.ID(), SharedPosition, ObservationTime, true};
		SLink* Selected = nullptr;
		for (SLink& Link : Links)
		{
			if (Link.Active && Link.Source == Source.ID() && Link.Receiver == Receiver.ID())
			{
				Selected = &Link;
				break;
			}
		}
		if (!Selected)
		{
			for (SLink& Link : Links)
			{
				if (!Link.Active || s32(Device.dwTimeGlobal - Link.Until) >= 0)
				{
					Selected = &Link;
					break;
				}
			}
		}
		if (!Selected)
		{
			Selected = &Links[0];
			for (SLink& Link : Links)
			{
				if (s32(Link.Until - Selected->Until) < 0)
				{
					Selected = &Link;
				}
			}
		}
		*Selected = {Source.ID(), Receiver.ID(), Device.dwTimeGlobal + LinkDisplayTime, Source.EnemySharingWireless, true};
	}
	void Mark(const CBaseMonster& Monster, bool Sight, bool Visible = true, bool CloseDetection = false)
	{
		if (!IsEnabled())
		{
			return;
		}
		xrCriticalSectionGuard Guard(Mutex);
		if (!IsEnabled())
		{
			return;
		}
		if (OwnerLevel != g_pGameLevel || Generation != g_MonsterLogicTelemetry.Generation.load(std::memory_order_relaxed))
		{
			ResetLocked();
		}
		SMarker* Selected = nullptr;
		for (SMarker& Marker : Markers)
		{
			if (Marker.Active && Marker.Monster == Monster.ID())
			{
				Selected = &Marker;
				break;
			}
		}
		if (!Selected && (Sight || CloseDetection) && !Visible)
		{
			return;
		}
		if (!Selected)
		{
			for (SMarker& Marker : Markers)
			{
				if (!Marker.Active)
				{
					Selected = &Marker;
					break;
				}
			}
			if (!Selected)
			{
				Selected = &Markers[MarkerCursor];
				MarkerCursor = (MarkerCursor + 1) % MaxMarkers;
			}
			*Selected = {Monster.ID(), false, false, false, 0, true};
		}
		if (CloseDetection)
		{
			Selected->CloseDetection = Visible;
		}
		else if (Sight)
		{
			Selected->Sight = Visible;
		}
		else
		{
			Selected->Call = true;
			Selected->CallUntil = Device.dwTimeGlobal + CallDisplayTime;
		}
		Selected->Active = Selected->Sight || Selected->CloseDetection || Selected->Call;
	}
	void RecordNoiseReaction(const CBaseMonster& Monster, const Fvector& Position, float Power, bool Panic, bool Explosion, bool Investigating, bool General, u32 SoundType, bool ActorSource, float Interest)
	{
		if (!IsEnabled()) return;
		xrCriticalSectionGuard Guard(Mutex);
		if (!IsEnabled()) return;
		if (OwnerLevel != g_pGameLevel || Generation != g_MonsterLogicTelemetry.Generation.load(std::memory_order_relaxed)) ResetLocked();
		SNoiseReactionMarker* Selected = nullptr;
		for (auto& Marker : NoiseReactions)
		{
			if (Marker.Monster == Monster.ID()) { Selected = &Marker; break; }
		}
		if (!Selected) Selected = &NoiseReactions[NoiseReactionCursor++ % MaxMarkers];
		*Selected = {Monster.ID(), Device.dwTimeGlobal + 5000, Position, Power, Panic, Explosion, Investigating, General, SoundType, ActorSource, Interest};
	}
	void RecordBehavior(CBaseMonster& Monster)
	{
		if (!IsEnabled()) return;
		xrCriticalSectionGuard Guard(Mutex);
		if (!IsEnabled()) return;
		if (OwnerLevel != g_pGameLevel || Generation != g_MonsterLogicTelemetry.Generation.load(std::memory_order_relaxed)) ResetLocked();
		SBehavior* Selected = nullptr;
		for (auto& Item : Behaviors) if (Item.Monster == Monster.ID()) { Selected = &Item; break; }
		if (!Selected) Selected = &Behaviors[BehaviorCursor++ % MaxMarkers];
		*Selected = {};
		const auto& Policy = Monster.Behavior;
		Selected->Monster = Monster.ID(); Selected->Time = Device.dwTimeGlobal;
		Selected->Mode = Policy.Mode; Selected->Goal = Policy.Goal; Selected->Anchor = Policy.Anchor;
		Selected->Moving = Policy.Moving; Selected->Searching = Policy.Searching; Selected->Psi = Monster.EnemyTrackingLive;
		Selected->Radius = Policy.SearchRadius; Selected->Angle = Policy.SearchAngle;

		Selected->Approach = s32(Policy.ApproachUntil - Device.dwTimeGlobal) > 0 ? Policy.ApproachUntil - Device.dwTimeGlobal : 0;
		Selected->Search = s32(Policy.SearchUntil - Device.dwTimeGlobal) > 0 ? Policy.SearchUntil - Device.dwTimeGlobal :
			s32(Policy.ActionUntil - Device.dwTimeGlobal) > 0 ? Policy.ActionUntil - Device.dwTimeGlobal : 0;
		const CEntityAlive* Target = Monster.EnemyMan.get_enemy();
		if (!Target) return;
		const auto Entry = Monster.EnemyMemory.get_memory().find(Target);
		if (Entry == Monster.EnemyMemory.get_memory().end() || !Monster.EnemyMemory.IsActual(Entry->second)) return;
		Selected->Target = Target->ID(); Selected->Anchor = Entry->second.position;
		Selected->Memory = Monster.EnemyMemory.GetRemainingMemoryTime(Entry->second);
		Selected->Committed = Entry->second.InspectionCommitted;
		Selected->Evidence = Monster.EnemyTrackingLive ? "PSI" : Entry->second.Evidence;
		Selected->TrailCount = Monster.EnemyTrackingLive ? 0 : std::min(Entry->second.TrailCount, 5u);
		std::copy(std::begin(Entry->second.Trail), std::end(Entry->second.Trail), Selected->Trail);
		std::copy(std::begin(Entry->second.TrailTimes), std::end(Entry->second.TrailTimes), Selected->TrailTimes);
		Selected->Heading = Entry->second.HasTrailHeading ? Entry->second.TrailHeading : Fvector{0.f, 0.f, 0.f};
		if (!Entry->second.HasTrailHeading) Selected->Angle = 360.f;
		if (Monster.PursuitSearchingAround) { Selected->Anchor = Monster.UnreachableSearchAnchor; Selected->Angle = 360.f; }
		SGoal* Goal = nullptr;
		for (auto& Item : Goals) if (Item.Active && Item.Receiver == Monster.ID() && Item.Target == Target->ID()) { Goal = &Item; break; }
		if (!Goal) Goal = &Goals[GoalCursor++ % MaxGoals];
		*Goal = {Monster.ID(), Target->ID(), Entry->second.position, Entry->second.time, true};
	}
	void RecordZone(u32 Group, u32 Key, const Fvector& Position, float Radius, u32 Until, const char* Kind)
	{
		if (!IsEnabled()) return;
		xrCriticalSectionGuard Guard(Mutex);
		if (!IsEnabled()) return;
		SZone* Selected = nullptr;
		for (auto& Zone : Zones) if (Zone.Group == Group && Zone.Key == Key) { Selected = &Zone; break; }
		if (!Selected) Selected = &Zones[ZoneCursor++ % 128];
		*Selected = {Group, Key, Until, Position, Radius, Kind};
	}
	static const char* RussianLabel(const char* Value)
	{
		static const struct { const char* Key; const char* Text; } Labels[] =
		{
			{"PSI", "Пси"},
			{"NATIVE", "Штатное поведение"},
			{"WANDER / NATIVE", "Блуждание"},
			{"ATTACK", "Атака"},
			{"RETREAT / PANIC", "Испуг / отступление"},
			{"DRAG CORPSE", "Тащит тело"},
			{"FEED / APPROACH CORPSE", "Кормление"},
			{"HAZARD / SAFE ARC", "Обход опасности"},
			{"AVOID DEATH ZONE", "Обход кладбища"},
			{"WAIT", "Ожидание"},
			{"TRAIL SEARCH / INSPECT", "Поиск по следу"},
			{"SECTOR SEARCH", "Поиск по сектору"},
			{"PRESSURE / BLOCKED", "Манёвры у преграды"},
			{"LOCAL SEARCH", "Поиск у преграды"},
			{"PSI PURSUIT", "Пси-преследование"},
			{"PURSUIT", "Преследование"},
			{"LAST KNOWN APPROACH", "К последней точке"},
			{"WIDE ARC", "Широкий обход"},
			{"ENCIRCLE", "Окружение"},
			{"PRESSURE / ARC", "Давление сбоку"},
			{"FLANK", "Заход сбоку"},
			{"LUNGE / REPOSITION", "Подготовка выпада"},
			{"RETREAT / REGROUP", "Отход к союзникам"},
			{"CHARGE", "Прорыв"},
			{"CAUTIOUS", "Осторожный подход"},
			{"DIRECT APPROACH", "Прямой подход"},
			{"BODY INVESTIGATION", "Исследует тело"},
			{"DEATH ALERT SEARCH", "Поиск после смерти"},
			{"DEATH ALERT", "Тревога от смерти"},
			{"NOISE APPROACH", "К месту шума"},
			{"SOUND CHECK", "Прислушивается"},
			{"NOISE SEARCH", "Поиск у звука"},
			{"OBSERVATION", "Наблюдение"},
			{"VISUAL", "Зрение"},
			{"CLOSE", "Близость"},
			{"SOUND", "Слух"},
			{"RADIO", "Сарафан"},
			{"PSI RADIO", "Пси-сарафан"},
			{"NONE", "Нет"},
			{"UPDATED", "Обновлено"},
			{"OTHER TARGET", "Другая цель"},
			{"LOCAL SEARCH / SEES", "Преграда / видит"},
			{"VISUAL PURSUIT", "Зрительная погоня"},
			{"PSI LIVE", "Пси-слежение"},
			{"NO AI NODE", "Нет узла AI"},
			{"HOME LIMIT", "Граница территории"},
			{"PANIC", "Испуг"},
			{"SEARCH AT POINT", "Поиск у точки"},
			{"AT POINT", "У точки"},
			{"LAST KNOWN PURSUIT", "К последней точке"},
			{"WAIT / SEARCH", "Ожидание / поиск"},
			{"ANOMALY", "Аномалия"},
			{"DANGER", "Опасность"},
			{"FIGHT", "Бой"},
			{"BODY DONE", "Тело исследовано"},
			{"BODY KNOWN", "Известное тело"},
			{"ALERT", "Тревога"},
			{"INVESTIGATION", "Расследование"},
		};
		for (const auto& Label : Labels)
		{
			if (!xr_strcmp(Value, Label.Key)) return Label.Text;
		}
		return Value;
	}
	void Draw(LevelInspector& Primitives)
	{
		if (!IsEnabled() || !g_pGameLevel)
		{
			return;
		}
		SNoiseReactionMarker ReactionSnapshot[MaxMarkers]{};
		SBehavior BehaviorSnapshot[MaxMarkers]{};
		SZone ZoneSnapshot[128]{};
		SLink Snapshot[MaxLinks]{};
		SMarker MarkerSnapshot[MaxMarkers]{};
		SGoal GoalSnapshot[MaxGoals]{};
		{
			xrCriticalSectionGuard Guard(Mutex);
			if (!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ||
				OwnerLevel != g_pGameLevel || Generation != g_MonsterLogicTelemetry.Generation.load(std::memory_order_relaxed))
			{
				ResetLocked();
				return;
			}
			std::copy(std::begin(NoiseReactions), std::end(NoiseReactions), ReactionSnapshot);
			std::copy(std::begin(Behaviors), std::end(Behaviors), BehaviorSnapshot);
			std::copy(std::begin(Zones), std::end(Zones), ZoneSnapshot);
			for (u32 Index = 0; Index < MaxGoals; ++Index)
			{
				GoalSnapshot[Index] = Goals[Index];
			}
			for (u32 Index = 0; Index < MaxLinks; ++Index)
			{
				if (Links[Index].Active && s32(Device.dwTimeGlobal - Links[Index].Until) >= 0)
				{
					Links[Index].Active = false;
				}
				Snapshot[Index] = Links[Index];
			}
			for (u32 Index = 0; Index < MaxMarkers; ++Index)
			{
				if (Markers[Index].Call && s32(Device.dwTimeGlobal - Markers[Index].CallUntil) >= 0)
				{
					Markers[Index].Call = false;
				}
				Markers[Index].Active = Markers[Index].Sight || Markers[Index].CloseDetection || Markers[Index].Call;
				MarkerSnapshot[Index] = Markers[Index];
			}
		}
		const float PreviousFontHeight = Primitives.dbg_font->GetHeight();
		Primitives.dbg_font->SetHeight(PreviousFontHeight * .5f);
		ALife::_OBJECT_ID FocusMonster = ALife::INVALID_OBJECT_ID;
		float BestFocus = .97f;
		for (const auto& Item : BehaviorSnapshot)
		{
			if (Item.Monster == ALife::INVALID_OBJECT_ID || Device.dwTimeGlobal - Item.Time > 1500) continue;
			CObject* Object = Level().Objects.net_Find(Item.Monster);
			if (!Object || Object->getDestroy()) continue;
			Fvector Direction; Direction.sub(Object->Center(), Device.vCameraPosition);
			if (Direction.magnitude() < EPS_L) continue;
			Direction.normalize();
			const float Score = Direction.dotproduct(Device.vCameraDirection);
			if (Score > BestFocus) { BestFocus = Score; FocusMonster = Item.Monster; }
		}
		Fvector2 LabelPositions[MaxMarkers]{};
		u32 LabelCount = 0;
		for (const auto& Reaction : ReactionSnapshot)
		{
			if (!Reaction.Until || s32(Reaction.Until - Device.dwTimeGlobal) <= 0) continue;
			CObject* Object = Level().Objects.net_Find(Reaction.Monster);
			CBaseMonster* Monster = Object ? Object->cast_base_monster() : nullptr;
			if (!Monster || Monster->getDestroy() || !Monster->g_Alive()) continue;
			const u32 Color = Reaction.Panic ? color_rgba(210, 80, 255, 255) : color_rgba(80, 180, 255, 255);
			for (u32 Axis = 0; Axis < 3; ++Axis)
			{
				Fvector From = Reaction.Position, To = Reaction.Position; From[Axis] -= .4f; To[Axis] += .4f;
				Primitives.append_line(From, To, Color);
			}
			Primitives.append_line(Monster->Center(), Reaction.Position, Color);
			string256 Label;
			xr_sprintf(Label, "%s | %s | интерес %.0f%% | #%u", NoiseTypeName(Reaction.SoundType, Reaction.ActorSource),
				Reaction.Panic ? "Испуг" : Reaction.Investigating ? "Идёт проверять" : "Прислушивается", Reaction.Interest * 100.f, u32(Reaction.Monster));
			Fvector LabelPosition = Reaction.Position;
			LabelPosition.y += .6f;
			Primitives.append_text3d(LabelPosition, shared_str(Label), Color);
		}
		ALife::_OBJECT_ID DrawnSources[MaxLinks]{};
		u32 SourceCount = 0;
		for (const SLink& Link : Snapshot)
		{
			if (!Link.Active)
			{
				continue;
			}
			CObject* SourceObject = Level().Objects.net_Find(Link.Source);
			CObject* ReceiverObject = Level().Objects.net_Find(Link.Receiver);
			CBaseMonster* Source = SourceObject ? SourceObject->cast_base_monster() : nullptr;
			CBaseMonster* Receiver = ReceiverObject ? ReceiverObject->cast_base_monster() : nullptr;
			if (!Source || !Receiver || Source->getDestroy() || Receiver->getDestroy() || !Source->g_Alive() || !Receiver->g_Alive())
			{
				continue;
			}
			const Fvector From = Source->Center(), To = Receiver->Center();
			const u32 Color = Link.Wireless ? color_rgba(0, 220, 255, 255) : color_rgba(255, 220, 0, 255);
			if (std::find(DrawnSources, DrawnSources + SourceCount, Link.Source) == DrawnSources + SourceCount)
			{
				DrawnSources[SourceCount++] = Link.Source;
				Primitives.append_sphere(From, std::max(.5f, Source->Radius()) + (Link.Wireless ? .45f : .25f), Color, Link.Wireless ? color_rgba(0, 220, 255, 35) : color_rgba(255, 220, 0, 35));
			}
			Fvector Direction;
			Direction.sub(To, From);
			const float Length = Direction.magnitude();
			if (Length > EPS_L)
			{
				Direction.div(Length);
				const float HeadLength = std::min(1.f, Length * .25f);
				Fvector HeadStart;
				HeadStart.mad(To, Direction, -HeadLength);
				Primitives.append_line(From, HeadStart, Color);
				Primitives.append_lines_arrow(HeadStart, Direction, HeadLength, Color);
			}
		}
		Fvector DrawnPoints[MaxGoals]{};
		u32 FollowingCounts[MaxGoals]{}, AssignedCounts[MaxGoals]{};
		u32 PointTargets[MaxGoals]{};
		bool LivePoints[MaxGoals]{};
		u32 PointCount = 0;
		const u32 GoalColor = color_rgba(255, 220, 0, 255);
		for (const SGoal& Goal : GoalSnapshot)
		{
			if (!Goal.Active)
			{
				continue;
			}
			CObject* ReceiverObject = Level().Objects.net_Find(Goal.Receiver);
			CObject* TargetObject = Level().Objects.net_Find(Goal.Target);
			CBaseMonster* Receiver = ReceiverObject ? ReceiverObject->cast_base_monster() : nullptr;
			CEntityAlive* Target = TargetObject ? TargetObject->cast_entity_alive() : nullptr;
			if (!Receiver || !Target || Receiver->getDestroy() || Target->getDestroy() || !Receiver->g_Alive() || !Target->g_Alive())
			{
				continue;
			}
			const auto& Memory = Receiver->EnemyMemory.get_memory();
			const auto Entry = Memory.find(Target);
			if (Entry == Memory.end() || !Receiver->EnemyMemory.IsActual(Entry->second))
			{
				continue;
			}
			const bool Selected = Receiver->EnemyMan.get_enemy() == Target;
			// Only assigned inspection points are presented as tasks; history alone is not an assignment.
			if (!Selected) continue;
			// Ordinary receivers use memory; live tracking remains explicit for psi.
			Fvector Point = Receiver->EnemyTrackingLive ? Receiver->EnemyMan.GetKnownEnemyPosition(Target) : Entry->second.position;
			Point.y += .2f;
			u32 PointIndex = PointCount;
			bool DrawPoint = true;
			for (u32 Index = 0; Index < PointCount; ++Index)
			{
				if (PointTargets[Index] == u32(Target->ID()) && DrawnPoints[Index].distance_to_sqr(Point) < .25f)
				{
					PointIndex = Index;
					DrawPoint = false;
					break;
				}
			}
			if (DrawPoint)
			{
				DrawnPoints[PointCount++] = Point;
				LivePoints[PointIndex] = Receiver->EnemyTrackingLive;
				PointTargets[PointIndex] = Target->ID();
				for (u32 Axis = 0; Axis < 3; ++Axis)
				{
					Fvector From = Point, To = Point;
					From[Axis] -= .65f;
					To[Axis] += .65f;
					Primitives.append_line(From, To, GoalColor);
				}
				const Fvector Up = {0.f, 1.f, 0.f};
				Primitives.append_lines_arrow(Point, Up, 1.8f, GoalColor);

			}
			if (Selected && Entry->second.InspectionCommitted) ++AssignedCounts[PointIndex];
			if (Selected && !Receiver->HasDamagePanic() && !Receiver->GetScriptControl() &&
				!Receiver->PursuitSearchingAround && !Receiver->Behavior.Searching &&
				(Receiver->Behavior.Moving || Receiver->LastKnownPursuitWasActive) &&
				Receiver->path().get_target_set().distance_to_sqr(Entry->second.position) < 4.f &&
				!Receiver->HasReachedEnemyPoint(Entry->second.position, Entry->second.vertex))
			{
				++FollowingCounts[PointIndex];
			}
			Primitives.append_line(Receiver->Center(), Point, color_rgba(255, 220, 0, Selected ? 180 : 70));
			if (Receiver->Behavior.Enabled) continue;
			const char* State = !Selected ? "OTHER TARGET" :
				Receiver->PursuitSearchingAround ? (Receiver->EnemyMan.see_enemy_now(Target) ? "LOCAL SEARCH / SEES" : "LOCAL SEARCH") :
				Receiver->EnemyMan.see_enemy_now(Target) ? "VISUAL PURSUIT" :
				Receiver->EnemyTrackingLive ? "PSI LIVE" :
				!ai().level_graph().valid_vertex_id(Entry->second.vertex) ? "NO AI NODE" :
				!Receiver->Home->at_home(Entry->second.position) ? "HOME LIMIT" :
				(Receiver->HasDamagePanic() || is_state(Receiver->StateMan->get_state_type(), eStatePanic)) ? "PANIC" :
				Entry->second.ReachedPointTime != 0 ? "SEARCH AT POINT" :
				Receiver->HasReachedEnemyPoint(Entry->second.position, Entry->second.vertex) ? "AT POINT" :
				Receiver->LastKnownPursuitWasActive ? "LAST KNOWN PURSUIT" : "WAIT / SEARCH";
			string256 Label;
			const bool Updated = Entry->second.time != Goal.ObservationTime || Entry->second.position.distance_to_sqr(Goal.SharedPosition) > .01f;
			const float Remaining = float(Receiver->EnemyMemory.GetRemainingMemoryTime(Entry->second)) * .001f;
			xr_sprintf(Label, "#%u %s", u32(Receiver->ID()), RussianLabel(State));
			Fvector LabelPosition = Receiver->Center();
			LabelPosition.y += 1.5f;
			if (Primitives.append_text3d(LabelPosition, shared_str(Label), GoalColor))
			{
				xr_sprintf(Label, "%s | память %.1f с", Updated ? "Обновлено" : "Сарафан", Remaining);
				Primitives.append_text_next(shared_str(Label));
			}
		}
		for (u32 Index = 0; Index < PointCount; ++Index)
		{
			Fvector LabelPosition = DrawnPoints[Index];
			LabelPosition.y += 2.f;
			string256 Label;
			xr_sprintf(Label, "%s | идут: %u | назначено: %u", LivePoints[Index] ? "Пси-цель" : "Последняя точка", FollowingCounts[Index], AssignedCounts[Index]);
			Primitives.append_text3d(LabelPosition, shared_str(Label), GoalColor);
		}
		for (const auto& Item : BehaviorSnapshot)
		{
			if (Item.Monster == ALife::INVALID_OBJECT_ID || Device.dwTimeGlobal - Item.Time > 1500) continue;
			CObject* Object = Level().Objects.net_Find(Item.Monster);
			CBaseMonster* Monster = Object ? Object->cast_base_monster() : nullptr;
			if (!Monster || Monster->getDestroy() || !Monster->g_Alive()) continue;
			Fvector LabelPosition = Monster->Center(); LabelPosition.y += 1.5f;
			string256 Label;
			Fvector4 Projected; Device.mFullTransform.transform(Projected, LabelPosition);
			bool ClearLabel = Projected.w > 0.f && Projected.z >= 0.f && std::abs(Projected.x) <= 1.f && std::abs(Projected.y) <= 1.f;
			const float X = (1.f + Projected.x) * .5f * Device.TargetWidth;
			const float Y = (1.f - Projected.y) * .5f * Device.TargetHeight;
			for (u32 Index = 0; Index < LabelCount; ++Index)
			{
				if (std::abs(LabelPositions[Index].x - X) < 220.f && std::abs(LabelPositions[Index].y - Y) < 20.f) ClearLabel = false;
			}
			if (ClearLabel)
			{
				LabelPositions[LabelCount++].set(X, Y);
				xr_sprintf(Label, "#%u %s", u32(Item.Monster), RussianLabel(Item.Mode));
				Primitives.append_text3d(LabelPosition, shared_str(Label), GoalColor);
			}
			if (Item.Monster == FocusMonster)
			{
				Primitives.dbg_font->SetHeight(PreviousFontHeight * .7f);
				xr_sprintf(Label, "МОНСТР #%u | %s", u32(Item.Monster), RussianLabel(Item.Mode));
				Primitives.append_text2d(20.f, Device.TargetHeight * .62f, shared_str(Label), GoalColor, CGameFont::alLeft);
				if (Item.Target == ALife::INVALID_OBJECT_ID) xr_sprintf(Label, "Цель: нет");
				else xr_sprintf(Label, "Цель: %u | %s", u32(Item.Target), RussianLabel(Item.Evidence)); Primitives.append_text_next(shared_str(Label));
				xr_sprintf(Label, "Память %.1f с | подход %.1f с | поиск %.1f с", Item.Memory * .001f, Item.Approach * .001f, Item.Search * .001f); Primitives.append_text_next(shared_str(Label));
				if (Item.Committed) Primitives.append_text_next(shared_str("Проверка точки закреплена"));
				Primitives.dbg_font->SetHeight(PreviousFontHeight * .5f);
			}

			for (u32 Index = 0; Index < Item.TrailCount; ++Index)
			{
				Fvector Point = Item.Trail[Index]; Point.y += .15f;
				Primitives.append_sphere(Point, .12f, GoalColor, 0);
				if (Index) { Fvector Previous = Item.Trail[Index - 1]; Previous.y += .15f; Primitives.append_line(Previous, Point, GoalColor); }
			}
			if (Item.Moving)
			{
				Fvector Point = Item.Goal; Point.y += .15f;
				Primitives.append_line(Monster->Center(), Point, color_rgba(255, 140, 0, 220));
				Primitives.append_sphere(Point, .25f, color_rgba(255, 140, 0, 255), 0);
			}
			if (Item.Searching && Item.Search > Device.dwTimeGlobal - Item.Time && !Item.Psi)
			{
				Fvector Anchor = Item.Anchor; Anchor.y += .2f;
				const float BaseAngle = atan2f(Item.Heading.z, Item.Heading.x);
				const float HalfAngle = deg2rad(Item.Angle * .5f);
				Fvector Last = Anchor;
				for (u32 Index = 0; Index <= 12; ++Index)
				{
					const float Angle = BaseAngle - HalfAngle + 2.f * HalfAngle * float(Index) / 12.f;
					Fvector Point = Anchor; Point.x += cosf(Angle) * Item.Radius; Point.z += sinf(Angle) * Item.Radius;
					if (Index) Primitives.append_line(Last, Point, color_rgba(255, 180, 0, 130));
					if (Index == 0 || Index == 6 || Index == 12) Primitives.append_line(Anchor, Point, color_rgba(255, 180, 0, 180));
					Last = Point;
				}
			}
		}
		for (const auto& Zone : ZoneSnapshot)
		{
			if (!Zone.Until || s32(Zone.Until - Device.dwTimeGlobal) <= 0) continue;
			const u32 Color = !xr_strcmp(Zone.Kind, "NOISE SEARCH") ? color_rgba(80, 180, 255, 180) :
				!xr_strcmp(Zone.Kind, "ANOMALY") ? color_rgba(255, 50, 30, 180) :
				!xr_strcmp(Zone.Kind, "DANGER") ? color_rgba(255, 140, 30, 180) :
				!xr_strcmp(Zone.Kind, "FIGHT") ? color_rgba(255, 210, 60, 180) : color_rgba(220, 100, 255, 180);
			Fvector Last = Zone.Position; Last.x += Zone.Radius;
			for (u32 Index = 1; Index <= 24; ++Index)
			{
				const float Angle = PI_MUL_2 * float(Index) / 24.f;
				Fvector Point = Zone.Position; Point.x += cosf(Angle) * Zone.Radius; Point.z += sinf(Angle) * Zone.Radius;
				Primitives.append_line(Last, Point, Color); Last = Point;
			}
			string256 Label;
			xr_sprintf(Label, "%s | группа %u | %.1f с", RussianLabel(Zone.Kind), Zone.Group, (Zone.Until - Device.dwTimeGlobal) * .001f);
			Primitives.append_text3d(Zone.Position, shared_str(Label), Color);
		}
		for (const SMarker& Marker : MarkerSnapshot)
		{
			if (!Marker.Active)
			{
				continue;
			}
			CObject* Object = Level().Objects.net_Find(Marker.Monster);
			CBaseMonster* Monster = Object ? Object->cast_base_monster() : nullptr;
			if (!Monster || Monster->getDestroy() || !Monster->g_Alive())
			{
				continue;
			}
			const Fvector Center = Monster->Center();
			if (Marker.Sight && g_actor && Monster->EnemyMan.see_enemy_now(g_actor))
			{
				Primitives.append_sphere(Center, std::max(.5f, Monster->Radius()) + .05f, color_rgba(70, 255, 70, 255), 0);
			}
			if (Marker.CloseDetection && g_actor && !Monster->EnemyTrackingLive &&
				Monster->EnemyCloseDetectionRadius > 0.f && Monster->EnemyMan.is_enemy(g_actor) &&
				Monster->Position().distance_to_sqr(g_actor->Position()) <=
					Monster->EnemyCloseDetectionRadius * Monster->EnemyCloseDetectionRadius)
			{
				Primitives.append_sphere(Center, std::max(.5f, Monster->Radius()) + .85f, color_rgba(255, 255, 255, 255), 0);
			}
			if (Marker.Call)
			{
				Primitives.append_sphere(Center, std::max(.5f, Monster->Radius()) + .65f, color_rgba(255, 70, 220, 255), 0);
				string128 CallLabel;
				const float Age = float(CallDisplayTime - (Marker.CallUntil - Device.dwTimeGlobal)) * .001f;
				xr_sprintf(CallLabel, "КЛИЧ: %.1f с назад", Age);
				Primitives.append_text3d(Center, shared_str(CallLabel), color_rgba(255, 70, 220, 255));
			}
		}
		Primitives.dbg_font->SetHeight(PreviousFontHeight);
	}
};

inline CMonsterEnemySharingDebug& MonsterEnemySharingDebug()
{
	static CMonsterEnemySharingDebug Debug;
	return Debug;
}
