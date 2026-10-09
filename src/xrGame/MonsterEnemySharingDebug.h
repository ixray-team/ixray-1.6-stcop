#pragma once

#include "../xrCore/Kernel/xrSyncronize.h"
#include "../xrCore/Kernel/EngineExternal.h"
#include "../xrEngine/MonsterLogicTelemetry.h"
#include "HUDManager.h"
#include "Level.h"
#include "Actor.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "ai/monsters/monster_home.h"
#include "ai/monsters/state_manager.h"
#include "ai_space.h"
#include "level_graph.h"

class CMonsterEnemySharingDebug
{
	static constexpr u32 MaxLinks = 32;
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
	std::atomic<bool> Enabled{false};
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
		GoalCursor = 0;
		MarkerCursor = 0;
		Generation = g_MonsterLogicTelemetry.Generation.load(std::memory_order_relaxed);
		OwnerLevel = g_pGameLevel;
	}
public:
	bool IsEnabled() const { return Enabled.load(std::memory_order_relaxed); }
	u32 GetEpoch() const { return Epoch.load(std::memory_order_relaxed); }
	void SetEnabled(bool Value)
	{
		Enabled.store(Value, std::memory_order_relaxed);
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
	void Draw(LevelInspector& Primitives)
	{
		if (!IsEnabled() || !g_pGameLevel)
		{
			return;
		}
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
			// Ordinary receivers use memory; live tracking remains explicit for psi.
			Fvector Point = Receiver->EnemyTrackingLive ? Receiver->EnemyMan.GetKnownEnemyPosition(Target) : Entry->second.position;
			Point.y += .2f;
			bool DrawPoint = true;
			for (u32 Index = 0; Index < PointCount; ++Index)
			{
				if (DrawnPoints[Index].distance_to_sqr(Point) < .25f)
				{
					DrawPoint = false;
					break;
				}
			}
			if (DrawPoint)
			{
				DrawnPoints[PointCount++] = Point;
				for (u32 Axis = 0; Axis < 3; ++Axis)
				{
					Fvector From = Point, To = Point;
					From[Axis] -= .65f;
					To[Axis] += .65f;
					Primitives.append_line(From, To, GoalColor);
				}
				const Fvector Up = {0.f, 1.f, 0.f};
				Primitives.append_lines_arrow(Point, Up, 1.8f, GoalColor);
				Fvector LabelPosition = Point;
				LabelPosition.y += 2.f;
				Primitives.append_text3d(LabelPosition, shared_str(Receiver->EnemyTrackingLive ? "PSI LIVE" : "LAST KNOWN"), GoalColor);
			}
			const bool Selected = Receiver->EnemyMan.get_enemy() == Target;
			Primitives.append_line(Receiver->Center(), Point, color_rgba(255, 220, 0, Selected ? 180 : 70));
			const char* State = !Selected ? "OTHER TARGET" :
				Receiver->PursuitSearchingAround ? (Receiver->EnemyMan.see_enemy_now(Target) ? "SEARCH AROUND / SEES" : "SEARCH AROUND") :
				Receiver->EnemyMan.see_enemy_now(Target) ? "SEES" :
				Receiver->EnemyTrackingLive ? "PSI LIVE" :
				!ai().level_graph().valid_vertex_id(Entry->second.vertex) ? "NO AI NODE" :
				!Receiver->Home->at_home(Entry->second.position) ? "HOME LIMIT" :
				(Receiver->HasDamagePanic() || is_state(Receiver->StateMan->get_state_type(), eStatePanic)) ? "PANIC" :
				Entry->second.ReachedPointTime != 0 ? "SEARCH AT POINT" :
				Receiver->HasReachedEnemyPoint(Entry->second.position, Entry->second.vertex) ? "AT POINT" :
				Receiver->LastKnownPursuitWasActive ? "GO TO POINT" : "WAIT / SEARCH";
			string128 Label;
			const bool Updated = Entry->second.time != Goal.ObservationTime || Entry->second.position.distance_to_sqr(Goal.SharedPosition) > .01f;
			const float Remaining = float(Receiver->EnemyMemory.GetRemainingMemoryTime(Entry->second)) * .001f;
			xr_sprintf(Label, "#%u %s | %s | memory %.1fs", u32(Receiver->ID()), State, Updated ? "UPDATED" : "RADIO", Remaining);
			Fvector LabelPosition = Receiver->Center();
			LabelPosition.y += 1.5f;
			Primitives.append_text3d(LabelPosition, shared_str(Label), GoalColor);
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
				string64 CallLabel;
				const float Age = float(CallDisplayTime - (Marker.CallUntil - Device.dwTimeGlobal)) * .001f;
				xr_sprintf(CallLabel, "CALL %.1fs AGO", Age);
				Primitives.append_text3d(Center, shared_str(CallLabel), color_rgba(255, 70, 220, 255));
			}
		}
	}
};

inline CMonsterEnemySharingDebug& MonsterEnemySharingDebug()
{
	static CMonsterEnemySharingDebug Debug;
	return Debug;
}
