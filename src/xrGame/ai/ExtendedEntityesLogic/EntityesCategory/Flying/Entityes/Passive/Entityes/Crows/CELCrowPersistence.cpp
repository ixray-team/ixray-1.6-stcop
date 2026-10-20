#include "stdafx.h"
#include <cmath>
#include <algorithm>
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowPersistence.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowEntity.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowServer.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingGeometry.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingSettings.h"
#include "ai_space.h"
#include "alife_simulator.h"
#include "alife_object_registry.h"
#include "alife_time_manager.h"
#include "game_graph.h"
#include "Level.h"
#include "../xrCore/Save/SaveInterface.h"

// Связывает согласование online-состояния вороны с ALife с необходимыми службами и состоянием владельца.
CELCrowPersistence::CELCrowPersistence(CELCrowEntity& OwnerEntity, const SELCrowSettings& CrowSettings, SELCrowState& CrowState) : Owner(OwnerEntity), Settings(CrowSettings), State(CrowState) {}
// Находит серверное ALife-состояние текущей вороны.
CELCrowServer* CELCrowPersistence::ResolveServer() const
{
	if (!Owner.Local() || !ai().get_alife())
	{
		return nullptr;
	}
	return smart_cast<CELCrowServer*>(ai().alife().objects().object(Owner.ID(), true));
}
// Проверяет тело и размещает ворону при переходе в online.
bool CELCrowPersistence::Place()
{
	const Fbox& Bounds = Level().ObjectSpace.GetBoundingVolume();
	Fvector Candidate;
	Fvector Normal;
	EELGeometryResult Query;
	if (!HasPlacementSupport)
	{
		Candidate = Owner.Position();
		const float Angle = float(Owner.ID()) * 2.39996323f + float(PlacementAttempt) * 2.39996323f;
		const float Radius = Settings.PerchMaxRadius * float(PlacementAttempt % Settings.Attempts) / float(Settings.Attempts);
		Candidate.x += std::cos(Angle) * Radius;
		Candidate.z += std::sin(Angle) * Radius;
		clamp(Candidate.x, Bounds.min.x + Settings.GroundOffset, Bounds.max.x - Settings.GroundOffset);
		clamp(Candidate.z, Bounds.min.z + Settings.GroundOffset, Bounds.max.z - Settings.GroundOffset);
		Candidate.y = Bounds.max.y + 1.f;
		Query = CELFlyingGeometry::QuerySupport(Candidate, Bounds.max.y - Bounds.min.y + 2.f, PlacementContact, Normal, &Owner);
		if (Query == EELGeometryResult::Pending)
		{
			return false;
		}
		if (Query == EELGeometryResult::Blocked || Normal.y < std::cos(deg2rad(Settings.MaxSlope)))
		{
			++PlacementAttempt;
			return false;
		}
		HasPlacementSupport = true;
	}
	Candidate = PlacementContact;
	Candidate.y += State.Rest == EELCrowRest::None || State.Rest == EELCrowRest::Corpse ? State.Altitude : Settings.GroundOffset;
	if (!Bounds.contains(Candidate))
	{
		++PlacementAttempt;
		HasPlacementSupport = false;
		return false;
	}
	const SELFlyingSettings& FlightSettings = CELFlyingSettings::Read(*Owner.cNameSect());
	Query = CELFlyingGeometry::QueryBody(Candidate, FlightSettings.CollisionRadius, FlightSettings.CollisionCenterHeight, &Owner);
	if (Query == EELGeometryResult::Pending)
	{
		return false;
	}
	if (Query == EELGeometryResult::Blocked)
	{
		++PlacementAttempt;
		HasPlacementSupport = false;
		return false;
	}
	// This is spawn/online projection only, never recovery of an active movement command.
	float Heading;
	float Pitch;
	float Bank;
	Owner.XFORM().getHPB(Heading, Pitch, Bank);
	Owner.XFORM().setHPB(Heading, 0.f, 0.f);
	Owner.Position() = Candidate;
	Owner.spatial_move();
	if (State.Rest == EELCrowRest::Corpse)
	{
		State.Rest = EELCrowRest::None;
	}
	State.NeedsPlacement = false;
	return true;
}
// Восстанавливает состояние поведения из серверного представления.
bool CELCrowPersistence::Restore()
{
	if (HasRestored)
	{
		return true;
	}
	if (!HasReadServer)
	{
		CELCrowServer* Server = ResolveServer();
		if (Server && Server->State.Valid && (!HasClientState || Server->State.UpdatedOffline))
		{
			State = Server->State;
			State.Enabled = Settings.Enabled;
		}
		else if (!HasClientState)
		{
			State.NeedsPlacement = true;
		}
		HasReadServer = true;
	}
	if (State.NeedsPlacement || State.UpdatedOffline)
	{
		State.NeedsPlacement = true;
		if (!Place())
		{
			return false;
		}
	}
	State.UpdatedOffline = false;
	State.Valid = true;
	HasRestored = true;
	Sync(true);
	return true;
}
// Передаёт актуальное online-состояние вороны серверному объекту.
void CELCrowPersistence::Sync(bool Forced)
{
	if (!HasRestored || (!Forced && s32(Device.dwTimeGlobal - NextSync) < 0))
	{
		return;
	}
	NextSync = Device.dwTimeGlobal + u32(Settings.SyncInterval * 1000.f);
	CELCrowServer* Server = ResolveServer();
	if (!Server)
	{
		return;
	}
	State.Time = ai().alife().time_manager().game_time();
	State.Valid = true;
	State.UpdatedOffline = false;
	State.NeedsPlacement = false;
	Server->State = State;
}
// Сохраняет или восстанавливает состояние через штатный интерфейс сериализации.
void CELCrowPersistence::Serialize(ISaveObject& Object)
{
	if (Object.IsSave())
	{
		Sync(true);
	}
	BEGIN_CHUNK(Object, "ScavengerCrowBehavior")
	{
		bool LegacyEnabled = Settings.Enabled;
		float LegacySafeTime = 0.f;
		bool LegacyHasHome = false;
		Fvector LegacyHome = Owner.Position();
		Object << LegacyEnabled << State.Satiety << LegacySafeTime << LegacyHasHome;
		Object << LegacyHome.x << LegacyHome.y << LegacyHome.z;
	}
	BEGIN_CHUNK(Object, "ExtendedCrowState")
	{
		u8 Version = 1;
		Object << Version;
		if (Version != 1)
		{
			return;
		}
		u8 Rest = u8(State.Rest);
		Object << State.Fear << State.Remaining << State.Altitude << Rest << State.Corpse;
		Object << State.Danger.x << State.Danger.y << State.Danger.z;
		Object << State.Rain.Observed << State.Rain.Active << State.Rain.Remaining;
		Object << State.Night.Observed << State.Night.Active << State.Night.Remaining;
		if (!Object.IsSave())
		{
			State.Rest = Rest <= u8(EELCrowRest::Night) ? EELCrowRest(Rest) : EELCrowRest::None;
			State.Valid = true;
		}
	}
	if (!Object.IsSave())
	{
		if (!_valid(State.Satiety) || !_valid(State.Fear) || !_valid(State.Remaining) || !_valid(State.Altitude) || !_valid(State.Danger) || !_valid(State.Rain.Remaining) || !_valid(State.Night.Remaining))
		{
			State = SELCrowState();
		}
		clamp(State.Satiety, 0.f, 1.f);
		clamp(State.Fear, 0.f, 1.f);
		State.Altitude = std::max(1.f, State.Altitude);
		State.Enabled = Settings.Enabled;
		State.Remaining = std::max(0.f, State.Remaining);
		State.Rain.Remaining = std::max(0.f, State.Rain.Remaining);
		State.Night.Remaining = std::max(0.f, State.Night.Remaining);
		HasClientState = true;
		HasReadServer = false;
		HasRestored = false;
		HasPlacementSupport = false;
	}
}
