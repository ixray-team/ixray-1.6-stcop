#include "FFxExtendedEntityesLogic/stdafx.h"
#include <cmath>
#include <algorithm>
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELSharedCrowMemory.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingGeometry.h"
#include "FFxExtendedEntityesLogic/Navigation/CELSharedNavigationScheduler.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "xrGame/entity_alive.h"
#include "xrGame/Level.h"

// Находит фактическую доступную точку контакта с поверхностью пищи.
bool CELSharedCrowMemory::FoodContact(CGameObject& Bird, CEntityAlive& Corpse, const Fvector& From, Fvector& Point, bool HasReservedRay)
{
	if (!HasReservedRay && !ExtendedEntityes::Navigation().TryGeometryRays(1))
	{
		return false;
	}
	Fvector Direction;
	Corpse.Center(Direction);
	Direction.sub(From);
	const float Distance = Direction.magnitude();
	if (Distance <= EPS_L)
	{
		return false;
	}
	Direction.div(Distance);
	collide::rq_result Hit;
	if (!Level().ObjectSpace.RayPick(From, Direction, Distance + .1f, collide::rqtBoth, Hit, &Bird) || Hit.O != &Corpse)
	{
		return false;
	}
	Point.mad(From, Direction, Hit.range);
	return true;
}

// Проверяет, не пересекается ли тело вороны с трупом при кормлении.
bool CELSharedCrowMemory::BodyOutsideCorpse(CEntityAlive& Corpse, const Fvector& Root, float Radius, bool HasReservedRays)
{
	if (!Corpse.collidable.model || (!HasReservedRays && !ExtendedEntityes::Navigation().TryGeometryRays(4)))
	{
		return false;
	}
	const float ProbeRadius = std::max(.08f, Radius);
	Fvector Center = Root;
	Center.y += .02f;
	Fvector Start;
	Fvector Direction;
	u32 Axis;
	collide::rq_results Hits;
	for (Axis = 0; Axis < 3; ++Axis)
	{
		Start = Center;
		Start[Axis] -= ProbeRadius;
		Direction.set(0.f, 0.f, 0.f);
		Direction[Axis] = 1.f;
		collide::ray_defs Ray(Start, Direction, ProbeRadius * 2.f, CDB::OPT_FULL_TEST, collide::rqtObject);
		if (Level().ObjectSpace.RayQuery(Hits, Corpse.collidable.model, Ray))
		{
			return false;
		}
	}
	Fvector CorpseCenter;
	Corpse.Center(CorpseCenter);
	Fvector Away;
	Away.sub(Center, CorpseCenter).normalize_safe();
	Start.mad(Center, Away, Corpse.Radius() * 2.f + ProbeRadius);
	Direction.sub(Center, Start);
	const float Distance = Direction.magnitude();
	if (Distance <= EPS_L)
	{
		return false;
	}
	Direction.div(Distance);
	collide::ray_defs Ray(Start, Direction, Distance + ProbeRadius, CDB::OPT_FULL_TEST, collide::rqtObject);
	return !Level().ObjectSpace.RayQuery(Hits, Corpse.collidable.model, Ray);
}

// Временно исключает неудачное место посадки из повторного выбора.
void CELSharedCrowMemory::RejectPerch(u16 CorpseIdentifier, const Fvector& Contact)
{
	xr_map<u16, SCorpse>::iterator Found = Corpses.find(CorpseIdentifier);
	if (Found == Corpses.end())
	{
		return;
	}
	xr_vector<SPerch>::iterator Perch;
	for (Perch = Found->second.Perches.begin(); Perch != Found->second.Perches.end(); ++Perch)
	{
		if (Perch->Contact.distance_to_sqr(Contact) < .0001f)
		{
			Perch->Valid = false;
		}
	}
}

// Строит ограниченный набор мест кормления вокруг тела.
const xr_vector<CELSharedCrowMemory::SPerch>* CELSharedCrowMemory::CorpsePerches(CGameObject& Bird, u16 CorpseIdentifier, float Gap, float GroundOffset)
{
	CEntityAlive* Corpse = ExtendedEntityes::FindEntity(CorpseIdentifier);
	if (!Corpse || Corpse->g_Alive())
	{
		Invalidate(CorpseIdentifier);
		return nullptr;
	}
	Fvector Center;
	Corpse->Center(Center);
	const Fbox& Bounds = Corpse->BoundingBox();
	xr_map<u16, SCorpse>::iterator Found = Corpses.find(CorpseIdentifier);
	if (Found != Corpses.end() &&
		((Settings.CorpseRefresh && Device.dwTimeGlobal - Found->second.Created > Settings.CorpseRefresh) ||
		 Found->second.Center.distance_to_sqr(Center) > Settings.CorpseMoveDistance * Settings.CorpseMoveDistance ||
		 Found->second.Position.distance_to_sqr(Corpse->Position()) > Settings.CorpseMoveDistance * Settings.CorpseMoveDistance ||
		 Found->second.Forward.distance_to_sqr(Corpse->XFORM().k) > .0004f ||
		 Found->second.Bounds.min.distance_to_sqr(Bounds.min) > Settings.CorpseMoveDistance * Settings.CorpseMoveDistance ||
		 Found->second.Bounds.max.distance_to_sqr(Bounds.max) > Settings.CorpseMoveDistance * Settings.CorpseMoveDistance))
	{
		Corpses.erase(Found);
		Found = Corpses.end();
	}
	if (Found == Corpses.end())
	{
		if (Settings.CorpseLimit && Corpses.size() >= Settings.CorpseLimit)
		{
			return nullptr;
		}
		SCorpse Record;
		Record.Center = Center;
		Record.Position = Corpse->Position();
		Record.Forward = Corpse->XFORM().k;
		Record.Bounds = Bounds;
		Record.Gap = Gap;
		Record.GroundOffset = GroundOffset;
		Record.Created = Device.dwTimeGlobal;
		Found = Corpses.emplace(CorpseIdentifier, std::move(Record)).first;
	}
	SCorpse& Record = Found->second;
	Record.LastUse = Device.dwTimeGlobal;
	Record.Requester = Bird.ID();
	Record.Pending = u32(std::count_if(Record.Perches.begin(), Record.Perches.end(), [](const SPerch& Perch)
									   { return Perch.Valid; })) < std::max(Settings.PerchCount, FoodUsers(CorpseIdentifier) + 1);
	return &Record.Perches;
}

// Проверяет и формирует конкретное место посадки у пищи.
bool CELSharedCrowMemory::BuildPerch(u16 CorpseIdentifier, SCorpse& Record)
{
	CEntityAlive* Bird = ExtendedEntityes::FindEntity(Record.Requester);
	CEntityAlive* Corpse = ExtendedEntityes::FindEntity(CorpseIdentifier);
	if (!Bird || !Corpse || Corpse->g_Alive())
	{
		Record.Pending = false;
		return false;
	}
	// Reserve the whole bounded probe before advancing the candidate or failure count.
	if (!ExtendedEntityes::Navigation().TryGeometryRays(6))
	{
		return false;
	}
	const float Angle = float(Record.Attempt++) * 2.39996323f + float(CorpseIdentifier) * .13f;
	const Fvector Away = {std::cos(Angle), 0.f, std::sin(Angle)};
	Fvector From;
	Fvector Flesh;
	Fvector Contact;
	Fvector Normal;
	From.mad(Record.Center, Away, Corpse->Radius() + 1.f);
	bool Valid = FoodContact(*Bird, *Corpse, From, Flesh, true);
	if (Valid)
	{
		From.mad(Flesh, Away, std::max(.23f, Record.Gap));
		From.y = Record.Center.y + 2.f;
		Valid = CELFlyingGeometry::Support(From, 5.f, Contact, Normal, Bird, true) && Normal.y > .8f;
	}
	if (Valid)
	{
		From = Contact;
		From.y += Record.GroundOffset;
		Valid = BodyOutsideCorpse(*Corpse, From, .16f, true) && !std::any_of(Record.Perches.begin(), Record.Perches.end(), [&Contact](const SPerch& Perch)
																			 { return Perch.Valid && Perch.Contact.distance_to_sqr(Contact) < .04f; });
	}
	if (!Valid)
	{
		if (++Record.Failures >= Settings.PerchAttempts)
		{
			Record.Failures = 0;
			Record.RetryAfter = Device.dwTimeGlobal + Settings.PerchRetry;
		}
		return false;
	}
	xr_vector<SPerch>::iterator Unused = std::find_if(Record.Perches.begin(), Record.Perches.end(), [](const SPerch& Perch)
													  { return !Perch.Valid; });
	if (Unused != Record.Perches.end())
	{
		*Unused = {Contact, Flesh, true};
	}
	else
	{
		Record.Perches.push_back({Contact, Flesh, true});
	}
	Record.Failures = 0;
	Record.Pending = false;
	return true;
}
