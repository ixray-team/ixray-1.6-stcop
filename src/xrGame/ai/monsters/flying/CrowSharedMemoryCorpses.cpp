#include "StdAfx.h"
#include "CrowSharedMemory.h"
#include "flying_movement_controller.h"
#include "../../../Level.h"
#include "../../../entity_alive.h"
#include "../../../../xrCore/Collision/xr_area.h"

void CCrowSharedMemory::RejectPerch(u32 Corpse, const Fvector& Contact)
{
	auto Entry = Corpses.find(Corpse);
	if (Entry == Corpses.end()) return;
	for (auto& Perch : Entry->second.Perches)
	{
		if (Perch.Contact.distance_to_sqr(Contact) < .0001f) Perch.Valid = false;
	}
}

bool CCrowSharedMemory::FoodContact(CGameObject& Bird, CEntityAlive& Corpse, const Fvector& From, Fvector& Point)
{
	Fvector Centre;
	Corpse.Center(Centre);
	const Fbox& Box = Corpse.BoundingBox();
	float Best = type_max(float);
	bool Found = false;
	for (u32 Index = 0; Index < 3; ++Index)
	{
		Fvector Target = Centre;
		Target.y += (Box.max.y - Box.min.y) * float(Index) * .2f;
		Fvector Direction;
		Direction.sub(Target, From);
		const float Range = Direction.magnitude();
		if (Range < EPS_L) continue;
		Direction.div(Range);
		collide::rq_result Hit;
		if (!Level().ObjectSpace.RayPick(From, Direction, Range + .1f, collide::rqtBoth, Hit, &Bird) || Hit.O != &Corpse) continue;
		if (Hit.range < Best)
		{
			Best = Hit.range;
			Point.mad(From, Direction, Hit.range);
			Found = true;
		}
	}
	return Found;
}

bool CCrowSharedMemory::BodyOutsideCorpse(CGameObject& Bird, CEntityAlive& Corpse, const Fvector& Root, float Radius)
{
	// Three short cross-sections catch existing overlap as well as a blocked step.
	// Query only this ragdoll, avoiding a spatial search through the flock.
	if (!Corpse.collidable.model) return false;
	const float R = std::max(.08f, Radius);
	Fvector Centre = Root;
	Centre.y += .02f;
	for (u32 Axis = 0; Axis < 3; ++Axis)
	{
		Fvector Start = Centre, Direction = {0,0,0};
		Start[Axis] -= R;
		Direction[Axis] = 1.f;
		collide::rq_results Hits;
		collide::ray_defs Ray(Start, Direction, R * 2.f, CDB::OPT_FULL_TEST, collide::rqtObject);
		if (Level().ObjectSpace.RayQuery(Hits, Corpse.collidable.model, Ray)) return false;
	}
	// Probe from outside directly towards the proposed body centre.
	Fvector CorpseCentre;
	Corpse.Center(CorpseCentre);
	Fvector Away;
	Away.sub(Centre, CorpseCentre);
	if (Away.square_magnitude() < EPS_S) return false;
	Away.normalize();
	Fvector Outside, Direction;
	Outside.mad(Centre, Away, Corpse.Radius() * 2.f + R);
	Direction.sub(Centre, Outside);
	const float Range = Direction.magnitude();
	Direction.div(Range);
	collide::rq_results Hits;
	collide::ray_defs Ray(Outside, Direction, Range + R, CDB::OPT_FULL_TEST, collide::rqtObject);
	if (Level().ObjectSpace.RayQuery(Hits, Corpse.collidable.model, Ray)) return false;
	return true;
}

const xr_vector<CCrowSharedMemory::SPerch>* CCrowSharedMemory::CorpsePerches(CGameObject& Bird, u32 Id, float Gap, float GroundOffset)
{
	InvalidationFrame = u32(-1);
	Maintain();
	auto* Object = Level().Objects.net_Find(Id);
	auto* Corpse = Object ? smart_cast<CEntityAlive*>(Object) : nullptr;
	if (!Corpse || Corpse->getDestroy() || Corpse->g_Alive())
	{
		InvalidateObject(Id);
		return nullptr;
	}
	Fvector Centre;
	Corpse->Center(Centre);
	const Fbox& Box = Corpse->BoundingBox();
	const u32 Now = Device.dwTimeGlobal;
	auto Entry = Corpses.find(Id);
	if (Entry != Corpses.end() && ((Config.CorpseRefreshMs && u32(Now - Entry->second.Created) > Config.CorpseRefreshMs) ||
		Entry->second.Centre.distance_to_sqr(Centre) > _sqr(Config.CorpseMoveDistance) || Entry->second.Position.distance_to_sqr(Corpse->Position()) > _sqr(Config.CorpseMoveDistance) ||
		Entry->second.Forward.distance_to_sqr(Corpse->XFORM().k) > .0004f ||
		Entry->second.BoxMin.distance_to_sqr(Box.min) > _sqr(Config.CorpseMoveDistance) || Entry->second.BoxMax.distance_to_sqr(Box.max) > _sqr(Config.CorpseMoveDistance) ||
		std::abs(Entry->second.Gap - Gap) > .001f || std::abs(Entry->second.GroundOffset - GroundOffset) > .001f))
	{
		Corpses.erase(Entry);
		Entry = Corpses.end();
	}
	if (Entry == Corpses.end())
	{
		if (Config.MaxCachedCorpses && Corpses.size() >= Config.MaxCachedCorpses)
		{
			auto Oldest = std::max_element(Corpses.begin(), Corpses.end(), [&](const auto& A, const auto& B)
				{ return u32(Now - A.second.LastUse) < u32(Now - B.second.LastUse); });
			Corpses.erase(Oldest);
		}
		SCorpse Value;
		Value.Id = Id;
		Value.Centre = Centre; Value.Position = Corpse->Position(); Value.Forward = Corpse->XFORM().k;
		Value.BoxMin = Box.min; Value.BoxMax = Box.max;
		Value.Gap = Gap; Value.GroundOffset = GroundOffset;
		Value.Created = Value.LastUse = Now;
		Entry = Corpses.emplace(Id, std::move(Value)).first;
	}
	Entry->second.LastUse = Now;
	// Demand comes from birds approaching this corpse, not the total flock size.
	const u32 Desired = std::max(Config.PerchCount, FoodUsers(Id) + 1u);
	u32 Ready = u32(std::count_if(Entry->second.Perches.begin(), Entry->second.Perches.end(), [](const SPerch& Perch) { return Perch.Valid; }));
	if (Ready >= Desired) return &Entry->second.Perches;
	if (Entry->second.FailedAttempts >= Config.PerchAttempts)
	{
		if (u32(Now - Entry->second.RetryStarted) < Config.PerchRetryMs) return &Entry->second.Perches;
		Entry->second.FailedAttempts = 0;
	}
	if (ProbeFrame != Device.dwFrame)
	{
		ProbeFrame = Device.dwFrame;
		ProbesRemaining = Config.PerchProbesPerFrame;
	}
	while (Ready < Desired && ProbesRemaining && Entry->second.FailedAttempts < Config.PerchAttempts)
	{
		--ProbesRemaining;
		++Entry->second.FailedAttempts;
		Entry->second.RetryStarted = Now;
		// Golden-angle sequence does not tie the number of positions to a fixed ring.
		const float Angle = float(Entry->second.Attempt++) * 2.39996323f + float(Id) * .13f;
		Fvector Away = {cosf(Angle),0,sinf(Angle)};
		Fvector From;
		From.mad(Centre, Away, Corpse->Radius() + 1.f);
		Fvector Flesh;
		if (!FoodContact(Bird, *Corpse, From, Flesh)) continue;
		Fvector Probe;
		Probe.mad(Flesh, Away, std::max(.23f, Gap));
		Probe.y = Centre.y + 2.f;
		Fvector Contact, Normal;
		if (!CFlyingMovementController::support(Bird, Probe, 5.f, Contact, Normal)) continue;
		Fvector Root = Contact;
		Root.y += GroundOffset;
		if (!BodyOutsideCorpse(Bird, *Corpse, Root, .16f)) continue;
		if (std::any_of(Entry->second.Perches.begin(), Entry->second.Perches.end(), [&](const SPerch& Perch)
			{ return Perch.Valid && Perch.Contact.distance_to_sqr(Contact) < .04f; })) continue;
		auto Unused = std::find_if(Entry->second.Perches.begin(), Entry->second.Perches.end(), [](const SPerch& Perch) { return !Perch.Valid; });
		if (Unused != Entry->second.Perches.end()) *Unused = {Contact, Flesh, true};
		else Entry->second.Perches.push_back({Contact, Flesh, true});
		++Ready;
		Entry->second.FailedAttempts = 0;
	}
	return &Entry->second.Perches;
}
