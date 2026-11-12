#include "FFxExtendedEntityesLogic/stdafx.h"
#include <cmath>
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedSight.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedEnvironment.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "FFxExtendedEntityesLogic/CELSharedPerceptionBudget.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/Level.h"
#include "xrEngine/GameMtlLib.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"

namespace ExtendedEntityesSight
{
struct SContact
{
	u32 CheckedAt;
	u32 OwnerGeneration;
	u32 TargetGeneration;
	bool Visible;
};
using SContactKey = std::pair<ALife::_OBJECT_ID, ALife::_OBJECT_ID>;
xr_map<SContactKey, SContact> Contacts;
struct SScentContact
{
	u32 CheckedAt;
	u32 OwnerGeneration;
	u32 TargetGeneration;
	u32 Until;
	float Radius;
	bool Visible;
};
xr_map<SContactKey, SScentContact> ScentContacts;
struct SEvasionContact
{
	ALife::_OBJECT_ID Target;
	u32 OwnerGeneration;
	u32 TargetGeneration;
	u32 Until;
	u32 CheckAfter;
	u32 VerifiedAt;
	bool Visible;
};
xr_map<ALife::_OBJECT_ID, SEvasionContact> EvasionContacts;

float BodyDistance(CBaseMonster& Owner, const CObject& Target)
{
	Fmatrix Inverse;
	Inverse.invert(Target.XFORM());
	Fvector Point;
	Inverse.transform_tiny(Point, Owner.eye_matrix.c);
	Fvector Nearest = Point;
	const Fbox& Box = Target.BoundingBox();
	clamp(Nearest.x, Box.min.x, Box.max.x);
	clamp(Nearest.y, Box.min.y, Box.max.y);
	clamp(Nearest.z, Box.min.z, Box.max.z);
	return Point.distance_to(Nearest);
}

struct SQuery
{
	CBaseMonster* Observer = nullptr;
	CObject* Target = nullptr;
	bool IsVisible = true;
	bool SkipLeaves = false;
};

// Both static and dynamic hits use the native material visibility policy.
bool Inspect(collide::rq_result& Hit, LPVOID Context)
{
	SQuery& Query = *static_cast<SQuery*>(Context);
	if (Hit.O == Query.Target)
	{
		Query.IsVisible = true;
		return false;
	}
	if (!Hit.O && Query.SkipLeaves)
	{
		const CDB::TRI& Triangle = Level().ObjectSpace.GetStaticTris()[Hit.element];
		const SGameMtl* Material = GMLib.GetMaterialByIdx(Triangle.material);
		if (Material && (Material->Flags.is(SGameMtl::flPassable) || Material->Flags.is(SGameMtl::flActorObstacle)))
		{
			const char* Name = Material->m_Name.c_str();
			if (Name && (!xr_strcmp(Name, "materials\\bush") || !xr_strcmp(Name, "materials\\bush_sux") || !xr_strcmp(Name, "materials\\leaves") || !xr_strcmp(Name, "materials\\foliage"))) { return true; }
		}
	}
	if (Query.Observer->feel_vision_mtl_transp(Hit.O, Hit.element) >= 1.f)
	{
		return true;
	}
	Query.IsVisible = false;
	return false;
}
} // namespace ExtendedEntityesSight

// Проверяет видимость цели, отличая ожидание бюджета от закрытого обзора.
EELSightResult CELSharedSight::Check(CBaseMonster& OwnerMonster, CObject& Candidate, float Angle, float Radius, bool UseWeather)
{
	return CheckFrom(OwnerMonster, Candidate, Angle, Radius, OwnerMonster.eye_matrix.c, UseWeather);
}

// Проверяет обзор из заданной точки той же нативной трассировкой и общей квотой.
EELSightResult CELSharedSight::CheckFrom(CBaseMonster& OwnerMonster, CObject& Candidate, float Angle, float Radius, const Fvector& StartPosition, bool UseWeather)
{
	const auto Report = [&](EELSightResult Result, const char* Reason)
	{
		if (CELSharedDebug::Instance().CanLogCombat(OwnerMonster.Position()) && s32(Device.dwTimeGlobal - OwnerMonster.NextSightTraceAt) >= 0)
		{
			OwnerMonster.NextSightTraceAt = Device.dwTimeGlobal + 1000;
			Msg("[monsters_debug] SIGHT t=%u id=%u candidate=%u result=%s radius=%.1f angle=%.1f", Device.dwTimeGlobal, OwnerMonster.ID(), Candidate.ID(), Reason, Radius, Angle);
		}
		return Result;
	};
	CEntityAlive* TargetEntity = Candidate.cast_entity_alive();
	if (TargetEntity && ExtendedEntityes::IsIgnored(*TargetEntity))
	{
		return Report(EELSightResult::Occluded, "actor_ignored");
	}
	Fvector TargetPosition;
	Candidate.Center(TargetPosition);
	Fvector Direction;
	Direction.sub(TargetPosition, StartPosition);
	const float Distance = Direction.magnitude();
	if (Distance <= EPS_L || Distance > Radius * (UseWeather ? CELSharedEnvironment::Vision(CELSharedSettings::Profile(*OwnerMonster.cNameSect())) : 1.f))
	{
		return Report(EELSightResult::Occluded, "distance_or_weather");
	}
	Direction.div(Distance);
	if (Angle < 360.f && OwnerMonster.Direction().dotproduct(Direction) < std::cos(deg2rad(Angle * .5f)))
	{
		return Report(EELSightResult::Occluded, "outside_view_cone");
	}
	if (!ExtendedEntityes::PerceptionBudget().TryRay())
	{
		return Report(EELSightResult::WaitingForBudget, "ray_budget");
	}
	ExtendedEntityesSight::SQuery Query;
	Query.Observer = &OwnerMonster;
	Query.Target = &Candidate;
	collide::rq_results Results;
	collide::ray_defs Ray(StartPosition, Direction, Distance, CDB::OPT_FULL_TEST, collide::rqtBoth);
	Level().ObjectSpace.RayQuery(Results, Ray, ExtendedEntityesSight::Inspect, &Query, nullptr, &OwnerMonster);
	return Report(Query.IsVisible ? EELSightResult::Visible : EELSightResult::Occluded, Query.IsVisible ? "visible" : "geometry_occluded");
}

// Сообщает только подтверждённую доступной проверкой видимость цели.
bool CELSharedSight::Visible(CBaseMonster& OwnerMonster, CObject& Candidate, float Angle, float Radius, bool UseWeather)
{
	return Check(OwnerMonster, Candidate, Angle, Radius, UseWeather) == EELSightResult::Visible;
}

// Contact awareness is independent of the view cone, but still requires an unobstructed ray.
EELSightResult CELSharedSight::CheckClose(CBaseMonster& OwnerMonster, CObject& Candidate)
{
	CEntityAlive* Entity = Candidate.cast_entity_alive();
	if (!Entity || !Entity->g_Alive() || Entity->getDestroy() || ExtendedEntityes::IsIgnored(*Entity))
	{
		return EELSightResult::Occluded;
	}
	const float Radius = CELSharedSettings::Profile(*OwnerMonster.cNameSect()).CloseDetectionRadius * CELSharedEnvironment::CloseFactor();
	if (Radius <= 0.f || ExtendedEntityesSight::BodyDistance(OwnerMonster, Candidate) > Radius)
	{
		return EELSightResult::Occluded;
	}
	const ExtendedEntityesSight::SContactKey Key{OwnerMonster.ID(), Candidate.ID()};
	const u32 Now = Device.dwTimeGlobal;
	const u32 OwnerGeneration = ExtendedEntityes::OwnerGeneration(OwnerMonster.ID());
	const u32 TargetGeneration = ExtendedEntityes::OwnerGeneration(Candidate.ID());
	auto Found = ExtendedEntityesSight::Contacts.find(Key);
	if (Found != ExtendedEntityesSight::Contacts.end() && Found->second.OwnerGeneration == OwnerGeneration && Found->second.TargetGeneration == TargetGeneration && Now - Found->second.CheckedAt < 100)
	{
		return Found->second.Visible ? EELSightResult::Visible : EELSightResult::Occluded;
	}
	Fvector Center;
	Candidate.Center(Center);
	const float Distance = Center.distance_to(OwnerMonster.eye_matrix.c);
	// Ray to the actual center also protects against seeing through a floor while inside a large box.
	const EELSightResult Result = Distance <= EPS_L ? EELSightResult::Visible : CheckFrom(OwnerMonster, Candidate, 360.f, Distance + EPS_L, OwnerMonster.eye_matrix.c, false);
	if (Result != EELSightResult::WaitingForBudget)
	{
		if (ExtendedEntityesSight::Contacts.size() >= 1024)
		{
			for (auto It = ExtendedEntityesSight::Contacts.begin(); It != ExtendedEntityesSight::Contacts.end();)
			{
				if (Now - It->second.CheckedAt >= 250)
				{
					It = ExtendedEntityesSight::Contacts.erase(It);
				}
				else
				{
					++It;
				}
			}
		}
		if (ExtendedEntityesSight::Contacts.size() < 1024 || ExtendedEntityesSight::Contacts.find(Key) != ExtendedEntityesSight::Contacts.end())
		{
			ExtendedEntityesSight::Contacts[Key] = {Now, OwnerGeneration, TargetGeneration, Result == EELSightResult::Visible};
		}
	}
	return Result == EELSightResult::WaitingForBudget && HasCloseContact(OwnerMonster, *Entity) ? EELSightResult::Visible : Result;
}

// Native combat consumes the cached result without issuing rays from its hot visibility queries.
bool CELSharedSight::HasCloseContact(CBaseMonster& OwnerMonster, const CEntityAlive& Candidate)
{
	if (HasScentContact(OwnerMonster, Candidate)) { return true; }
	if (!Candidate.g_Alive() || Candidate.getDestroy() || ExtendedEntityes::IsIgnored(Candidate))
	{
		return false;
	}
	const ExtendedEntityesSight::SContactKey Key{OwnerMonster.ID(), Candidate.ID()};
	const auto Found = ExtendedEntityesSight::Contacts.find(Key);
	if (Found == ExtendedEntityesSight::Contacts.end() || !Found->second.Visible || Device.dwTimeGlobal - Found->second.CheckedAt >= 250 || Found->second.OwnerGeneration != ExtendedEntityes::OwnerGeneration(OwnerMonster.ID()) || Found->second.TargetGeneration != ExtendedEntityes::OwnerGeneration(Candidate.ID()))
	{
		return false;
	}
	const float Radius = CELSharedSettings::Profile(*OwnerMonster.cNameSect()).CloseDetectionRadius * CELSharedEnvironment::CloseFactor();
	return Radius > 0.f && ExtendedEntityesSight::BodyDistance(OwnerMonster, Candidate) <= Radius;
}

void CELSharedSight::Remove(ALife::_OBJECT_ID Identifier)
{
	for (auto It = ExtendedEntityesSight::ScentContacts.begin(); It != ExtendedEntityesSight::ScentContacts.end();)
	{
		if (It->first.first == Identifier || It->first.second == Identifier) { It = ExtendedEntityesSight::ScentContacts.erase(It); }
		else { ++It; }
	}
	for (auto It = ExtendedEntityesSight::EvasionContacts.begin(); It != ExtendedEntityesSight::EvasionContacts.end();)
	{
		if (It->first == Identifier || It->second.Target == Identifier)
		{
			It = ExtendedEntityesSight::EvasionContacts.erase(It);
		}
		else
		{
			++It;
		}
	}
	for (auto It = ExtendedEntityesSight::Contacts.begin(); It != ExtendedEntityesSight::Contacts.end();)
	{
		if (It->first.first == Identifier || It->first.second == Identifier)
		{
			It = ExtendedEntityesSight::Contacts.erase(It);
		}
		else
		{
			++It;
		}
	}
}

void CELSharedSight::Clear()
{
	ExtendedEntityesSight::Contacts.clear();
	ExtendedEntityesSight::ScentContacts.clear();
	ExtendedEntityesSight::EvasionContacts.clear();
}

void CELSharedSight::KeepEvasionTarget(CBaseMonster& Owner, CEntityAlive& Target, u32 Until)
{
	if (!CELSharedSettings::Profile(*Owner.cNameSect()).DodgeTrackTarget)
	{
		return;
	}
	const u32 Now = Device.dwTimeGlobal;
	auto& Cache = ExtendedEntityesSight::EvasionContacts;
	auto Found = Cache.find(Owner.ID());
	if (Found != Cache.end() && Found->second.Target == Target.ID() && Found->second.OwnerGeneration == ExtendedEntityes::OwnerGeneration(Owner.ID()) && Found->second.TargetGeneration == ExtendedEntityes::OwnerGeneration(Target.ID()) && s32(Found->second.Until - Now) > 0)
	{
		Found->second.Until = Until;
		return;
	}
	// Called only for the already confirmed target of an accepted maneuver.
	Cache[Owner.ID()] = {Target.ID(), ExtendedEntityes::OwnerGeneration(Owner.ID()), ExtendedEntityes::OwnerGeneration(Target.ID()), Until, Now, Now, true};
}

void CELSharedSight::UpdateEvasionTarget(CBaseMonster& Owner)
{
	auto& Cache = ExtendedEntityesSight::EvasionContacts;
	auto Found = Cache.find(Owner.ID());
	if (Found == Cache.end())
	{
		return;
	}
	auto& Contact = Found->second;
	CEntityAlive* Target = ExtendedEntityes::FindEntity(Contact.Target);
	const u32 Now = Device.dwTimeGlobal;
	if (!Target || !Target->g_Alive() || Target->getDestroy() || ExtendedEntityes::IsIgnored(*Target) || Contact.OwnerGeneration != ExtendedEntityes::OwnerGeneration(Owner.ID()) || Contact.TargetGeneration != ExtendedEntityes::OwnerGeneration(Target->ID()) || s32(Now - Contact.Until) >= 0)
	{
		Cache.erase(Found);
		return;
	}
	if (s32(Now - Contact.CheckAfter) < 0)
	{
		return;
	}
	const auto& Profile = CELSharedSettings::Profile(*Owner.cNameSect());
	Contact.CheckAfter = Now + Profile.DodgeTrackInterval;
	// Ignore only the temporary body-facing cone; range, weather, materials and ray budget remain active.
	const EELSightResult Result = Check(Owner, *Target, 360.f, Owner.eye_range);
	if (Result != EELSightResult::WaitingForBudget)
	{
		Contact.Visible = Result == EELSightResult::Visible;
		Contact.VerifiedAt = Now;
	}
}

bool CELSharedSight::HasEvasionTarget(CBaseMonster& Owner, const CEntityAlive& Target)
{
	const auto Found = ExtendedEntityesSight::EvasionContacts.find(Owner.ID());
	if (Found == ExtendedEntityesSight::EvasionContacts.end())
	{
		return false;
	}
	const auto& Contact = Found->second;
	const auto& Profile = CELSharedSettings::Profile(*Owner.cNameSect());
	return Profile.DodgeTrackTarget && Target.g_Alive() && !Target.getDestroy() && !ExtendedEntityes::IsIgnored(Target) && Contact.Target == Target.ID() && Contact.Visible && s32(Contact.Until - Device.dwTimeGlobal) > 0 && Device.dwTimeGlobal - Contact.VerifiedAt <= std::max(Profile.DodgeVisibilityGrace, Profile.DodgeTrackInterval) && Contact.OwnerGeneration == ExtendedEntityes::OwnerGeneration(Owner.ID()) && Contact.TargetGeneration == ExtendedEntityes::OwnerGeneration(Target.ID());
}

// A recent visual contact permits short-range scent through leaves, never solid obstacles.
EELSightResult CELSharedSight::CheckScent(CBaseMonster& Owner, CEntityAlive& Candidate, u32 Until, float Radius)
{
	using namespace ExtendedEntityesSight;
	const u32 Now = Device.dwTimeGlobal;
	const SContactKey Key{Owner.ID(), Candidate.ID()};
	const u32 OwnerGeneration = ExtendedEntityes::OwnerGeneration(Owner.ID());
	const u32 TargetGeneration = ExtendedEntityes::OwnerGeneration(Candidate.ID());
	if (!Candidate.g_Alive() || Candidate.getDestroy() || ExtendedEntityes::IsIgnored(Candidate) || !CELSharedEnvironment::CanSmell() || s32(Until - Now) <= 0 || Owner.Position().distance_to_sqr(Candidate.Position()) > _sqr(Radius)) { return EELSightResult::Occluded; }
	const auto Previous = ScentContacts.find(Key);
	if (Previous != ScentContacts.end() && Previous->second.OwnerGeneration == OwnerGeneration && Previous->second.TargetGeneration == TargetGeneration && Previous->second.Until == Until && Now - Previous->second.CheckedAt < 100u)
	{
		return Previous->second.Visible ? EELSightResult::Visible : EELSightResult::Occluded;
	}
	if (!ExtendedEntityes::PerceptionBudget().TryRay()) { return EELSightResult::WaitingForBudget; }
	Fvector Center;
	Candidate.Center(Center);
	Fvector Direction = Fvector().sub(Center, Owner.eye_matrix.c);
	const float Range = Direction.magnitude();
	SQuery Query;
	Query.Observer = &Owner;
	Query.Target = &Candidate;
	Query.SkipLeaves = true;
	if (Range > EPS_L)
	{
		Direction.div(Range);
		collide::rq_results Results;
		collide::ray_defs Ray(Owner.eye_matrix.c, Direction, Range, CDB::OPT_FULL_TEST, collide::rqtBoth);
		Level().ObjectSpace.RayQuery(Results, Ray, Inspect, &Query, nullptr, &Owner);
	}
	if (ScentContacts.size() >= 1024)
	{
		for (auto It = ScentContacts.begin(); It != ScentContacts.end();)
		{
			if (Now - It->second.CheckedAt >= 250u) { It = ScentContacts.erase(It); }
			else { ++It; }
		}
	}
	if (ScentContacts.size() < 1024 || ScentContacts.find(Key) != ScentContacts.end())
	{
		ScentContacts[Key] = {Now, OwnerGeneration, TargetGeneration, Until, Radius, Query.IsVisible};
	}
	if (CELSharedDebug::Instance().CanLogCombat(Owner.Position()) && s32(Now - Owner.NextSightTraceAt) >= 0)
	{
		Owner.NextSightTraceAt = Now + 1000u;
		Msg("[monsters_debug] SIGHT t=%u id=%u candidate=%u result=%s radius=%.1f remaining_ms=%u", Now, Owner.ID(), Candidate.ID(), Query.IsVisible ? "scent_contact" : "scent_blocked_by_geometry", Radius, Until - Now);
	}
	return Query.IsVisible ? EELSightResult::Visible : EELSightResult::Occluded;
}

bool CELSharedSight::HasScentContact(CBaseMonster& Owner, const CEntityAlive& Candidate)
{
	using namespace ExtendedEntityesSight;
	const auto Found = ScentContacts.find({Owner.ID(), Candidate.ID()});
	if (Found == ScentContacts.end()) { return false; }
	const SScentContact& Contact = Found->second;
	const u32 Now = Device.dwTimeGlobal;
	return Candidate.g_Alive() && !Candidate.getDestroy() && !ExtendedEntityes::IsIgnored(Candidate) && CELSharedEnvironment::CanSmell() && Contact.Visible && Now - Contact.CheckedAt < 250u && s32(Contact.Until - Now) > 0 && Contact.OwnerGeneration == ExtendedEntityes::OwnerGeneration(Owner.ID()) && Contact.TargetGeneration == ExtendedEntityes::OwnerGeneration(Candidate.ID()) && Owner.Position().distance_to_sqr(Candidate.Position()) <= _sqr(Contact.Radius);
}
