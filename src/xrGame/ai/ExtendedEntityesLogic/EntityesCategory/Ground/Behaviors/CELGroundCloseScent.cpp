#include "stdafx.h"
#include "CELGroundCloseScent.h"
#include "../../../Interfaces/IELAdapter.h"
#include "../../../Perceptions/CELSharedMemory.h"
#include "../../../Perceptions/CELSharedPerceptionProfiles.h"
#include "../../../Debug/Debug.h"
#include "../../../EntryPoint.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "ai_object_location.h"

void CELGroundCloseScent::See(IELAdapter& Adapter, CEntityAlive& Target, u32 Time, const SELPerceptionProfile& Profile)
{
	if (!Profile.CanUseCloseScent || !Profile.CloseScentTime || Profile.CloseScentRadius <= 0.f ||
		Adapter.Monster().Position().distance_to_sqr(Target.Position()) > Profile.CloseScentRadius * Profile.CloseScentRadius)
	{
		return;
	}
	if (Contacts.size() < 32 || Contacts.find(Target.ID()) != Contacts.end())
	{
		Contacts[Target.ID()] = {Time, ExtendedEntityes::OwnerGeneration(Target.ID())};
	}
}

void CELGroundCloseScent::Update(IELAdapter& Adapter, CELSharedMemory& Memory, const SELPerceptionProfile& Profile)
{
	for (auto It = Contacts.begin(); It != Contacts.end();)
	{
		CEntityAlive* Target = ExtendedEntityes::FindEntity(It->first);
		if (!Profile.CanUseCloseScent || !Target || !Adapter.IsEnemy(*Target) ||
			ExtendedEntityes::OwnerGeneration(It->first) != It->second.Generation ||
			Device.dwTimeGlobal - It->second.SeenAt >= Profile.CloseScentTime ||
			Adapter.Monster().Position().distance_to_sqr(Target->Position()) > Profile.CloseScentRadius * Profile.CloseScentRadius)
		{
			It = Contacts.erase(It);
			continue;
		}
		const SELObservation* Known = Memory.Find(Target->ID());
		// A same-tick visual confirmation always keeps precedence over scent.
		if (Known && Known->ObservedAt < Device.dwTimeGlobal)
		{
			SELObservation Observation;
			Observation.TargetIdentifier = Target->ID();
			Observation.Position = Target->Position();
			Observation.VertexIdentifier = Target->ai_location().level_vertex_id();
			Observation.Source = EELObservationSource::CloseScent;
			Observation.ObservedAt = Device.dwTimeGlobal;
			Memory.Remember(Observation, Device.dwTimeGlobal, Profile.MemoryMax, Profile.CanRecordTrail ? Profile.TrailLength : 0.f);
		}
		++It;
	}
}

void CELGroundCloseScent::FillDebug(SELDebugRecord& Record, const SELPerceptionProfile& Profile) const
{
	for (const auto& Contact : Contacts)
	{
		const s32 Remaining = s32(Contact.second.SeenAt + Profile.CloseScentTime - Device.dwTimeGlobal);
		if (Remaining > 0)
		{
			Record.ScentRadius = Profile.CloseScentRadius;
			string256 Label;
			xr_sprintf(Label, "БЛИЖНЕЕ ЧУТЬЁ: #%u | %.1f м | осталось %.1f с", Contact.first, Profile.CloseScentRadius, float(Remaining) * .001f);
			Record.Senses.push_back(Label);
		}
	}
}

void CELGroundCloseScent::Remove(u16 Target)
{
	Contacts.erase(Target);
}
