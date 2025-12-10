#include "stdafx.h"
#include "ArtCombiner.h"

#include "Level.h"

CArtCombiner::CArtCombiner()
{
	Container = xr_make_unique<CArtContainerComponent>();
}

void CArtCombiner::Load(LPCSTR section)
{
	CArtefact::Load(section);
	Container->Load(section);
}

bool CArtCombiner::net_Spawn(CSE_Abstract* DC)
{
	auto Casted = smart_cast<CSE_ALifeItemArtefactCombiner*>(DC);
	VERIFY(DC);
	for (auto Child : Casted->children)
	{
		auto ChildObj = Level().Objects.net_Find(Child);
		CArtefact* ChildArt = nullptr;
		if (I_ASSERT(ChildObj))
		{
			ChildArt = ChildObj->cast_artefact();
		}
		if (I_ASSERT(ChildArt))
		{
			Container->PutArtefactToContainer(ChildArt);
		}
	}
	
	return CArtefact::net_Spawn(DC);
}

void CArtCombiner::save(NET_Packet& output_packet)
{
	CArtefact::save(output_packet);
	Container->save(output_packet);
}

void CArtCombiner::load(IReader& input_packet)
{
	CArtefact::load(input_packet);
	Container->load(input_packet);
}

void CArtCombiner::Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object,"CArtCombiner")
	{
		CArtefact::Serialize(Object);
		Container->Serialize(Object);
	}
}

u32 CArtCombiner::Cost() const
{
	auto res = CArtefact::Cost();
	res += Container->Cost();
	return res;
}

float CArtCombiner::Weight() const
{
	auto res = CArtefact::Weight();
	res += Container->Weight();
	return res;
}

/*bool CArtCombiner::CheckInventoryIconItemSimilarity(CInventoryItem* other)
{
	if (!inherited::CheckInventoryIconItemSimilarity(other))
	{
		return false;
	}
	auto comb = smart_cast<CArtCombiner*>(other);
	VERIFY(comb);
	if(comb->m_sArtefactsInside.size() != m_sArtefactsInside.size())
	{
		return false;
	}
	xr_hash_set<xr_string> sections;
	sections.reserve(m_sArtefactsInside.size() * 2);
	for (const auto& art : m_sArtefactsInside) {
		sections.insert(art->m_section_id.c_str());
	}
	for (const auto& art : comb->m_sArtefactsInside) {
		sections.insert(art->m_section_id.c_str());
	}
	return sections.size() == m_sArtefactsInside.size();
}*/

float CArtCombiner::GetHealthPower() const
{
	auto base = CArtefact::GetHealthPower();
	base += Container->GetHealthPower();
	return base;
}

float CArtCombiner::GetRadiationPower() const
{
	auto base = CArtefact::GetRadiationPower();
	base += Container->GetRadiationPower();
	return base;
}

float CArtCombiner::GetSatietyPower() const
{
	auto base = CArtefact::GetSatietyPower();
	base += Container->GetSatietyPower();
	return base;
}

float CArtCombiner::GetPowerPower() const
{
	auto base = CArtefact::GetPowerPower();
	base += Container->GetPowerPower();
	return base;
}

float CArtCombiner::GetBleedingPower() const
{
	auto base = CArtefact::GetBleedingPower();
	base += Container->GetBleedingPower();
	return base;
}

float CArtCombiner::AdditionalInventoryWeight() const
{
	auto base = CArtefact::AdditionalInventoryWeight();
	base += Container->AdditionalInventoryWeight();
	return base;
}

float CArtCombiner::GetJumpPower() const
{
	auto base = CArtefact::GetJumpPower();
	base += Container->GetJumpPower();
	return base;
}

float CArtCombiner::GetWalkPower() const
{
	auto base = CArtefact::GetWalkPower();
	base += Container->GetWalkPower();
	return base;
}

float CArtCombiner::GetImmunity(ALife::EHitType hit_type)
{
	auto base = m_ArtefactHitImmunities.GetHitImmunity(hit_type);
	base += Container->GetImmunity(hit_type);
	return base;
}

float CArtCombiner::AffectHit(float power, ALife::EHitType hit_type)
{
	auto base = CArtefact::AffectHit(power, hit_type);
	base += Container->AffectHit(power, hit_type);
	return base;
}
