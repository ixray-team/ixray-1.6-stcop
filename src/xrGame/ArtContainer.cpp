#include "stdafx.h"
#include "ArtContainer.h"
#include "Artefact.h"
#include "Level.h"
#include "Actor.h"

CArtContainerComponent::CArtContainerComponent()
{
    m_iContainerSize = 1;
    m_sArtefactsInside.clear();
}

CArtContainerComponent::~CArtContainerComponent()
{
}

void CArtContainerComponent::Load(str_c section)
{
    m_iContainerSize = pSettings->r_s32(section, "container_size");
}

void CArtContainerComponent::save(NET_Packet& packet)
{
}

void CArtContainerComponent::load(IReader& packet)
{
}

void CArtContainerComponent::Serialize(ISaveObject& Object)
{
    BEGIN_CHUNK(Object,"CArtContainer")
    {
    }
}

bool CArtContainerComponent::CanStoreArt(CArtefact* art) const
{
    return true;
}

void CArtContainerComponent::PutArtefactToContainer(CArtefact* artefact)
{
    m_sArtefactsInside.emplace_back(artefact);
}

void CArtContainerComponent::TakeArtefactFromContainer(CArtefact* artefact)
{
    std::erase(m_sArtefactsInside, artefact);
}

u32 CArtContainerComponent::Cost() const
{
    u32 res = 0;

    for (const auto& artefact : m_sArtefactsInside){
        res += artefact->Cost();
    }

    return res;
}

float CArtContainerComponent::Weight() const
{
    float res = 0;

    for (const auto& artefact : m_sArtefactsInside)
    {
        res += artefact->Weight();
    }

    return res;
}

float CArtContainerComponent::GetHealthPower() const
{
    float res = 0;

    for (const auto& artefact : m_sArtefactsInside)
    {
        res += artefact->GetHealthPower();
    }

    return res;
}

float CArtContainerComponent::GetRadiationPower() const
{
    float res = 0;

    for (const auto& artefact : m_sArtefactsInside)
    {
        res += artefact->GetHealthPower();
    }

    return res;
}

float CArtContainerComponent::GetSatietyPower() const
{
    float res = 0;

    for (const auto& artefact : m_sArtefactsInside)
    {
        res += artefact->GetSatietyPower();
    }

    return res;
}

float CArtContainerComponent::GetThirstPower() const
{
    float res = 0;

    for (const auto& artefact : m_sArtefactsInside)
    {
        res += artefact->GetThirstPower();
    }

    return res;
}

float CArtContainerComponent::GetPowerPower() const
{
    float res = 0;

    for (const auto& artefact : m_sArtefactsInside)
    {
        res += artefact->GetPowerPower();
    }

    return res;
}

float CArtContainerComponent::GetBleedingPower() const
{
    float res = 0;

    for (const auto& artefact : m_sArtefactsInside)
    {
        res += artefact->GetBleedingPower();
    }

    return res;
}

float CArtContainerComponent::GetJumpPower() const
{
    float res = 0;

    for (const auto& artefact : m_sArtefactsInside)
    {
        res += artefact->GetJumpPower();
    }

    return res;
}

float CArtContainerComponent::GetWalkPower() const
{
    float res = 0;

    for (const auto& artefact : m_sArtefactsInside)
    {
        res += artefact->GetWalkPower();
    }

    return res;
}

float CArtContainerComponent::GetImmunity(ALife::EHitType hit_type) const
{
    float res = 0;

    for (const auto& artefact : m_sArtefactsInside)
    {
        res += artefact->GetImmunity(hit_type);
    }

    return res;
}

float CArtContainerComponent::AffectHit(float Power, ALife::EHitType hit_type) const
{
    for (const auto& artefact : m_sArtefactsInside)
    {
        Power = artefact->AffectHit(Power, hit_type);
    }

    return Power;
}

float CArtContainerComponent::AdditionalInventoryWeight() const
{
    float res = 0;

    for (const auto& artefact : m_sArtefactsInside)
    {
        res += artefact->AdditionalInventoryWeight();
    }

    return res;
}
