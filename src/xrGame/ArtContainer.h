#pragma once
#include "../xrCore/Save/SaveObject.h"

class CArtefact;

class CArtContainerComponent
{

protected:
	size_t					m_iContainerSize;
	xr_vector<CArtefact*>	m_sArtefactsInside;

public:

	CArtContainerComponent(void);
	~CArtContainerComponent(void);

	void Load(LPCSTR section);

	void save(NET_Packet& output_packet);
	void load(IReader& input_packet);

	void Serialize(ISaveObject& Object);

	u32 Cost() const;
	float Weight() const;

	size_t GetContainerSize() const { return m_iContainerSize; }
	void SetContainerSize(size_t new_size) { m_iContainerSize = new_size; }
	const xr_vector<CArtefact*>& GetArtefactsInside() const { return m_sArtefactsInside; }
	xr_vector<CArtefact*>& GetArtefactsInside() { return m_sArtefactsInside; }
	bool IsFull() const { return m_sArtefactsInside.size() >= m_iContainerSize; }

	bool CanStoreArt(CArtefact* art) const;

	void PutArtefactToContainer(CArtefact* artefact);
	void TakeArtefactFromContainer(CArtefact* artefact);

	float GetHealthPower() const;
	float GetRadiationPower() const;
	float GetSatietyPower() const;
	float GetThirstPower() const;
	float GetPowerPower() const;
	float GetBleedingPower() const;
	
	float GetJumpPower() const;
	float GetWalkPower() const;
	float GetImmunity(ALife::EHitType hit_type) const;
	float AffectHit(float Power, ALife::EHitType hit_type) const;
	float AdditionalInventoryWeight() const;
};

