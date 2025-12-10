#pragma once
#include "ArtContainer.h"
#include "Artefact.h"
class CArtCombiner final :
    public CArtefact
{
	typedef CArtefact inherited;

	xr_unique_ptr<CArtContainerComponent> Container;

public:
	CArtCombiner();

	void Load(LPCSTR section) override;
	bool net_Spawn(CSE_Abstract* DC) override;

	virtual void save(NET_Packet& output_packet) override;
	virtual void load(IReader& input_packet) override;
	virtual void Serialize(ISaveObject& Object) override;

	u32 Cost() const override;
	float Weight() const override;

	//virtual bool CheckInventoryIconItemSimilarity(CInventoryItem* other) override; // Not in use yet

	virtual float GetHealthPower() const override;
	virtual float GetRadiationPower() const override;
	virtual float GetSatietyPower() const override;
	virtual float GetPowerPower() const override;
	virtual float GetBleedingPower() const override;
	virtual float AdditionalInventoryWeight() const override;
	virtual float GetJumpPower() const override;
	virtual float GetWalkPower() const override;

	virtual float GetImmunity(ALife::EHitType hit_type) override;

	virtual float AffectHit(float power, ALife::EHitType hit_type) override;

};

