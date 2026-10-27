#pragma once

#include "inventory_item_object.h"
#include "../xrEngine/Feel_Touch.h"
#include "HudSound.h"

class CAnomalyZone;
class CParticlesObject;

class CLegacyDetector final : public CInventoryItemObject, public Feel::Touch
{
	using inherited = CInventoryItemObject;

	struct ZoneType
	{
		float MinFrequency = 0.0f;
		float MaxFrequency = 0.0f;
		HUD_SOUND_ITEM Sound;
		shared_str MapLocation;
	};

	struct ZoneInfo
	{
		u32 SoundTime = 0;
		xr_shared_ptr<CParticlesObject> Particle;
		bool MapLocationAdded = false;
	};

	xr_map<CLASS_ID, ZoneType> ZoneTypes;
	xr_map<CAnomalyZone*, ZoneInfo> Zones;
	shared_str NightVisionParticle;
	float DetectionRadius = 0.0f;
	u32 DetectorType = 1;
	bool Working = false;

	void SetWorking(bool Enabled);
	void ClearZones();
	void UpdateNightVision();
	void RemoveZone(CAnomalyZone* Zone, ZoneInfo& Info);
	bool IsZoneVisible(CAnomalyZone* Zone) const;

public:
	~CLegacyDetector() override;
	void Load(const char* Section) override;
	void net_Destroy() override;
	void shedule_Update(u32 Delta) override;
	void UpdateCL() override;
	void OnH_B_Independent(bool JustBeforeDestroy) override;
	void OnMoveToSlot(const SInvItemPlace& Previous) override;
	void OnMoveToBelt(const SInvItemPlace& Previous) override;
	void OnMoveToRuck(const SInvItemPlace& Previous) override;
	u32 ef_detector_type() const override { return DetectorType; }
	bool IsWorking() const { return Working; }

	bool feel_touch_contact(CObject* Object) override;
	void feel_touch_new(CObject* Object) override;
	void feel_touch_delete(CObject* Object) override;
};
