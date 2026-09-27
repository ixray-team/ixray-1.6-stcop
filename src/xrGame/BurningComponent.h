#pragma once

class TBurningComponent
{
public:
	
	void Load(str_c SectionName);
	void AddBurning(float TickDamage, float DamageDecreasing, CEntityAlive* NewInitiator);
	void Update(u32 dt, CEntityAlive* Owner);
	IC bool IsBurning() const { return CurrentTickDamage > EPS && CurrentDamageDecreasing > EPS; }
	IC CEntityAlive* GetInitiator() const { return LastInitiator; }
	
private:
	xr_vector<ref_sound> m_burning_sounds_variants;
	CEntityAlive* LastInitiator = nullptr;
	shared_str m_burning_fx = "damage_fx\\burn_creatures00";
	float CurrentTickDamage = 0.0f;
	float CurrentDamageDecreasing = FLT_MAX;
	s32 TimeUntilUpdate = 0.0f;

	//CGameObject* GetComponentOwner() const;
	//mutable CGameObject* ComponentOwner = nullptr;

	ECS_COMPONENT(TBurningComponent)
		ECS_VALUE(CurrentTickDamage, "CurrentTickDamage");
		ECS_VALUE(CurrentDamageDecreasing, "CurrentDamageDecreasing");
	ECS_END
};