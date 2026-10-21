#pragma once
class IELAdapter;
struct SELFlyingSettings;

class CELFlyingAttackAnimation final
{
public:
	CELFlyingAttackAnimation(IELAdapter& OwnerAdapter, const SELFlyingSettings& FlightSettings);
	void Fly();
	void Attack();
	void Rest();
	void Reset();

private:
	void Play(const shared_str& Cycle);
	void SetGrounded(bool Enabled);
	IELAdapter& Adapter;
	const SELFlyingSettings& Settings;
	shared_str CurrentCycle;
};
